// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Webhook rate limiting
//!
//! Sliding-window per-IP rate limiter applied to all webhook ingress paths.
//! The window is always 60 seconds; the limit is configurable via
//! `[server] rate_limit_rpm` (requests per minute). When the limit is
//! exceeded the handler returns 429 Too Many Requests with a `Retry-After`
//! header.
//!
//! No external crates: uses a `Mutex<HashMap<IpAddr, VecDeque<Instant>>>`
//! with lock time bounded by the dequeue sweep. Under webhook load (at most
//! a few hundred requests/minute per IP) this is negligible. Idle peers are
//! swept out as the map grows, so its size follows recent traffic.
//!
//! The key is the TCP peer address. Forwarding headers are not trusted, so
//! behind a reverse proxy every client shares the proxy's budget.

use std::collections::{HashMap, VecDeque};
use std::net::IpAddr;
use std::sync::Mutex;
use std::time::{Duration, Instant};

use axum::{
    body::Body,
    extract::{ConnectInfo, State},
    http::{Request, StatusCode},
    middleware::Next,
    response::{IntoResponse, Response},
};
use std::net::SocketAddr;

use super::webhooks::AppState;

// ConnectInfo and SocketAddr are used in the middleware body via
// request.extensions().get::<ConnectInfo<SocketAddr>>().

/// Map size below which idle peers are never swept.
const SWEEP_FLOOR: usize = 4096;

/// Per-peer request times, plus the map size at which idle peers are next
/// swept out.
#[derive(Default)]
struct LimiterState {
    peers: HashMap<IpAddr, VecDeque<Instant>>,
    sweep_at: usize,
}

/// Sliding-window per-IP rate limiter (60-second window).
///
/// Memory is bounded by recent traffic, not by every address ever seen: when
/// the map grows past `max(SWEEP_FLOOR, 2 × size after the last sweep)`,
/// peers with no request inside the window are dropped. Sweeps are therefore
/// amortised O(1) per request.
pub struct WebhookRateLimiter {
    state: Mutex<LimiterState>,
    window: Duration,
    limit: u32,
}

impl WebhookRateLimiter {
    pub fn new(requests_per_minute: u32) -> Self {
        Self::with_window(requests_per_minute, Duration::from_secs(60))
    }

    fn with_window(requests_per_minute: u32, window: Duration) -> Self {
        Self {
            state: Mutex::new(LimiterState {
                peers: HashMap::new(),
                sweep_at: SWEEP_FLOOR,
            }),
            window,
            limit: requests_per_minute,
        }
    }

    /// Returns `true` if the request is within the allowed rate.
    pub fn check_ip(&self, ip: IpAddr) -> bool {
        let now = Instant::now();
        let cutoff = now.checked_sub(self.window).unwrap_or(now);
        let mut state = self.state.lock().expect("rate limiter mutex poisoned");

        if state.peers.len() >= state.sweep_at {
            state
                .peers
                .retain(|_, times| times.back().is_some_and(|&t| t >= cutoff));
            state.sweep_at = SWEEP_FLOOR.max(state.peers.len().saturating_mul(2));
        }

        let timestamps = state.peers.entry(ip).or_default();

        // Evict entries older than the window
        while timestamps.front().map(|&t| t < cutoff).unwrap_or(false) {
            timestamps.pop_front();
        }

        if (timestamps.len() as u32) < self.limit {
            timestamps.push_back(now);
            true
        } else {
            false
        }
    }

    #[cfg(test)]
    fn tracked_peers(&self) -> usize {
        self.state
            .lock()
            .expect("rate limiter mutex poisoned")
            .peers
            .len()
    }

    pub fn limit(&self) -> u32 {
        self.limit
    }
}

/// Axum middleware: reject requests over the per-IP limit with 429.
///
/// Applied only to webhook routes. Health and metrics endpoints are
/// intentionally excluded so monitoring systems are never blocked.
///
/// Peer address is read from request extensions rather than using the
/// `ConnectInfo` extractor directly, so the middleware degrades gracefully
/// in test environments that don't call `into_make_service_with_connect_info`.
pub async fn rate_limit_middleware(
    State(state): State<AppState>,
    request: Request<Body>,
    next: Next,
) -> Response {
    if let Some(ref limiter) = state.rate_limiter {
        // Extract ConnectInfo from extensions (present only when
        // axum::serve is called with into_make_service_with_connect_info).
        // If absent (test environment), skip rate limiting rather than failing.
        let peer_ip = request
            .extensions()
            .get::<ConnectInfo<SocketAddr>>()
            .map(|ci| ci.0.ip());

        if let Some(ip) = peer_ip {
            if !limiter.check_ip(ip) {
                tracing::warn!(
                    %ip,
                    limit = limiter.limit(),
                    "Webhook rate limit exceeded"
                );
                let limit_str = limiter.limit().to_string();
                return (
                    StatusCode::TOO_MANY_REQUESTS,
                    [
                        ("Retry-After", "60"),
                        ("X-RateLimit-Limit", limit_str.as_str()),
                        ("X-RateLimit-Window", "60"),
                    ],
                    "Rate limit exceeded — too many webhook requests from this IP",
                )
                    .into_response();
            }
        }
    }
    next.run(request).await
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::{IpAddr, Ipv4Addr};

    #[test]
    fn test_allows_requests_within_limit() {
        let limiter = WebhookRateLimiter::new(5);
        let ip = IpAddr::V4(Ipv4Addr::new(1, 2, 3, 4));
        for _ in 0..5 {
            assert!(limiter.check_ip(ip), "should allow up to the limit");
        }
    }

    #[test]
    fn test_blocks_over_limit() {
        let limiter = WebhookRateLimiter::new(3);
        let ip = IpAddr::V4(Ipv4Addr::new(10, 0, 0, 1));
        for _ in 0..3 {
            limiter.check_ip(ip);
        }
        assert!(!limiter.check_ip(ip), "4th request should be blocked");
    }

    #[test]
    fn test_different_ips_are_independent() {
        let limiter = WebhookRateLimiter::new(2);
        let ip1 = IpAddr::V4(Ipv4Addr::new(1, 1, 1, 1));
        let ip2 = IpAddr::V4(Ipv4Addr::new(2, 2, 2, 2));

        limiter.check_ip(ip1);
        limiter.check_ip(ip1);
        // ip1 is now at limit, ip2 should still be free
        assert!(!limiter.check_ip(ip1), "ip1 should be blocked");
        assert!(limiter.check_ip(ip2), "ip2 should still be allowed");
    }

    fn nth_ip(i: usize) -> IpAddr {
        IpAddr::V6(std::net::Ipv6Addr::from(0x2001_0db8_u128 << 96 | i as u128))
    }

    #[test]
    fn idle_peers_are_swept_once_the_map_is_large() {
        let limiter = WebhookRateLimiter::with_window(5, Duration::from_millis(1));
        for i in 0..SWEEP_FLOOR {
            assert!(limiter.check_ip(nth_ip(i)));
        }
        assert_eq!(limiter.tracked_peers(), SWEEP_FLOOR);
        std::thread::sleep(Duration::from_millis(5));
        // Every earlier peer is now idle; the next new peer triggers a sweep.
        assert!(limiter.check_ip(nth_ip(SWEEP_FLOOR)));
        assert_eq!(limiter.tracked_peers(), 1);
    }

    #[test]
    fn active_peers_survive_a_sweep_and_stay_limited() {
        let limiter = WebhookRateLimiter::new(2);
        let busy = IpAddr::V4(Ipv4Addr::new(10, 0, 0, 9));
        assert!(limiter.check_ip(busy));
        assert!(limiter.check_ip(busy));
        for i in 0..SWEEP_FLOOR {
            limiter.check_ip(nth_ip(i));
        }
        // A sweep has run (the map reached the floor) inside the window, so
        // nothing was idle and `busy` is still at its limit.
        assert!(limiter.tracked_peers() > SWEEP_FLOOR);
        assert!(
            !limiter.check_ip(busy),
            "a sweep must not reset an active peer"
        );
    }
}
