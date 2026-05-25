// SPDX-License-Identifier: MPL-2.0
//! Caching utilities — faithful port of `utils/cache.py` (a TTL cache and
//! an LRU query cache). Kept for parity; hand-rolled to avoid extra
//! dependencies.

use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Time-to-live cache with a maximum size. Mirrors `cachetools.TTLCache`
/// usage in `CacheManager`.
pub struct TtlCache<V> {
    map: HashMap<String, (V, Instant)>,
    order: Vec<String>,
    maxsize: usize,
    ttl: Duration,
}

impl<V: Clone> TtlCache<V> {
    pub fn new(maxsize: usize, ttl_secs: u64) -> Self {
        Self {
            map: HashMap::new(),
            order: Vec::new(),
            maxsize,
            ttl: Duration::from_secs(ttl_secs),
        }
    }

    fn expired(&self, at: Instant) -> bool {
        at.elapsed() >= self.ttl
    }

    pub fn get(&mut self, key: &str) -> Option<V> {
        let drop = match self.map.get(key) {
            Some((_, at)) if self.expired(*at) => true,
            Some((v, _)) => return Some(v.clone()),
            None => return None,
        };
        if drop {
            self.map.remove(key);
            self.order.retain(|k| k != key);
        }
        None
    }

    pub fn set(&mut self, key: String, value: V) {
        if !self.map.contains_key(&key) {
            if self.order.len() >= self.maxsize {
                if let Some(old) = self.order.first().cloned() {
                    self.order.remove(0);
                    self.map.remove(&old);
                }
            }
            self.order.push(key.clone());
        }
        self.map.insert(key, (value, Instant::now()));
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.order.clear();
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

/// Least-recently-used cache. Mirrors `cachetools.LRUCache` usage in
/// `QueryCache`.
pub struct LruCache<V> {
    map: HashMap<String, V>,
    order: Vec<String>,
    maxsize: usize,
}

impl<V: Clone> LruCache<V> {
    pub fn new(maxsize: usize) -> Self {
        Self {
            map: HashMap::new(),
            order: Vec::new(),
            maxsize,
        }
    }

    fn touch(&mut self, key: &str) {
        self.order.retain(|k| k != key);
        self.order.push(key.to_string());
    }

    pub fn get(&mut self, key: &str) -> Option<V> {
        if self.map.contains_key(key) {
            self.touch(key);
            self.map.get(key).cloned()
        } else {
            None
        }
    }

    pub fn set(&mut self, key: String, value: V) {
        if !self.map.contains_key(&key) && self.order.len() >= self.maxsize {
            if let Some(old) = self.order.first().cloned() {
                self.order.remove(0);
                self.map.remove(&old);
            }
        }
        self.map.insert(key.clone(), value);
        self.touch(&key);
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.order.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ttl_cache_evicts_at_maxsize() {
        let mut c: TtlCache<i32> = TtlCache::new(2, 3600);
        c.set("a".into(), 1);
        c.set("b".into(), 2);
        c.set("c".into(), 3);
        assert_eq!(c.len(), 2);
        assert_eq!(c.get("a"), None);
        assert_eq!(c.get("c"), Some(3));
    }

    #[test]
    fn lru_cache_evicts_least_recently_used() {
        let mut c: LruCache<i32> = LruCache::new(2);
        c.set("a".into(), 1);
        c.set("b".into(), 2);
        let _ = c.get("a"); // a now most-recent
        c.set("c".into(), 3); // evicts b
        assert_eq!(c.get("b"), None);
        assert_eq!(c.get("a"), Some(1));
    }
}
