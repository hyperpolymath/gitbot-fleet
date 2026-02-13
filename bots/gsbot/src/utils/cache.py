"""Caching utilities for the Garment Sustainability Bot."""

from functools import wraps
from typing import Callable, Optional, Any
from cachetools import TTLCache, LRUCache
import hashlib
import json

from config.settings import config
from utils.logger import get_logger

logger = get_logger(__name__)


class CacheManager:
    """Manages caching for bot operations."""

    def __init__(self):
        """Initialize cache manager."""
        if config.ENABLE_CACHING:
            self.cache = TTLCache(
                maxsize=config.CACHE_MAXSIZE,
                ttl=config.CACHE_TTL
            )
            logger.info(
                f"Cache initialized: maxsize={config.CACHE_MAXSIZE}, "
                f"ttl={config.CACHE_TTL}s"
            )
        else:
            self.cache = None
            logger.info("Caching disabled")

    def _generate_key(self, func_name: str, args: tuple, kwargs: dict) -> str:
        """
        Generate cache key from function name and arguments.

        Args:
            func_name: Function name
            args: Positional arguments
            kwargs: Keyword arguments

        Returns:
            Cache key string
        """
        # Convert args and kwargs to a hashable representation
        key_data = {
            "func": func_name,
            "args": str(args),
            "kwargs": str(sorted(kwargs.items()))
        }
        key_str = json.dumps(key_data, sort_keys=True)
        return hashlib.md5(key_str.encode()).hexdigest()

    def get(self, key: str) -> Optional[Any]:
        """
        Get value from cache.

        Args:
            key: Cache key

        Returns:
            Cached value or None
        """
        if self.cache is None:
            return None

        try:
            value = self.cache.get(key)
            if value is not None:
                logger.debug(f"Cache hit: {key}")
            return value
        except Exception as e:
            logger.error(f"Cache get error: {e}")
            return None

    def set(self, key: str, value: Any) -> None:
        """
        Set value in cache.

        Args:
            key: Cache key
            value: Value to cache
        """
        if self.cache is None:
            return

        try:
            self.cache[key] = value
            logger.debug(f"Cache set: {key}")
        except Exception as e:
            logger.error(f"Cache set error: {e}")

    def clear(self) -> None:
        """Clear all cached values."""
        if self.cache is not None:
            self.cache.clear()
            logger.info("Cache cleared")

    def get_stats(self) -> dict:
        """
        Get cache statistics.

        Returns:
            Dictionary with cache stats
        """
        if self.cache is None:
            return {"enabled": False}

        return {
            "enabled": True,
            "size": len(self.cache),
            "maxsize": self.cache.maxsize,
            "ttl": self.cache.ttl if hasattr(self.cache, 'ttl') else None,
        }


# Global cache manager instance
cache_manager = CacheManager()


def cached(ttl: Optional[int] = None):
    """
    Decorator to cache function results.

    Args:
        ttl: Time-to-live in seconds (overrides default)

    Usage:
        @cached(ttl=300)
        def expensive_function(arg1, arg2):
            # ... expensive computation
            return result
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        def wrapper(*args, **kwargs):
            if not config.ENABLE_CACHING:
                return func(*args, **kwargs)

            # Generate cache key
            cache_key = cache_manager._generate_key(
                func.__name__,
                args,
                kwargs
            )

            # Try to get from cache
            cached_value = cache_manager.get(cache_key)
            if cached_value is not None:
                return cached_value

            # Compute and cache result
            result = func(*args, **kwargs)
            cache_manager.set(cache_key, result)

            return result

        # Add cache management methods to function
        wrapper.clear_cache = lambda: cache_manager.clear()
        wrapper.cache_stats = lambda: cache_manager.get_stats()

        return wrapper

    return decorator


class QueryCache:
    """Specialized cache for database queries."""

    def __init__(self, maxsize: int = 100):
        """
        Initialize query cache.

        Args:
            maxsize: Maximum number of cached queries
        """
        self.cache = LRUCache(maxsize=maxsize)
        logger.info(f"Query cache initialized: maxsize={maxsize}")

    def get(self, query: str, params: tuple = ()) -> Optional[Any]:
        """
        Get cached query result.

        Args:
            query: SQL query string
            params: Query parameters

        Returns:
            Cached result or None
        """
        key = self._make_key(query, params)
        return self.cache.get(key)

    def set(self, query: str, params: tuple, result: Any) -> None:
        """
        Cache query result.

        Args:
            query: SQL query string
            params: Query parameters
            result: Query result to cache
        """
        key = self._make_key(query, params)
        self.cache[key] = result

    def _make_key(self, query: str, params: tuple) -> str:
        """Create cache key from query and parameters."""
        return hashlib.md5(
            f"{query}:{params}".encode()
        ).hexdigest()

    def clear(self) -> None:
        """Clear query cache."""
        self.cache.clear()
        logger.info("Query cache cleared")


# Global query cache instance
query_cache = QueryCache()
