% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities_cache.R
\name{LRUCache}
\alias{LRUCache}
\title{Class that implements LRU cache with expiring items.}
\description{
LRU caches remove items in a FIFO manner, such that the oldest
             items to be used are the first to be removed.
             If you attempt to retrieve a cache item that is older than the
             expiration time, the item will be invalidated.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{Element}}{Class describes the values in the cache}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-LRUCache-new}{\code{LRUCache$new()}}
\item \href{#method-LRUCache-clear}{\code{LRUCache$clear()}}
\item \href{#method-LRUCache-get}{\code{LRUCache$get()}}
\item \href{#method-LRUCache-put}{\code{LRUCache$put()}}
\item \href{#method-LRUCache-clone}{\code{LRUCache$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LRUCache-new"></a>}}
\if{latex}{\out{\hypertarget{method-LRUCache-new}{}}}
\subsection{Method \code{new()}}{
Initialize an ``LRUCache`` instance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LRUCache$new(max_cache_items, expiration_horizon, retrieval_function)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{max_cache_items}}{(int): Maximum number of items to store in cache.}

\item{\code{expiration_horizon}}{(datetime.timedelta): Maximum time duration a cache element can
persist before being invalidated.}

\item{\code{retrieval_function}}{(Callable[[KeyType, ValType], ValType]): Function which maps cache
keys and current values to new values. This function must have kwarg arguments
``key`` and ``value``. This function is called as a fallback when the key
is not found in the cache, or a key has expired.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LRUCache-clear"></a>}}
\if{latex}{\out{\hypertarget{method-LRUCache-clear}{}}}
\subsection{Method \code{clear()}}{
Deletes all elements from the cache.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LRUCache$clear()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LRUCache-get"></a>}}
\if{latex}{\out{\hypertarget{method-LRUCache-get}{}}}
\subsection{Method \code{get()}}{
Returns value corresponding to key in cache.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LRUCache$get(key, data_source_fallback = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{(KeyType): Key in cache to retrieve.}

\item{\code{data_source_fallback}}{(Optional[bool]): True if data should be retrieved if
it's stale or not in cache. Default: True.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LRUCache-put"></a>}}
\if{latex}{\out{\hypertarget{method-LRUCache-put}{}}}
\subsection{Method \code{put()}}{
Adds key to cache using ``retrieval_function``.
             If value is provided, this is used instead. If the key is already in cache,
             the old element is removed. If the cache size exceeds the size limit, old
             elements are removed in order to meet the limit.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LRUCache$put(key, value = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{(KeyType): Key in cache to retrieve.}

\item{\code{value}}{(Optional[ValType]): Value to store for key. Default: None.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LRUCache-clone"></a>}}
\if{latex}{\out{\hypertarget{method-LRUCache-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LRUCache$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
