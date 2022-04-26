# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/jumpstart/cache.py


KeyType = TypeVar("KeyType")
ValType = TypeVar("ValType")

#' @title Class that implements LRU cache with expiring items.
#' @description LRU caches remove items in a FIFO manner, such that the oldest
#'              items to be used are the first to be removed.
#'              If you attempt to retrieve a cache item that is older than the
#'              expiration time, the item will be invalidated.
#' @export
LRUCache = R6Class("LRUCache",
  public = list(

    #' @description Class describes the values in the cache.
    #'              This object stores the value itself as well as a timestamp so that this
    #'              element can be invalidated if it becomes too old.
    Element = R6Class("Element",
      public = list(

        #' @description Initialize an ``Element`` instance for ``LRUCache``.
        #' @param value (ValType): Value that is stored in cache.
        #' @param creation_time (datetime.datetime): Time at which cache item was created.
        initialize = function(value, creation_time){
          self$value = value
          self$creation_time = creation_time
        }
      ),
      lock_objects = F
    ),

    #' @description Initialize an ``LRUCache`` instance.
    #' @param max_cache_items (int): Maximum number of items to store in cache.
    #' @param expiration_horizon (datetime.timedelta): Maximum time duration a cache element can
    #'              persist before being invalidated.
    #' @param retrieval_function (Callable[[KeyType, ValType], ValType]): Function which maps cache
    #'              keys and current values to new values. This function must have kwarg arguments
    #'              ``key`` and ``value``. This function is called as a fallback when the key
    #'              is not found in the cache, or a key has expired.
    initialize = function(max_cache_items,
                          expiration_horizon,
                          retrieval_function){
      private$.max_cache_items = max_cache_items
      private$.lru_cache=list()
      private$.expiration_horizon = expiration_horizon
      private$.retrieval_function = retrieval_function
    },

    #' @description Deletes all elements from the cache.
    clear = function(){
      private$.lru_cache = list()
    },

    #' @description Returns value corresponding to key in cache.
    #' @param key (KeyType): Key in cache to retrieve.
    #' @param data_source_fallback (Optional[bool]): True if data should be retrieved if
    #'              it's stale or not in cache. Default: True.
    get = function(key,
                   data_source_fallback=TRUE){
      if (data_source_fallback){
        if (key %in% names(private$.lru_cache)){
          return(private$.get_item(key, FALSE))
        }
        self$put(key)
        return(private$.get_item(key, FALSE))
      }
      return(private$.get_item(key, TRUE))
    },

    #' @description Adds key to cache using ``retrieval_function``.
    #'              If value is provided, this is used instead. If the key is already in cache,
    #'              the old element is removed. If the cache size exceeds the size limit, old
    #'              elements are removed in order to meet the limit.
    #' @param key (KeyType): Key in cache to retrieve.
    #' @param value (Optional[ValType]): Value to store for key. Default: None.
    put = function(key,
                   value = NULL){
      curr_value = NULL
      if (key %in% names(private$.lru_cache)){
        curr_value = private$.lru_cache[[key]]
        private$.lru_cache[[key]] = NULL
      }

      while (length(private$.lru_cache) >= private$.max_cache_items){
        len = length(private$.lru_cache)
        private$.lru_cache[[len]] = NULL
      }

      if (is.null(value)){
        value = private$.retrieval_function(  # type: ignore
          key=key, value= if (!is.null(curr_value)) curr_value$element else NULL
        )
      }

      private$.lru_cache[[key]] = self$Element$new(
        value=value, creation_time= tz_now_utc()
      )
    }
  ),
  private = list(
    .max_cache_items = 0L,
    .lru_cache = NULL,
    .expiration_horizon = NULL,
    .retrieval_function = NULL,

    # Returns value from cache corresponding to key.
    # If ``fail_on_old_value``, a KeyError is raised instead of a new value
    # getting fetched.
    # Args:
    #   key (KeyType): Key in cache to retrieve.
    # fail_on_old_value (bool): True if a KeyError is raised when the cache value
    # is old.
    # Raises:
    #   KeyError: If key is not in cache or if key is old in cache
    # and fail_on_old_value is True.
    .get_item = function(key,
                         fail_on_old_value){
      tryCatch({
        element = private$.lru_cache[[key]]
        private$.lru_cache[[key]] = NULL
        curr_time = tz_now_utc()
        element_age = curr_time - element$creation_time
        if (element_age > private$.expiration_horizon){
          if (fail_on_old_value){
            KeyError$new(
              sprintf("%s has aged beyond allowed time %s. ", key, private$.expiration_horizon),
              sprintf("Element created at %s.", element$creation_time)
            )
          }
          element$value = private$.retrieval_function(
            key=key, value=element$value
          )
          element$creation_time = curr_time
        }
        private$.lru_cache[[key]] = element
        return(element$value)
      }, KeyError = function(e)
        KeyError$new(sprintf("%s not found in LRUCache!", key))
      )
    }
  )
)


# Returns number of elements in cache.
#' @export
length.LRUCache = function(x){
  length(LRUCache$.__enclos_env__$private$.lru_cache)
}
