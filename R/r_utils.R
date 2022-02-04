#' @import R6
#' @importFrom utils getFromNamespace help
#' @importFrom urltools url_parse

# Copied from https://github.com/paws-r/paws/blob/main/examples/s3_multipart_upload.R
# and modified under Apache 2.0.
# See the NOTICE file at the top of this package for attribution.
KB = 1024
MB = KB ^ 2
GB = KB ^ 3

`%||%` <- function(x, y) if (is.null(x)) return(y) else return(x)

get_aws_env <- function(x) {
  x <- Sys.getenv(x)
  if (nchar(x) == 0) return(NULL) else return(x)
}

#' @title Get methods from other packages
#' @description This function allows to use soft dependencies.
#' @keywords internal
#' @param fun function to export
#' @param pkg package to method from
#' @family r_utils
#' @export
pkg_method <- function(fun, pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(fun,' requires the ', pkg,' package, please install it first and try again',
         call. = F)}
  fun_name <- getFromNamespace(fun, pkg)
  return(fun_name)
}

get_profile_name <- pkg_method("get_profile_name", "paws.common")
get_region <- pkg_method("get_region", "paws.common")

#' @title Checks is R6 is a sub class
#' @param subclass (R6):
#' @param cls (R6):
#' @keywords internal
#' @family r_utils
#' @export
IsSubR6Class <- function(subclass, cls) {
  if(is.null(subclass)) return(NULL)
  if (!is.R6Class(subclass))
    stop("subclass is not a R6ClassGenerator.", call. = F)
  parent <- subclass$get_inherit()
  cls %in% c(subclass$classname, IsSubR6Class(parent))
}

#' @title Write large raw connections in chunks
#' @param obj (raw): raw connection or raw vector
#' @param filename (str): file to write raw vector to
#' @keywords internal
#' @family r_utils
#' @export
write_bin <- function(obj,
                      filename) {
  # If R version is 4.0.0 + then use writeBin due to long vector support
  # https://github.com/HenrikBengtsson/Wishlist-for-R/issues/97
  if (getRversion() > R_system_version("4.0.0")){
    writeBin(obj, filename)
  } else {
    # use readr if R version < 4.0.0 for extra speed
    if((!requireNamespace("readr", quietly = TRUE))){
      readr_write_raw(obj, filename)
    } else {
      base_write_loop(obj, filename)
    }
  }
  return(invisible(TRUE))
}

readr_write_raw <- function(obj, filename){
  # to avoid readr trying to unzip files and causing potential errors
  write_file = pkg_method("write_file", "readr")
  pos <- regexpr("\\.([[:alnum:]]+)$", filename)
  parts = (
    if(pos > -1L)
      list(file = substring(filename, 1, pos-1L), ext = substring(filename, pos + 1L))
    else list(file = filename)
  )
  write_file(obj, parts$file)
  file.rename(parts$file, paste(parts, collapse = "."))
}

base_write_loop <- function(obj,
                           filename,
                           chunk_size = (GB*2)-2){
  # Only 2^31 - 1 bytes can be written in a single call
  max_len <- length(obj)
  start <- seq(1, max_len, chunk_size)
  end <- c(start[-1]-1, max_len)
  if (length(start) == 1) {
    writeBin(obj, filename)
  } else {
    # Open for reading and appending.
    con <- file(filename, "a+b")
    on.exit(close(con))
    sapply(seq_along(start), function(i){writeBin(obj[start[i]:end[i]], con)})
  }
}

s3_upload <- function(client,
                      file,
                      bucket,
                      key,
                      # Using 100 MB multipart upload size due to AWS recommendation:
                      # https://docs.aws.amazon.com/AmazonS3/latest/userguide/mpuoverview.html
                      multipart_chunksize = 100 * MB,
                      ...){
  if(5 * MB > multipart_chunksize)
    ValueError$new(paste(
      "`multipart_chunksize` is too small please increase `multipart_chunksize` > 5MB,",
      "https://docs.aws.amazon.com/AmazonS3/latest/userguide/qfacts.html"
    ))
  file_size = file.size(file)
  multipart = file_size > multipart_chunksize
  if(isFALSE(multipart)){
    out <- client$put_object(
      Body = readBin(file, what = "raw", n = file_size),
      Key = key,
      Bucket = bucket,
      ...
    )
  } else {
    LOGGER$debug("Uploading file '%s' in multipart to: 's3://%s'", file, fs::path(bucket, key))
    multipart <- client$create_multipart_upload(
      Bucket = bucket,
      Key = key,
      ...
    )
    tryCatch({
      parts <- s3_upload_multipart_parts(
        client, file, file_size, multipart_chunksize, bucket, key, multipart$UploadId
      )
      client$complete_multipart_upload(
        Bucket = bucket,
        Key = key,
        MultipartUpload = list(Parts = parts),
        UploadId = multipart$UploadId
      )
    },
    error = function(cond){
      client$abort_multipart_upload(
        Bucket = bucket,
        Key = key,
        UploadId = multipart$UploadId
      )
      LOGGER$error("Failed to Upload file in Multiparts")
      stop(cond)
    })
  }
}

s3_upload_multipart_parts <- function(client,
                                      file,
                                      file_size,
                                      multipart_chunksize,
                                      bucket,
                                      key,
                                      upload_id) {
  num_parts <- ceiling(file_size / multipart_chunksize)
  con <- base::file(file, open = "rb")
  on.exit({close(con)})
  parts = lapply(seq_len(num_parts), function(i){
    LOGGER$debug("Upload %s part %s of %s", file, i, num_parts)
    part_resp <- client$upload_part(
      Body = readBin(con, what = "raw", n = multipart_chunksize),
      Bucket = bucket,
      Key = key,
      PartNumber = i,
      UploadId = upload_id
    )
    return(list(ETag = part_resp$ETag, PartNumber = i))
  })
  return(parts)
}

#' @title If api call fails retry call
#' @param expr (code): AWS code to rety
#' @param retry (int): number of retries
#' @keywords internal
#' @family r_utils
#' @export
retry_api_call <- function(expr, retry = 5){

  # if number of retries is equal to 0 then retry is skipped
  if (retry == 0) {
    resp <- tryCatch(eval.parent(substitute(expr)),
                     error = function(e) e)
  }

  for (i in seq_len(retry)) {
    resp <- tryCatch(eval.parent(substitute(expr)),
                     error = function(e) e)

    if(inherits(resp, "http_500")){

      # stop retry if statement is an invalid request
      if (grepl("InvalidRequestException", resp)) {stop(resp)}

      backoff_len <- runif(n=1, min=0, max=(2^i - 1))

      message(resp, "Request failed. Retrying in ", round(backoff_len, 1), " seconds...")

      Sys.sleep(backoff_len)
    } else {break}
  }

  if (inherits(resp, "error")) stop(resp)

  resp
}

#' @title Check if list is empty
#' @param obj (list):
#' @family r_utils
#' @keywords internal
#' @export
islistempty = function(obj) {(is.null(obj) || length(obj) == 0)}

#' @title split string
#' @param str (str): string to split
#' @param split (str): string used for splitting.
#' @family r_utils
#' @keywords internal
#' @export
split_str <- function(str, split = ",") unlist(strsplit(str, split = split))

#' @title Format of R6 classes
#' @param self ([R6::R6Class])
#' @family r_utils
#' @keywords internal
#' @export
format_class <- function(self){
  return(sprintf(
    "<%s at %s>",
    class(self)[1],
    data.table::address(self))
  )
}

#' @title Create Enum "like" environments
#' @param ... (obj): parameters to be create into an Enum like environment
#' @param .class (str):
#' @family r_utils
#' @keywords internal
#' @export
Enum <- function(..., .class=NULL) {
  kwargs = list(...)
  env = list2env(kwargs, parent = emptyenv())
  lockEnvironment(env, bindings = TRUE)
  subclass <- Filter(Negate(is.null) ,c(.class, "Enum"))
  class(env) <- c(subclass, class(env))
  return(env)
}

#' @export
print.Enum <- function(x, ...){
  l_env = as.list(x)
  values = paste(names(x), shQuote(unname(l_env)), sep = ": ")
  cat(sprintf("<Enum environment: %s>\n", data.table::address(x)))
  cat("Values:\n")
  cat(paste("  -", values, collapse = "\n"))
}

#' @title Split string from the right
#' @param str : string to be split
#' @param separator (str): Method splits string starting from the right (default `\\.`)
#' @param maxsplit (number): The maxsplit defines the maximum number of splits.
#' @family r_utils
#' @export
rsplit <- function(str, separator="\\.", maxsplit) {
  vec = unlist(strsplit(str, separator))
  len = length(vec)
  px = (length(vec) - maxsplit)
  c(paste(vec[1:px], collapse=separator), vec[(px+1):len])
}

#' @title Check if list is named
#' @param x : object
#' @family r_utils
#' @export
is_list_named = function(x){
  inherits(x, "list") && length(names(x)) > 0
}

paws_error_code <- function(error){
  return(error[["error_response"]][["__type"]] %||% error[["error_response"]][["Code"]])
}

to_str <- function(obj, ...){
  UseMethod("to_str")
}

to_str.default <- function(obj, ...){
  as.character(obj)
}

to_str.list <- function(obj, ...){
  jsonlite::toJSON(obj, auto_unbox = F)
}

to_str.numeric <- function(obj, ...){
  format(obj, scientific = F)
}

# Correctly mimic python append method for list
# Full credit to package rlist: https://github.com/renkun-ken/rlist/blob/2692e064fc7b6cc7fe7079b3762df37bc25b3dbd/R/list.insert.R#L26-L44
list.append = function (.data, ...) {
  if (is.list(.data)) c(.data, list(...)) else c(.data, ..., recursive = FALSE)
}

#' @title Helper function to return help documentation for sagemaker R6 classes.
#' @param cls (R6::R6Class): R6 class
#' @family r_utils
#' @export
cls_help = function(cls){
  cls_name = class(cls)[[1]]
  cls_env = tryCatch({
    get(cls_name)$parent_env
  }, error = function(e){
    NULL
  })
  pkg_name = if(is.null(cls_env)) NULL else get0(".packageName", envir = cls_env, inherits = FALSE)
  if(is.null(pkg_name)) {
    utils::help((cls_name))
  } else {
    utils::help((cls_name), (pkg_name))
  }
}

pkg_name = function(){
  env <- topenv(environment())
  get0(".packageName", envir = env, inherits = FALSE)
}

parse_url = function(url){
  url = ifelse(is.null(url) | is.logical(url) , "", url)
  url = ifelse(grepl("/", url), url, sprintf("/%s", url))
  urltools::url_parse(url)
}

#' @title Check if file is tar archived or not.
#' @description Check the magic bytes at offset 257.
#'              If they match "ustar" including the null terminator, the file is probably a tar.
#'              \url{https://www.gnu.org/software/tar/manual/html_node/Standard.html}
#' @param path A character of filepath to tar archived file.
#' @family r_utils
#' @export
is_tarfile <- function(path){
  stopifnot(is.character(path))
  if(!fs::is_file(path)){
    ValueError$new("`path` must be a valid file to be checked.")
  }
  # https://stackoverflow.com/questions/32180215/how-to-check-whether-a-file-is-in-tar-format
  con <- gzfile(path.expand(path), "rb")
  on.exit(close(con))
  magic <- readBin(con, "raw", n = 262L)
  rawToChar(magic[258:262]) == "ustar"
}

sys_set_env <- function(key, value){
  config_env = list(value)
  names(config_env) = key
  do.call(Sys.setenv, config_env)
}

temp_dir = function(dir = NULL){
  dir_name = paste0("tmp", paste(sample(c(0:9, letters), 8, replace = T),collapse = ""))
  dir_path = file.path(dir %||% tempdir(), dir_name)
  fs::dir_create(dir_path)
  return(dir_path)
}
