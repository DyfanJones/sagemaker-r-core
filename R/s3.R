# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/s3.py

#' @include utils.R
#' @include r_utils.R

#' @importFrom urltools url_parse
#' @importFrom fs path_join

#' @title validation check of s3 uri
#' @param x (str): character to validate if s3 uri or not
#' @export
is.s3_uri <- function(x) {
  if(is.null(x) || !is.character(x)) return(FALSE)
  regex <- '^s3://[a-z0-9\\.-]+(/(.*)?)?$'
  grepl(regex, x)
}

#' @title split s3 uri
#' @param url (str): s3 uri to split into bucket and key
#' @name parse_s3
#' @export
split_s3_uri <- function(url) {
  stopifnot(is.null(url) || is.character(url))
  parsed_url <- url_parse(url)
  if (parsed_url$scheme != "s3")
    ValueError$new(sprintf("Expecting 's3' scheme, got: %s in %s.", parsed_url$scheme, url))
  return(list(
    bucket = parsed_url$domain,
    key = parsed_url$path)
  )
}

#' @rdname parse_s3
#' @export
parse_s3_url <- split_s3_uri

#' @title Creates S3 uri paths
#' @description Returns the arguments joined by a slash ("/"), similarly to ``file.path()`` (on Unix).
#'              If the first argument is "s3://", then that is preserved.
#' @param ... : The strings to join with a slash.
#' @return character: The joined string.
#' @export
s3_path_join = function(...){
  args=list(...)
  if(grepl("^s3://", args[[1]])){
    path = trimws(args[2:length(args)], "left", "/")
    path = fs::path_join(c(args[[1]], path))
    return(as.character(gsub("s3:/", "s3://", path)))
  }
  return(as.character(trimws(fs::path_join(as.character(args)), "left", "/")))
}

#' @title S3Uploader Class
#' @description Contains static methods for uploading directories or files to S3
#' @export
S3Uploader = R6Class("S3Uploader",
  public = list(

    #' @description Static method that uploads a given file or directory to S3.
    #' @param local_path (str): Path (absolute or relative) of local file or directory to upload.
    #' @param desired_s3_uri (str): The desired S3 location to upload to. It is the prefix to
    #'              which the local filename will be added.
    #' @param kms_key (str): The KMS key to use to encrypt the files.
    #' @param sagemaker_session (sagemaker.session.Session): Session object which
    #'              manages interactions with Amazon SageMaker APIs and any other
    #'              AWS services needed. If not specified, the estimator creates one
    #'              using the default AWS configuration chain.
    #' @return The S3 uri of the uploaded file(s).
    upload = function(local_path = NULL,
                      desired_s3_uri = NULL,
                      kms_key=NULL,
                      sagemaker_session=NULL){

      sagemaker_session = sagemaker_session %||% Session$new()
      s3_parts = parse_s3_url(desired_s3_uri)
      params = list(
        path=local_path,
        bucket=s3_parts$bucket,
        key_prefix=s3_parts$key
      )
      if (!is.null(kms_key)){
        params[["SSEKMSKeyId"]] = kms_key
        params[["ServerSideEncryption"]] = "aws:kms"
      }

      return(do.call(sagemaker_session$upload_data, params))
    },

    #' @description Static method that uploads a given file or directory to S3.
    #' @param body (str): String representing the body of the file.
    #' @param desired_s3_uri (str): The desired S3 uri to upload to.
    #' @param kms_key (str): The KMS key to use to encrypt the files.
    #' @param sagemaker_session (sagemaker.session.Session): AWS session to use. Automatically
    #'              generates one if not provided.
    #' @return str: The S3 uri of the uploaded file(s).
    upload_string_as_file_body = function(body,
                                          desired_s3_uri=NULL,
                                          kms_key=NULL,
                                          sagemaker_session=NULL){
      sagemaker_session = sagemaker_session %||% Session$new()
      s3_parts = parse_s3_url(desired_s3_uri)

      sagemaker_session$upload_string_as_file_body(
        body=body, bucket=s3_parts$bucket, key=s3_parts$key, kms_key=kms_key)

      return(desired_s3_uri)
    },

    #' @description format class
    format = function(){
      return(format_class(self))
    }
  )
)

#' @title S3Downloader
#' @description Contains static methods for downloading directories or files from S3.
#' @export
S3Downloader = R6Class("S3Downloader",
  public =list(

    #' @description Static method that downloads a given S3 uri to the local machine.
    #' @param s3_uri (str): An S3 uri to download from.
    #' @param local_path (str): A local path to download the file(s) to.
    #' @param kms_key (str): The KMS key to use to decrypt the files.
    #' @param sagemaker_session (sagemaker.session.Session): Session object which
    #'              manages interactions with Amazon SageMaker APIs and any other
    #'              AWS services needed. If not specified, the estimator creates one
    #'              using the default AWS configuration chain.
    download = function(s3_uri,
                        local_path,
                        kms_key=NULL,
                        sagemaker_session=NULL){

      sagemaker_session = sagemaker_session %||% Session$new()
      s3_parts = parse_s3_url(s3_uri)
      params = list(
        path = local_path,
        bucket = s3_parts$bucket,
        key_prefix=s3_parts$key
      )
      params[["SSECustomerKey"]] = kms_key
      return(do.call(sagemaker_session$download_data, params))
    },

    #' @description Static method that returns the contents of an s3 uri file body as a string.
    #' @param s3_uri (str): An S3 uri that refers to a single file.
    #' @param sagemaker_session (sagemaker.session.Session): AWS session to use. Automatically
    #'              generates one if not provided.
    #' @return str: The body of the file.
    read_file = function(s3_uri,
                         sagemaker_session=NULL){

      sagemaker_session = sagemaker_session %||% Session$new()
      s3_parts = parse_s3_url(s3_uri)

      return(sagemaker_session$read_s3_file(bucket=s3_parts$bucket, key_prefix=s3_parts$key))
    },

    #' @description Static method that lists the contents of an S3 uri.
    #' @param s3_uri (str): The S3 base uri to list objects in.
    #' @param sagemaker_session (sagemaker.session.Session): AWS session to use. Automatically
    #'              generates one if not provided.
    #' @return [str]: The list of S3 URIs in the given S3 base uri.
    list = function(s3_uri,
                    sagemaker_session = NULL){
      sagemaker_session = sagemaker_session %||% Session$new()
      s3_parts = parse_s3_url(s3_uri)

      file_keys = sagemaker_session$list_s3_files(bucket=s3_parts$bucket, key_prefix=s3_parts$key)
      return(lapply(file_keys, function(file_key) s3_path_join("s3://", s3_parts$bucket, file_key)))
    },

    #' @description format class
    format = function(){
      return(format_class(self))
    }
  )
)
