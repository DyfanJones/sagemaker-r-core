# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/utils.py

#' @include r_utils.R
#' @include error.R

#' @importFrom stats runif
#' @importFrom utils tar untar
#' @importFrom fs path path_abs is_dir is_file dir_create dir_copy file_copy file_exists


#' @title Core variables
#' @name common_variables
#' @family sagemaker_utils
#' @keywords internal
NULL

#' @rdname common_variables
#' @export
ECR_URI_PATTERN <- "^(\\d+)(\\.)dkr(\\.)ecr(\\.)(.+)(\\.)(.*)(/)(.*:.*)$"

#' @rdname common_variables
#' @export
MAX_BUCKET_PATHS_COUNT = 5

#' @rdname common_variables
#' @export
S3_PREFIX = "s3://"

#' @rdname common_variables
#' @export
HTTP_PREFIX = "http://"

#' @rdname common_variables
#' @export
HTTPS_PREFIX = "https://"

#' @rdname common_variables
#' @export
DEFAULT_SLEEP_TIME_SECONDS = 10

#' @title Create a training job name based on the image name and a timestamp.
#' @param image (str): Image name.
#' @param max_length (int): Maximum length for the resulting string (default: 63).
#' @return str: Training job name using the algorithm from the image name and a
#'        timestamp.
#' @family sagemaker_utils
#' @export
name_from_image <- function(image, max_length=63L){
  return(name_from_base(base_name_from_image(image), max_length = max_length))
}

#' @title Append a timestamp to the provided string.
#' @description This function assures that the total length of the resulting string is
#'              not longer than the specified max length, trimming the input parameter if
#'              necessary.
#' @param base (str): String used as prefix to generate the unique name.
#' @param max_length (int): Maximum length for the resulting string (default: 63).
#' @param short (bool): Whether or not to use a truncated timestamp (default: False).
#' @return str: Input parameter with appended timestamp.
#' @family sagemaker_utils
#' @export
name_from_base <- function(base, max_length = 63, short = FALSE){
  timestamp = if(short) sagemaker_short_timestamp() else sagemaker_timestamp()
  trimmed_base = substring(base, 1,(max_length - length(timestamp) - 1))
  return(sprintf("%s-%s", trimmed_base, timestamp))
}

#' @title Create a unique name from base str
#' @param base (str): String used as prefix to generate the unique name.
#' @param max_length (int): Maximum length for the resulting string (default: 63).
#' @return str: Input parameter with appended timestamp.
#' @family sagemaker_utils
#' @export
unique_name_from_base <- function(base, max_length=63){
  unique = sprintf("%04x", as.integer(runif(1,max= 16**4))) # 4-digit hex
  ts = as.character(as.integer(Sys.time()))
  available_length = max_length - 2 - nchar(ts) - nchar(unique)
  trimmed = substring(base, 1, available_length)
  return(sprintf("%s-%s-%s",trimmed, ts, unique))
}

#' @title Extract the base name of the image to use as the 'algorithm name' for the job.
#' @param image (str): Image name.
#' @return str: Algorithm name, as extracted from the image name.
#' @family sagemaker_utils
#' @export
base_name_from_image <- function(image){
  stopifnot(is.character(image))
  m <- grepl("^(.+/)?([^:/]+)(:[^:]+)?$", image)
  algo_name = if(!is.null(m)) gsub(".*/|:.*", "", image) else image
  return(algo_name)
}

#' @title Extract the base name of the resource name (for use with future resource name generation).
#' @description This function looks for timestamps that match the ones produced by
#'              :func:`~sagemaker.utils.name_from_base`.
#' @param name (str): The resource name.
#' @return str: The base name, as extracted from the resource name.
#' @family sagemaker_utils
#' @export
base_from_name <- function(name){
  pattern = "^(.+)-(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}-\\d{2}-\\d{3}|\\d{6}-\\d{4})"
  m = regmatches(name, regexec(pattern, name))[[1]]
  return(if (!islistempty(m)) m[[2]] else name)
}

#' @title Return a timestamp with millisecond precision.
#' @family sagemaker_utils
#' @export
sagemaker_timestamp <- function(){
  moment = Sys.time()
  moment_ms = split_str(format(as.numeric(moment,3), nsmall = 3), "\\.")[2]
  strftime(moment,paste0("%Y-%m-%d-%H-%M-%S-",moment_ms),tz="GMT")
}

#' @title Return a timestamp that is relatively short in length
#' @family sagemaker_utils
#' @export
sagemaker_short_timestamp <- function() return(strftime(Sys.time(), "%y%m%d-%H%M"))

#' @title Return a dict of key and value pair if value is not None, otherwise return an empty dict.
#' @param key (str): input key
#' @param value (str): input value
#' @return dict: dict of key and value or an empty dict.
#' @family sagemaker_utils
#' @export
build_dict <- function(key, value = NULL){
  if (!islistempty(value)) {
    dict = list(value)
    names(dict) = key
    return(dict)
  }
  return(list())
}

#' @title Placeholder
#' @param key_path (str):
#' @param config (str):
#' @keywords internal
#' @family sagemaker_utils
#' @export
get_config_value <- function(key_path, config = NULL){
  if(is.null(config))
    return(NULL)

  current_section = config
  for(key in split_str(key_path, "\\.")){
    if (key %in% names(current_section))
      current_section = current_section[[key]]
    else
      return(NULL)
  }
  return(current_section)
}

#' @title Return short version in the format of x.x
#' @param framework_version (str): The version string to be shortened.
#' @return str: The short version string
#' @family sagemaker_utils
#' @export
get_short_version = function(framework_version){
  x = numeric_version(framework_version)
  return(paste(c(x[[1,1]], x[1,2]), collapse = "."))
}

#' @title Returns true if training job's secondary status message has changed.
#' @param current_job_description (str): Current job description, returned from DescribeTrainingJob call.
#' @param prev_job_description (str): Previous job description, returned from DescribeTrainingJob call.
#' @return boolean: Whether the secondary status message of a training job changed
#'              or not.
#' @family sagemaker_utils
#' @export
secondary_training_status_changed <- function(current_job_description=NULL,
                                             prev_job_description=NULL){
  current_secondary_status_transitions = current_job_description$SecondaryStatusTransitions

  if(is.null(current_secondary_status_transitions) ||
     length(current_secondary_status_transitions) ==0){
    return(FALSE)
  }
  prev_job_secondary_status_transitions = (
    if(!is.null(prev_job_description))
      prev_job_description$SecondaryStatusTransitions
    else NULL
  )
  last_message = (if (!is.null(prev_job_secondary_status_transitions)
                      && length(prev_job_secondary_status_transitions) > 0){
    prev_job_secondary_status_transitions[[length(prev_job_secondary_status_transitions)]]$StatusMessage
  } else {""})

  message = current_job_description$SecondaryStatusTransitions[[
    length(current_job_description$SecondaryStatusTransitions)
    ]]$StatusMessage
  return(message != last_message)
}

#' @title Returns a string contains last modified time and the secondary training
#'              job status message.
#' @param job_description (str): Returned response from DescribeTrainingJob call
#' @param prev_description (str): Previous job description from DescribeTrainingJob call
#' @return str: Job status string to be printed.
#' @family sagemaker_utils
#' @export
secondary_training_status_message <- function(job_description=NULL,
                                              prev_description=NULL){
  if (is.null(job_description)
      || is.null(job_description$SecondaryStatusTransitions)
      || length(job_description$SecondaryStatusTransitions) == 0){
    return("")}

  prev_description_secondary_transitions = (
    if(!is.null(prev_description))
      prev_description$SecondaryStatusTransitions
    else NULL
  )
  prev_transitions_num = (
    if(!is.null(prev_description_secondary_transitions))
      length(prev_description$SecondaryStatusTransitions)
    else 0
  )
  current_transitions = job_description$SecondaryStatusTransitions

  if (length(current_transitions) == prev_transitions_num){
    # Secondary status is not changed but the message changed.
    transitions_to_print = current_transitions[[prev_transitions_num]]
  } else{
    # Secondary status is changed we need to print all the entries.
    transitions_to_print = current_transitions[[length(current_transitions)]]
  }

  status_strs = sprintf("%s %s - %s",
    strftime(job_description$LastModifiedTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    transitions_to_print$Status,
    transitions_to_print$StatusMessage)

  return(paste(status_strs, collapse = "\n"))
}

#' @title Download a folder from S3 to a local path
#' @param bucket_name (str): S3 bucket name
#' @param prefix (str): S3 prefix within the bucket that will be downloaded. Can
#'              be a single file.
#' @param target (str): destination path where the downloaded items will be placed
#' @param sagemaker_session (sagemaker.session.Session): a sagemaker session to
#'              interact with S3.
#' @family sagemaker_utils
#' @export
download_folder = function(bucket_name,
                           prefix,
                           target,
                           sagemaker_session){
  s3 = sagemaker_session$s3

  prefix = trimws(prefix, "left", "/")

  # Try to download the prefix as an object first, in case it is a file and not a 'directory'.
  # Do this first, in case the object has broader permissions than the bucket.
  if(!grepl("/$", prefix)){
    tryCatch({
      file_destination = fs::path(target, basename(prefix))
      obj = s3$get_object(Bucket = bucket_name, Key = prefix)
      write_bin(obj$Body, file_destination)
      return(invisible(NULL))
    },
    error = function(e){
      err_info = attributes(e)
      if(err_info$status_code == "404" && grepl("NoSuchKey", e$message)){
        # S3 also throws this error if the object is a folder,
        # so assume that is the case here, and then raise for an actual 404 later.
        return(NULL)
      } else {
        stop(e)
      }
    })
  }

  .download_files_under_prefix(bucket_name, prefix, target, s3)
}

#' @title Download all S3 files which match the given prefix
#' @param bucket_name (str): S3 bucket name
#' @param prefix (str): S3 prefix within the bucket that will be downloaded
#' @param target (str): destination path where the downloaded items will be placed
#' @param s3 (paws::s3): S3 resource
#' @family sagemaker_utils
#' @keywords internal
.download_files_under_prefix = function(bucket_name,
                                        prefix,
                                        target,
                                        s3){
  next_token = NULL
  keys = character()
  # Loop through the contents of the bucket, 1,000 objects at a time. Gathering all keys into
  # a "keys" list.
  while(!identical(next_token, character(0))){
    response = s3$list_objects_v2(
      Bucket = bucket_name,
      Prefix = prefix,
      ContinuationToken = next_token)
    # For each object, save its key or directory.
    keys = c(keys, sapply(response$Contents, function(x) x$Key))
    keys = keys[!grepl("/$", keys)] # remove none files.
    next_token = response$ContinuationToken
  }

  for(obj_sum in keys){
    s3_relative_path = trimws(substr(obj_sum,nchar(prefix)+1,nchar(obj_sum)), "left", "/")
    file_path = fs::path(target, s3_relative_path)
    fs::dir_create(dirname(file_path))
    obj = s3$get_object(Bucket = bucket_name, Key = obj_sum)
    write_bin(obj$Body, file_path)
  }
}

#' @title Create a tar file containing all the source_files
#' @param source_files (str): vector of file paths that will be contained in the tar file
#' @param target (str): A character vector of filepaths to be archived. Default to archive all files
#'                      into a temporary file.
#' @keywords internal
#' @return (str): path to created tar file
#' @family sagemaker_utils
#' @export
create_tar_file <- function(source_files, target=NULL){
  if (!is.null(target)){
    filename = target
  } else {
    filename = tempfile(fileext = ".tar.gz")
  }
  # ensure target directory is created
  fs::dir_create(dirname(filename))
  # get the absolute path from filename
  filename=file.path(fs::path_abs(dirname(filename)), basename(filename))

  tar_subdir(filename, src=source_files)
  return(filename)
}

#' @title Unpack model tarball and creates a new model tarball with the provided
#'              code script.
#' @description This function does the following: - uncompresses model tarball from S3 or
#'              local system into a temp folder - replaces the inference code from the model
#'              with the new code provided - compresses the new model tarball and saves it
#'              in S3 or local file system
#' @param inference_script (str): path or basename of the inference script that
#'              will be packed into the model
#' @param source_directory (str): path including all the files that will be packed
#'              into the model
#' @param dependencies (list[str]): A list of paths to directories (absolute or
#'              relative) with any additional libraries that will be exported to the
#' @param model_uri (str): S3 or file system location of the original model tar
#' @param repacked_model_uri (str): path or file system location where the new
#'              model will be saved
#' @param sagemaker_session (sagemaker.session.Session): a sagemaker session to
#'              interact with S3.
#' @param kms_key (str): KMS key ARN for encrypting the repacked model file
#' @return str: path to the new packed model
#' @family sagemaker_utils
#' @export
repack_model <- function(inference_script,
                         source_directory,
                         dependencies,
                         model_uri,
                         repacked_model_uri,
                         sagemaker_session,
                         kms_key=NULL){
  dependencies = dependencies %||% list()

  tmp = tempdir()

  # extract model from tar.gz
  model_dir = .extract_model(model_uri, sagemaker_session, tmp)

  # append file to model directory
  .create_or_update_code_dir(
    model_dir, inference_script, source_directory, dependencies, sagemaker_session, tmp
  )

  # repackage model_dir
  tmp_model_path = fs::path(tmp, "temp-model.tar.gz")
  tar_subdir(tmp_model_path, model_dir)

  # remove temp directory/tar.gz
  on.exit(unlink(c(tmp_model_path, model_dir), recursive = T))

  # save model
  .save_model(repacked_model_uri, tmp_model_path, sagemaker_session, kms_key=kms_key)
}

tar_subdir <- function(tarfile, src, compress = "gzip", ...){
  if(any(fs::is_file(src))){
    src_dir = unique(dirname(src))
    src_file = basename(src)
  } else if(any(fs::is_dir(src))){
    src_dir = unique(src)
    src_file = "."
  }
  if(!any(fs::file_exists(src))){
    ValueError$new(sprintf(
      "Directory '%s' doesn't exist, please check directory location and try again.",
      src)
    )
  }
  current_dir = getwd()
  setwd(src_dir)
  on.exit(setwd(current_dir))

  tar_path = (if(nzchar(Sys.getenv("tar"))) Sys.getenv("tar") else Sys.getenv("TAR"))

  utils::tar(tarfile= tarfile, files=src_file, compression=compress, tar = tar_path, ...)
}

.create_or_update_code_dir <- function(model_dir=NULL,
                                       inference_script=NULL,
                                       source_directory=NULL,
                                       dependencies=NULL,
                                       sagemaker_session=NULL,
                                       tmp=NULL) {
  code_dir = fs::path(model_dir, "code")
  if (!is.null(source_directory) &&
      startsWith(tolower(source_directory), "s3://")) {
    local_code_path = fs::path(tmp, "local_code.tar.gz")
    download_file_from_url(source_directory, local_code_path, sagemaker_session)
    utils::untar(local_code_path, exdir = code_dir)
    on.exit(unlink(local_code_path, recursive = T))
  } else if (!is.null(source_directory)) {
    if (file.exists(code_dir)) {
      unlink(code_dir, recursive = TRUE)}
    fs::dir_copy(source_directory, code_dir)
  } else {
    if (!file.exists(code_dir)){
      fs::dir_create(code_dir)}
    tryCatch({
      fs::file_copy(inference_script, code_dir, overwrite=TRUE)
    },
    error = function(e){
      if (file.exists(fs::path(code_dir, inference_script)))
        return(invisible(NULL))
      else
        stop(e)
    })
  }

  lib_dir = fs::path(code_dir, "lib")
  fs::dir_create(lib_dir)
  for (dependency in dependencies) {
    if (fs::is_dir(dependency)) {
      fs::dir_copy(dependency, fs::path(lib_dir, basename(dependency)))
    } else {
      fs::file_copy(dependency, lib_dir, overwrite = TRUE)
    }
  }
}

.extract_model <- function(model_uri, sagemaker_session, tmp){
  tmp_model_dir = fs::path(tmp, "model")
  fs::dir_create(tmp_model_dir)
  if(startsWith(tolower(model_uri), "s3://")){
    local_model_path = fs::path(tmp, "tar_file")
    download_file_from_url(model_uri, local_model_path, sagemaker_session)
  } else{
    local_model_path = gsub("file://", "", model_uri)
  }
  utils::untar(local_model_path, exdir = tmp_model_dir)
  return(tmp_model_dir)
}

.save_model <- function(repacked_model_uri,
                        tmp_model_path,
                        sagemaker_session,
                        kms_key) {
  if (startsWith(tolower(repacked_model_uri), "s3://")) {
    s3_parts = split_s3_uri(repacked_model_uri)
    s3_parts$key = gsub(basename(s3_parts$key), basename(repacked_model_uri), s3_parts$key)
    obj = readBin(tmp_model_path, "raw", n = file.size(tmp_model_path))

    kwargs = list(Body=obj, Bucket=s3_parts$bucket, Key=s3_parts$key)
    if (!is.null(kms_key)) {
      kwargs$ServerSideEncryption = "aws:kms"
      kwargs$SSEKMSKeyId = kms_key
    }
    do.call(sagemaker_session$s3$put_object, kwargs)
  } else {
    fs::file_copy(tmp_model_path,gsub("file://", "", repacked_model_uri))
  }
  return(invisible(NULL))
}

#' @title Download a Single File from S3 into a local path
#' @param url (str): file path within the bucket
#' @param dst (str): destination directory for the downloaded file.
#' @param sagemaker_session (sagemaker.session.Session): a sagemaker session to
#'              interact with S3.
#' @export
download_file_from_url = function(url,
                                  dst,
                                  sagemaker_session){
  url = urltools::url_parse(url)
  download_file(url$domain, trimws(url$path, "left", "/"), dst, sagemaker_session)
}

#' @title Download a Single File from S3 into a local path
#' @param bucket_name (str): S3 bucket name
#' @param path (str): file path within the bucket
#' @param target (str): destination directory for the downloaded file.
#' @param sagemaker_session (sagemaker.session.Session): a sagemaker session to
#'              interact with S3.
#' @family sagemaker_utils
#' @export
download_file = function(bucket_name,
                         path,
                         target,
                         sagemaker_session){
  path = trimws(path, "left", "/")
  s3 = sagemaker_session$s3

  obj = s3$get_object(Bucket = bucket_name, Key = path)
  write_bin(obj$Body, target)
}

#' @title Get the AWS endpoint specific for the given region.
#' @description We need this function because the AWS SDK does not yet honor
#'              the ``region_name`` parameter when creating an AWS STS client.
#'              For the list of regional endpoints, see
#'              \url{https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html#id_credentials_region-endpoints}.
#' @param service_name (str): Name of the service to resolve an endpoint for (e.g., s3)
#' @param region (str): AWS region name
#' @return str: AWS STS regional endpoint
#' @family sagemaker_utils
#' @export
regional_hostname <- function(service_name, region){
  hostname = list(
    '%s.%s.amazonaws.com.cn'='cn-*',
    '%s.%s.amazonaws.com'='us-gov-*',
    '%s.%s.c2s.ic.gov'='us-iso-*',
    '%s.%s.sc2s.sgov.gov'='us-isob-*',
    '%s.%s.amazonaws.com'= "*"
  )
  matches = hostname[sapply(hostname, function(x) grepl(x, region))]
  matches = matches[order(nchar(matches), decreasing = TRUE)][1]
  return(sprintf(names(matches), service_name, region))
}

#' @title Get the AWS STS endpoint specific for the given region.
#' @description We need this function because the AWS SDK does not yet honor
#'              the ``region_name`` parameter when creating an AWS STS client.
#'              For the list of regional endpoints, see
#'              \url{https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html#id_credentials_region-endpoints}.
#' @param region (str): AWS region name
#' @return str: AWS STS regional endpoint
#' @family sagemaker_utils
#' @export
sts_regional_endpoint <- function(region){
  endpoint_data = regional_hostname("sts", region )
  return(sprintf("https://%s", endpoint_data))
}

#' @title Retries until max retry count is reached.
#' @param max_retry_count (int): The retry count.
#' @param exception_message_prefix (str): The message to include in the exception on failure.
#' @param seconds_to_sleep (int): The number of seconds to sleep between executions.
#' @family sagemaker_utils
#' @export
retries <- function(max_retry_count,
                    exception_message_prefix,
                    seconds_to_sleep = 2) {
  value <- 0
  function() {
    value <<- value + 1
    if (value <= max_retry_count){
      Sys.sleep(seconds_to_sleep)
      return(TRUE)
    } else {
      SagemakerError$new(
        sprintf("'%s' has reached the maximum retry count of %s",
                exception_message_prefix, max_retry_count))
    }
  }
}

#' @title Given a region name (ex: "cn-north-1"), return the corresponding aws partition ("aws-cn").
#' @description region (str): The region name for which to return the corresponding partition.
#' @return str: partition corresponding to the region name passed in.
#' @keywords internal
#' @family sagemaker_utils
#' @export
.aws_partition = function(region){
  partitions = list(
    'aws-cn'='cn-*',
    'aws-us-gov'='us-gov-*',
    'aws-iso'='us-iso-*',
    'aws-iso-b'='us-isob-*',
    'aws'= "*"
  )
  matches = partitions[sapply(partitions, function(x) grepl(x, region))]
  matches = matches[order(nchar(matches), decreasing = TRUE)][1]
  return(names(matches))
}
