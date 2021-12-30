# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/local/utils.py

#' @importFrom urltools url_parse
#' @import fs
#' @import sagemaker.common

#' @title Creates intermediate directory structure for relative_path.
#' @description Create all the intermediate directories required for relative_path to
#'              exist within destination_directory. This assumes that relative_path is a
#'              directory located within root_dir.
#' @param destination_directory (str): root of the destination directory where the
#'              directory structure will be created.
#' @param relative_path (str): relative path that will be created within
#'              destination_directory
#' @export
copy_directory_structure = function(destination_directory, relative_path){
  full_path = fs::path_join(c(destination_directory, relative_path))
  if (fs::dir_exists(full_path))
    return(NULL)
  fs::dir_create(destination_directory, relative_path)
}

#' @title Move source to destination.
#' @description Can handle uploading to S3.
#' @param source (str): root directory to move
#' @param destination (str): file:// or s3:// URI that source will be moved to.
#' @param job_name (str): SageMaker job name.
#' @param sagemaker_session (sagemaker.Session): a sagemaker_session to interact
#'              with S3 if needed
#' @return (str): destination URI
#' @export
move_to_destination = function(source,
                               destination,
                               job_name,
                               sagemaker_session){
  parsed_uri = url_parse(destination)
  if (parsed_uri$scheme == "file"){
    recursive_copy(source, parsed_uri$path)
    final_uri = destination
  } else if (parsed_uri$scheme == "s3"){
    bucket = parsed_uri$domain
    path = sagemaker.common::s3_path_join(parsed_uri$path, job_name)
    final_uri = sagemaker.common::s3_path_join("s3://", bucket, path)
    sagemaker_session$upload_data(source, bucket, path)
  } else {
    ValueError$new(sprintf("Invalid destination URI, must be s3:// or file://, got: %s",destination))
  }
  fs::dir_delete(source)
  return(final_uri)
}

#' @title A wrapper around distutils.dir_util.copy_tree.
#' @description This won't throw any exception when the source directory does not exist.
#' @param source (str): source path
#' @param destination (str): destination path
recursive_copy = function(source, destination){
  if(fs::is_dir(source))
    fs::dir_copy(source, destination)
}
