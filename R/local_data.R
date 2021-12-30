# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/local/data.py

#' @include local_utils.R

#' @import R6
#' @import sagemaker.common
#' @importFrom sagemaker.mlcore read_records_io
#' @importFrom urltools url_parse
#' @importFrom utils object.size
#' @importFrom fs path


#' @title Return an Instance of :class:`sagemaker.local.data.DataSource`.
#' @description The instance can handle the provided data_source URI.
#'              data_source can be either file:// or s3://
#' @param data_source (str): a valid URI that points to a data source.
#' @param sagemaker_session (:class:`sagemaker.session.Session`): a SageMaker Session to
#'              interact with S3 if required.
#' @return sagemaker.local.data.DataSource: an Instance of a Data Source
get_data_source_instance = function(data_source, sagemaker_session){
  parsed_uri = url_parse(data_source)
  if (parsed_uri$scheme == "file")
    return(LocalFileDataSource$new(fs::path(parsed_uri$domain, parsed_uri$path)))
  if (parsed_uri$scheme == "s3")
    return(S3DataSource$new(parsed_uri$domain, parsed_uri$path, sagemaker_session))
  ValueError$new(sprintf(
    "data_source must be either file or s3. parsed_uri.scheme: %s", parsed_uri$scheme)
  )
}

#' @title Return an Instance of :class:`sagemaker.local.data.Splitter`.
#' @description The instance returned is according to the specified `split_type`.
#' @param split_type (str): either 'Line' or 'RecordIO'. Can be left as None to
#'              signal no data split will happen.
#' @return :class:`sagemaker.local.data.Splitter`: an Instance of a Splitter
get_splitter_instance = function(split_type = NULL){
  if (is.null(split_type))
    return(NoneSplitter$new())
  if (split_type == "Line")
    return(LineSplitter$new())
  # if (split_type == "RecordIO")
  #   return(RecordIOSplitter$new())
  ValueError$new(sprintf("Invalid Split Type: %s", split_type))
}

#' @title Return an Instance of :class:`sagemaker.local.data.BatchStrategy` according to `strategy`
#' @param strategy (str): Either 'SingleRecord' or 'MultiRecord'
#' @param splitter (:class:`sagemaker.local.data.Splitter): splitter to get the data from.
#' @return :class:`sagemaker.local.data.BatchStrategy`: an Instance of a BatchStrategy
get_batch_strategy_instance = function(strategy, splitter){
  if (strategy == "SingleRecord")
    return(SingleRecordStrategy$new(splitter))
  if (strategy == "MultiRecord")
    return(MultiRecordStrategy$new(splitter))
  ValueError$new(sprintf('Invalid Batch Strategy: %s - Valid Strategies: "SingleRecord", "MultiRecord"',
                 strategy))
}

#' @title DataSource class
#' @keywords internal
DataSource = R6Class("DataSource",
  public = list(

    #' @description Retrieve the list of absolute paths to all the files in this data source.
    #' @return List[str]: List of absolute paths.
    get_file_list = function(){
      NotImplementedError$new()
    },

    #' @description Retrieve the absolute path to the root directory of this data source.
    #' @return str: absolute path to the root directory of this data source.
    get_root_dir = function(){
      NotImplementedError$new()
    }
  )
)

#' @title LocalFileDataSource class
#' @description Represents a data source within the local filesystem.
#' @export
LocalFileDataSource = R6Class("LocalFileDataSource",
  inherit = DataSource,
  public = list(

    #' @description Initialize LocalFileDataSource class
    #' @param root_path (str):
    initialize = function(root_path){
      super$initialize()

      self$root_path = fs::path_abs(root_path)
      if(!fs::dir_exists(self$root_path))
        RuntimeError$new(sprintf("Invalid data source: %s does not exist.", self$root_path))
    },

    #' @description Retrieve the list of absolute paths to all the files in this data source.
    #' @return List[str] List of absolute paths.
    get_file_list = function(){
      if (fs::is_dir(self$root_path)){
        return(lapply(
          fs::dir_ls(self$root_path, type = "file"),
          function(x) fs::path(self$root_path,x))
        )
      }
      return(list(self$root_path))
    },

    #' @description Retrieve the absolute path to the root directory of this data source.
    #' @return str: absolute path to the root directory of this data source.
    get_root_dir = function(){
      if (fs::is_dir(self$root_path))
        return(self$root_path)
      return(dirname(self$root_path))
    }
  )
)

#' @title Defines a data source given by a bucket and S3 prefix.
#' @description The contents will be downloaded and then processed as local data.
#' @export
S3DataSource = R6Class("S3DataSource",
  inherit = DataSource,
  public = list(

    #' @description Create an S3DataSource instance.
    #' @param bucket (str): S3 bucket name
    #' @param prefix (str): S3 prefix path to the data
    #' @param sagemaker_session (:class:`sagemaker.session.Session`): a sagemaker_session with the
    #'              desired settings to talk to S3
    initialize = function(bucket, prefix, sagemaker_session){
      super$initialize()

      # Create a temporary dir to store the S3 contents
      root_dir = get_config_value(
        "local.container_root", sagemaker_session$config
      )
      if (!is.null(root_dir))
        root_dir = fs::path_abs(root_dir)

      working_dir = tempfile.mkdtemp(dir=root_dir %||% tempdir())
      # Docker cannot mount Mac OS /var folder properly see
      # https://forums.docker.com/t/var-folders-isnt-mounted-properly/9600
      # Only apply this workaround if the user didn't provide an alternate storage root dir.
      if (is.null(root_dir) && Sys.info()[["sysname"]] == "Darwin")
        working_dir = sprintf("/private%s",working_dir)

      download_folder(bucket, prefix, working_dir, sagemaker_session)
      self$files = LocalFileDataSource$new(working_dir)
    },

    #' @description Retrieve the list of absolute paths to all the files in this data source.
    #' @return List(str): List of absolute paths.
    get_file_list = function(){
      return(self$files$get_file_list())
    },

    #' @description Retrieve the absolute path to the root directory of this data source.
    #' @return str: absolute path to the root directory of this data source.
    get_root_dir = function(){
      return(self$files$get_root_dir())
    }
  )
)

#' @title Splitter class
#' @keywords internal
Splitter = R6Class("Splitter",
  public = list(

    #' @description Split a file into records using a specific strategy
    #' @param file (str): path to the file to split
    #' @return generator for the individual records that were split from the file
    split = function(file){
      NotImplementedError$new()
    }
  )
)

#' @title NoneSplitter class
#' @description Does not split records, essentially reads the whole file.
#' @export
NoneSplitter = R6Class("NoneSplitter",
  inherit = Splitter,
  public = list(

    #' @description Split a file into records using a specific strategy.
    #'              For this NoneSplitter there is no actual split happening and the file
    #'              is returned as a whole.
    #' @param file (str): path to the file to split
    #' @return generator for the individual records that were split from
    #'              the file
    split = function(file){
      fh <- base::file(file, "rb")
      on.exit(close(fh))
      buf <- readBin(fh, "raw", file.size(file))
      if(!private$.is_binary(buf))
        return(rawToChar(buf))
      return(buf)
    }
  ),
  private = list(
    .text_chars = charToRaw(paste0("\a\b\t\n\f\r\033 !\"#$%&'()*+,-./0123456789:;",
    "<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\x80\x81",
    "\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8d\x8e\x8f\x90\x91\x92\x93\x94",
    "\x95\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f\xa0\xa1\xa2\xa3\xa4\xa5\xa6\xa7",
    "\xa8\xa9\xaa\xab\xac\xad\xae\xaf\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba",
    "\xbb\xbc\xbd\xbe\xbf\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9\xca\xcb\xcc\xcd",
    "\xce\xcf\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf\xe0",
    "\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\xf0\xf1\xf2\xf3",
    "\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff")),

    .is_binary = function(buf) {
      if(length(buf[!(buf %in% private$.text_chars)]) != 0)
        return(TRUE)
      else
        return (FALSE)
    }
  )
)

#' @title Split records by new line.
#' @export
LineSplitter = R6Class("LineSplitter",
  inherit = Splitter,
  public = list(

    #' @description Split a file into records using a specific strategy
    #'              This LineSplitter splits the file on each line break.
    #' @param file (str): path to the file to split
    #' @return list: for the individual records that were split from
    #'              the file
    split = function(file){
      readLines(file, warn = FALSE)
    }
  )
)

# Move to package with read_recordio method in

#' @title Split using Amazon Recordio.
#' @description Not useful for string content.
#' @export
RecordIOSplitter = R6Class("RecordIOSplitter",
  inherit = Splitter,
  public = list(

    #' @description Split a file into records using a specific strategy
    #'              This RecordIOSplitter splits the data into individual RecordIO
    #'              records.
    #' @param file (str): path to the file to split
    #' @return generator for the individual records that were split from
    #'              the file
    split = function(file){
      f = readBin(file, what = "raw", n = file.size(file))
      sagemaker.mlcore::read_records_io(f)
    }
  )
)

#' @title BatchStrategy class
#' @keywords internal
BatchStrategy = R6Class("BatchStrategy",
  public = list(

    #' @description Create a Batch Strategy Instance
    #' @param splitter (sagemaker.local.data.Splitter): A Splitter to pre-process
    #'              the data before batching.
    initialize = function(splitter){
      self$splitter = splitter
    },

    #' @description Group together as many records as possible to fit in the specified size.
    #' @param file (str): file path to read the records from.
    #' @param size (int): maximum size in MB that each group of records will be
    #'              fitted to. passing 0 means unlimited size.
    #' @return generator of records
    pad = function(file, size){
      NotImplementedError$new()
    }
  )
)

#' @title Feed multiple records at a time for batch inference.
#' @description Will group up as many records as possible within the payload specified.
#' @export
MultiRecordStrategy = R6Class("MultiRecordStrategy",
  inherit = BatchStrategy,
  public = list(

    #' @description Group together as many records as possible to fit in the specified size.
    #' @param file (str): file path to read the records from.
    #' @param size (int): maximum size in MB that each group of records will be
    #'              fitted to. passing 0 means unlimited size.
    #' @return generator of records
    pad = function(file, size=6){
      buffer = ""
      for(element in self$splitter$split(file)){
        if(.payload_size_within_limit(paste0(buffer, element), size))
          buffer = paste0(buffer,element)
        else{
          tmp = buffer
          buffer = element
          return(tmp)
        }
      }
      if(.validate_payload_size(buffer, size))
        return(buffer)
    }
  )
)

#' @title Feed a single record at a time for batch inference.
#' @description If a single record does not fit within the payload specified it will
#'              throw a RuntimeError.
#' @export
SingleRecordStrategy = R6Class("SingleRecordStrategy",
  inherit = BatchStrategy,
  public = list(

    #' @description Group together as many records as possible to fit in the specified size.
    #'              This SingleRecordStrategy will not group any record and will return
    #'              them one by one as long as they are within the maximum size.
    #' @param file (str): file path to read the records from.
    #' @param size (int): maximum size in MB that each group of records will be
    #'              fitted to. passing 0 means unlimited size.
    #' @return generator of records
    pad = function(file, size = 6){
      for (element in self$splitter$split(file)){
        if (.validate_payload_size(element, size))
          return(element)
      }
    }
  )
)

.payload_size_within_limit = function(payload, size){
  size_in_bytes = size * 1024 * 1024
  if (size == 0)
    return(TRUE)
  return(utils::object.size(payload) < size_in_bytes)
}

#' @title Check if a payload is within the size in MB threshold.
#' @description Raise an exception if the payload is beyond the size in MB threshold.
#' @param payload : data that will be checked
#' @param size (int): max size in MB
#' @return bool: True if within bounds. if size=0 it will always return True
.validate_payload_size = function(payload, size){
  if(.payload_size_within_limit(payload, size))
    return(TRUE)
  RuntimeError$new(sprintf("Record is larger than %sMB. Please increase your max_payload", size))
}
