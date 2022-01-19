# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/inputs.py

#' @include utils.R

#' @import R6

#' @title Create a definition for input data used by an SageMaker training job.
#' @description Amazon SageMaker channel configurations for S3 data sources.
#' @export
TrainingInput = R6Class("TrainingInput",
  public = list(

   #' @field config
   #' A SageMaker ``DataSource`` referencing a SageMaker ``S3DataSource``.
   config = NULL,

   #' @description See AWS documentation on the ``CreateTrainingJob`` API for more details on the parameters.
   #' @param s3_data (str): Defines the location of s3 data to train on.
   #' @param distribution (str): Valid values: 'FullyReplicated', 'ShardedByS3Key'
   #'              (default: 'FullyReplicated').
   #' @param compression (str): Valid values: 'Gzip', None (default: None). This is used only in
   #'              Pipe input mode.
   #' @param content_type (str): MIME type of the input data (default: None).
   #' @param record_wrapping (str): Valid values: 'RecordIO' (default: None).
   #' @param s3_data_type (str): Valid values: 'S3Prefix', 'ManifestFile', 'AugmentedManifestFile'.
   #'              If 'S3Prefix', ``s3_data`` defines a prefix of s3 objects to train on.
   #'              All objects with s3 keys beginning with ``s3_data`` will be used to train.
   #'              If 'ManifestFile' or 'AugmentedManifestFile', then ``s3_data`` defines a
   #'              single S3 manifest file or augmented manifest file (respectively),
   #'              listing the S3 data to train on. Both the ManifestFile and
   #'              AugmentedManifestFile formats are described in the SageMaker API documentation:
   #'              https://docs.aws.amazon.com/sagemaker/latest/dg/API_S3DataSource.html
   #' @param input_mode (str): Optional override for this channel's input mode (default: None).
   #'              By default, channels will use the input mode defined on
   #'              ``sagemaker.estimator.EstimatorBase.input_mode``, but they will ignore
   #'              that setting if this parameter is set.
   #'                  * None - Amazon SageMaker will use the input mode specified in the ``Estimator``
   #'                  * 'File' - Amazon SageMaker copies the training dataset from the S3 location to
   #'                      a local directory.
   #'                  * 'Pipe' - Amazon SageMaker streams data directly from S3 to the container via
   #'                      a Unix-named pipe.
   #' @param attribute_names (list[str]): A list of one or more attribute names to use that are
   #'              found in a specified AugmentedManifestFile.
   #' @param target_attribute_name (str): The name of the attribute will be predicted (classified)
   #'              in a SageMaker AutoML job. It is required if the input is for SageMaker AutoML job.
   #' @param shuffle_config (ShuffleConfig): If specified this configuration enables shuffling on
   #'              this channel. See the SageMaker API documentation for more info:
   #'              https://docs.aws.amazon.com/sagemaker/latest/dg/API_ShuffleConfig.html
   initialize = function(s3_data,
                         distribution=NULL,
                         compression=NULL,
                         content_type=NULL,
                         record_wrapping=NULL,
                         s3_data_type="S3Prefix",
                         input_mode=NULL,
                         attribute_names=NULL,
                         target_attribute_name=NULL,
                         shuffle_config=NULL){
      self$config = list(
        DataSource = list(S3DataSource = list(S3DataType = s3_data_type, S3Uri = s3_data))
      )
      if (!(!is.null(target_attribute_name) || !is.null(distribution)))
         distribution = "FullyReplicated"

      self$config$DataSource$S3DataSource$S3DataDistributionType = distribution
      self$config$CompressionType = compression
      self$config$ContentType = content_type
      self$config$RecordWrapperType = record_wrapping
      self$config$InputMode = input_mode
      self$config$DataSource$S3DataSource$AttributeNames = attribute_names
      self$config$TargetAttributeName = target_attribute_name
      if(!is.null(shuffle_config)) self$config$ShuffleConfig = list(Seed = shuffle_config$seed)
   },

   #' @description format class
   format = function(){
      return(format_class(self))
   }
  )
)

#' @title ShuffleConfig Class
#' @description For configuring channel shuffling using a seed
#'              For more detail, see the AWS documentation:
#'              https://docs.aws.amazon.com/sagemaker/latest/dg/API_ShuffleConfig.html
#' @export
ShuffleConfig = R6Class("ShuffleConfig",
   public = list(

      #' @field seed
      #' value used to seed the shuffled sequence.
      seed = NULL,

      #' @description Create a ShuffleConfig.
      #' @param seed (numeric): value used to seed the shuffled sequence.
      initialize = function(seed){
         self$seed = seed
      },

      #' @description format class
      format = function(){
         return(format_class(self))
      }
   )
)

#' @title CreateModelInput
#' @description A class containing parameters which can be used to create a SageMaker Model
#'              Parameters:
#' @export
CreateModelInput = R6Class("CreateModelInput",
  public = list(

    #' @field instance_type
    #' type or EC2 instance will be used for model deployment
    instance_type = NULL,

    #' @field accelerator_type
    #' elastic inference accelerator type.
    accelerator_type = NULL,

    #' @description Initialize CreateModelInput class
    #' @param instance_type (str): type or EC2 instance will be used for model deployment.
    #' @param accelerator_type (str): elastic inference accelerator type.
    initialize = function(instance_type=NULL,
                          accelerator_type=NULL){
      stopifnot(is.character(instance_type) || is.null(instance_type),
                is.character(accelerator_type) || is.null(accelerator_type))
      self$instance_type = instance_type
      self$accelerator_type = accelerator_type
    },

    #' @description format class
    format = function(){
      return(format_class(self))
    }
  )
)

#' @title CompilationInput
#' @description Create a class containing all the parameters.
#'              It can be used when calling ``Model$compile_model()``
#' @export
CompilationInput = R6Class("CompilationInput",
  public = list(

    #' @field target_instance_type
    #' Identifies the device that you want to run your model after compilation
    target_instance_type=NULL,

    #' @field input_shape
    #' Specifies the name and shape of the expected inputs for your trained model
    input_shape=NULL,

    #' @field output_path
    #' Specifies where to store the compiled model
    output_path=NULL,

    #' @field framework
    #' The framework that is used to train the original model
    framework=NULL,

    #' @field framework_version
    #' The version of the framework
    framework_version=NULL,

    #' @field compile_max_run
    #' Timeout in seconds for compilation
    compile_max_run=15*60,

    #' @field tags
    #' List of tags for labelling a compilation job
    tags=NULL,

    #' @field job_name
    #' The name of the compilation job
    job_name=NULL,

    #' @field target_platform_os
    #' Target Platform OS
    target_platform_os=NULL,

    #' @field target_platform_arch
    #' Target Platform Architecture
    target_platform_arch=NULL,

    #' @field target_platform_accelerator
    #' Target Platform Accelerator
    target_platform_accelerator=NULL,

    #' @field compiler_options
    #' Additional parameters for compiler
    compiler_options=NULL,

    #' @description Initialize CompilationInput class
    #' @param target_instance_type (str): Identifies the device that you want to
    #'              run your model after compilation, for example: ml_c5. For allowed
    #'              strings see
    #'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_OutputConfig.html}.
    #' @param input_shape (str): Specifies the name and shape of the expected
    #'              inputs for your trained model in json dictionary form
    #' @param output_path (str): Specifies where to store the compiled model
    #' @param framework (str, optional): The framework that is used to train the original
    #'              model. Allowed values: 'mxnet', 'tensorflow', 'keras', 'pytorch',
    #'              'onnx', 'xgboost' (default: None)
    #' @param framework_version (str, optional): The version of the framework (default: None)
    #' @param compile_max_run (int, optional): Timeout in seconds for compilation (default:
    #'              15 * 60). After this amount of time Amazon SageMaker Neo
    #'              terminates the compilation job regardless of its current status.
    #' @param tags (list[dict], optional): List of tags for labelling a compilation job.
    #'              For more, see
    #'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html}.
    #' @param job_name (str, optional): The name of the compilation job (default: None)
    #' @param target_platform_os (str, optional): Target Platform OS, for example: 'LINUX'.
    #'              (default: None)
    #'              For allowed strings see
    #'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_OutputConfig.html}.
    #'              It can be used instead of target_instance_family.
    #' @param target_platform_arch (str, optional): Target Platform Architecture, for example: 'X86_64'.
    #'              (default: None)
    #'              For allowed strings see
    #'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_OutputConfig.html}.
    #'              It can be used instead of target_instance_family.
    #' @param target_platform_accelerator (str, optional): Target Platform Accelerator,
    #'              for example: 'NVIDIA'. (default: None)
    #'              For allowed strings see
    #'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_OutputConfig.html}.
    #'              It can be used instead of target_instance_family.
    #' @param compiler_options (dict, optional): Additional parameters for compiler. (default: None)
    #'              Compiler Options are TargetPlatform / target_instance_family specific. See
    #'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_OutputConfig.html} for details.
    initialize = function(target_instance_type=NULL,
                          input_shape=NULL,
                          output_path=NULL,
                          framework=NULL,
                          framework_version=NULL,
                          compile_max_run=15*60,
                          tags=NULL,
                          job_name=NULL,
                          target_platform_os=NULL,
                          target_platform_arch=NULL,
                          target_platform_accelerator=NULL,
                          compiler_options=NULL){
      stopifnot(
        is.null(target_instance_type) || is.character(target_instance_type),
        is.null(input_shape) || is.character(input_shape),
        is.null(output_path) || is.character(output_path),
        is.null(framework) || is.character(framework),
        is.null(framework_version) || is.character(framework_version),
        is.numeric(compile_max_run),
        is.null(tags) || is.list(tags),
        is.null(job_name) || is.character(job_name),
        is.null(target_platform_os) || is.character(target_platform_os),
        is.null(target_platform_arch) || is.character(target_platform_arch),
        is.null(target_platform_accelerator) || is.character(target_platform_accelerator),
        is.null(compiler_options) || is.list(compiler_options)
      )
      self$target_instance_type = target_instance_type
      self$input_shape = input_shape
      self$output_path= output_path
      self$framework = framework
      self$framework_version = framework_version
      self$compile_max_run = compile_max_run
      self$tags = tags
      self$job_name = job_name
      self$target_platform_os = target_platform_os
      self$target_platform_arch = target_platform_arch
      self$target_platform_accelerator = target_platform_accelerator
      self$compiler_options = compiler_options
    },

    #' @description format class
    format = function(){
      return(format_class(self))
    }
  )
)

#' @title TransformInput
#' @description Create a class containing all the parameters.
#'              It can be used when calling ``sagemaker.transformer.Transformer.transform()``
TransformInput = R6Class("TransformInput",
  public = list(

    #' @field data
    #' Place holder
    data=NULL,

    #' @field data_type
    #' Place holder
    data_type="S3Prefix",

    #' @field content_type
    #' Place holder
    content_type=NULL,

    #' @field compression_type
    #' Place holder
    compression_type=NULL,

    #' @field split_type
    #' Place holder
    split_type=NULL,

    #' @field input_filter
    #' Place holder
    input_filter=NULL,

    #' @field output_filter
    #' Place holder
    output_filter=NULL,

    #' @field join_source
    #' Place holder
    join_source=NULL,

    #' @field model_client_config
    #' Place holder
    model_client_config=NULL,

    #' @description Initialize TransformInput class
    #' @param data (str): Place holder
    #' @param data_type (str): Place holder
    #' @param content_type (str): Place holder
    #' @param compression_type (str): Place holder
    #' @param split_type (str): Place holder
    #' @param input_filter (str): Place holder
    #' @param output_filter (str): Place holder
    #' @param join_source (str): Place holder
    #' @param model_client_config (str): Place holder
    initialize = function(data=NULL,
                          data_type="S3Prefix",
                          content_type=NULL,
                          compression_type=NULL,
                          split_type=NULL,
                          input_filter=NULL,
                          output_filter=NULL,
                          join_source=NULL,
                          model_client_config=NULL){
      stopifnot(
        is.null(data) || is.character(data),
        is.character(data_type),
        is.null(content_type) || is.character(content_type),
        is.null(compression_type) || is.character(compression_type),
        is.null(split_type) || is.character(split_type),
        is.null(input_filter) || is.character(input_filter),
        is.null(output_filter) || is.character(output_filter),
        is.null(join_source) || is.character(join_source),
        is.null(model_client_config) || is.list(model_client_config)
      )
      self$data=data
      self$data_type=data_type
      self$content_type=content_type
      self$compression_type=content_type
      self$split_type=split_type
      self$input_filter=input_filter
      self$output_filter=output_filter
      self$join_source=join_source
      self$model_client_config=model_client_config
    },

    #' @description format class
    format = function(){
      return(format_class(self))
    }
  )
)


#' @title Amazon SageMaker channel configurations for file system data sources.
#' @export
FileSystemInput = R6Class("FileSystemInput",
  public = list(
    #' @field config (dict[str, dict])\cr
    #' A Sagemaker File System ``DataSource``.
    config = NULL,

    #' @description Create a new file system input used by an SageMaker training job.
    #' @param file_system_id (str): An Amazon file system ID starting with 'fs-'.
    #' @param file_system_type (str): The type of file system used for the input.
    #'              Valid values: 'EFS', 'FSxLustre'.
    #' @param directory_path (str): Absolute or normalized path to the root directory (mount point) in
    #'              the file system.
    #'              Reference: https://docs.aws.amazon.com/efs/latest/ug/mounting-fs.html and
    #'              https://docs.aws.amazon.com/fsx/latest/LustreGuide/mount-fs-auto-mount-onreboot.html
    #' @param file_system_access_mode (str): Permissions for read and write.
    #'              Valid values: 'ro' or 'rw'. Defaults to 'ro'.
    #' @param content_type :
    initialize = function(file_system_id,
                          file_system_type = c("FSxLustre", "EFS"),
                          directory_path,
                          file_system_access_mode= c("ro", "rw"),
                          content_type=NULL){

      file_system_type = match.arg(file_system_type)
      file_system_access_mode = match.arg(file_system_access_mode)

      self$config = list(
        "DataSource"= list(
          "FileSystemDataSource"= list(
            "FileSystemId"= file_system_id,
            "FileSystemType"= file_system_type,
            "DirectoryPath"= directory_path,
            "FileSystemAccessMode"= file_system_access_mode
          )
        )
      )
      self$config$ContentType = content_type
    },

    #' @description format class
    format = function(){
       return(format_class(self))
    }
  )
)
