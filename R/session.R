# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/session.py

#' @include r_utils.R
#' @include logs.R
#' @include paws_session.R
#' @include vpc_utils.R
#' @include studio.R
#' @include pkg_variables.R

#' @import R6
#' @import jsonlite
#' @import lgr
#' @importFrom fs is_dir dir_ls path_dir path_rel path_file path path_ext dir_create
#' @importFrom utils modifyList

NOTEBOOK_METADATA_FILE <- "/opt/ml/metadata/resource-metadata.json"

.STATUS_CODE_TABLE <- list(
  "COMPLETED"= "Completed",
  "INPROGRESS"= "InProgress",
  "FAILED"= "Failed",
  "STOPPED"= "Stopped",
  "STOPPING"= "Stopping",
  "STARTING"= "Starting")

LogState <- list(STARTING = 1,
                 WAIT_IN_PROGRESS = 2,
                 TAILING = 3,
                 JOB_COMPLETE = 4,
                 COMPLETE = 5)

#' @title Sagemaker Session Class
#' @name Session
#' @description Manage interactions with the Amazon SageMaker APIs and any other AWS services needed.
#'              This class provides convenient methods for manipulating entities and resources that Amazon
#'              SageMaker uses, such as training jobs, endpoints, and input datasets in S3.
#'              AWS service calls are delegated to an underlying paws session, which by default
#'              is initialized using the AWS configuration chain. When you make an Amazon SageMaker API call
#'              that accesses an S3 bucket location and one is not specified, the ``Session`` creates a default
#'              bucket based on a naming convention which includes the current AWS account ID.
#' @export
Session = R6Class("Session",
  public = list(

    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    #'              Initialize a SageMaker \code{Session}.
    #' @param paws_session (\link[sagemaker.common]{PawsSession}): The underlying AWS credentails passed to paws SDK.
    #' @param sagemaker_client (\link[paws]{sagemaker}): Client which makes Amazon SageMaker service
    #'              calls other than ``InvokeEndpoint`` (default: None). Estimators created using this
    #'              ``Session`` use this client. If not provided, one will be created using this
    #'              instance's ``paws session``.
    #' @param sagemaker_runtime_client (\link[paws]{sagemakerruntime}): Client which makes
    #'              ``InvokeEndpoint`` calls to Amazon SageMaker (default: None). Predictors created
    #'              using this ``Session`` use this client. If not provided, one will be created using
    #'              this instance's ``paws session``.
    #' @param default_bucket (str): The default Amazon S3 bucket to be used by this session.
    #'              This will be created the next time an Amazon S3 bucket is needed (by calling
    #'              :func:\code{default_bucket}).
    #'              If not provided, a default bucket will be created based on the following format:
    #'              "sagemaker-{region}-{aws-account-id}". Example: "sagemaker-my-custom-bucket".
    initialize = function(paws_session = NULL,
                          sagemaker_client = NULL,
                          sagemaker_runtime_client = NULL,
                          default_bucket = NULL) {
      private$.default_bucket_name_override = default_bucket
      self$s3 = NULL
      self$config = NULL
      self$lambda_client = NULL

      private$.initialize(
        paws_session=paws_session,
        sagemaker_client=sagemaker_client,
        sagemaker_runtime_client=sagemaker_runtime_client,
        sagemaker_featurestore_runtime_client=NULL)
    },

    #' @description Upload local file or directory to S3.If a single file is specified for upload, the resulting S3 object key is
    #'              ``{key_prefix}/{filename}`` (filename does not include the local path, if any specified).
    #'              If a directory is specified for upload, the API uploads all content, recursively,
    #'              preserving relative structure of subdirectories. The resulting object key names are:
    #'              ``{key_prefix}/{relative_subdirectory_path}/filename``.
    #' @param path (str): Path (absolute or relative) of local file or directory to upload.
    #' @param bucket (str): Name of the S3 Bucket to upload to (default: None). If not specified, the
    #'              default bucket of the ``Session`` is used (if default bucket does not exist, the
    #'              ``Session`` creates it).
    #' @param key_prefix (str): Optional S3 object key name prefix (default: 'data'). S3 uses the
    #'              prefix to create a directory structure for the bucket content that it display in
    #'              the S3 console.
    #' @param ... (any): Optional extra arguments that may be passed to the upload operation.
    #'              Similar to ExtraArgs parameter in S3 upload_file function. Please refer to the
    #'              ExtraArgs parameter documentation here:
    #'              https://boto3.amazonaws.com/v1/documentation/api/latest/guide/s3-uploading-files.html#the-extraargs-parameter
    #' @return str: The S3 URI of the uploaded file(s). If a file is specified in the path argument,
    #'              the URI format is: ``s3://{bucket name}/{key_prefix}/{original_file_name}``.
    #'              If a directory is specified in the path argument, the URI format is ``s3://{bucket name}/{key_prefix}``.
    upload_data = function(path, bucket = NULL, key_prefix = "data", ...){

      key_suffix = NULL
      # get all files in directory
      if(fs::is_dir(path)){
        local_path = fs::dir_ls(path, all = T, type = "file", recurse = T)
        dirpath = fs::path_dir(local_path)
        s3_relative_prefix = ifelse(path == dirpath, "", fs::path_rel(dirpath))
        s3_key = fs::path(key_prefix, s3_relative_prefix, fs::path_file(local_path))
      } else {
        name = fs::path_file(path)
        local_path = path
        s3_key = fs::path(key_prefix, name)
        key_suffix = name
      }

      # if bucke parameter hasn't been selected use class parameter
      bucket = bucket %||% self$default_bucket()

      # Upload file to s3
      for (i in 1:length(local_path)){
        s3_upload(self$s3, local_path[i], bucket, s3_key[i], ...)
      }
      s3_uri = sprintf("s3://%s/%s", bucket, key_prefix)
      # If a specific file was used as input (instead of a directory), we return the full S3 key
      # of the uploaded object. This prevents unintentionally using other files under the same
      # prefix during training.
      if (!is.null(key_suffix))
        s3_uri = sprintf("%s/%s", gsub("/$","", s3_uri), key_suffix)

      return(s3_uri)
    },

    #' @description Upload a string as a file body.
    #' @param body (str): String representing the body of the file.
    #' @param bucket (str): Name of the S3 Bucket to upload to (default: None). If not specified, the
    #'              default bucket of the ``Session`` is used (if default bucket does not exist, the
    #'              ``Session`` creates it).
    #' @param key (str): S3 object key. This is the s3 path to the file.
    #' @param kms_key (str): The KMS key to use for encrypting the file.
    #' @return str: The S3 URI of the uploaded file.
    #'              The URI format is: ``s3://{bucket name}/{key}``.
    upload_string_as_file_body = function(body,
                                          bucket,
                                          key,
                                          kms_key=NULL){
      params = list(
        Bucket=bucket,
        Body=charToRaw(body),
        Key = key
      )
      if (!is.null(kms_key)){
        params[["SSEKMSKeyId"]] = kms_key
        params[["ServerSideEncryption"]] = "aws:kms"
      }

      do.call(self$s3$put_object, params)

      s3_uri = sprintf("s3://%s/%s",bucket, key)
      return (s3_uri)
    },

    #' @description Download file or directory from S3.
    #' @param path (str): Local path where the file or directory should be downloaded to.
    #' @param bucket (str): Name of the S3 Bucket to download from.
    #' @param key_prefix (str): Optional S3 object key name prefix.
    #' @param ... (any): Optional extra arguments that may be passed to the
    #'              download operation. Please refer to the ExtraArgs parameter in the boto3
    #'              documentation here:
    #'              https://boto3.amazonaws.com/v1/documentation/api/latest/guide/s3-example-download-file.html
    #' @return
    #' NULL invisibly
    download_data = function(path, bucket, key_prefix = "", ...){

      keys = character()
      params = list(Bucket=bucket, Prefix=key_prefix, ContinuationToken=NULL)

      # Loop through the contents of the bucket, 1,000 objects at a time. Gathering all keys into
      # a "keys" list.
      while(!identical(params$ContinuationToken, character(0))){
        response = do.call(self$s3$list_objects_v2, params)
        # For each object, save its key or directory.
        keys = c(keys, sapply(response$Contents, function(x) x$Key))
        params$ContinuationToken = response$ContinuationToken
      }

      # For each object key, create the directory on the local machine if needed, and then
      tail_s3_uri_path = fs::path_file(keys)
      if (nchar(fs::path_ext(key_prefix)) == 0)
        tail_s3_uri_path = fs::path_rel(keys, key_prefix)
      destination_path = fs::path(path, tail_s3_uri_path)
      fs::dir_create(fs::path_dir(destination_path))

      # download the file.
      for (i in 1:length(keys)){
        obj = self$s3$get_object(Bucket = bucket, Key = keys[i], ...)
        write_bin(obj$Body, destination_path[i])
      }
      return(invisible(NULL))
    },

    #' @description Read a single file from S3.
    #' @param bucket (str): Name of the S3 Bucket to download from.
    #' @param key_prefix (str): S3 object key name prefix.
    #' @return str: The body of the s3 file as a string.
    read_s3_file = function(bucket,
                            key_prefix){
      # Explicitly passing a None kms_key to paws throws a validation error.
      s3_object = self$s3$get_object(Bucket=bucket, Key=key_prefix)
      con = rawConnection(s3_object$Body)
      on.exit(close(con))
      return(readLines(con, encoding = "UTF-8", warn = FALSE))
    },

    #' @description Lists the S3 files given an S3 bucket and key.
    #' @param bucket (str): Name of the S3 Bucket to download from.
    #' @param key_prefix (str): S3 object key name prefix.
    #' @return (str): The list of files at the S3 path.
    list_s3_files = function(bucket, key_prefix=NULL){
      next_token = NULL
      keys = character()
      # Loop through the contents of the bucket, 1,000 objects at a time. Gathering all keys into
      # a "keys" list.
      while(!identical(next_token, character(0))){
        response = self$s3$list_objects_v2(
          Bucket = bucket,
          Prefix = key_prefix,
          ContinuationToken = next_token)
        keys = c(keys, sapply(response$Contents, function(x) x$Key))
        next_token = response$ContinuationToken
      }
      return(keys)
    },

    #' @description Return the name of the default bucket to use in relevant Amazon SageMaker interactions.
    #' @return (str): The name of the default bucket, which is of the form:
    #'  ``sagemaker-{region}-{AWS account ID}``.
    default_bucket = function(){

      if (!is.null(private$.default_bucket)) return(private$.default_bucket)

      region = self$paws_region_name

      default_bucket = private$.default_bucket_name_override

      if(is.null(default_bucket)) {
        account = self$paws_session$client(
          "sts", config = list(region = region, endpoint = sts_regional_endpoint(region))
          )$get_caller_identity()$Account
        default_bucket = sprintf("sagemaker-%s-%s", region, account)
      }

      private$.create_s3_bucket_if_it_does_not_exist(bucket_name = self$bucket, region = region)

      private$.default_bucket = default_bucket

      return(private$.default_bucket)
    },

    #' @description Create an Amazon SageMaker training job. Train the learner on a set of observations of the provided `task`.
    #'              Mutates the learner by reference, i.e. stores the model alongside other information in field `$state`.
    #'
    #' @param input_mode (str): The input mode that the algorithm supports. Valid modes:
    #'              \itemize{
    #'                \item{\strong{'File':} Amazon SageMaker copies the training dataset from the S3 location to
    #'                        a directory in the Docker container.}
    #'                \item{\strong{'Pipe':} Amazon SageMaker streams data directly from S3 to the container via a
    #'                        Unix-named pipe.}}
    #' @param input_config (list): A list of Channel objects. Each channel is a named input source.
    #'              Please refer to the format details described:
    #'              https://botocore.readthedocs.io/en/latest/reference/services/sagemaker.html#SageMaker.Client.create_training_job
    #' @param role (str): An AWS IAM role (either name or full ARN). The Amazon SageMaker training
    #'              jobs and APIs that create Amazon SageMaker endpoints use this role to access
    #'              training data and model artifacts. You must grant sufficient permissions to this
    #'              role.
    #' @param job_name (str): Name of the training job being created.
    #' @param output_config (dict): The S3 URI where you want to store the training results and
    #'              optional KMS key ID.
    #' @param resource_config (dict): Contains values for ResourceConfig:
    #'              \itemize{
    #'                \item{\strong{instance_count (int):} Number of EC2 instances to use for training.
    #'                              The key in resource_config is 'InstanceCount'.}
    #'                \item{\strong{instance_type (str):} Type of EC2 instance to use for training, for example,
    #'                              'ml.c4.xlarge'. The key in resource_config is 'InstanceType'.}}
    #' @param vpc_config (dict): Contains values for VpcConfig:
    #'              \itemize{
    #'                \item{\strong{subnets (list[str]):} List of subnet ids.
    #'                              The key in vpc_config is 'Subnets'.}
    #'                \item{\strong{security_group_ids (list[str]):} List of security group ids.
    #'                              The key in vpc_config is 'SecurityGroupIds'.}}
    #' @param hyperparameters (dict): Hyperparameters for model training. The hyperparameters are
    #'              made accessible as a dict[str, str] to the training code on SageMaker. For
    #'              convenience, this accepts other types for keys and values, but ``str()`` will be
    #'              called to convert them before training.
    #' @param stop_condition (dict): Defines when training shall finish. Contains entries that can
    #'              be understood by the service like ``MaxRuntimeInSeconds``.
    #' @param tags (list[dict]): List of tags for labeling a training job. For more, see
    #'              https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html.
    #' @param metric_definitions (list[dict]): A list of dictionaries that defines the metric(s)
    #'              used to evaluate the training jobs. Each dictionary contains two keys: 'Name' for
    #'              the name of the metric, and 'Regex' for the regular expression used to extract the
    #'              metric from the logs.
    #' @param enable_network_isolation (bool): Whether to request for the training job to run with
    #'              network isolation or not.
    #' @param image_uri (str): Docker image_uri containing training code.
    #' @param algorithm_arn (str): Algorithm Arn from Marketplace.
    #' @param encrypt_inter_container_traffic (bool): Specifies whether traffic between training
    #'              containers is encrypted for the training job (default: ``False``).
    #' @param use_spot_instances (bool): whether to use spot instances for training.
    #' @param checkpoint_s3_uri (str): The S3 URI in which to persist checkpoints
    #'              that the algorithm persists (if any) during training. (default: ``None``).
    #' @param checkpoint_local_path (str): The local path that the algorithm
    #'              writes its checkpoints to. SageMaker will persist all files
    #'              under this path to `checkpoint_s3_uri` continually during
    #'              training. On job startup the reverse happens - data from the
    #'              s3 location is downloaded to this path before the algorithm is
    #'              started. If the path is unset then SageMaker assumes the
    #'              checkpoints will be provided under `/opt/ml/checkpoints/`.
    #'              (Default: \code{NULL}).
    #' @param experiment_config (dict): Experiment management configuration. Dictionary contains
    #'              three optional keys, 'ExperimentName', 'TrialName', and 'TrialComponentDisplayName'.
    #'              (Default: \code{NULL})
    #' @param debugger_rule_configs Configuration information for debugging rules
    #' @param debugger_hook_config Configuration information for debugging rules
    #' @param tensorboard_output_config Xonfiguration information for tensorboard output
    #' @param enable_sagemaker_metrics (bool): enable SageMaker Metrics Time
    #'              Series. For more information see:
    #'              https://docs.aws.amazon.com/sagemaker/latest/dg/API_AlgorithmSpecification.html#SageMaker-Type-AlgorithmSpecification-EnableSageMakerMetricsTimeSeries
    #'              (Default: \code{NULL}).
    #' @param profiler_rule_configs (list[dict]): A list of profiler rule configurations.
    #' @param profiler_config (dict): Configuration for how profiling information is emitted
    #'              with SageMaker Profiler. (default: ``None``).
    #' @param environment (dict[str, str]) : Environment variables to be set for
    #'              use during training job (default: ``None``)
    #' @param retry_strategy (dict): Defines RetryStrategy for InternalServerFailures.
    #'              * max_retry_attsmpts (int): Number of times a job should be retried.
    #'              The key in RetryStrategy is 'MaxRetryAttempts'.
    #' @return
    #' str: ARN of the training job, if it is created.
    train = function(input_mode,
                     input_config,
                     role,
                     job_name,
                     output_config = NULL,
                     resource_config = NULL,
                     vpc_config = NULL,
                     hyperparameters = NULL,
                     stop_condition = NULL,
                     tags = NULL,
                     metric_definitions = NULL,
                     enable_network_isolation=FALSE,
                     image_uri=NULL,
                     algorithm_arn=NULL,
                     encrypt_inter_container_traffic=FALSE,
                     use_spot_instances=FALSE,
                     checkpoint_s3_uri=NULL,
                     checkpoint_local_path=NULL,
                     experiment_config=NULL,
                     debugger_rule_configs=NULL,
                     debugger_hook_config=NULL,
                     tensorboard_output_config=NULL,
                     enable_sagemaker_metrics=NULL,
                     profiler_rule_configs=NULL,
                     profiler_config=NULL,
                     environment=NULL,
                     retry_strategy=NULL){

      tags = .append_project_tags(tags)
      train_request = private$.get_train_request(
        input_mode=input_mode,
        input_config=input_config,
        role=role,
        job_name=job_name,
        output_config=output_config,
        resource_config=resource_config,
        vpc_config=vpc_config,
        hyperparameters=hyperparameters,
        stop_condition=stop_condition,
        tags=tags,
        metric_definitions=metric_definitions,
        enable_network_isolation=enable_network_isolation,
        image_uri=image_uri,
        algorithm_arn=algorithm_arn,
        encrypt_inter_container_traffic=encrypt_inter_container_traffic,
        use_spot_instances=use_spot_instances,
        checkpoint_s3_uri=checkpoint_s3_uri,
        checkpoint_local_path=checkpoint_local_path,
        experiment_config=experiment_config,
        debugger_rule_configs=debugger_rule_configs,
        debugger_hook_config=debugger_hook_config,
        tensorboard_output_config=tensorboard_output_config,
        enable_sagemaker_metrics=enable_sagemaker_metrics,
        profiler_rule_configs=profiler_rule_configs,
        profiler_config=profiler_config,
        environment=enviornment,
        retry_strategy=retry_strategy
      )

      LOGGER$info("Creating training-job with name: %s", job_name)
      LOGGER$debug("train request: %s", toJSON(train_request, pretty = T, auto_unbox = T))

      return(do.call(self$sagemaker$create_training_job, train_request))
    },

    #' @description Calls the UpdateTrainingJob API for the given job name and returns the response.
    #' @param job_name (str): Name of the training job being updated.
    #' @param profiler_rule_configs (list): List of profiler rule configurations. (default: ``None``).
    #' @param profiler_config (dict): Configuration for how profiling information is emitted with
    #'              SageMaker Profiler. (default: ``None``).
    update_training_job = function(job_name,
                                   profiler_rule_configs=NULL,
                                   profiler_config=NULL){
      update_training_job_request = private$.get_update_training_job_request(
        job_name=job_name,
        profiler_rule_configs=profiler_rule_configs,
        profiler_config=profiler_config
      )
      LOGGER$info("Updating training job with name %s", job_name)
      LOGGER$debug("Update request: %s", toJSON(update_training_job_request, pretty = T, auto_unbox = T))
      return(do.call(self$sagemaker$update_training_job, update_training_job_request))
    },

    #' @description Create an Amazon SageMaker processing job.
    #' @param inputs ([dict]): List of up to 10 ProcessingInput dictionaries.
    #' @param output_config (dict): A config dictionary, which contains a list of up
    #'               to 10 ProcessingOutput dictionaries, as well as an optional KMS key ID.
    #' @param job_name (str): The name of the processing job. The name must be unique
    #'               within an AWS Region in an AWS account. Names should have minimum
    #'               length of 1 and maximum length of 63 characters.
    #' @param resources (dict): Encapsulates the resources, including ML instances
    #'               and storage, to use for the processing job.
    #' @param stopping_condition (dict[str,int]): Specifies a limit to how long
    #'               the processing job can run, in seconds.
    #' @param app_specification (dict[str,str]): Configures the processing job to
    #'               run the given image. Details are in the processing container
    #'               specification.
    #' @param environment (dict): Environment variables to start the processing
    #'               container with.
    #' @param network_config (dict): Specifies networking options, such as network
    #'               traffic encryption between processing containers, whether to allow
    #'               inbound and outbound network calls to and from processing containers,
    #'               and VPC subnets and security groups to use for VPC-enabled processing
    #'               jobs.
    #' @param role_arn (str): The Amazon Resource Name (ARN) of an IAM role that
    #'               Amazon SageMaker can assume to perform tasks on your behalf.
    #' @param tags ([dict[str,str]]): A list of dictionaries containing key-value
    #'               pairs.
    #' @param experiment_config (dict): Experiment management configuration. Dictionary contains
    #'               three optional keys, 'ExperimentName', 'TrialName', and 'TrialComponentDisplayName'.
    #'               (Default: \code{NULL})
    process = function(inputs = NULL,
                       output_config = NULL,
                       job_name = NULL,
                       resources = NULL,
                       stopping_condition = NULL,
                       app_specification = NULL,
                       environment = NULL,
                       network_config = NULL,
                       role_arn,
                       tags = NULL,
                       experiment_config=NULL){
      tags = .append_project_tags(tags)
      process_request = private$.get_process_request(
        inputs=inputs,
        output_config=output_config,
        job_name=job_name,
        resources=resources,
        stopping_condition=stopping_condition,
        app_specification=app_specification,
        environment=environment,
        network_config=network_config,
        role_arn=role_arn,
        tags=tags,
        experiment_config=experiment_config
      )
      LOGGER$info("Creating processing-job with name %s", job_name)
      LOGGER$debug("process request: %s", toJSON(process_request, pretty = T, auto_unbox = T))
      return(do.call(self$sagemaker$create_processing_job, process_request))
    },

    #' @description Create an Amazon SageMaker monitoring schedule.
    #' @param monitoring_schedule_name (str): The name of the monitoring schedule. The name must be
    #'              unique within an AWS Region in an AWS account. Names should have a minimum length
    #'              of 1 and a maximum length of 63 characters.
    #' @param schedule_expression (str): The cron expression that dictates the monitoring execution
    #'              schedule.
    #' @param statistics_s3_uri (str): The S3 uri of the statistics file to use.
    #' @param constraints_s3_uri (str): The S3 uri of the constraints file to use.
    #' @param monitoring_inputs ([dict]): List of MonitoringInput dictionaries.
    #' @param monitoring_output_config (dict): A config dictionary, which contains a list of
    #'              MonitoringOutput dictionaries, as well as an optional KMS key ID.
    #' @param instance_count (int): The number of instances to run.
    #' @param instance_type (str): The type of instance to run.
    #' @param volume_size_in_gb (int): Size of the volume in GB.
    #' @param volume_kms_key (str): KMS key to use when encrypting the volume.
    #' @param image_uri (str): The image uri to use for monitoring executions.
    #' @param entrypoint (str): The entrypoint to the monitoring execution image.
    #' @param arguments (str): The arguments to pass to the monitoring execution image.
    #' @param record_preprocessor_source_uri (str or None): The S3 uri that points to the script that
    #'              pre-processes the dataset (only applicable to first-party images).
    #' @param post_analytics_processor_source_uri (str or None): The S3 uri that points to the script
    #'              that post-processes the dataset (only applicable to first-party images).
    #' @param max_runtime_in_seconds (int): Specifies a limit to how long
    #'              the processing job can run, in seconds.
    #' @param environment (dict): Environment variables to start the monitoring execution
    #'              container with.
    #' @param network_config (dict): Specifies networking options, such as network
    #'              traffic encryption between processing containers, whether to allow
    #'              inbound and outbound network calls to and from processing containers,
    #'              and VPC subnets and security groups to use for VPC-enabled processing
    #'              jobs.
    #' @param role_arn (str): The Amazon Resource Name (ARN) of an IAM role that
    #'              Amazon SageMaker can assume to perform tasks on your behalf.
    #' @param tags ([dict[str,str]]): A list of dictionaries containing key-value
    #'              pairs.
    create_monitoring_schedule = function(monitoring_schedule_name,
                                          schedule_expression = NULL,
                                          statistics_s3_uri = NULL,
                                          constraints_s3_uri = NULL,
                                          monitoring_inputs = NULL,
                                          monitoring_output_config = NULL,
                                          instance_count = 1,
                                          instance_type = NULL,
                                          volume_size_in_gb = NULL,
                                          volume_kms_key = NULL,
                                          image_uri = NULL,
                                          entrypoint = NULL,
                                          arguments = NULL,
                                          record_preprocessor_source_uri = NULL,
                                          post_analytics_processor_source_uri = NULL,
                                          max_runtime_in_seconds = NULL,
                                          environment = NULL,
                                          network_config = NULL,
                                          role_arn= NULL,
                                          tags = NULL){

      monitoring_schedule_request = list(
        MonitoringScheduleName = monitoring_schedule_name,
        MonitoringScheduleConfig = list(MonitoringJobDefinition =
                                        list(MonitoringInputs = monitoring_inputs,
                                             RoleArn = role_arn,
                                             MonitoringAppSpecification = list(ImageUri = image_uri))))

      MonitoringResources = list(ClusterConfig = list(
        InstanceCount = instance_count,
        InstanceType = instance_type,
        VolumeSizeInGB = volume_size_in_gb))

      monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringResources = list(MonitoringResources)

      if(!is.null(schedule_expression)) monitoring_schedule_request$MonitoringScheduleConfig$ScheduleConfig = list(ScheduleExpression = schedule_expression)

      monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringOutputConfig = monitoring_output_config

      BaselineConfig = NULL
      if (!is.null(statistics_s3_uri) || !is.null(constraints_s3_uri)){
        BaselineConfig = list()
        if(!is.null(statistics_s3_uri)) BaselineConfig$StatisticsResource = list(S3Uri = statistics_s3_uri)
        if(!is.null(constraints_s3_uri)) BaselineConfig$ConstraintsResource = list(S3Uri = constraints_s3_uri)
      }

      monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$BaselineConfig = BaselineConfig
      monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringAppSpecification$RecordPreprocessorSourceUri = record_preprocessor_source_uri
      monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringAppSpecification$PostAnalyticsProcessorSourceUri = post_analytics_processor_source_uri
      monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringAppSpecification$ContainerEntrypoint = entrypoint
      monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringAppSpecification$ContainerArguments = arguments
      monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringResources$ClusterConfig$VolumeKmsKeyId = volume_kms_key

      if(!is.null(max_runtime_in_seconds))
        monitoring_schedule_request$MonitoringScheduleConfig$MonitoringScheduleConfig$MonitoringJobDefinition$StoppingCondition = list(MaxRuntimeInSeconds = max_runtime_in_seconds)

      monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$Environment = environment
      monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$NetworkConfig = network_config

      tags = .append_project_tags(tags)
      if (!islistempty(tags))
        monitoring_schedule_request[["Tags"]] = tags

      LOGGER$info("Creating monitoring schedule name %s", monitoring_schedule_name)
      LOGGER$debug("monitoring_schedule_request= %s", toJSON(monitoring_schedule_request, pretty = T, auto_unbox = T))

      return(do.call(self$sagemaker$create_monitoring_schedule, monitoring_schedule_request))
    },

    #' @description Update an Amazon SageMaker monitoring schedule.
    #' @param monitoring_schedule_name (str): The name of the monitoring schedule. The name must be
    #'              unique within an AWS Region in an AWS account. Names should have a minimum length
    #'              of 1 and a maximum length of 63 characters.
    #' @param schedule_expression (str): The cron expression that dictates the monitoring execution
    #'              schedule.
    #' @param statistics_s3_uri (str): The S3 uri of the statistics file to use.
    #' @param constraints_s3_uri (str): The S3 uri of the constraints file to use.
    #' @param monitoring_inputs ([dict]): List of MonitoringInput dictionaries.
    #' @param monitoring_output_config (dict): A config dictionary, which contains a list of
    #'              MonitoringOutput dictionaries, as well as an optional KMS key ID.
    #' @param instance_count (int): The number of instances to run.
    #' @param instance_type (str): The type of instance to run.
    #' @param volume_size_in_gb (int): Size of the volume in GB.
    #' @param volume_kms_key (str): KMS key to use when encrypting the volume.
    #' @param image_uri (str): The image uri to use for monitoring executions.
    #' @param entrypoint (str): The entrypoint to the monitoring execution image.
    #' @param arguments (str): The arguments to pass to the monitoring execution image.
    #' @param record_preprocessor_source_uri (str or None): The S3 uri that points to the script that
    #' @param pre-processes the dataset (only applicable to first-party images).
    #' @param post_analytics_processor_source_uri (str or None): The S3 uri that points to the script
    #'              that post-processes the dataset (only applicable to first-party images).
    #' @param max_runtime_in_seconds (int): Specifies a limit to how long
    #'              the processing job can run, in seconds.
    #' @param environment (dict): Environment variables to start the monitoring execution
    #'              container with.
    #' @param network_config (dict): Specifies networking options, such as network
    #'             traffic encryption between processing containers, whether to allow
    #'             inbound and outbound network calls to and from processing containers,
    #'             and VPC subnets and security groups to use for VPC-enabled processing
    #'             jobs.
    #' @param role_arn (str): The Amazon Resource Name (ARN) of an IAM role that
    #'             Amazon SageMaker can assume to perform tasks on your behalf.
    update_monitoring_schedule = function(monitoring_schedule_name,
                                          schedule_expression=NULL,
                                          statistics_s3_uri=NULL,
                                          constraints_s3_uri=NULL,
                                          monitoring_inputs=NULL,
                                          monitoring_output_config=NULL,
                                          instance_count=NULL,
                                          instance_type=NULL,
                                          volume_size_in_gb=NULL,
                                          volume_kms_key=NULL,
                                          image_uri=NULL,
                                          entrypoint=NULL,
                                          arguments=NULL,
                                          record_preprocessor_source_uri=NULL,
                                          post_analytics_processor_source_uri=NULL,
                                          max_runtime_in_seconds=NULL,
                                          environment=NULL,
                                          network_config=NULL,
                                          role_arn=NULL){
      existing_desc = self$sagemaker$describe_monitoring_schedule(MonitoringScheduleName=monitoring_schedule_name)

      existing_schedule_config = NULL
      if (!is.null(existing_desc$MonitoringScheduleConfig)
          && !is.null(existing_desc$MonitoringScheduleConfig$ScheduleConfig)
          && !is.null(existing_desc$MonitoringScheduleConfig$ScheduleConfig$ScheduleExpression)){
        existing_schedule_config = existing_desc$MonitoringScheduleConfig$ScheduleConfig$ScheduleExpression
      }

      request_schedule_expression = schedule_expression %||% existing_schedule_config
      request_monitoring_inputs = monitoring_inputs %||% existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringInputs
      request_instance_count = instance_count %||% existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringResources$ClusterConfig$InstanceCount
      request_instance_type = instance_type %||% existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringResources$ClusterConfig$InstanceType
      request_volume_size_in_gb = volume_size_in_gb %||% existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringResources$ClusterConfig$VolumeSizeInGB
      request_image_uri = image_uri %||% existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringAppSpecification$ImageUri
      request_role_arn = role_arn %||% existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$RoleArn

      monitoring_schedule_request = list(
        MonitoringScheduleName = monitoring_schedule_name,
        MonitoringScheduleConfig = list(MonitoringJobDefinition = list(
          MonitoringInputs = request_monitoring_inputs,
          MonitoringResources = list(
            ClusterConfig = list(
              InstanceCount = request_instance_count,
              InstanceType = request_instance_type,
              VolumeSizeInGB = request_volume_size_in_gb)),
          MonitoringAppSpecification = list(ImageUri = request_image_uri),
          RoleArn = request_role_arn)))

      if(!is.null(existing_schedule_config))
        monitoring_schedule_request$MonitoringScheduleConfig$ScheduleConfig$ScheduleExpression = list(ScheduleExpression = request_schedule_expression)

      existing_monitoring_output_config = existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringOutputConfig

      monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringOutputConfig = monitoring_output_config %||% existing_monitoring_output_config

      existing_statistics_s3_uri = NULL
      existing_constraints_s3_uri = NULL

      if(!is.null(existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$BaselineConfig)){

        if (!is.null(existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$BaselineConfig$StatisticsResource)){
          existing_statistics_s3_uri = existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$BaselineConfig$StatisticsResource$S3Uri}

        if (!is.null(existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$BaselineConfig$ConstraintsResource)){
          existing_constraints_s3_uri = existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$BaselineConfig$ConstraintsResource$S3Uri
        }


        if (!is.null(statistics_s3_uri)
            || !is.null(constraints_s3_uri)
            || !is.null(existing_statistics_s3_uri)
            || !is.null(existing_constraints_s3_uri)){
          monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$BaselineConfig = list()}

        if (!is.null(statistics_s3_uri) || !is.null(existing_statistics_s3_uri)){
          monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$BaselineConfig$StatisticsResource = list(S3Uri= statistics_s3_uri %||% existing_statistics_s3_uri)}

        if (!is.null(constraints_s3_uri) || !is.null(existing_constraints_s3_uri)){
          monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$BaselineConfig$ConstraintsResource = list(S3Uri= constraints_s3_uri %||% existing_constraints_s3_uri)}

        existing_record_preprocessor_source_uri = existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringAppSpecification$RecordPreprocessorSourceUri

        monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringAppSpecification$RecordPreprocessorSourceUri = record_preprocessor_source_uri %||% existing_record_preprocessor_source_uri

        existing_post_analytics_processor_source_uri = existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringAppSpecification$PostAnalyticsProcessorSourceUri

        monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringAppSpecification$PostAnalyticsProcessorSourceUri = post_analytics_processor_source_uri %||% existing_post_analytics_processor_source_uri

        existing_entrypoint = existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringAppSpecification$ContainerEntrypoint

        monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringAppSpecification$ContainerEntrypoint = entrypoint %||% existing_entrypoint

        existing_arguments = existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringAppSpecification$ContainerArguments

        monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringAppSpecification$ContainerArguments = arguments %||% existing_arguments

        existing_volume_kms_key = existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringResources$ClusterConfig$VolumeKmsKeyId

        monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$MonitoringResources$ClusterConfig$VolumeKmsKeyId = volume_kms_key %||% existing_volume_kms_key

        existing_max_runtime_in_seconds = NULL
        if (!is.null(existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$StoppingCondition)){
          existing_max_runtime_in_seconds = existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$StoppingCondition$MaxRuntimeInSeconds}

        if (!is.null(max_runtime_in_seconds) || !is.null(existing_max_runtime_in_seconds)){
          monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$StoppingCondition = list(MaxRuntimeInSeconds= max_runtime_in_seconds %||% existing_max_runtime_in_seconds)}

          existing_environment = existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$Environment

          monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$Environment = environment %||% existing_environment

          existing_network_config = existing_desc$MonitoringScheduleConfig$MonitoringJobDefinition$NetworkConfig

          monitoring_schedule_request$MonitoringScheduleConfig$MonitoringJobDefinition$NetworkConfig = network_config %||% existing_network_config
      }

      LOGGER$info("Updating monitoring schedule with name: %s", monitoring_schedule_name)
      LOGGER$debug("monitoring_schedule_request= %s", toJSON(monitoring_schedule_request, pretty = T, auto_unbox = T))

      return(do.call(self$sagemaker$update_monitoring_schedule, monitoring_schedule_request))
    },

    #' @description Starts a monitoring schedule.
    #' @param monitoring_schedule_name (str): The name of the Amazon SageMaker Monitoring
    #'              Schedule to start.
    start_monitoring_schedule = function(monitoring_schedule_name){
      writeLines("")
      writeLines(sprintf("Starting Monitoring Schedule with name: %s",monitoring_schedule_name))

      self$sagemaker$start_monitoring_schedule(MonitoringScheduleName=monitoring_schedule_name)
    },

    #' @description Stops a monitoring schedule.
    #' @param monitoring_schedule_name (str): The name of the Amazon SageMaker Monitoring
    #'              Schedule to stop.
    stop_monitoring_schedule = function(monitoring_schedule_name){
      writeLines("")
      writeLines(sprintf("Stopping Monitoring Schedule with name: %s",monitoring_schedule_name))
      self$sagemaker$stop_monitoring_schedule(MonitoringScheduleName=monitoring_schedule_name)
    },

    #' @description Deletes a monitoring schedule.
    #' @param monitoring_schedule_name (str): The name of the Amazon SageMaker Monitoring
    #'              Schedule to delete.
    delete_monitoring_schedule = function(monitoring_schedule_name){
      writeLines("")
      writeLines(sprintf("Deleting Monitoring Schedule with name: %s",monitoring_schedule_name))
      self$sagemaker$delete_monitoring_schedule(MonitoringScheduleName=monitoring_schedule_name)
    },

    #' @description  Calls the DescribeMonitoringSchedule API for the given monitoring schedule name
    #'               and returns the response.
    #' @param monitoring_schedule_name (str): The name of the processing job to describe.
    #' @return dict: A dictionary response with the processing job description.
    describe_monitoring_schedule = function(monitoring_schedule_name){
      self$sagemaker$describe_monitoring_schedule(MonitoringScheduleName=monitoring_schedule_name)
    },

    #' @description Lists the monitoring executions associated with the given monitoring_schedule_name.
    #' @param monitoring_schedule_name (str): The monitoring_schedule_name for which to retrieve the
    #'              monitoring executions.
    #' @param sort_by (str): The field to sort by. Can be one of: "CreationTime", "ScheduledTime",
    #'              "Status". Default: "ScheduledTime".
    #' @param sort_order (str): The sort order. Can be one of: "Ascending", "Descending".
    #'               Default: "Descending".
    #' @param max_results (int): The maximum number of results to return. Must be between 1 and 100.
    #' @return dict: Dictionary of monitoring schedule executions.
    list_monitoring_executions = function(monitoring_schedule_name,
                                          sort_by="ScheduledTime",
                                          sort_order="Descending",
                                          max_results=100){
      self$sagemaker$list_monitoring_executions(
        MonitoringScheduleName=monitoring_schedule_name,
        SortBy=sort_by,
        SortOrder=sort_order,
        MaxResults=max_results)
    },

    #' @description Lists the monitoring executions associated with the given monitoring_schedule_name.
    #' @param endpoint_name (str): The name of the endpoint to filter on. If not provided, does not
    #'              filter on it. Default: None.
    #' @param sort_by (str): The field to sort by. Can be one of: "Name", "CreationTime", "Status".
    #'              Default: "CreationTime".
    #' @param sort_order (str): The sort order. Can be one of: "Ascending", "Descending".
    #'              Default: "Descending".
    #' @param max_results (int): The maximum number of results to return. Must be between 1 and 100.
    #' @return dict: Dictionary of monitoring schedule executions.
    list_monitoring_schedules = function(endpoint_name=NULL,
                                         sort_by="CreationTime",
                                         sort_order="Descending",
                                         max_results=100){

      if (!is.null(endpoint_name)){
        response = self$sagemaker$list_monitoring_schedules(
          EndpointName=endpoint_name,
          SortBy=sort_by,
          SortOrder=sort_order,
          MaxResults=max_results,
        )
      } else{
          response = self$sagemaker$list_monitoring_schedules(
            SortBy=sort_by,
            SortOrder=sort_order,
            MaxResults=max_results)}

      return(response)
    },

    #' @description Calls the DescribeProcessingJob API for the given job name
    #'              and returns the True if the job was successful. False otherwise.
    #' @param job_name (str): The name of the processing job to describe.
    #' @return bool: Whether the processing job was successful.
    was_processing_job_successful = function(job_name){
      job_desc = self$sagemaker$describe_processing_job(ProcessingJobName=job_name)
      return(job_desc$ProcessingJobStatus == "Completed")
    },

    #' @description Calls the DescribeProcessingJob API for the given job name
    #'              and returns the response.
    #' @param job_name (str): The name of the processing job to describe.
    #' @return dict: A dictionary response with the processing job description.
    describe_processing_job = function(job_name){
      return(self$sagemaker$describe_processing_job(ProcessingJobName=job_name))
    },

    #' @description Calls the StopProcessingJob API for the given job name.
    #' @param job_name (str): The name of the processing job to stop.
    stop_processing_job = function(job_name){
      return(self$sagemaker$stop_processing_job(ProcessingJobName=job_name))
    },

    #' @description Calls the StopTrainingJob API for the given job name.
    #' @param job_name (str): The name of the training job to stop.
    stop_training_job = function(job_name){
      return(self$sagemaker$stop_training_job(TrainingJobName=job_name))
    },

    #' @description Calls the DescribeTrainingJob API for the given job name
    #'              and returns the response.
    #' @param job_name (str): The name of the training job to describe.
    #' @return dict: A dictionary response with the training job description.
    describe_training_job = function(job_name){
      return(self$sagemaker$describe_training_job(TrainingJobName=job_name))
    },

    #' @description Create an Amazon SageMaker AutoML job.
    #' @param input_config (list[dict]): A list of Channel objects. Each channel contains "DataSource"
    #'              and "TargetAttributeName", "CompressionType" is an optional field.
    #' @param output_config (dict): The S3 URI where you want to store the training results and
    #'              optional KMS key ID.
    #' @param auto_ml_job_config (dict): A dict of AutoMLJob config, containing "StoppingCondition",
    #'              "SecurityConfig", optionally contains "VolumeKmsKeyId".
    #' @param role (str): The Amazon Resource Name (ARN) of an IAM role that
    #'               Amazon SageMaker can assume to perform tasks on your behalf.
    #' @param job_name (str): A string that can be used to identify an AutoMLJob. Each AutoMLJob
    #'               should have a unique job name.
    #' @param problem_type (str): The type of problem of this AutoMLJob. Valid values are
    #'               "Regression", "BinaryClassification", "MultiClassClassification". If None,
    #'               SageMaker AutoMLJob will infer the problem type automatically.
    #' @param job_objective (dict): AutoMLJob objective, contains "AutoMLJobObjectiveType" (optional),
    #'               "MetricName" and "Value".
    #' @param generate_candidate_definitions_only (bool): Indicates whether to only generate candidate
    #'                definitions. If True, AutoML.list_candidates() cannot be called. Default: False.
    #' @param tags ([dict[str,str]]): A list of dictionaries containing key-value
    #'                pairs.
    #' @return NULL invisible
    auto_ml = function(input_config,
                       output_config,
                       auto_ml_job_config,
                       role,
                       job_name,
                       problem_type=NULL,
                       job_objective=NULL,
                       generate_candidate_definitions_only=FALSE,
                       tags=NULL){
      auto_ml_job_request = list(
        "AutoMLJobName"=job_name,
        "InputDataConfig"=input_config,
        "OutputDataConfig"=output_config,
        "AutoMLJobConfig"=auto_ml_job_config,
        "RoleArn"=role,
        "GenerateCandidateDefinitionsOnly"=generate_candidate_definitions_only
      )
      auto_ml_job_request[["AutoMLJobObjective"]] = job_objective
      auto_ml_job_request[["ProblemType"]] = problem_type
      tags = .append_project_tags(tags)
      if (!islistempty(tags))
        auto_ml_job_request[["Tags"]] = tags

      LOGGER$info("Creating auto-ml-job with name: %s", job_name)
      LOGGER$debug("auto ml request: %s", jsonlite::toJSON(auto_ml_job_request, pretty = T, auto_unbox = T))
      return(do.call(self$sagemaker$create_auto_ml_job, auto_ml_job_request))
    },

    #' @description Calls the DescribeAutoMLJob API for the given job name
    #'              and returns the response.
    #' @param job_name (str): The name of the AutoML job to describe.
    #' @return dict: A dictionary response with the AutoML Job description.
    describe_auto_ml_job = function(job_name){
      return(self$sagemaker$describe_auto_ml_job(AutoMLJobName=job_name))
    },

    #' @description Returns the list of candidates of an AutoML job for a given name.
    #' @param job_name (str): The name of the AutoML job. If None, will use object's
    #'              latest_auto_ml_job name.
    #' @param status_equals (str): Filter the result with candidate status, values could be
    #'              "Completed", "InProgress", "Failed", "Stopped", "Stopping"
    #' @param candidate_name (str): The name of a specified candidate to list.
    #'              Default to NULL
    #' @param candidate_arn (str): The Arn of a specified candidate to list.
    #'              Default to NULL.
    #' @param sort_order (str): The order that the candidates will be listed in result.
    #'              Default to NULL.
    #' @param sort_by (str): The value that the candidates will be sorted by.
    #'              Default to NULL.
    #' @param max_results (int): The number of candidates will be listed in results,
    #'              between 1 to 100. Default to None. If None, will return all the candidates.
    #' @return list: A list of dictionaries with candidates information
    list_candidates = function(job_name,
                               status_equals=NULL,
                               candidate_name=NULL,
                               candidate_arn=NULL,
                               sort_order=NULL,
                               sort_by=NULL,
                               max_results=NULL){
      list_candidates_args = list("AutoMLJobName"=job_name)
      list_candidates_args[["StatusEquals"]] = status_equals
      list_candidates_args[["CandidateNameEquals"]] = candidate_name
      list_candidates_args[["CandidateArnEquals"]] = candidate_arn
      list_candidates_args[["SortOrder"]] = sort_order
      list_candidates_args[["SortBy"]] = sort_by
      list_candidates_args[["MaxResults"]] = max_results

      return(do.call(self$sagemaker$list_candidates_for_auto_ml_job, list_candidates_args))
    },

    #' @description Wait for an Amazon SageMaker AutoML job to complete.
    #' @param job (str): Name of the auto ml job to wait for.
    #' @param poll (int): Polling interval in seconds (default: 5).
    #' @return (dict): Return value from the ``DescribeAutoMLJob`` API.
    wait_for_auto_ml_job = function(job, poll=5){
      desc = private$.wait_until(private$.auto_ml_job_status(job), poll)
      private$.check_job_status(job, desc, "AutoMLJobStatus")
      return(desc)
    },

    #' @description Display the logs for a given AutoML job, optionally tailing them until the
    #'              job is complete. If the output is a tty or a Jupyter cell, it will be color-coded
    #'              based on which instance the log entry is from.
    #' @param job_name (str): Name of the Auto ML job to display the logs for.
    #' @param wait (bool): Whether to keep looking for new log entries until the job completes
    #'              (Default: FALSE).
    #' @param poll (int): The interval in seconds between polling for new log entries and job
    #'              completion (Default: 10).
    logs_for_auto_ml_job = function(job_name,
                                    wait=FALSE,
                                    poll=10){

      description = self$sagemaker$describe_auto_ml_job(AutoMLJobName=job_name)

      init_log = .log_init(self, description, "AutoML")

      state = .get_initial_job_state(description, "AutoMLJobStatus", wait)

      last_describe_job_call = Sys.time()
      while(TRUE){
        .flush_log_streams(init_log$stream_names,
                           init_log$instance_count,
                           init_log$client,
                           init_log$log_group,
                           job_name,
                           sm_env$positions)

        if(state == LogState$COMPLETE) {break}

        Sys.sleep(poll)

        if(state == LogState$JOB_COMPLETE) {
          writeLines("")
          state = LogState$COMPLETE
        } else if(Sys.time() - last_describe_job_call >= 30){
          description = self$sagemaker$describe_auto_ml_job(AutoMLJobName=job_name)
          last_describe_job_call = Sys.time()

          status = description$AutoMLJobStatus

          if (status %in% c("Completed", "Failed", "Stopped")) {
            writeLines("")
            state = LogState$JOB_COMPLETE
          }
        }
      }

      if (wait) {
        private$.check_job_status(job_name, description, "AutoMLJobStatus")}
    },

    #' @description Create an Amazon SageMaker Neo compilation job.
    #' @param input_model_config (dict): the trained model and the Amazon S3 location where it is
    #'              stored.
    #' @param output_model_config (dict): Identifies the Amazon S3 location where you want Amazon
    #'              SageMaker Neo to save the results of compilation job
    #' @param role (str): An AWS IAM role (either name or full ARN). The Amazon SageMaker Neo
    #'              compilation jobs use this role to access model artifacts. You must grant
    #'              sufficient permissions to this role.
    #' @param job_name (str): Name of the compilation job being created.
    #' @param stop_condition (dict): Defines when compilation job shall finish. Contains entries
    #'              that can be understood by the service like ``MaxRuntimeInSeconds``.
    #' @param tags (list[dict]): List of tags for labeling a compile model job. For more, see
    #'              https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html.
    #' @return str: ARN of the compile model job, if it is created.
    compile_model = function(input_model_config,
                             output_model_config,
                             role,
                             job_name,
                             stop_condition,
                             tags){
      compilation_job_request = list(
        "InputConfig"=input_model_config,
        "OutputConfig"=output_model_config,
        "RoleArn"=role,
        "StoppingCondition"=stop_condition,
        "CompilationJobName"=job_name
      )
      tags = .append_project_tags(tags)
      if (!islistempty(tags))
        compilation_job_request[["Tags"]] = tags

      LOGGER$info("Creating compilation-job with name: %s", job_name)
      return(do.call(self$sagemaker$create_compilation_job, compilation_job_request))
    },

    #' @description Create an Amazon SageMaker hyperparameter tuning job
    #' @param job_name (str): Name of the tuning job being created.
    #' @param strategy (str): Strategy to be used for hyperparameter estimations.
    #' @param objective_type (str): The type of the objective metric for evaluating training jobs.
    #'              This value can be either 'Minimize' or 'Maximize'.
    #' @param objective_metric_name (str): Name of the metric for evaluating training jobs.
    #' @param max_jobs (int): Maximum total number of training jobs to start for the hyperparameter
    #'              tuning job.
    #' @param max_parallel_jobs (int): Maximum number of parallel training jobs to start.
    #' @param parameter_ranges (dict): Dictionary of parameter ranges. These parameter ranges can be
    #'              one of three types: Continuous, Integer, or Categorical.
    #' @param static_hyperparameters (dict): Hyperparameters for model training. These
    #'              hyperparameters remain unchanged across all of the training jobs for the
    #'              hyperparameter tuning job. The hyperparameters are made accessible as a dictionary
    #'              for the training code on SageMaker.
    #' @param image_uri (str): Docker image containing training code.
    #' @param algorithm_arn (str): Resource ARN for training algorithm created on or subscribed from
    #'              AWS Marketplace (Default: \code{NULL}).
    #' @param input_mode (str): The input mode that the algorithm supports. Valid modes:
    #'              \itemize{
    #'                \item{\strong{'File'} - Amazon SageMaker copies the training dataset from the S3 location to
    #'                      a directory in the Docker container.}
    #'                \item{\strong{'Pipe'} - Amazon SageMaker streams data directly from S3 to the container via a
    #'                      Unix-named pipe.}}
    #' @param metric_definitions (list[dict]): A list of dictionaries that defines the metric(s)
    #'              used to evaluate the training jobs. Each dictionary contains two keys: 'Name' for
    #'              the name of the metric, and 'Regex' for the regular expression used to extract the
    #'              metric from the logs. This should be defined only for jobs that don't use an
    #'              Amazon algorithm.
    #' @param role (str): An AWS IAM role (either name or full ARN). The Amazon SageMaker
    #'              training jobs and APIs that create Amazon SageMaker endpoints use this role to
    #'              access training data and model artifacts. You must grant sufficient permissions
    #'              to this role.
    #' @param input_config (list): A list of Channel objects. Each channel is a named input source.
    #'              Please refer to the format details described:
    #'              https://botocore.readthedocs.io/en/latest/reference/services/sagemaker.html#SageMaker.Client.create_training_job
    #' @param output_config (dict): The S3 URI where you want to store the training results and
    #'              optional KMS key ID.
    #' @param resource_config (dict): Contains values for ResourceConfig:
    #'              \itemize{
    #'                \item{\strong{instance_count (int):} Number of EC2 instances to use for training.
    #'                              The key in resource_config is 'InstanceCount'.}
    #'                \item{\strong{instance_type (str):} Type of EC2 instance to use for training, for example,
    #'                              'ml.c4.xlarge'. The key in resource_config is 'InstanceType'.}}
    #' @param stop_condition (dict): When training should finish, e.g. ``MaxRuntimeInSeconds``.
    #' @param tags (list[dict]): List of tags for labeling the tuning job. For more, see
    #'             https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html.
    #' @param warm_start_config (dict): Configuration defining the type of warm start and
    #'             other required configurations.
    #' @param early_stopping_type (str): Specifies whether early stopping is enabled for the job.
    #'             Can be either 'Auto' or 'Off'. If set to 'Off', early stopping will not be
    #'             attempted. If set to 'Auto', early stopping of some training jobs may happen, but
    #'             is not guaranteed to.
    #' @param enable_network_isolation (bool): Specifies whether to isolate the training container
    #'             (Default: \code{FALSE}).
    #' @param encrypt_inter_container_traffic (bool): Specifies whether traffic between training
    #'             containers is encrypted for the training jobs started for this hyperparameter
    #'             tuning job (Default: \code{FALSE}).
    #' @param vpc_config (dict): Contains values for VpcConfig (default: None):
    #'              \itemize{
    #'                \item{\strong{subnets (list[str]):} List of subnet ids.
    #'                              The key in vpc_config is 'Subnets'.}
    #'                \item{\strong{security_group_ids (list[str]):} List of security group ids.
    #'                              The key in vpc_config is 'SecurityGroupIds'.}}
    #' @param use_spot_instances (bool): whether to use spot instances for training.
    #' @param checkpoint_s3_uri (str): The S3 URI in which to persist checkpoints
    #'             that the algorithm persists (if any) during training. (Default: \code{FALSE}).
    #' @param checkpoint_local_path (str): The local path that the algorithm
    #'             writes its checkpoints to. SageMaker will persist all files
    #'             under this path to `checkpoint_s3_uri` continually during
    #'             training. On job startup the reverse happens - data from the
    #'             s3 location is downloaded to this path before the algorithm is
    #'             started. If the path is unset then SageMaker assumes the
    #'             checkpoints will be provided under `/opt/ml/checkpoints/`.
    #'             (Default: \code{NULL}).
    tune = function(job_name,
                    strategy = c("Bayesian", "Random"),
                    objective_type,
                    objective_metric_name,
                    max_jobs,
                    max_parallel_jobs,
                    parameter_ranges,
                    static_hyperparameters,
                    input_mode,
                    metric_definitions,
                    role,
                    input_config,
                    output_config,
                    resource_config,
                    stop_condition,
                    tags,
                    warm_start_config,
                    enable_network_isolation=FALSE,
                    image_uri=NULL,
                    algorithm_arn=NULL,
                    early_stopping_type="Off",
                    encrypt_inter_container_traffic=FALSE,
                    vpc_config=NULL,
                    use_spot_instances=FALSE,
                    checkpoint_s3_uri=NULL,
                    checkpoint_local_path=NULL){

      strategy = match.arg(strategy)

      tune_request = list(
        "HyperParameterTuningJobName" = job_name,
        "HyperParameterTuningJobConfig" = private$.map_tuning_config(
          strategy=strategy,
          max_jobs=max_jobs,
          max_parallel_jobs=max_parallel_jobs,
          objective_type=objective_type,
          objective_metric_name=objective_metric_name,
          parameter_ranges=parameter_ranges,
          early_stopping_type=early_stopping_type),
        "TrainingJobDefinition" = private$.map_training_config(
          static_hyperparameters=static_hyperparameters,
          role=role,
          input_mode=input_mode,
          image_uri=image_uri,
          algorithm_arn=algorithm_arn,
          metric_definitions=metric_definitions,
          input_config=input_config,
          output_config=output_config,
          resource_config=resource_config,
          vpc_config=vpc_config,
          stop_condition=stop_condition,
          enable_network_isolation=enable_network_isolation,
          encrypt_inter_container_traffic=encrypt_inter_container_traffic,
          use_spot_instances=use_spot_instances,
          checkpoint_s3_uri=checkpoint_s3_uri,
          checkpoint_local_path=checkpoint_local_path
          )
      )
      tune_request$WarmStartConfig = warm_start_config
      tags = .append_project_tags(tags)
      if (!islistempty(tags))
        tune_request[["Tags"]] = tags

      LOGGER$info("Creating hyperparameter tuning job with name: %s", job_name)
      LOGGER$debug("tune request: %s", toJSON(tune_request, pretty = T, auto_unbox = T))
      return(do.call(self$sagemaker$create_hyper_parameter_tuning_job, tune_request))
    },

    #' @description Create an Amazon SageMaker hyperparameter tuning job. This method supports creating
    #'              tuning jobs with single or multiple training algorithms (estimators), while the ``tune()``
    #'              method above only supports creating tuning jobs with single training algorithm.
    #' @param job_name (str): Name of the tuning job being created.
    #' @param tuning_config (dict): Configuration to launch the tuning job.
    #' @param training_config (dict): Configuration to launch training jobs under the tuning job
    #'              using a single algorithm.
    #' @param training_config_list (list[dict]): A list of configurations to launch training jobs
    #'              under the tuning job using one or multiple algorithms. Either training_config
    #'              or training_config_list should be provided, but not both.
    #' @param warm_start_config (dict): Configuration defining the type of warm start and
    #'              other required configurations.
    #' @param tags (list[dict]): List of tags for labeling the tuning job. For more, see
    #'              https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html.
    create_tuning_job = function(job_name,
                                 tuning_config,
                                 training_config=NULL,
                                 training_config_list=NULL,
                                 warm_start_config=NULL,
                                 tags=NULL){
      if (is.null(training_config) && is.null(training_config_list)){
        ValueError$new("Either training_config or training_config_list should be provided.")}
      if (!is.null(training_config) && !is.null(training_config_list)){
        ValueError$new("Only one of training_config and training_config_list should be provided.")}

      tune_request = private$.get_tuning_request(
        job_name=job_name,
        tuning_config=tuning_config,
        training_config=training_config,
        training_config_list=training_config_list,
        warm_start_config=warm_start_config,
        tags=tags
      )

      LOGGER$info("Creating hyperparameter tuning job with name: %s", job_name)
      LOGGER$debug("tune request: %s", toJSON(tune_request, pretty = T, auto_unbox = T))

      return(do.call(self$sagemaker$create_hyper_parameter_tuning_job, tune_request))
    },

    #' @description Calls the DescribeHyperParameterTuningJob API for the given job name
    #'              and returns the response.
    #' @param job_name (str): The name of the hyperparameter tuning job to describe.
    #' @return dict: A dictionary response with the hyperparameter tuning job description.
    describe_tuning_job = function(job_name){
      return(self$sagemaker$describe_hyper_parameter_tuning_job(
        HyperParameterTuningJobName=job_name)
      )
    },

    #' @description Stop the Amazon SageMaker hyperparameter tuning job with the specified name.
    #' @param name (str): Name of the Amazon SageMaker hyperparameter tuning job.
    stop_tuning_job = function(name){
      LOGGER$info("Stopping tuning job: %s", name)
      tryCatch({
        self$sagemaker$stop_hyper_parameter_tuning_job(HyperParameterTuningJobName=name)
        },
        error = function(e) {
          error_code = paws_error_code(e)
          if(identical(error_code, "ValidationException")) {
            LOGGER$info("Tuning job: %s is alread stopped or not running.", name)
          } else {
            LOGGER$error("Error occurred while attempting to stop tuning job: %s. Please try again.", name)
            stop(e)
          }
      })
    },

    #' @description Create an Amazon SageMaker transform job.
    #' @param job_name (str): Name of the transform job being created.
    #' @param model_name (str): Name of the SageMaker model being used for the transform job.
    #' @param strategy (str): The strategy used to decide how to batch records in a single request.
    #'              Possible values are 'MultiRecord' and 'SingleRecord'.
    #' @param max_concurrent_transforms (int): The maximum number of HTTP requests to be made to
    #'              each individual transform container at one time.
    #' @param max_payload (int): Maximum size of the payload in a single HTTP request to the
    #'              container in MB.
    #' @param env (dict): Environment variables to be set for use during the transform job.
    #' @param input_config (dict): A dictionary describing the input data (and its location) for the
    #'              job.
    #' @param output_config (dict): A dictionary describing the output location for the job.
    #' @param resource_config (dict): A dictionary describing the resources to complete the job.
    #' @param experiment_config (dict): A dictionary describing the experiment configuration for the
    #'              job. Dictionary contains three optional keys,
    #'              'ExperimentName', 'TrialName', and 'TrialComponentDisplayName'.
    #' @param tags (list[dict]): List of tags for labeling a transform job.
    #' @param data_processing (dict): A dictionary describing config for combining the input data and
    #'              transformed data. For more, see
    #'              https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html.
    #' @param model_client_config (dict): A dictionary describing the model configuration for the
    #'             job. Dictionary contains two optional keys,
    #'             'InvocationsTimeoutInSeconds', and 'InvocationsMaxRetries'.
    transform = function(job_name=NULL,
                         model_name=NULL,
                         strategy=NULL,
                         max_concurrent_transforms=NULL,
                         max_payload=NULL,
                         env=NULL,
                         input_config=NULL,
                         output_config=NULL,
                         resource_config=NULL,
                         experiment_config=NULL,
                         tags=NULL,
                         data_processing=NULL,
                         model_client_config=NULL){
      tags = .append_project_tags(tags)
      request_list = private$.get_transform_request(
        job_name=job_name,
        model_name=model_name,
        strategy=strategy,
        max_concurrent_transforms=max_concurrent_transforms,
        max_payload=max_payload,
        env=env,
        input_config=input_config,
        output_config=output_config,
        resource_config=resource_config,
        experiment_config=experiment_config,
        tags=tags,
        data_processing=data_processing,
        model_client_config=model_client_config)
      LOGGER$info("Creating transform job with name: %s", job_name)
      LOGGER$debug("Transform request: %s", toJSON(request_list, pretty = T, auto_unbox = T))

      return(do.call(self$sagemaker$create_transform_job, request_list))
    },

    #' @description Create an Amazon SageMaker ``Model``.
    #'              Specify the S3 location of the model artifacts and Docker image containing
    #'              the inference code. Amazon SageMaker uses this information to deploy the
    #'              model in Amazon SageMaker. This method can also be used to create a Model for an Inference
    #'              Pipeline if you pass the list of container definitions through the containers parameter.
    #' @param name (str): Name of the Amazon SageMaker ``Model`` to create.
    #' @param role (str): An AWS IAM role (either name or full ARN). The Amazon SageMaker training
    #'              jobs and APIs that create Amazon SageMaker endpoints use this role to access
    #'              training data and model artifacts. You must grant sufficient permissions to this
    #'              role.
    #' @param container_defs (list[dict[str, str]] or [dict[str, str]]): A single container
    #'              definition or a list of container definitions which will be invoked sequentially
    #'              while performing the prediction. If the list contains only one container, then
    #'              it'll be passed to SageMaker Hosting as the ``PrimaryContainer`` and otherwise,
    #'              it'll be passed as ``Containers``.You can also specify the  return value of
    #'              ``sagemaker.get_container_def()`` or ``sagemaker.pipeline_container_def()``,
    #'              which will used to create more advanced container configurations, including model
    #'              containers which need artifacts from S3.
    #' @param vpc_config (dict[str, list[str]]): The VpcConfig set on the model (default: None)
    #'              \itemize{
    #'                \item{\strong{'Subnets' (list[str]):} List of subnet ids.}
    #'                \item{\strong{'SecurityGroupIds' (list[str]):} List of security group ids.}}
    #' @param enable_network_isolation (bool): Wether the model requires network isolation or not.
    #' @param primary_container (str or dict[str, str]): Docker image which defines the inference
    #'              code. You can also specify the return value of ``sagemaker.container_def()``,
    #'              which is used to create more advanced container configurations, including model
    #'              containers which need artifacts from S3. This field is deprecated, please use
    #'              container_defs instead.
    #' @param tags (list[list[str, str]]): Optional. The list of tags to add to the model.
    #'              Example: \code{tags = list(list('Key'= 'tagname', 'Value'= 'tagvalue'))}
    #'              For more information about tags, see
    #'              https://boto3.amazonaws.com/v1/documentation/api/latest/reference/services/sagemaker.html#SageMaker.Client.add_tags
    #' @return str: Name of the Amazon SageMaker ``Model`` created.
    create_model = function(name,
                            role,
                            container_defs = NULL,
                            vpc_config = NULL,
                            enable_network_isolation = FALSE,
                            primary_container = NULL,
                            tags = NULL){
      tags = .append_project_tags(tags)
      create_model_request = private$.create_model_request(
        name=name,
        role=role,
        container_defs=container_defs,
        vpc_config=vpc_config,
        enable_network_isolation=enable_network_isolation,
        primary_container=primary_container,
        tags=tags)
      LOGGER$info("Creating model with name: %s", name)
      LOGGER$debug("CreateModel request: %s", toJSON(create_model_request, pretty = T, auto_unbox = T))

      tryCatch({
        do.call(self$sagemaker$create_model, create_model_request)
        },
        error=function(e){
          error_code = paws_error_code(e)
          if (identical(error_code, "ValidationException")
              && grepl("Cannot create already existing model", e$error_response$Message)){
            LOGGER$warn("Using already existing model: %s", name)
          } else {
            stop(e)
          }
      })
      return(name)
    },

    #' @description Create an Amazon SageMaker ``Model`` from a SageMaker Training Job.
    #' @param training_job_name (str): The Amazon SageMaker Training Job name.
    #' @param name (str): The name of the SageMaker ``Model`` to create (default: None).
    #'              If not specified, the training job name is used.
    #' @param role (str): The ``ExecutionRoleArn`` IAM Role ARN for the ``Model``, specified either
    #'              by an IAM role name or role ARN. If None, the ``RoleArn`` from the SageMaker
    #'              Training Job will be used.
    #' @param image_uri (str): The Docker image reference (default: None). If None, it
    #'              defaults to the Training Image in ``training_job_name``.
    #' @param model_data_url (str): S3 location of the model data (default: None). If None, defaults
    #'              to the ``ModelS3Artifacts`` of ``training_job_name``.
    #' @param env (dict[string,string]): Model environment variables (default: {}).
    #' @param enable_network_isolation (bool): Whether the model requires network isolation or not.
    #' @param vpc_config_override (dict[str, list[str]]): Optional override for VpcConfig set on the
    #'              model. Default: use VpcConfig from training job.
    #'              \itemize{
    #'                \item{\strong{'Subnets' (list[str])} List of subnet ids.}
    #'                \item{\strong{'SecurityGroupIds' (list[str])} List of security group ids.}}
    #' @param tags (list[list[str, str]]): Optional. The list of tags to add to the model.
    #'              For more, see https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html.
    #' @return str: The name of the created ``Model``.
    create_model_from_job = function(training_job_name,
                                     name=NULL,
                                     role=NULL,
                                     image_uri=NULL,
                                     model_data_url=NULL,
                                     env=NULL,
                                     enable_network_isolation=FALSE,
                                     vpc_config_override= "VPC_CONFIG_DEFAULT",
                                     tags=NULL){

      training_job = self$sagemaker$describe_training_job(TrainingJobName=training_job_name)
      name = name %||% training_job_name
      role = role %||% training_job$RoleArn
      env = env %||% list()
      primary_container = container_def(
        image_uri %||% training_job$AlgorithmSpecification$TrainingImage,
        model_data_url=model_data_url %||% training_job$ModelArtifacts$S3ModelArtifacts,
        env=env
      )
      vpc_config = private$.vpc_config_from_training_job(training_job, vpc_config_override)
      return (self$create_model(
        name,
        role,
        primary_container,
        enable_network_isolation=enable_network_isolation,
        vpc_config=vpc_config,
        tags=tags
        )
      )
    },

    #' @description Create a SageMaker Model Package from the results of training with an Algorithm Package
    #' @param name (str): ModelPackage name
    #' @param description (str): Model Package description
    #' @param algorithm_arn (str): arn or name of the algorithm used for training.
    #' @param model_data (str): s3 URI to the model artifacts produced by training
    create_model_package_from_algorithm = function(name,
                                                   description = NULL,
                                                   algorithm_arn = NULL,
                                                   model_data = NULL){
      request = list(
        "ModelPackageName"=name,
        "ModelPackageDescription"=description,
        "SourceAlgorithmSpecification"=list(
          "SourceAlgorithms"=list(list("AlgorithmName"=algorithm_arn, "ModelDataUrl"=model_data))
        )
      )
      LOGGER$info("Creating model package with name: %s", name)
      tryCatch({
        do.call(self$sagemaker$create_model_package, request)
        }, error = function(e) {
          error_code = paws_error_code(e)
          if (identical(error_code, "ValidationException")
              && grepl("ModelPackage already exists", e$error_response$Message)) {
            LOGGER$warn("Using already existing model package: %s", name)
          } else {stop(e)}
      })
    },

    #' @description Get request dictionary for CreateModelPackage API.
    #' @param containers (list): A list of inference containers that can be used for inference
    #'              specifications of Model Package (default: None).
    #' @param content_types (list): The supported MIME types for the input data (default: None).
    #' @param response_types (list): The supported MIME types for the output data (default: None).
    #' @param inference_instances (list): A list of the instance types that are used to
    #'              generate inferences in real-time (default: None).
    #' @param transform_instances (list): A list of the instance types on which a transformation
    #'              job can be run or on which an endpoint can be deployed (default: None).
    #' @param model_package_name (str): Model Package name, exclusive to `model_package_group_name`,
    #'              using `model_package_name` makes the Model Package un-versioned (default: None).
    #' @param model_package_group_name (str): Model Package Group name, exclusive to
    #'              `model_package_name`, using `model_package_group_name` makes the Model Package
    #'              versioned (default: None).
    #' @param model_metrics (ModelMetrics): ModelMetrics object (default: None).
    #' @param metadata_properties (MetadataProperties): MetadataProperties object (default: None)
    #' @param marketplace_cert (bool): A boolean value indicating if the Model Package is certified
    #'              for AWS Marketplace (default: False).
    #' @param approval_status (str): Model Approval Status, values can be "Approved", "Rejected",
    #'              or "PendingManualApproval" (default: "PendingManualApproval").
    #' @param description (str): Model Package description (default: None).
    #' @param drift_check_baselines (DriftCheckBaselines): DriftCheckBaselines object (default: None).
    create_model_package_from_containers = function(containers=NULL,
                                                    content_types=NULL,
                                                    response_types=NULL,
                                                    inference_instances=NULL,
                                                    transform_instances=NULL,
                                                    model_package_name=NULL,
                                                    model_package_group_name=NULL,
                                                    model_metrics=NULL,
                                                    metadata_properties=NULL,
                                                    marketplace_cert=FALSE,
                                                    approval_status="PendingManualApproval",
                                                    description=NULL,
                                                    drift_check_baselines=NULL){
      request = private$.get_create_model_package_request(
        model_package_name,
        model_package_group_name,
        containers,
        content_types,
        response_types,
        inference_instances,
        transform_instances,
        model_metrics,
        metadata_properties,
        marketplace_cert,
        approval_status,
        description,
        drift_check_baselines
      )
      return(do.call(self$sagemaker$create_model_package, request))
    },

    #' @description Wait for an Amazon SageMaker endpoint deployment to complete.
    #' @param model_package_name (str): Name of the ``Endpoint`` to wait for.
    #' @param poll (int): Polling interval in seconds (default: 5).
    #' @return dict: Return value from the ``DescribeEndpoint`` API.
    wait_for_model_package = function(model_package_name,
                                      poll = 5){
      desc = private$.wait_until(private$.create_model_package_status(model_package_name), poll)

      status = desc$ModelPackageStatus

      if (status != "Completed"){
        reason = desc$FailureReason
        message = sprintf("Error creating model package %s: %s Reason: %s",
          model_package_name, status, reason)
        UnexpectedStatusError$new(
          message,
          allowed_statuses="Completed",
          actual_status=status)
      }
      return(desc)
    },

    #' @description Calls the DescribeModel API for the given model name.
    #' @param name (str): The name of the SageMaker model.
    #' @return dict: A dictionary response with the model description.
    describe_model = function(name){
      return(self$sagemaker$describe_model(ModelName=name))
    },

    #' @description Create an Amazon SageMaker endpoint configuration.
    #'              The endpoint configuration identifies the Amazon SageMaker model (created using the
    #'              ``CreateModel`` API) and the hardware configuration on which to deploy the model. Provide
    #'              this endpoint configuration to the ``CreateEndpoint`` API, which then launches the
    #'              hardware and deploys the model.
    #' @param name (str): Name of the Amazon SageMaker endpoint configuration to create.
    #' @param model_name (str): Name of the Amazon SageMaker ``Model``.
    #' @param initial_instance_count (int): Minimum number of EC2 instances to launch. The actual
    #'              number of active instances for an endpoint at any given time varies due to
    #'              autoscaling.
    #' @param instance_type (str): Type of EC2 instance to launch, for example, 'ml.c4.xlarge'.
    #' @param accelerator_type (str): Type of Elastic Inference accelerator to attach to the
    #'              instance. For example, 'ml.eia1.medium'.
    #'              For more information: https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html
    #' @param tags (list[list[str, str]]): Optional. The list of tags to add to the endpoint config.
    #' @param kms_key (str): The KMS key that is used to encrypt the data on the storage volume
    #'              attached to the instance hosting the endpoint.
    #' @param data_capture_config_dict (dict): Specifies configuration related to Endpoint data
    #'              capture for use with Amazon SageMaker Model Monitoring. Default: None.
    #'              Example: \code{tags = list(list('Key'= 'tagname', 'Value'= 'tagvalue'))}
    #'              For more information about tags, see
    #'              https://boto3.amazonaws.com/v1/documentation/api/latest/reference/services/sagemaker.html#SageMaker.Client.add_tags
    #' @return str: Name of the endpoint point configuration created.
    create_endpoint_config = function(name,
                                      model_name,
                                      initial_instance_count,
                                      instance_type,
                                      accelerator_type=NULL,
                                      tags=NULL,
                                      kms_key=NULL,
                                      data_capture_config_dict=NULL){
      LOGGER$info("Creating endpoint-config with name %s", name)

      tags = tags %||% list()

      request = list(
        "EndpointConfigName"=name,
        "ProductionVariants"=list(
          production_variant(
            model_name,
            instance_type,
            initial_instance_count,
            accelerator_type=accelerator_type
          )
        )
      )

      tags = .append_project_tags(tags)
      if(!islistempty(tags))
        request[["Tags"]] = tags
      request[["KmsKeyId"]] = kms_key
      request[["DataCaptureConfig"]] = data_capture_config_dict

      do.call(self$sagemaker$create_endpoint_config, request)
      return(name)
    },

    #' @description Create an Amazon SageMaker endpoint configuration from an existing one. Updating any
    #'              values that were passed in.
    #'              The endpoint configuration identifies the Amazon SageMaker model (created using the
    #'              ``CreateModel`` API) and the hardware configuration on which to deploy the model. Provide
    #'              this endpoint configuration to the ``CreateEndpoint`` API, which then launches the
    #'              hardware and deploys the model.
    #' @param existing_config_name (str): Name of the existing Amazon SageMaker endpoint
    #'              configuration.
    #' @param new_config_name (str): Name of the Amazon SageMaker endpoint configuration to create.
    #' @param new_tags (List[list[str, str]]): Optional. The list of tags to add to the endpoint
    #'              config. If not specified, the tags of the existing endpoint configuration are used.
    #'              If any of the existing tags are reserved AWS ones (i.e. begin with "aws"),
    #'              they are not carried over to the new endpoint configuration.
    #' @param new_kms_key (str): The KMS key that is used to encrypt the data on the storage volume
    #'              attached to the instance hosting the endpoint (default: None). If not specified,
    #'              the KMS key of the existing endpoint configuration is used.
    #' @param new_data_capture_config_list (dict): Specifies configuration related to Endpoint data
    #'              capture for use with Amazon SageMaker Model Monitoring (default: None).
    #'              If not specified, the data capture configuration of the existing
    #'              endpoint configuration is used.
    #' @param new_production_variants (list[dict]): The configuration for which model(s) to host and
    #'              the resources to deploy for hosting the model(s). If not specified,
    #'              the ``ProductionVariants`` of the existing endpoint configuration is used.
    #' @return str: Name of the endpoint point configuration created.
    create_endpoint_config_from_existing = function(existing_config_name,
                                                    new_config_name,
                                                    new_tags=NULL,
                                                    new_kms_key=NULL,
                                                    new_data_capture_config_list=NULL,
                                                    new_production_variants=NULL){

      LOGGER$info("Creating endpoint-config with name %s", new_config_name)

      existing_endpoint_config_desc = self$sagemaker$describe_endpoint_config(
        EndpointConfigName=existing_config_name
      )

      request = list("EndpointConfigName"=new_config_name)
      request[["ProductionVariants"]] = (
        new_production_variants %||% existing_endpoint_config_desc[["ProductionVariants"]]
      )

      request_tags = new_tags %||% self$list_tags(existing_endpoint_config_desc[["EndpointConfigArn"]])
      request_tags = .append_project_tags(request_tags)
      if(!islistempty(request_tags))
        request[["Tags"]] = request_tags

      if (!is.null(new_kms_key) || !is.null(existing_endpoint_config_desc[["KmsKeyId"]]))
        request[["KmsKeyId"]] = new_kms_key %||% existing_endpoint_config_desc[["KmsKeyId"]]

      request_data_capture_config_list = (
        new_data_capture_config_list %||% existing_endpoint_config_desc[["DataCaptureConfig"]]
      )

      request[["DataCaptureConfig"]] = new_data_capture_config_list

      return(do.call(self$sagemaker$create_endpoint_config, request))
    },

    #' @description Create an Amazon SageMaker ``Endpoint`` according to the endpoint configuration
    #'              specified in the request.
    #'              Once the ``Endpoint`` is created, client applications can send requests to obtain
    #'              inferences. The endpoint configuration is created using the ``CreateEndpointConfig`` API.
    #' @param endpoint_name (str): Name of the Amazon SageMaker ``Endpoint`` being created.
    #' @param config_name (str): Name of the Amazon SageMaker endpoint configuration to deploy.
    #' @param tags (list[list[str, str]]): Optional. The list of tags to add to the endpoint config.
    #' @param wait (bool): Whether to wait for the endpoint deployment to complete before returning
    #'              (Default: \code{TRUE}).
    #' @return str: Name of the Amazon SageMaker ``Endpoint`` created.
    create_endpoint = function(endpoint_name,
                               config_name,
                               tags=NULL,
                               wait=TRUE){
      LOGGER$info("Creating endpoint with name %s", endpoint_name)

      tags = tags %||% list()
      tags = .append_project_tags(tags)

      self$sagemaker$create_endpoint(
        EndpointName=endpoint_name, EndpointConfigName=config_name, Tags=tags
      )
      if (wait)
        self$wait_for_endpoint(endpoint_name)
      return(endpoint_name)
    },

    #' @description Update an Amazon SageMaker ``Endpoint`` according to the endpoint configuration
    #'              specified in the request
    #' @param endpoint_name (str): Name of the Amazon SageMaker ``Endpoint`` being created.
    #' @param endpoint_config_name (str): Name of the Amazon SageMaker endpoint configuration to deploy.
    #' @param wait (bool): Whether to wait for the endpoint deployment to complete before returning
    #'              (Default: \code{TRUE}).
    #' @return str: Name of the Amazon SageMaker ``Endpoint`` being updated.
    update_endpoint = function(endpoint_name,
                               endpoint_config_name,
                               wait=TRUE){
      if (!.deployment_entity_exists(self$sagemaker$describe_endpoint(EndpointName=endpoint_name))){
        ValueError$new(
          sprintf("Endpoint with name '%s' does not exist; please use an existing endpoint name",
                  endpoint_name)
        )
      }
      self$sagemaker$update_endpoint(
        EndpointName=endpoint_name,EndpointConfigName=endpoint_config_name
      )
      if (wait) self$wait_for_endpoint(endpoint_name)
      return(endpoint_name)
    },

    #' @description Delete an Amazon SageMaker ``Endpoint``.
    #' @param endpoint_name (str): Name of the Amazon SageMaker ``Endpoint`` to delete.
    delete_endpoint = function(endpoint_name){
      LOGGER$info("Deleting endpoint with name: %s", endpoint_name)
      self$sagemaker$delete_endpoint(EndpointName=endpoint_name)
    },
    #' @description Delete an Amazon SageMaker endpoint configuration.
    #' @param endpoint_config_name (str): Name of the Amazon SageMaker endpoint configuration to
    #'              delete.
    delete_endpoint_config = function(endpoint_config_name){
      LOGGER$info("Deleting endpoint configuration with name: %s", endpoint_config_name)
      self$sagemaker$delete_endpoint_config(EndpointConfigName=endpoint_config_name)
    },

    #' @description Delete an Amazon SageMaker Model.
    #' @param model_name (str): Name of the Amazon SageMaker model to delete.
    delete_model= function(model_name){
      LOGGER$info("Deleting model with name: %s", model_name)
      self$sagemaker$delete_model(ModelName=model_name)
    },

    #' @description List the tags given an Amazon Resource Name
    #' @param resource_arn (str): The Amazon Resource Name (ARN) for which to get the tags list.
    #' @param max_results (int): The maximum number of results to include in a single page.
    #'              This method takes care of that abstraction and returns a full list.
    list_tags = function(resource_arn,
                         max_results=50){
      tags_list = list()

      tryCatch({
        list_tags_response = self$sagemaker$list_tags(ResourceArn=resource_arn, MaxResults=max_results)
        tags_list = c(tags_list, list_tags_response$Tags)

        next_token = list_tags_response$nextToken

        while(!is.null(next_token) || length(next_token) != 0){
          list_tags_response = self$sagemaker$list_tags(
            ResourceArn=resource_arn,
            MaxResults=max_results,
            NextToken=next_token)
          tags_list = c(tags_list, list_tags_response$Tags)
          next_token = list_tags_response$nextToken
        }
        non_aws_tags = list()
        for(tag in tags_list){
          if (!grepl("aws:", tag$Key)){
            non_aws_tags = c(non_aws_tags, tag)}
        }
        return(non_aws_tags)
      },
      error = function(e){
        LOGGER$error("Error retrieving tags. resource_arn: %s",resource_arn)
        stop(e)
      })
    },

    #' @description Wait for an Amazon SageMaker training job to complete.
    #' @param job (str): Name of the training job to wait for.
    #' @param poll (int): Polling interval in seconds (default: 5).
    #' @return (dict): Return value from the ``DescribeTrainingJob`` API.
    wait_for_job = function(job, poll = 5){

      # make sure no previous job descriptions are picked up
      private$.last_job_desc = NULL

      desc = private$.wait_until_training_done(private$.train_done(job), poll)

      private$.check_job_status(job, desc, "TrainingJobStatus")

      # clean up last job description
      private$.last_job_desc = NULL

      return(desc)
    },

    #' @description Wait for an Amazon SageMaker Processing job to complete.
    #' @param job (str): Name of the processing job to wait for.
    #' @param poll (int): Polling interval in seconds (Default: 5).
    #' @return (dict): Return value from the ``DescribeProcessingJob`` API.
    wait_for_processing_job = function(job, poll=5){
      desc = private$.wait_until(private$compilation_job_status(job), poll)
      private$.check_job_status(job, desc, "CompilationJobStatus")
      return(desc)
    },

    #' @description Wait for an Amazon SageMaker Neo compilation job to complete.
    #' @param job (str): Name of the compilation job to wait for.
    #' @param poll (int): Polling interval in seconds (Default: 5).
    #' @return (dict): Return value from the ``DescribeCompilationJob`` API.
    wait_for_compilation_job = function(job,
                                        poll=5){
      desc = private$.wait_until(private$.compilation_job_status(job), poll)
      private$.check_job_status(job, desc, "CompilationJobStatus")
      return(desc)
    },

    #' @description Wait for an Amazon SageMaker Edge packaging job to complete.
    #' @param job (str): Name of the edge packaging job to wait for.
    #' @param poll (int): Polling interval in seconds (default: 5).
    #' @return (dict): Return value from the ``DescribeEdgePackagingJob`` API.
    wait_for_edge_packaging_job = function(job, poll=5){
      desc =  private$.wait_until(private$.edge_packaging_job_status(job), poll)
      private$.check_job_status(job, desc, "EdgePackagingJobStatus")
      return(desc)
    },

    #' @description Wait for an Amazon SageMaker hyperparameter tuning job to complete.
    #' @param job (str): Name of the tuning job to wait for.
    #' @param poll (int): Polling interval in seconds (default: 5).
    #' @return (dict): Return value from the ``DescribeHyperParameterTuningJob`` API.
    wait_for_tuning_job = function(job, poll=5){
      desc = private$.wait_until(private$.tuning_job_status(job), poll)
      private$.check_job_status(job, desc, "HyperParameterTuningJobStatus")
      return(desc)
    },

    #' @description Calls the DescribeTransformJob API for the given job name
    #'              and returns the response.
    #' @param job_name (str): The name of the transform job to describe.
    #' @return dict: A dictionary response with the transform job description.
    describe_transform_job = function(job_name){
      return (self$sagemaker$describe_transform_job(TransformJobName=job_name))
    },

    #' @description Wait for an Amazon SageMaker transform job to complete.
    #' @param job (str): Name of the transform job to wait for.
    #' @param poll (int): Polling interval in seconds (default: 5).
    #' @return (dict): Return value from the ``DescribeTransformJob`` API.
    wait_for_transform_job = function(job, poll = 5){
      desc = private$.wait_until(private$.transform_job_status(job), poll)
      private$.check_job_status(job, desc, "TransformJobStatus")
      return(desc)
    },

    #' @description Stop the Amazon SageMaker hyperparameter tuning job with the specified name.
    #' @param name (str): Name of the Amazon SageMaker batch transform job.
    stop_transform_job = function(name){
      LOGGER$info("Stopping transform job: %s", name)
      tryCatch({
        self$sagemaker$stop_transform_job(TransformJobName=name)
        }, error = function(e){
          error_code = paws_error_code(e)
           # allow to pass if the job already stopped
           if (identictal(error_code, "ValidationException")){
             LOGGER$info("Transform job: %s is already stopped or not running.", name)
           } else{
             LOGGER$error("Error occurred while attempting to stop transform job: %s", name)
             stop(e)}
      })
    },

    #' @description Wait for an Amazon SageMaker endpoint deployment to complete.
    #' @param endpoint (str): Name of the ``Endpoint`` to wait for.
    #' @param poll (int): Polling interval in seconds (Default: 30).
    #' @return dict: Return value from the ``DescribeEndpoint`` API.
    wait_for_endpoint = function(endpoint, poll=30){
      desc = private$.wait_until(private$.deploy_done(endpoint), poll)
      status = desc$EndpointStatus

      if(status != "InService"){
        reason = desc$FailureReason
        message = sprintf("Error hosting endpoint %s: %s. Reason: %s.", endpoint, status, reason)
        UnexpectedStatusError$new(
          message,
          allowed_statuses="InService",
          actual_status=status)
      }
      return(desc)
    },

    #' @description Create an ``Endpoint`` using the results of a successful training job.
    #'              Specify the job name, Docker image containing the inference code, and hardware
    #'              configuration to deploy the model. Internally the API, creates an Amazon SageMaker model
    #'              (that describes the model artifacts and the Docker image containing inference code),
    #'              endpoint configuration (describing the hardware to deploy for hosting the model), and
    #'              creates an ``Endpoint`` (launches the EC2 instances and deploys the model on them). In
    #'              response, the API returns the endpoint name to which you can send requests for inferences.
    #' @param job_name (str): Name of the training job to deploy the results of.
    #' @param initial_instance_count (int): Minimum number of EC2 instances to launch. The actual
    #'              number of active instances for an endpoint at any given time varies due to
    #'              autoscaling.
    #' @param instance_type (str): Type of EC2 instance to deploy to an endpoint for prediction,
    #'              for example, 'ml.c4.xlarge'.
    #' @param deployment_image (str): The Docker image which defines the inference code to be used
    #'              as the entry point for accepting prediction requests. If not specified, uses the
    #'              image used for the training job.
    #' @param name (str): Name of the ``Endpoint`` to create. If not specified, uses the training job
    #'              name.
    #' @param role (str): An AWS IAM role (either name or full ARN). The Amazon SageMaker training
    #'              jobs and APIs that create Amazon SageMaker endpoints use this role to access
    #'              training data and model artifacts. You must grant sufficient permissions to this
    #'              role.
    #' @param wait (bool): Whether to wait for the endpoint deployment to complete before returning
    #'              (Default: True).
    #' @param model_environment_vars (dict[str, str]): Environment variables to set on the model
    #'              container (Default: NULL).
    #' @param vpc_config_override (dict[str, list[str]]): Overrides VpcConfig set on the model.
    #'              Default: use VpcConfig from training job.
    #'              \itemize{
    #'                \item{\strong{'Subnets' (list[str]):} List of subnet ids.}
    #'                \item{\strong{'SecurityGroupIds' (list[str]):} List of security group ids.}}
    #' @param accelerator_type (str): Type of Elastic Inference accelerator to attach to the
    #'              instance. For example, 'ml.eia1.medium'.
    #'              For more information: https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html
    #' @param data_capture_config (DataCaptureConfig): Specifies
    #'              configuration related to Endpoint data capture for use with
    #'              Amazon SageMaker Model Monitoring. Default: None.
    #' @return str: Name of the ``Endpoint`` that is created.
    endpoint_from_job = function(job_name,
                                 initial_instance_count,
                                 instance_type,
                                 deployment_image=NULL,
                                 name=NULL,
                                 role=NULL,
                                 wait=TRUE,
                                 model_environment_vars=NULL,
                                 vpc_config_override="VPC_CONFIG_DEFAULT",
                                 accelerator_type=NULL,
                                 data_capture_config=NULL){

      job_desc = self$sagemaker$describe_training_job(TrainingJobName=job_name)
      output_url = job_desc$ModelArtifacts$S3ModelArtifacts

      deployment_image = deployment_image %||% job_desc$AlgorithmSpecification$TrainingImage
      role = role %||% job_desc$RoleArn
      name = name %||% job_name
      vpc_config_override = if(vpc_config_override == "VPC_CONFIG_DEFAULT") job_desc$VpcConfig else vpc_sanitize(vpc_config_override)

      return (self$endpoint_from_model_data(
        model_s3_location=output_url,
        deployment_image=deployment_image,
        initial_instance_count=initial_instance_count,
        instance_type=instance_type,
        name=name,
        role=role,
        wait=wait,
        model_environment_vars=model_environment_vars,
        model_vpc_config=vpc_config_override,
        accelerator_type=accelerator_type,
        data_capture_config=data_capture_config))
    },

    #' @description Create and deploy to an ``Endpoint`` using existing model data stored in S3.
    #' @param model_s3_location (str): S3 URI of the model artifacts to use for the endpoint.
    #' @param deployment_image (str): The Docker image which defines the runtime code to be used as
    #'              the entry point for accepting prediction requests.
    #' @param initial_instance_count (int): Minimum number of EC2 instances to launch. The actual
    #'              number of active instances for an endpoint at any given time varies due to
    #'              autoscaling.
    #' @param instance_type (str): Type of EC2 instance to deploy to an endpoint for prediction,
    #'              e.g. 'ml.c4.xlarge'.
    #' @param name (str): Name of the ``Endpoint`` to create. If not specified, uses a name
    #'              generated by combining the image name with a timestamp.
    #' @param role (str): An AWS IAM role (either name or full ARN). The Amazon SageMaker training
    #'              jobs and APIs that create Amazon SageMaker endpoints use this role to access
    #'              training data and model artifacts.
    #'              You must grant sufficient permissions to this role.
    #' @param wait (bool): Whether to wait for the endpoint deployment to complete before returning
    #'              (Default: True).
    #' @param model_environment_vars (dict[str, str]): Environment variables to set on the model
    #'              container (Default: NULL).
    #' @param model_vpc_config (dict[str, list[str]]): The VpcConfig set on the model (default: None)
    #'              \itemize{
    #'                \item{\strong{'Subnets' (list[str]):} List of subnet ids.}
    #'                \item{\strong{'SecurityGroupIds' (list[str]):} List of security group ids.}}
    #' @param accelerator_type (str): Type of Elastic Inference accelerator to attach to the instance.
    #'              For example, 'ml.eia1.medium'.
    #'              For more information: https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html
    #' @param data_capture_config (DataCaptureConfig): Specifies
    #'              configuration related to Endpoint data capture for use with
    #'              Amazon SageMaker Model Monitoring. Default: None.
    #' @return str: Name of the ``Endpoint`` that is created.
    endpoint_from_model_data = function(model_s3_location,
                                        deployment_image,
                                        initial_instance_count,
                                        instance_type,
                                        name=NULL,
                                        role=NULL,
                                        wait=TRUE,
                                        model_environment_vars=NULL,
                                        model_vpc_config=NULL,
                                        accelerator_type=NULL,
                                        data_capture_config=NULL){
      model_environment_vars = model_environment_vars %||% list()
      name = name %||% name_from_image(deployment_image)
      model_vpc_config = vpc_sanitize(model_vpc_config)

      if (.deployment_entity_exists(self$sagemaker$describe_endpoint(EndpointName=name))){
        ValueError$new(
          sprintf('Endpoint with name "%s" already exists; please pick a different name.',name)
        )
      }
      if (!.deployment_entity_exists(self$sagemaker$describe_model(ModelName=name))){
        primary_container = container_def(
          image_uri=deployment_image,
          model_data_url=model_s3_location,
          env=model_environment_vars
        )
        self$create_model(
          name=name,
          role=role,
          container_defs=primary_container,
          vpc_config=model_vpc_config
        )
      }
      data_capture_config_list = NULL
      if(!is.null(data_capture_config))
        data_capture_config_list = data_capture_config$new()$to_request_list

      if (!.deployment_entity_exists(self$sagemaker$describe_endpoint_config(EndpointConfigName=name))){
        self$create_endpoint_config(
          name=name,
          model_name=name,
          initial_instance_count=initial_instance_count,
          instance_type=instance_type,
          accelerator_type=accelerator_type,
          data_capture_config_dict=data_capture_config_list
          )
      }
      self$create_endpoint(endpoint_name=name, config_name=name, wait=wait)
      return(name)
    },

    #' @description Create an SageMaker ``Endpoint`` from a list of production variants.
    #' @param name (str): The name of the ``Endpoint`` to create.
    #' @param production_variants (list[dict[str, str]]): The list of production variants to deploy.
    #' @param tags (list[dict[str, str]]): A list of key-value pairs for tagging the endpoint
    #'              (Default: None).
    #' @param kms_key (str): The KMS key that is used to encrypt the data on the storage volume
    #'              attached to the instance hosting the endpoint.
    #' @param wait (bool): Whether to wait for the endpoint deployment to complete before returning
    #'              (Default: True).
    #' @param data_capture_config_list (list): Specifies configuration related to Endpoint data
    #'              capture for use with Amazon SageMaker Model Monitoring. Default: None.
    #' @return str: The name of the created ``Endpoint``.
    endpoint_from_production_variants = function(name,
                                                 production_variants,
                                                 tags=NULL,
                                                 kms_key=NULL,
                                                 wait=TRUE,
                                                 data_capture_config_list=NULL){
      config_options = list("EndpointConfigName"=name, "ProductionVariants"=production_variants)
      tags = .append_project_tags(tags)
      if (!islistempty(tags))
        config_options[["Tags"]] = tags
      config_options[["KmsKeyId"]] = kms_key
      config_options[["DataCaptureConfig"]] = data_capture_config_list

      LOGGER$info("Creating endpoint-config with name %s", name)
      do.call(self$sagemaker$create_endpoint_config, config_options)

      return (self$create_endpoint(endpoint_name=name, config_name=name, tags=tags, wait=wait))
    },

    #' @description Expand an IAM role name into an ARN.
    #'              If the role is already in the form of an ARN, then the role is simply returned. Otherwise
    #'              we retrieve the full ARN and return it.
    #' @param role (str): An AWS IAM role (either name or full ARN).
    #' @return str: The corresponding AWS IAM role ARN.
    expand_role = function(role){
      iam = self$paws_session$client("iam")
      if(grepl("/", role)) return(role)
      return(iam$get_role(RoleName = role)$Role$Arn)
    },

    #' @description  Returns the ARN user or role whose credentials are used to call the API.
    #' @return str: The ARN user or role
    get_caller_identity_arn = function(){

      if(file.exists(NOTEBOOK_METADATA_FILE)){
        metadata = read_json(NOTEBOOK_METADATA_FILE)
        instance_name = metadata[["ResourceName"]]
        domain_id = metadata[["DomainId"]]
        user_profile_name = metadata[["UserProfileName"]]

        tryCatch({
          if(is.null(domain_id)){
            instance_desc = self$sagemaker$describe_notebook_instance(NotebookInstanceName=instance_name)
            return(instance_desc$RoleArn)
          }
          user_profile_desc = self$sagemaker$describe_user_profile(
            DomainId=domain_id, UserProfileName=user_profile_name
          )

          # First, try to find role in userSettings
          if (!is.null(user_profile_desc[["UserSettings"]][["ExecutionRole"]]))
            return(user_profile_desc[["UserSettings"]][["ExecutionRole"]])

          # If not found, fallback to the domain
          domain_desc = self$sagemaker$describe_domain(DomainId=domain_id)
          return(domain_desc[["DefaultUserSettings"]][["ExecutionRole"]])
          }, error=function(e) {
            LOGGER$debug(
              "Couldn't call 'describe_notebook_instance' to get the Role \nARN of the instance %s.",
              instance_name)
        })
      }
      assumed_role = self$paws_session$client(
        "sts",
        config = list(
          region = self$paws_region_name,
          endpoint = sts_regional_endpoint(self$paws_region_name)
          )
      )$get_caller_identity()[["Arn"]]

      role = gsub("^(.+)sts::(\\d+):assumed-role/(.+?)/.*$", "\\1iam::\\2:role/\\3", assumed_role)

      # Call IAM to get the role's path
      role_name = substr(role, gregexpr("/",role)[[1]][1] + 1, nchar(role))
      tryCatch({
        role = self$paws_session$client("iam")$get_role(RoleName=role_name)[["Role"]][["Arn"]]
      }, error = function(e){
        LOGGER$warn(
          "Couldn't call 'get_role' to get Role ARN from role name %s to get Role path.",
          role_name
        )
        # This conditional has been present since the inception of SageMaker
        # Guessing this conditional's purpose was to handle lack of IAM permissions
        # https://github.com/aws/sagemaker-python-sdk/issues/2089#issuecomment-791802713
        if (grepl("AmazonSageMaker-ExecutionRole", assumed_role)){
          LOGGER$warn(paste(
            "Assuming role was created in SageMaker AWS console,",
            "as the name contains `AmazonSageMaker-ExecutionRole`.",
            "Defaulting to Role ARN with service-role in path.",
            "If this Role ARN is incorrect, please add",
            "IAM read permissions to your role or supply the",
            "Role Arn directly.")
          )
          role = gsub(
            "^(.+)sts::(\\d+):assumed-role/(.+?)/.*$",
            "\\1iam::\\2:role/service-role/\\3",
            assumed_role
          )
        }
      })
      return(role)
    },

    #' @description Display the logs for a given training job, optionally tailing them until the
    #'              job is complete. If the output is a tty or a Jupyter cell, it will be color-coded
    #'              based on which instance the log entry is from.
    #' @param job_name (str): Name of the training job to display the logs for.
    #' @param wait (bool): Whether to keep looking for new log entries until the job completes
    #'              (Default: False).
    #' @param poll (int): The interval in seconds between polling for new log entries and job
    #'              completion (Default: 10).
    #' @param log_type (str): Type of logs to return from building sagemaker process
    logs_for_job = function(job_name,
                            wait=FALSE,
                            poll=10,
                            log_type="All"){

      description = self$sagemaker$describe_training_job(TrainingJobName=job_name)
      writeLines(secondary_training_status_message(description, NULL), sep = "")

      init_log = .log_init(self, description, "Training")

      state = .get_initial_job_state(description, "TrainingJobStatus", wait)

      last_describe_job_call = Sys.time()
      last_description = description
      last_debug_rule_statuses = NULL
      while(TRUE){
        .flush_log_streams(init_log$stream_names,
                           init_log$instance_count,
                           init_log$client,
                           init_log$log_group,
                           job_name,
                           sm_env$positions)
        if(state == LogState$COMPLETE) {break}

        Sys.sleep(poll)
        if(state == LogState$JOB_COMPLETE) {
          writeLines("\n")
          state = LogState$COMPLETE
        } else if(Sys.time() - last_describe_job_call >= 30){
          description = self$sagemaker$describe_training_job(TrainingJobName=job_name)
          last_describe_job_call = Sys.time()

          if(secondary_training_status_changed(description, last_description)){
            writeLines("")
            writeLines(secondary_training_status_message(description, last_description), sep = "")
            last_description = description
          }

          status = description$TrainingJobStatus

          if (status %in% c("Completed", "Failed", "Stopped")){
            state = LogState$JOB_COMPLETE
          }
          debug_rule_statuses = description$DebugRuleEvaluationStatuses
          if(!islistempty(debug_rule_statuses)
             && .debug_rule_statuses_changed(debug_rule_statuses, last_debug_rule_statuses)
             && (log_type %in% c("All", "Rules"))){
            writeLines("\n")
            writeLines("********* Debugger Rule Status *********")
            writeLines("*")
            for (status in debug_rule_statuses){
              rule_log = sprintf("* %+18s: %-18s",
                status$RuleConfigurationName, status$RuleEvaluationStatus)
              writeLines(rule_log)
            }
            writeLines("*")
            writeLines(paste0(rep("*", 40), collapse = ""))
            last_debug_rule_statuses = debug_rule_statuses
          }
        }
      }

      if (wait) {
        private$.check_job_status(job_name, description, "TrainingJobStatus")

        spot_training = description$EnableManagedSpotTraining

        training_time = description$TrainingTimeInSeconds
        billable_time = description$BillableTimeInSeconds
        if (!is.null(training_time) || length(training_time) == 0)
          writeLines(sprintf("Training seconds: %s", training_time * init_log$instance_count))
        if (!is.null(billable_time) || length(billable_time) == 0)
          writeLines(sprintf("Billable seconds: %s", billable_time * init_log$instance_count))
        if (!is.null(spot_training) || length(spot_training) == 0){
          saving = (1 - as.numeric(billable_time) / training_time) * 100
          writeLines(sprintf("Managed Spot Training savings: %s", saving))}
      }
    },

    #' @description Display the logs for a given processing job, optionally tailing them until the
    #'              job is complete.
    #' @param job_name (str): Name of the training job to display the logs for.
    #' @param wait (bool): Whether to keep looking for new log entries until the job completes
    #'              (Default: False).
    #' @param poll (int): The interval in seconds between polling for new log entries and job
    #'              completion (Default: 10).
    logs_for_processing_job = function(job_name,
                                       wait=FALSE,
                                       poll=10){

      description = self$sagemaker$describe_processing_job(ProcessingJobName=job_name)

      init_log = .log_init(self, description, "Processing")

      state = .get_initial_job_state(description, "ProcessingJobStatus", wait)

      last_describe_job_call = Sys.time()
      while(TRUE){
        .flush_log_streams(init_log$stream_names,
                           init_log$instance_count,
                           init_log$client,
                           init_log$log_group,
                           job_name,
                           sm_env$positions)

        if(state == LogState$COMPLETE) {break}

        Sys.sleep(poll)

        if(state == LogState$JOB_COMPLETE) {
          writeLines("\n")
          state = LogState$COMPLETE
        } else if(Sys.time() - last_describe_job_call >= 30){
          description = self$sagemaker$describe_processing_job(ProcessingJobName=job_name)
          last_describe_job_call = Sys.time()

          status = description$ProcessingJobStatus

          if (status %in% c("Completed", "Failed", "Stopped")){
            state = LogState$JOB_COMPLETE
          }
        }
      }

      if (wait) {
        private$.check_job_status(job_name, description, "ProcessingJobStatus")}
    },

    #' @description Display the logs for a given transform job, optionally tailing them until the
    #'              job is complete. If the output is a tty or a Jupyter cell, it will be color-coded
    #'              based on which instance the log entry is from.
    #' @param job_name (str): Name of the transform job to display the logs for.
    #' @param wait (bool): Whether to keep looking for new log entries until the job completes
    #'              (Default: FALSE).
    #' @param poll (int): The interval in seconds between polling for new log entries and job
    #'              completion (Default: 10).
    logs_for_transform_job = function(job_name,
                                      wait=FALSE,
                                      poll=10){

      description = self$sagemaker$describe_transform_job(TransformJobName=job_name)

      init_log = .log_init(self, description, "Transform")

      state = .get_initial_job_state(description, "TransformJobStatus", wait)

      last_describe_job_call = Sys.time()
      while(TRUE){
        .flush_log_streams(init_log$stream_names,
                           init_log$instance_count,
                           init_log$client,
                           init_log$log_group,
                           job_name,
                           sm_env$positions)

        if(state == LogState$COMPLETE) {break}

        Sys.sleep(poll)

        if(state == LogState$JOB_COMPLETE) {
          writeLines("\n")
          state = LogState$COMPLETE
        } else if(Sys.time() - last_describe_job_call >= 30){
          description = self$sagemaker$describe_transform_job(TransformJobName=job_name)
          last_describe_job_call = Sys.time()

          status = description$TransformJobStatus

          if (status %in% c("Completed", "Failed", "Stopped")) state = LogState$JOB_COMPLETE
        }
      }

      if (wait) {
        private$.check_job_status(job_name, description, "TransformJobStatus")}
    },

    #' @description Deletes a FeatureGroup in the FeatureStore service.
    #' @param feature_group_name (str): name of the feature group to be deleted.
    delete_feature_group = function(feature_group_name){
      return(self$sagemaker$delete_feature_group(FeatureGroupName=feature_group_name))
    },

    #' @description Creates a FeatureGroup in the FeatureStore service.
    #' @param feature_group_name (str): name of the FeatureGroup.
    #' @param record_identifier_name (str): name of the record identifier feature.
    #' @param event_time_feature_name (str): name of the event time feature.
    #' @param feature_definitions (Sequence[Dict[str, str]]): list of feature definitions.
    #' @param role_arn (str): ARN of the role will be used to execute the api.
    #' @param online_store_config (Dict[str, str]): dict contains configuration of the
    #' @param feature online store.
    #' @param offline_store_config (Dict[str, str]): dict contains configuration of the
    #'              feature offline store.
    #' @param description (str): description of the FeatureGroup.
    #' @param tags (List[Dict[str, str]]): list of tags for labeling a FeatureGroup.
    #' @return Response dict from service.
    create_feature_group = function(feature_group_name,
                                    record_identifier_name,
                                    event_time_feature_name,
                                    feature_definitions,
                                    role_arn,
                                    online_store_config=NULL,
                                    offline_store_config=NULL,
                                    description=NULL,
                                    tags=NULL){
      tags = .append_project_tags(tags)
      if(islistempty(tags)) tags = NULL
      kwargs = list(
        FeatureGroupName=feature_group_name,
        RecordIdentifierFeatureName=record_identifier_name,
        EventTimeFeatureName=event_time_feature_name,
        FeatureDefinitions=feature_definitions,
        RoleArn=role_arn
      )
      kwargs = update_args(
        kwargs,
        OnlineStoreConfig=online_store_config,
        OfflineStoreConfig=offline_store_config,
        Description=description,
        Tags=tags
      )
      return(do.call(self$sagemaker$create_feature_group, kwargs))
    },

    #' @description Describe a FeatureGroup by name in FeatureStore service.
    #' @param feature_group_name (str): name of the FeatureGroup to descibe.
    #' @param next_token (str): next_token to get next page of features.
    #' @return Response dict from service.
    describe_feature_group = function(feature_group_name,
                                      next_token=NULL){
      kwargs = list(FeatureGroupName=feature_group_name)
      kwargs = update_args(kwargs, NextToken=next_token)
      return(do.call(self$sagemaker$describe_feature_group, kwargs))
    },

    #' @description Start Athena query execution.
    #' @param catalog (str): name of the data catalog.
    #' @param database (str): name of the data catalog database.
    #' @param query_string (str): SQL expression.
    #' @param output_location (str): S3 location of the output file.
    #' @param kms_key (str): KMS key id will be used to encrypt the result if given.
    #' @param Response dict from the service.
    start_query_execution = function(catalog,
                                     database,
                                     query_string,
                                     output_location,
                                     kms_key=NULL){
      kwargs = list(
        QueryString=query_string, QueryExecutionContext=list(Catalog=catalog, Database=database)
      )
      result_config = list(OutputLocation=output_location)
      if (!is.null(kms_key))
        result_config = modifyList(result_config, list(
          EncryptionConfiguration=list(EncryptionOption="SSE_KMS", KmsKey=kms_key))
        )
      kwargs = modifyList(kwargs, list(ResultConfiguration=result_config))
      athena_client = self$paws_session$client("athena", config=list(region=self$paws_region_name))
      return(do.call(athena_client$start_query_execution, kwargs))
    },

    #' @description Get execution status of the Athena query.
    #' @param query_execution_id (str): execution ID of the Athena query.
    get_query_execution = function(query_execution_id){
      athena_client = self$paws_session$client("athena", config=list(region=self$paws_region_name))
      return(athena_client$get_query_execution(QueryExecutionId=query_execution_id))
    },

    #' @description Wait for Athena query to finish.
    #' @param query_execution_id (str): execution ID of the Athena query.
    #' @param poll (int): time interval to poll get_query_execution API.
    wait_for_athena_query = function(query_execution_id,
                                     poll=5){
    query_state = (
      self$get_query_execution(query_execution_id=query_execution_id)[[
        "QueryExecution"]][["Status"]][["State"]]
    )
    while (!(query_state %in% c("SUCCEEDED", "FAILED"))){
      LOGGER$info("Query %s is being executed.", query_execution_id)
      Sys.sleep(poll)
      query_state = (
        self$get_query_execution(query_execution_id=query_execution_id)[[
          "QueryExecution"]][["Status"]][["State"]]
      )
    }
    if (query_state == "SUCCEEDED")
      LOGGER$info("Query %s successfully executed.", query_execution_id)
    else
      LOGGER$error("Failed to execute query %s.", query_execution_id)
    },

    #' @description Download query result file from S3.
    #' @param bucket (str): name of the S3 bucket where the result file is stored.
    #' @param prefix (str): S3 prefix of the result file.
    #' @param query_execution_id (str): execution ID of the Athena query.
    #' @param filename (str): name of the downloaded file.
    download_athena_query_result = function(bucket,
                                            prefix,
                                            query_execution_id,
                                            filename){
      obj = self$s3$get_object(Bucket = bucket, Key = sprintf("%s/%s.csv",prefix,query_execution_id))
      write_bin(obj$Body, filename)
    },

    #' @description Get the AWS account id of the caller.
    #' @return AWS account ID.
    account_id = function(){
      region = self$paws_region_name
      sts_client = self$paws_session$client(
        "sts", config = list(region=region, endpoint=sts_regional_endpoint(region))
      )
      return(sts_client$get_caller_identity()[["Account"]])
    },

    #' @description Return class documentation
    help = function(){
      cls_help(self)
    },

    #' @description foramt class
    format = function(){
      format_class(self)
    }
  ),
  private = list(
    .default_bucket = NULL,
    .default_bucket_name_override = NULL,

    # Initialize this SageMaker Session.
    # Creates or uses a boto_session, sagemaker_client and sagemaker_runtime_client.
    # Sets the region_name.
    .initialize = function(
      paws_session,
      sagemaker_client,
      sagemaker_runtime_client,
      sagemaker_featurestore_runtime_client = NULL){

      self$paws_session = if(inherits(paws_session, "PawsSession")) paws_session else PawsSession$new()

      if (is.null(self$paws_session$region_name))
          ValueError$new(
            "Must setup local AWS configuration with a region supported by SageMaker.")

      self$sagemaker = sagemaker_client %||% self$paws_session$client("sagemaker")
      self$sagemaker_runtime = sagemaker_runtime_client %||% self$paws_session$client("sagemakerruntime")
      self$s3 = self$paws_session$client("s3")
      if (!is.null(sagemaker_featurestore_runtime_client)) {
        LOGGER$error("Paws currently doesn't support `sagemaker-featurestore-runtime`")
        # self$sagemaker_featurestore_runtime_client = (
        #   sagemaker_featurestore_runtime_client  %||% self$paws_session$client("sagemakerfeaturestoreruntime")
      }
      self$local_mode = FALSE
    },

    # Creates an S3 Bucket if it does not exist.
    # Also swallows a few common exceptions that indicate that the bucket already exists or
    # that it is being created.
    # Args:
    #   bucket_name (str): Name of the S3 bucket to be created.
    #   region (str): The region in which to create the bucket.
    .create_s3_bucket_if_it_does_not_exist = function(bucket_name, region){
      resp <- tryCatch({self$s3$head_bucket(Bucket = bucket_name)}, error = function(e) e)

      # check if bucket exists: HTTP 404 bucket not found
      if(inherits(resp, "http_404")){
        tryCatch({
          self$s3$create_bucket(
            Bucket = bucket_name,
            CreateBucketConfiguration = list(LocationConstraint = region))
          LOGGER$info("Created S3 bucket: %s", bucket_name)
          }, error = function(e) {
            error_code = paws_error_code(e)
            message = e$error_response$Message
            if (identical(error_code, "BucketAlreadyOwnedByYou")) {
              invisible(NULL)
            } else if (identical(error_code, "OperationAborted") &&
                     grepl("conflicting conditional operation", message)) {
              # If this bucket is already being concurrently created, we don't need to create
              # it again.
              invisible(NULL)
            } else
              stop(e)
          }
        )
      }
    },

    # Construct CreateHyperParameterTuningJob request
    # Args:
    #   job_name (str): Name of the tuning job being created.
    # tuning_config (dict): Configuration to launch the tuning job.
    # training_config (dict): Configuration to launch training jobs under the tuning job
    # using a single algorithm.
    # training_config_list (list[dict]): A list of configurations to launch training jobs
    # under the tuning job using one or multiple algorithms. Either training_config
    # or training_config_list should be provided, but not both.
    # warm_start_config (dict): Configuration defining the type of warm start and
    # other required configurations.
    # tags (list[dict]): List of tags for labeling the tuning job. For more, see
    # https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html.
    # Returns:
    #   dict: A dictionary for CreateHyperParameterTuningJob request
    .get_tuning_request = function(job_name,
                                   tuning_config,
                                   training_config=NULL,
                                   training_config_list=NULL,
                                   warm_start_config=NULL,
                                   tags=NULL){
      tune_request = list(
        HyperParameterTuningJobName = job_name,
        HyperParameterTuningJobConfig = do.call(private$.map_tuning_config, tuning_config)
      )
      if(!is.null(training_config))
        tune_request$TrainingJobDefinition = do.call(private$.map_training_config, training_config)

      if (!is.null(training_config_list))
        tune_request$TrainingJobDefinitions= lapply(
          training_config_list, function(training_cfg)
            do.call(private$.map_training_config, training_cfg)
        )

      tune_request$WarmStartConfig = warm_start_config
      tags = .append_project_tags(tags)
      if (!islistempty(tags))
        tune_request[["Tags"]] = tags
      return(tune_request)
    },

    .map_tuning_config = function(strategy,
                                  max_jobs,
                                  max_parallel_jobs,
                                  early_stopping_type="Off",
                                  objective_type=NULL,
                                  objective_metric_name=NULL,
                                  parameter_ranges=NULL){
      tuning_config = list(Strategy = strategy,
                           ResourceLimits = list(
                             MaxNumberOfTrainingJobs = max_jobs,
                             MaxParallelTrainingJobs = max_parallel_jobs),
                           TrainingJobEarlyStoppingType =  early_stopping_type)

      # ----- bring .map_tuning_objective into function ------
      tuning_objective = NULL

      if (!is.null(objective_type) || !is.null(objective_metric_name)) {
        tuning_objective = list()
        tuning_objective$Type = objective_type
        tuning_objective$MetricName = objective_metric_name}
      # ------------------------------------------------------

      tuning_config$HyperParameterTuningJobObjective = tuning_objective

      tuning_config$ParameterRanges = parameter_ranges

      return(tuning_config)
    },

    # Constructs a request compatible for creating an Amazon SageMaker training job.
    # Args:
    #   input_mode (str): The input mode that the algorithm supports. Valid modes:
    #   * 'File' - Amazon SageMaker copies the training dataset from the S3 location to
    # a directory in the Docker container.
    # * 'Pipe' - Amazon SageMaker streams data directly from S3 to the container via a
    # Unix-named pipe.
    # input_config (list): A list of Channel objects. Each channel is a named input source.
    # Please refer to the format details described:
    #   https://botocore.readthedocs.io/en/latest/reference/services/sagemaker.html#SageMaker.Client.create_training_job
    # role (str): An AWS IAM role (either name or full ARN). The Amazon SageMaker training
    # jobs and APIs that create Amazon SageMaker endpoints use this role to access
    # training data and model artifacts. You must grant sufficient permissions to this
    # role.
    # job_name (str): Name of the training job being created.
    # output_config (dict): The S3 URI where you want to store the training results and
    # optional KMS key ID.
    # resource_config (dict): Contains values for ResourceConfig:
    #   * instance_count (int): Number of EC2 instances to use for training.
    # The key in resource_config is 'InstanceCount'.
    # * instance_type (str): Type of EC2 instance to use for training, for example,
    # 'ml.c4.xlarge'. The key in resource_config is 'InstanceType'.
    # vpc_config (dict): Contains values for VpcConfig:
    #   * subnets (list[str]): List of subnet ids.
    # The key in vpc_config is 'Subnets'.
    # * security_group_ids (list[str]): List of security group ids.
    # The key in vpc_config is 'SecurityGroupIds'.
    # hyperparameters (dict): Hyperparameters for model training. The hyperparameters are
    # made accessible as a dict[str, str] to the training code on SageMaker. For
    # convenience, this accepts other types for keys and values, but ``str()`` will be
    # called to convert them before training.
    # stop_condition (dict): Defines when training shall finish. Contains entries that can
    # be understood by the service like ``MaxRuntimeInSeconds``.
    # tags (list[dict]): List of tags for labeling a training job. For more, see
    # https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html.
    # metric_definitions (list[dict]): A list of dictionaries that defines the metric(s)
    # used to evaluate the training jobs. Each dictionary contains two keys: 'Name' for
    # the name of the metric, and 'Regex' for the regular expression used to extract the
    # metric from the logs.
    # enable_network_isolation (bool): Whether to request for the training job to run with
    # network isolation or not.
    # image_uri (str): Docker image containing training code.
    # algorithm_arn (str): Algorithm Arn from Marketplace.
    # encrypt_inter_container_traffic (bool): Specifies whether traffic between training
    # containers is encrypted for the training job (default: ``False``).
    # use_spot_instances (bool): whether to use spot instances for training.
    # checkpoint_s3_uri (str): The S3 URI in which to persist checkpoints
    # that the algorithm persists (if any) during training. (default:
    #                                                          ``None``).
    # checkpoint_local_path (str): The local path that the algorithm
    # writes its checkpoints to. SageMaker will persist all files
    # under this path to `checkpoint_s3_uri` continually during
    # training. On job startup the reverse happens - data from the
    # s3 location is downloaded to this path before the algorithm is
    # started. If the path is unset then SageMaker assumes the
    # checkpoints will be provided under `/opt/ml/checkpoints/`.
    # (default: ``None``).
    # experiment_config (dict): Experiment management configuration. Dictionary contains
    # three optional keys, 'ExperimentName', 'TrialName', and 'TrialComponentDisplayName'.
    # (default: ``None``)
    # enable_sagemaker_metrics (bool): enable SageMaker Metrics Time
    # Series. For more information see:
    #   https://docs.aws.amazon.com/sagemaker/latest/dg/API_AlgorithmSpecification.html#SageMaker-Type-AlgorithmSpecification-EnableSageMakerMetricsTimeSeries
    # (default: ``None``).
    # profiler_rule_configs (list[dict]): A list of profiler rule configurations.
    # profiler_config(dict): Configuration for how profiling information is emitted with
    # SageMaker Profiler. (default: ``None``).
    # environment (dict[str, str]) : Environment variables to be set for
    # use during training job (default: ``None``)
    # retry_strategy(dict): Defines RetryStrategy for InternalServerFailures.
    # * max_retry_attsmpts (int): Number of times a job should be retried.
    # The key in RetryStrategy is 'MaxRetryAttempts'.
    # Returns:
    #   Dict: a training request dict
    .get_train_request = function(input_mode,
                                  input_config,
                                  role,
                                  job_name,
                                  output_config,
                                  resource_config,
                                  vpc_config,
                                  hyperparameters,
                                  stop_condition,
                                  tags,
                                  metric_definitions,
                                  enable_network_isolation=FALSE,
                                  image_uri=NULL,
                                  algorithm_arn=NULL,
                                  encrypt_inter_container_traffic=FALSE,
                                  use_spot_instances=FALSE,
                                  checkpoint_s3_uri=NULL,
                                  checkpoint_local_path=NULL,
                                  experiment_config=NULL,
                                  debugger_rule_configs=NULL,
                                  debugger_hook_config=NULL,
                                  tensorboard_output_config=NULL,
                                  enable_sagemaker_metrics=NULL,
                                  profiler_rule_configs=NULL,
                                  profiler_config=NULL,
                                  environment=NULL,
                                  retry_strategy=NULL){
      train_request = list(
        "AlgorithmSpecification"=list("TrainingInputMode"= input_mode),
        "OutputDataConfig"= output_config,
        "TrainingJobName"= job_name,
        "StoppingCondition"= stop_condition,
        "ResourceConfig"= resource_config,
        "RoleArn"= role)

      if (!is.null(image_uri) && !is.null(algorithm_arn))
        ValueError$new(
          "image_uri and algorithm_arn are mutually exclusive.",
          sprintf("Both were provided: image_uri: %s algorithm_arn: %s", image_uri, algorithm_arn))
      if (is.null(image_uri) && is.null(algorithm_arn))
        ValueError$new("either image_uri or algorithm_arn is required. None was provided.")

      train_request$AlgorithmSpecification$TrainingImage = image_uri
      train_request$AlgorithmSpecification$AlgorithmName = algorithm_arn
      train_request$InputDataConfig = input_config
      train_request$AlgorithmSpecification$MetricDefinitions = metric_definitions
      train_request$AlgorithmSpecification$EnableSageMakerMetricsTimeSeries = enable_sagemaker_metrics

      if(!islistempty(hyperparameters))
        train_request$HyperParameters = hyperparameters

      # Paws currently doesn't support
      # train_request$Environment = environment
      LOGGER$info("Paws currently doesn't support `environment`")
      if(!islistempty(tags))
        train_request$Tags = tags
      train_request$VpcConfig = vpc_config

      if (!islistempty(experiment_config))
        train_request$ExperimentConfig = experiment_config

      if (isTRUE(enable_network_isolation))
        train_request$EnableNetworkIsolation = enable_network_isolation
      if (isTRUE(encrypt_inter_container_traffic))
        train_request$EnableInterContainerTrafficEncryption = encrypt_inter_container_traffic
      if (isTRUE(use_spot_instances))
        train_request$EnableManagedSpotTraining = use_spot_instances

      if (!is.null(checkpoint_s3_uri)){
        checkpoint_config = list("S3Uri"= checkpoint_s3_uri)
        checkpoint_config[["LocalPath"]] = checkpoint_local_path
        train_request$CheckpointConfig = checkpoint_config
      }
      train_request$DebugRuleConfigurations = debugger_rule_configs
      train_request$DebugHookConfig = debugger_hook_config
      train_request$TensorBoardOutputConfig = tensorboard_output_config
      train_request$ProfilerRuleConfigurations = profiler_rule_configs
      train_request$ProfilerConfig = profiler_config
      # Paws currently doesn't support Retry Strategory
      # train_request$RetryStrategy = retry_strategy
      LOGGER$info("Paws currently doesn't support `retry_strategy`")

      return(train_request)
    },

    # Constructs a request compatible for updateing an Amazon SageMaker training job.
    # Args:
    #   job_name (str): Name of the training job being updated.
    # profiler_rule_configs (list): List of profiler rule configurations. (default: ``None``).
    # profiler_config(dict): Configuration for how profiling information is emitted with
    # SageMaker Profiler. (default: ``None``).
    # Returns:
    #   Dict: an update training request dict
    .get_update_training_job_request = function(job_name,
                                                profiler_rule_configs=NULL,
                                                profiler_config=NULL){
      update_training_job_request = list(
        "TrainingJobName"=job_name
      )
      update_training_job_request[["ProfilerRuleConfigurations"]] = profiler_rule_configs
      update_training_job_request[["ProfilerConfig"]] = profiler_config

      return(update_training_job_request)
    },

    .get_process_request = function(inputs,
                                    output_config,
                                    job_name,
                                    resources,
                                    stopping_condition,
                                    app_specification,
                                    environment,
                                    network_config,
                                    role_arn,
                                    tags,
                                    experiment_config=NULL){
      process_request = list(
        ProcessingJobName = job_name,
        ProcessingResources = resources,
        AppSpecification = app_specification,
        RoleArn = role_arn
      )
      process_request$ProcessingInputs = inputs

      if(!islistempty(output_config$Outputs))
        process_request$ProcessingOutputConfig = output_config

      process_request$Environment = environment
      process_request$NetworkConfig = network_config
      process_request$StoppingCondition = stopping_condition
      if(!islistempty(tags))
        process_request$Tags = tags
      process_request$ExperimentConfig = experiment_config
      return(process_request)
    },

    .get_transform_request = function(job_name,
                                      model_name,
                                      strategy,
                                      max_concurrent_transforms,
                                      max_payload,
                                      env,
                                      input_config,
                                      output_config,
                                      resource_config,
                                      experiment_config,
                                      tags,
                                      data_processing,
                                      model_client_config=NULL){
      transform_request = list(
        "TransformJobName"=job_name,
        "ModelName"=model_name,
        "TransformInput"=input_config,
        "TransformOutput"=output_config,
        "TransformResources"=resource_config)

      transform_request[["BatchStrategy"]] = strategy
      transform_request[["MaxConcurrentTransforms"]] = max_concurrent_transforms
      transform_request[["MaxPayloadInMB"]] = max_payload
      transform_request[["Environment"]] = env
      if(!islistempty(tags))
        transform_request[["Tags"]] = tags
      transform_request[["DataProcessing"]] = data_processing

      if (!islistempty(experiment_config))
        transform_request[["ExperimentConfig"]] = experiment_config

      if (!islistempty(model_client_config))
        transform_request[["ModelClientConfig"]] = model_client_config

      return(transform_request)
    },

    .create_model_request = function(name,
                                     role,
                                     container_defs,
                                     vpc_config=NULL,
                                     enable_network_isolation=FALSE,
                                     primary_container=NULL,
                                     tags=NULL){
      if (!is.null(container_defs) && !is.null(primary_container))
        ValueError$new("Both container_defs and primary_container can not be passed as input")

      if (!is.null(primary_container)){
        msg = paste(
          "primary_container is going to be deprecated in a future release. Please use ",
          "container_defs instead.")
        warning(msg)
        container_defs = primary_container
      }

      role = self$expand_role(role)

      if (!is_list_named(container_defs)){
        container_definition = container_defs
      } else {
        container_definition = private$.expand_container_def(container_defs)
      }
      request = list("ModelName"=name, "ExecutionRoleArn"=role)
      if (is.list(container_definition) && !is_list_named(container_definition)){
        request[["Containers"]] = container_definition
      } else {
        request[["PrimaryContainer"]] = container_definition}
      if(!islistempty(tags))
        request[["Tags"]] = tags
      request[["VpcConfig"]] = vpc_config

      if (enable_network_isolation)
        request[["EnableNetworkIsolation"]] = TRUE

      return(request)
    },

    .map_training_config = function(static_hyperparameters,
                                    input_mode,
                                    role,
                                    output_config,
                                    resource_config,
                                    stop_condition,
                                    input_config=NULL,
                                    metric_definitions=NULL,
                                    image_uri=NULL,
                                    algorithm_arn=NULL,
                                    vpc_config=NULL,
                                    enable_network_isolation=FALSE,
                                    encrypt_inter_container_traffic=FALSE,
                                    estimator_name=NULL,
                                    objective_type=NULL,
                                    objective_metric_name=NULL,
                                    parameter_ranges=NULL,
                                    use_spot_instances=FALSE,
                                    checkpoint_s3_uri=NULL,
                                    checkpoint_local_path=NULL,
                                    max_retry_attempts=NULL){

      training_job_definition = list(
        StaticHyperParameters = static_hyperparameters,
        RoleArn = role,
        OutputDataConfig = output_config,
        ResourceConfig = resource_config,
        StoppingCondition = stop_condition)


      algorithm_spec = list(TrainingInputMode = input_mode)

      algorithm_spec$MetricDefinitions = metric_definitions


      if (!is.null(algorithm_arn)) {
        algorithm_spec$AlgorithmName = algorithm_arn
        } else {
        algorithm_spec$TrainingImage = image_uri}

      training_job_definition$AlgorithmSpecification = algorithm_spec

      training_job_definition$InputDataConfig = input_config

      training_job_definition$VpcConfig = vpc_config

      if (enable_network_isolation) training_job_definition$EnableNetworkIsolation = TRUE

      if (encrypt_inter_container_traffic) training_job_definition$EnableInterContainerTrafficEncryption = TRUE

      if (use_spot_instances)  training_job_definition$EnableManagedSpotTraining = TRUE


      if (!is.null(checkpoint_s3_uri)){
        checkpoint_config = list()
        checkpoint_config = list(S3Uri = checkpoint_s3_uri)
        if (!is.null(checkpoint_local_path)){
          checkpoint_config$LocalPath = checkpoint_local_path}
        training_job_definition$CheckpointConfig = checkpoint_config}

      training_job_definition$DefinitionName = estimator_name

      tuning_objective = NULL

      if (!is.null(objective_type) || !is.null(objective_metric_name)) {
        tuning_objective = list()
        tuning_objective$Type = objective_type
        tuning_objective$MetricName = objective_metric_name}

      training_job_definition$TuningObjective = tuning_objective
      training_job_definition$HyperParameterRanges = parameter_ranges
      if (!is.null(max_retry_attempts))
        training_job_definition$RetryStrategy = list("MaximumRetryAttempts"=max_retry_attempts)
      return(training_job_definition)
    },

    .expand_container_def = function(c_def){
      if (is.character(c_def))
        return(container_def(c_def))
      return(c_def)
    },

    .vpc_config_from_training_job = function(training_job_desc,
                                             vpc_config_override="VPC_CONFIG_DEFAULT"){

      if (identical(vpc_config_override, "VPC_CONFIG_DEFAULT")){
        return(training_job_desc$VpcConfig)
      }
      return(vpc_sanitize(vpc_config_override))
    },

    .wait_until = function(expr,
                           poll = 5){

      result = eval.parent(substitute(expr))
      while((is.null(result) || length(result) == 0)){
        Sys.sleep(poll)
        result = eval.parent(substitute(expr))
      }
      return(result)
    },

    .auto_ml_job_status = function(job_name){

      auto_ml_job_status_codes = list(
        "Completed"= "!",
        "InProgress"= ".",
        "Failed"= "*",
        "Stopped"= "s",
        "Stopping"= "_")

      in_progress_statuses = c("InProgress", "Stopping")

      desc = self$sagemaker$describe_auto_ml_job(AutoMLJobName=job_name)
      status = desc$AutoMLJobStatus

      msg = auto_ml_job_status_codes[[status]]
      if(is.null(msg)) msg = "?"

      writeLines(msg, sep="")
      flush(stdout())

      if (status %in% in_progress_statuses) return(NULL)

      writeLines("\n")
      return(desc)
    },

    .create_model_package_status = function(model_package_name){
      in_progress_statuses = c("InProgress", "Pending")

      desc = self$sagemaker$describe_model_package(ModelPackageName=model_package_name)
      status = desc$ModelPackageStatus

      writeLines(".", sep = "")
      flush(stdout())

      if (status %in% in_progress_statuses) return(NULL)

      writeLines("\n")
      return(desc)
    },

    .wait_until_training_done = function(expr,
                                         poll = 5){
      result = eval.parent(substitute(expr))
      while(!result$status){
        Sys.sleep(poll)
        result = eval.parent(substitute(expr))
      }
      return(result$job_desc)
    },

    .train_done = function(job_name){
      in_progress_statuses = c("InProgress", "Created")

      # Get last job description
      last_desc = private$.last_job_desc

      desc = self$sagemaker$describe_training_job(TrainingJobName=job_name)
      status = desc$TrainingJobStatus

      if(secondary_training_status_changed(desc, last_desc)){
        writeLines("")
        writeLines(secondary_training_status_message(desc, last_desc), sep = "")
      } else {
        writeLines(".", sep = "")
      }

      flush(stdout())
      # update last job description
      private$.last_job_desc = desc

      if(status %in% in_progress_statuses){
        return(list(job_desc = desc, status = FALSE))
      }

      writeLines("")
      return(list(job_desc = desc, status = TRUE))
    },

    .check_job_status = function(job,
                                 desc,
                                 status_key_name){
      status = desc[[status_key_name]]
      # convert status to camel case
      status = .STATUS_CODE_TABLE[[toupper(status)]]

      if(!(status %in% c("Completed", "Stopped"))){
        reason = desc$FailureReason
        job_type = gsub("JobStatus", " job", status_key_name)
        message = sprintf("Error for %s %s: %s. Reason: %s", job_type, job, status, reason)
        UnexpectedStatusError$new(
          message,
          allowed_statuses=c("Completed", "Stopped"),
          actual_status=status)
      }
    },
    # last job desc to help check if job has finished or not
    .last_job_desc = NULL,

    .compilation_job_status = function(job_name){
      compile_status_codes = list(
        "Completed"= "!",
        "InProgress"= ".",
        "Failed"= "*",
        "Stopped"= "s",
        "Stopping"= "_")
      in_progress_statuses = c("InProgress", "Stopping", "Starting")

      desc = self$sagemaker$describe_compilation_job(CompilationJobName=job_name)
      status = desc$CompilationJobStatus
      status = .STATUS_CODE_TABLE[[toupper(status)]]

      msg = compile_status_codes[[status]]
      if(is.null(msg)) msg = "?"

      writeLines(msg, sep = "")
      flush(stdout())

      if (status %in% in_progress_statuses) return(NULL)

      return(desc)
    },

    # Process the current status of a packaging job
    # Args:
    #   sagemaker_client (boto3.client.sagemaker): a sagemaker client
    # job_name (str): the name of the job to inspec
    # Returns:
    #   Dict: the status of the edge packaging job
    .edge_packaging_job_status = function(job_name){
      package_status_codes = list(
        "Completed"= "!",
        "InProgress"= ".",
        "Failed"= "*",
        "Stopped"= "s",
        "Stopping"= "_")
      in_progress_statuses = c("InProgress", "Stopping", "Starting")

      desc = self$sagemaker$describe_edge_packaging_job(EdgePackagingJobName=job_name)
      status = desc$EdgePackagingJobStatus

      status = .STATUS_CODE_TABLE[[toupper(status)]] %||% status
      writeLines((package_status_codes[[status]] %||% "?"), sep = "")
      flush(stdout())

      if (status %in% in_progress_statuses) return(NULL)

      return(desc)
    },

    .tuning_job_status = function(job_name){
      tuning_status_codes = list(
        "Completed"= "!",
        "InProgress"= ".",
        "Failed"= "*",
        "Stopped"= "s",
        "Stopping"= "_")

      in_progress_statuses = c("InProgress", "Stopping")

      desc = self$sagemaker$describe_hyper_parameter_tuning_job(
        HyperParameterTuningJobName=job_name)

      status = desc$HyperParameterTuningJobStatus

      msg = tuning_status_codes[[status]]
      if(is.null(msg)) msg = "?"

      writeLines(msg, sep = "")
      flush(stdout())

      if (status %in% in_progress_statuses) return(NULL)

      writeLines("\n")
      return(desc)
    },

    .transform_job_status = function(job_name){
      transform_job_status_codes = list(
        "Completed"= "!",
        "InProgress"= ".",
        "Failed"= "*",
        "Stopped"= "s",
        "Stopping"= "_")

      in_progress_statuses = c("InProgress", "Stopping")

      desc = self$sagemaker$describe_transform_job(TransformJobName=job_name)
      status = desc$TransformJobStatus

      msg = transform_job_status_codes[[status]]
      if(is.null(msg)) msg = "?"

      writeLines(msg, sep = "")
      flush(stdout())

      if (status %in% in_progress_statuses) return(NULL)

      writeLines("\n")
      return(desc)
    },

    .deploy_done = function(endpoint_name){
      hosting_status_codes = list(
        "OutOfService"= "x",
        "Creating"= "-",
        "Updating"= "-",
        "InService"= "!",
        "RollingBack"= "<",
        "Deleting"= "o",
        "Failed"= "*")
      in_progress_statuses = c("Creating", "Updating")

      desc = self$sagemaker$describe_endpoint(EndpointName=endpoint_name)
      status = desc$EndpointStatus

      msg = hosting_status_codes[[status]]
      if(is.null(msg)) msg = "?"

      writeLines(msg, sep = "")
      flush(stdout())

      if (status %in% in_progress_statuses)
        return(NULL)

      writeLines("")
      return (desc)
    },

    # Get request dictionary for CreateModelPackage API.
    # Args:
    #   model_package_name (str): Model Package name, exclusive to `model_package_group_name`,
    # using `model_package_name` makes the Model Package un-versioned (default: None).
    # model_package_group_name (str): Model Package Group name, exclusive to
    # `model_package_name`, using `model_package_group_name` makes the Model Package
    # versioned (default: None).
    # containers (list): A list of inference containers that can be used for inference
    # specifications of Model Package (default: None).
    # content_types (list): The supported MIME types for the input data (default: None).
    # response_types (list): The supported MIME types for the output data (default: None).
    # inference_instances (list): A list of the instance types that are used to
    # generate inferences in real-time (default: None).
    # transform_instances (list): A list of the instance types on which a transformation
    # job can be run or on which an endpoint can be deployed (default: None).
    # model_metrics (ModelMetrics): ModelMetrics object (default: None).
    # metadata_properties (MetadataProperties): MetadataProperties object (default: None).
    # marketplace_cert (bool): A boolean value indicating if the Model Package is certified
    # for AWS Marketplace (default: False).
    # approval_status (str): Model Approval Status, values can be "Approved", "Rejected",
    # or "PendingManualApproval" (default: "PendingManualApproval").
    # description (str): Model Package description (default: None).
    # drift_check_baselines (DriftCheckBaselines): DriftCheckBaselines object (default: None)
    .get_create_model_package_request = function(model_package_name=NULL,
                                                 model_package_group_name=NULL,
                                                 containers=NULL,
                                                 content_types=NULL,
                                                 response_types=NULL,
                                                 inference_instances=NULL,
                                                 transform_instances=NULL,
                                                 model_metrics=NULL,
                                                 metadata_properties=NULL,
                                                 marketplace_cert=FALSE,
                                                 approval_status="PendingManualApproval",
                                                 description=NULL,
                                                 drift_check_baselines=NULL){
      if (!is.null(model_package_name) && !is.null(model_package_group_name))
        ValueError$new(
          "model_package_name and model_package_group_name cannot be present at the ",
          "same time."
        )
      request_dict = list()
      request_dict$ModelPackageName = model_package_name
      request_dict$ModelPackageGroupName = model_package_group_name
      request_dict$ModelPackageDescription = description
      request_dict$ModelMetrics = model_metrics
      # Currently Paws doesn't support DriftCheckBaselines
      # request_dict$DriftCheckBaselines = drift_check_baselines
      request_dict$MetadataProperties = metadata_properties
      if (!is.null(containers)){
        if (!all(!is.null(content_types) && !is.null(response_types) &&
              !is.null(inference_instances) && !is.null(transform_instances))){
          ValueError$new(
            "content_types, response_types, inference_inferences and transform_instances ",
            "must be provided if containers is present."
          )
        }
        inference_specification = list(
          "Containers"= containers,
          "SupportedContentTypes"= content_types,
          "SupportedResponseMIMETypes"= response_types,
          "SupportedRealtimeInferenceInstanceTypes"= inference_instances,
          "SupportedTransformInstanceTypes"= transform_instances)
        request_dict$InferenceSpecification = inference_specification
      }
      request_dict$CertifyForMarketplace = marketplace_cert
      request_dict$ModelApprovalStatus = approval_status
      return(request_dict)
    }
  ),
  active = list(
    #' @field paws_region_name
    #' Returns aws region associated with Session
    paws_region_name = function() {self$paws_session$region_name}
  ),
  lock_objects = F
)

#' @title Get arguments for create_model_package method.
#' @param content_types (list): The supported MIME types for the input data.
#' @param response_types (list): The supported MIME types for the output data.
#' @param inference_instances (list): A list of the instance types that are used to
#'              generate inferences in real-time.
#' @param transform_instances (list): A list of the instance types on which a transformation
#'              job can be run or on which an endpoint can be deployed.
#' @param model_package_name (str): Model Package name, exclusive to `model_package_group_name`,
#'              using `model_package_name` makes the Model Package un-versioned (default: None).
#' @param model_package_group_name (str): Model Package Group name, exclusive to
#'              `model_package_name`, using `model_package_group_name` makes the Model Package
#'              versioned (default: None).
#' @param model_data : Placeholder
#' @param image_uri (str): Inference image uri for the container. Model class' self.image will
#'              be used if it is None (default: None).
#' @param model_metrics (ModelMetrics): ModelMetrics object (default: None).
#' @param metadata_properties (MetadataProperties): MetadataProperties object (default: None).
#' @param marketplace_cert (bool): A boolean value indicating if the Model Package is certified
#'              for AWS Marketplace (default: False).
#' @param approval_status (str): Model Approval Status, values can be "Approved", "Rejected",
#'              or "PendingManualApproval" (default: "PendingManualApproval").
#' @param description (str): Model Package description (default: None).
#' @param tags : Placeholder
#' @param container_def_list (list): A list of container defintiions.
#' @param drift_check_baselines (DriftCheckBaselines): DriftCheckBaselines object (default: None).
#' @return list: A dictionary of method argument names and values.
#' @export
get_model_package_args = function(content_types,
                                  response_types,
                                  inference_instances,
                                  transform_instances,
                                  model_package_name=NULL,
                                  model_package_group_name=NULL,
                                  model_data=NULL,
                                  image_uri=NULL,
                                  model_metrics=NULL,
                                  metadata_properties=NULL,
                                  marketplace_cert=FALSE,
                                  approval_status=NULL,
                                  description=NULL,
                                  tags=NULL,
                                  container_def_list=NULL,
                                  drift_check_baselines=NULL){
  if (!is.null(container_def_list)){
    containers = container_def_list
  } else {
    container = list(
      "Image"=image_uri,
      "ModelDataUrl"=model_data)
    containers = list(container)
  }
  model_package_args = list(
    "containers"=containers,
    "content_types"=content_types,
    "response_types"=response_types,
    "inference_instances"=inference_instances,
    "transform_instances"=transform_instances,
    "marketplace_cert"=marketplace_cert
  )
  model_package_args[["model_package_name"]] = model_package_name
  model_package_args[["model_package_group_name"]] = model_package_group_name
  if(!is.null(model_metrics)) {
    model_package_args[["model_metrics"]] = model_metrics$to_request_list()
  }
  # Paws currently doesn't support drift check baselines
  if(!is.null(drift_check_baselines)) {
    LOGGER$warn("Paws SDK currently doesn't drift_check_baselines. For the time being this parameter is ignored.")
    # model_package_args[["drift_check_baselines"]] = drift_check_baselines$to_request_list()
  }
  if(!is.null(metadata_properties)){
    model_package_args[["metadata_properties"]] = metadata_properties$to_request_list()
  }
  model_package_args[["approval_status"]] = approval_status
  model_package_args[["description"]] = description
  model_package_args[["tags"]] = tags
  return(model_package_args)
}

# Updates the request arguments dict with the value if populated.
# This is to handle the case that the service API doesn't like NoneTypes for argument values.
# Args:
#     request_args (Dict[str, Any]): the request arguments dict
#     ... : key, value pairs to update the args dict
update_args = function(args, ...){
  kwargs = list(...)
  kwargs = Filter(Negate(is.null), kwargs)
  return(modifyList(args, kwargs))
}

#' @title Create a definition for executing a container as part of a SageMaker model.
#' @param image_uri (str): Docker image to run for this container.
#' @param model_data_url (str): S3 URI of data required by this container,
#'              e.g. SageMaker training job model artifacts (default: None).
#' @param env (dict[str, str]): Environment variables to set inside the container (default: None).
#' @param container_mode (str): The model container mode. Valid modes:
#'              \itemize{
#'                \item{\strong{MultiModel:} Indicates that model container can support hosting multiple models}
#'                \item{\strong{SingleModel:} Indicates that model container can support hosting a single model
#'                              This is the default model container mode when container_mode = None}}
#' @param image_config (dict[str, str]): Specifies whether the image of model container is pulled
#'              from ECR, or private registry in your VPC. By default it is set to pull model
#'              container image from ECR. (default: None).
#' @return dict[str, str]: A complete container definition object usable with the CreateModel API if
#'              passed via `PrimaryContainers` field.
#' @export
container_def <- function(image_uri,
                          model_data_url=NULL,
                          env=NULL,
                          container_mode=NULL,
                          image_config=NULL){
  if(is.null(env)) env = list()
  c_def = list("Image" = image_uri, "Environment"= env)
  c_def$ModelDataUrl = model_data_url
  c_def$Mode = container_mode
  c_def$ImageConfig = image_config
  return(c_def)
}

#' @title Create a definition for executing a pipeline of containers as part of a SageMaker model.
#' @param models (list[sagemaker.Model]): this will be a list of ``sagemaker.Model`` objects in the
#'              order the inference should be invoked.
#' @param instance_type (str): The EC2 instance type to deploy this Model to. For example,
#'              'ml.p2.xlarge' (default: None).
#' @return list[dict[str, str]]: list of container definition objects usable with with the
#'              CreateModel API for inference pipelines if passed via `Containers` field.
#' @export
pipeline_container_def <- function(models, instance_type=NULL){
  c_defs = list()  # should contain list of container definitions in the same order customer passed
  for (model in models){
    c_defs = c(c_defs, model$prepare_container_def(instance_type))}
  return(c_defs)
}

#' @title Create a production variant description suitable for use in a ``ProductionVariant`` list.
#' @description This is also part of a ``CreateEndpointConfig`` request.
#' @param model_name (str): The name of the SageMaker model this production variant references.
#' @param instance_type (str): The EC2 instance type for this production variant. For example,
#'              ml.c4.8xlarge'.
#' @param initial_instance_count (int): The initial instance count for this production variant
#'              (default: 1).
#' @param variant_name (string): The ``VariantName`` of this production variant
#'              (default: 'AllTraffic').
#' @param initial_weight (int): The relative ``InitialVariantWeight`` of this production variant
#'              (default: 1).
#' @param accelerator_type (str): Type of Elastic Inference accelerator for this production variant.
#'              For example, 'ml.eia1.medium'.
#'              For more information: \url{https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html}
#' @param serverless_inference_config (list): Specifies configuration dict related to serverless
#'              endpoint. The dict is converted from sagemaker.model_monitor.ServerlessInferenceConfig
#'              object (default: None)
#' @return dict[str, str]: An SageMaker ``ProductionVariant`` description
#' @export
production_variant <- function(model_name,
                               instance_type=NULL,
                               initial_instance_count=NULL,
                               variant_name="AllTraffic",
                               initial_weight=1,
                               accelerator_type=NULL,
                               serverless_inference_config=NULL){

  production_variant_configuration = list(
    ModelName = model_name,
    VariantName = variant_name,
    InitialVariantWeight =  initial_weight
  )
  production_variant_configuration[["AcceleratorType"]] = accelerator_type
  if(!is.null(serverless_inference_config)){
    production_variant_configuration[["ServerlessConfig"]] = serverless_inference_config
  } else {
    initial_instance_count = initial_instance_count %||% 1
    production_variant_configuration[["InitialInstanceCount"]] = initial_instance_count
    production_variant_configuration[["InstanceType"]] = instance_type
  }
  return(production_variant_configuration)
}

.deployment_entity_exists <- function(describe_fn){
  tryCatch({
    eval.parent(substitute(describe_fn))
    return(TRUE)
  }, error = function(e){
    error_code = paws_error_code(e)
    if(!(identical(error_code, "ValidationException")
         && grepl("Could not find", e$error_response$Message))) {
      stop(e)
    }
  })
  return (FALSE)
}

#' @title Return the role ARN whose credentials are used to call the API.
#' @param sagemaker_session (Session): Current sagemaker session
#' @return (str): The role ARN
#' @export
get_execution_role <- function(sagemaker_session = NULL){
  sagemaker_session <- if(!inherits(sagemaker_session, "Session")) Session$new() else sagemaker_session

  arn <- sagemaker_session$get_caller_identity_arn()

  if (grepl(":role/", arn)) {
    return(arn)
  } else {
    message <- sprintf("The current AWS identity is not a role: %s, therefore it cannot be used as a SageMaker execution role", arn)
    ValueError$new(message)
  }
}

.get_initial_job_state <- function(description, status_key, wait){
  status = description[[status_key]]
  job_already_completed = status %in% c("Completed", "Failed", "Stopped")
  return(if(wait && !job_already_completed) LogState$TAILING else LogState$COMPLETE)
}

# Checks the rule evaluation statuses for SageMaker Debugger rules.
.debug_rule_statuses_changed <- function(current_statuses,
                                         last_statuses){
  if (islistempty(last_statuses)) return(TRUE)

  if (current_statuses$RuleConfigurationName == last_statuses$RuleConfigurationName
    && (current_statuses$RuleEvaluationStatus != last_statuses$RuleEvaluationStatus))
    return(TRUE)
  return(FALSE)
}

.log_init <- function(sagemaker_session, description, job){
  switch(job,
         "Training" = {instance_count = description$ResourceConfig$InstanceCount},
         "Transform" = {instance_count = description$TransformResources$InstanceCount},
         "Processing" = {instance_count = description$ProcessingResources$ClusterConfig$InstanceCount},
         "AutoML" = {instance_count = 0})
  stream_names = list()
  log_group = sprintf("/aws/sagemaker/%sJobs",job)

  # Increase retries allowed, to be interrupted by a transient exception.
  client = sagemaker_session$paws_session$client("cloudwatchlogs", config = list(max_retries = 15))

  # reset position pkg environmental variable
  sm_env$positions = NULL
  return(list(
    "client" = client,
    "stream_names" = stream_names,
    "log_group" = log_group,
    "instance_count"= instance_count)
  )
}

.flush_log_streams <- function(stream_names,
                              instance_count,
                              cloudwatchlogs,
                              log_group,
                              job_name,
                              positions = sm_env$positions){
  if (length(stream_names) < instance_count){
    tryCatch({streams = cloudwatchlogs$describe_log_streams(
      logGroupName=log_group,
      logStreamNamePrefix=paste0(job_name, "/"),
      orderBy="LogStreamName",
      limit=min(instance_count, 50))
    },
    error = function(e){
      # On the very first training job run on an account, there's no log group until
      # the container starts logging, so ignore any errors thrown about that
      error_code = paws_error_code(e)
      if (!identical(error_code, "ResourceNotFoundException"))
        stop(e)
    })

    stream_names = lapply(streams$logStreams, function(s) s$logStreamName)

    while (length(streams$nextToken) > 0){
      tryCatch({streams = cloudwatchlogs$describe_log_streams(
        logGroupName=log_group,
        logStreamNamePrefix=paste0(job_name, "/"),
        orderBy="LogStreamName",
        limit=50)
      }, error = function(e){
        # On the very first training job run on an account, there's no log group until
        # the container starts logging, so ignore any errors thrown about that
        error_code = paws_error_code(e)
        if (!identical(error_code, "ResourceNotFoundException"))
          stop(e)
      })
      stream_names = c(stream_names, lapply(streams$logStreams, function(s) s$logStreamName))
    }
  }

  if (length(stream_names) > 0) {
    events = multi_stream_iter(cloudwatchlogs, log_group, stream_names, positions)
    for (e in seq_along(events)){
      logs = lapply(events[[e]], function(l) l$message)
      # break if nothing exists in list
      if(islistempty(logs)) break
      writeLines(sagemaker_colour_wrapper(logs))
      count = length(events[[e]])
      if(events[[e]][[count]]$timestamp == sm_env$positions[[e]]$timestamp){
        sm_env$positions[[e]]$timestamp = events[[e]][[count]]$timestamp
        sm_env$positions[[e]]$skip = count + 1
      } else {
        sm_env$positions[[e]]$timestamp = events[[e]][[count]]$timestamp
        sm_env$positions[[e]]$skip = 1
      }
    }
  } else{
    writeLines(".", sep = "")
    flush(stdout())
  }
}
