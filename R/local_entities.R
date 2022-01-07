# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/local/entities.py

#' @include local_data.R
#' @include local_image.R
#' @include local_utils.R
#' @include local_session.R
#' @include r_utils.R

#' @import fs
#' @import jsonlite
#' @importFrom httr content handle GET
#' @import R6
#' @import sagemaker.common

.UNUSED_ARN = "local:arn-does-not-matter"
HEALTH_CHECK_TIMEOUT_LIMIT = 120

# Defines and starts a local processing job.
.LocalProcessingJob = R6Class(".LocalProcessingJob",
  public = list(

    # Creates a local processing job.
    # Args:
    #   container: the local container object.
    initialize = function(container){
      self$container = container
      self$state = "Created"
      self$start_time = NULL
      self$end_time = NULL
      self$processing_job_name = ""
      self$processing_inputs = NULL
      self$processing_output_config = NULL
      self$environment = NULL
    },

    # Starts a local processing job.
    # Args:
    #   processing_inputs: The processing input configuration.
    # processing_output_config: The processing input configuration.
    # environment: The collection of environment variables passed to the job.
    # processing_job_name: The processing job name.
    start = function(processing_inputs,
                     processing_output_config,
                     environment,
                     processing_job_name){
      self$state = private$.STARTING

      for (item in processing_inputs){
        if ("DatasetDefinition" %in% names(item))
          RuntimeError$new("DatasetDefinition is not currently supported in Local Mode")

        s3_input = item[["S3Input"]] %||% ValueError$new("Processing input must have a valid ['S3Input']")

        item[["DataUri"]] = s3_input[["S3Uri"]]

        if ("S3InputMode" %in% names(s3_input) && s3_input[["S3InputMode"]] != "File")
          RuntimeError$new(sprintf(
            "S3InputMode: %s is not currently supported in Local Mode",
            s3_input[["S3InputMode"]])
          )

        if ("S3DataDistributionType" %in% names(s3_input)
            && s3_input[["S3DataDistributionType"]] != "FullyReplicated")
          RuntimeError$new(sprintf(
            "DataDistribution: %s is not currently supported in Local Mode",
            s3_input[["S3DataDistributionType"]])
          )

        if ("S3CompressionType" %in% names(s3_input)
            && s3_input[["S3CompressionType"]] != "None")
          RuntimeError$new(sprintf(
            "CompressionType: %s is not currently supported in Local Mode",
            s3_input[["S3CompressionType"]])
          )
      }

      if (!missing(processing_output_config) && "Outputs" %in% names(processing_output_config)){
        processing_outputs = processing_output_config[["Outputs"]]

        for (item in processing_outputs){
          if ("FeatureStoreOutput" %in% names(item))
            RuntimeError$new(
              "FeatureStoreOutput is not currently supported in Local Mode")

          s3_input = item[["S3Input"]] %||% ValueError$new("Processing output must have a valid ['S3Input']")

          if (s3_output[["S3UploadMode"]] != "EndOfJob")
            RuntimeError$new(sprintf(
              "UploadMode: %s is not currently supported in Local Mode.",
              s3_output[["S3UploadMode"]])
            )
        }
      }

      self$start_time = Sys.time()
      self$state = private$.PROCESSING

      self$processing_job_name = processing_job_name
      self$processing_inputs = processing_inputs
      self$processing_output_config = processing_output_config
      self$environment = environment

      self$container$process(
        processing_inputs, processing_output_config, environment, processing_job_name
      )

      self$end_time = Sys.time()
      self$state = private$.COMPLETED
    },

    # Describes a local processing job.
    # Returns:
    #   An object describing the processing job.
    describe = function(){
      response = list(
        "ProcessingJobArn"=self$processing_job_name,
        "ProcessingJobName"=self$processing_job_name,
        "AppSpecification"=list(
          "ImageUri"=self$container$image,
          "ContainerEntrypoint"=self$container$container_entrypoint,
          "ContainerArguments"=self$container$container_arguments),
        "Environment"=self$environment,
        "ProcessingInputs"=self$processing_inputs,
        "ProcessingOutputConfig"=self$processing_output_config,
        "ProcessingResources"=list(
          "ClusterConfig"=list(
            "InstanceCount"=self$container$instance_count,
            "InstanceType"=self$container$instance_type,
            "VolumeSizeInGB"=30,
            "VolumeKmsKeyId"=NULL)
        ),
        "RoleArn"="<no_role>",
        "StoppingCondition"=list("MaxRuntimeInSeconds"=86400),
        "ProcessingJobStatus"=self$state,
        "ProcessingStartTime"=self$start_time,
        "ProcessingEndTime"=self$end_time
      )

      return(response)
    }
  ),
  private = list(
    .STARTING = "Starting",
    .PROCESSING = "Processing",
    .COMPLETED = "Completed"
  ),
  lock_objects=F
)


.LocalTrainingJob = R6Class(".LocalTrainingJob",
  public = list(

    initialize = function(container){
      self$container = container
      self$model_artifacts = NULL
      self$state = "created"
      self$start_time = NULL
      self$end_time = NULL
    },

    start = function(input_data_config,
                     output_data_config,
                     hyperparameters,
                     job_name){
      for (channel in input_data_config){
        if (!islistempty(channel[["DataSource"]])
            && "S3DataSource" %in% names(channel[["DataSource"]])){
          data_distribution = channel[["DataSource"]][["S3DataSource"]][["S3DataDistributionType"]]
          data_uri = channel[["DataSource"]][["S3DataSource"]][["S3Uri"]]
        } else if (!islistempty(channel[["DataSource"]])
                   && "FileDataSource" %in% names(channel[["DataSource"]])){
          data_distribution = channel[["DataSource"]][["FileDataSource"]][[
            "FileDataDistributionType"]]
          data_uri = channel[["DataSource"]][["FileDataSource"]][["FileUri"]]
        } else {
          ValueError$new(
            "Need channel['DataSource'] to have ['S3DataSource'] or ['FileDataSource']"
          )
        }
        # use a single Data URI - this makes handling S3 and File Data easier down the stack
        channel["DataUri"] = data_uri

        if (data_distribution != "FullyReplicated")
          RuntimeError$new(sprintf(
            "DataDistribution: %s is not currently supported in Local Mode",
            data_distribution)
          )
      }
      self$start_time = Sys.time()
      self$state = private$.TRAINING

      self$model_artifacts = self$container$train(
        input_data_config, output_data_config, hyperparameters, job_name
      )
      self$end_time = Sys.time()
      self$state = private$.COMPLETED
    },

    describe = function(){
      response = list(
        "ResourceConfig"=list("InstanceCount"=self$container$instance_count),
        "TrainingJobStatus"=self$state,
        "TrainingStartTime"=self$start_time,
        "TrainingEndTime"=self$end_time,
        "ModelArtifacts"=list("S3ModelArtifacts"=self.model_artifacts)
      )
      return(response)
    }
  ),
  private = list(
    .STARTING = "Starting",
    .TRAINING = "Training",
    .COMPLETED = "Completed",
    .states = c("Starting", "Training", "Completed")
  ),
  lock_objects = F
)

.LocalTransformJob = R6Class(".LocalTransformJob",
  public = list(

    initialize = function(transform_job_name,
                          model_name,
                          local_session=NULL){
      self$local_session = local_session %||% LocalSession$new()
      local_client = self$local_session$sagemaker

      self$name = transform_job_name
      self$model_name = model_name


      # TODO - support SageMaker Models not just local models. This is not
      # ideal but it may be a good thing to do.
      self$primary_container = local_client$describe_model(model_name)[["PrimaryContainer"]]
      self$container = NULL
      self$start_time = NULL
      self$end_time = NULL
      self$batch_strategy = NULL
      self$transform_resources = NULL
      self$input_data = NULL
      self$output_data = NULL
      self$environment = {}
      self$state = private$.CREATING
    },

    # Start the Local Transform Job
    # Args:
    #   input_data (dict): Describes the dataset to be transformed and the
    # location where it is stored.
    # output_data (dict): Identifies the location where to save the
    # results from the transform job
    # transform_resources (dict): compute instances for the transform job.
    # Currently only supports local or local_gpu
    # **kwargs: additional arguments coming from the boto request object
    start = function(input_data,
                     output_data,
                     transform_resources,
                     ...){
      kwargs = list(...)
      self$transform_resources = transform_resources
      self$input_data = input_data
      self$output_data = output_data

      image = self$primary_container[["Image"]]
      instance_type = transform_resources[["InstanceType"]]
      instance_count = 1

      environment = private$.get_container_environment(...)

      # Start the container, pass the environment and wait for it to start up
      self$container = .SageMakerContainer$new(
        instance_type, instance_count, image, self$local_session
      )
      self$container$serve(self$primary_container[["ModelDataUrl"]], environment)

      serving_port = get_config_value("local.serving_port", self$local_session$config) %||% 8080
      .wait_for_serving_container(serving_port)

      # Get capabilities from Container if needed
      endpoint_url = sprintf("http://localhost:%s/execution-parameters", serving_port)
      ll = .perform_request(endpoint_url)
      if (ll$code == 200){
        execution_parameters = httr::content(ll$response, as = "text", type = "application/json")
        # MaxConcurrentTransforms is ignored because we currently only support 1
        for (setting in c("BatchStrategy", "MaxPayloadInMB")){
          if (!(setting %in% names(kwargs)) && setting %in% names(execution_parameters))
            kwargs[[setting]] = execution_parameters[[setting]]
        }
      }

      # Apply Defaults if none was provided
      kwargs = modifyList(kwargs, do.call(private$.get_required_defaults, kwargs))

      self$start_time = Sys.time()
      self$batch_strategy = kwargs[["BatchStrategy"]]
      self$environment = kwargs["Environment"]

      # run the batch inference requests
      do.call(private$.perform_batch_inference, list(input_data, output_data, kwargs))
      self$end_time = Sys.time()
      self$state = private$.COMPLETED
    },

    # Describe this _LocalTransformJob
    # The response is a JSON-like dictionary that follows the response of
    # the boto describe_transform_job() API.
    # Returns:
    #   dict: description of this _LocalTransformJob
    describe = function(){
      response = list(
        "TransformJobStatus"=self$state,
        "ModelName"=self$model_name,
        "TransformJobName"=self$name,
        "TransformJobArn"=.UNUSED_ARN,
        "TransformEndTime"=self$end_time,
        "CreationTime"=self$start_time,
        "TransformStartTime"=self$start_time,
        "Environment"=list(),
        "BatchStrategy"=self$batch_strategy
      )

      response[["TransformResources"]] = self.transform_resources
      response[["TransformOutput"]] = self.output_data
      response[["TransformInput"]] = self.input_data

      return(response)
    }
  ),
  private = list(
    .CREATING = "Creating",
    .COMPLETED = "Completed",

    # Get all the Environment variables that will be passed to the container.
    # Certain input fields such as BatchStrategy have different values for
    # the API vs the Environment variables, such as SingleRecord vs
    # SINGLE_RECORD. This method also handles this conversion.
    # Args:
    #   **kwargs: existing transform arguments
    # Returns:
    #   dict: All the environment variables that should be set in the
    # container
    .get_container_environment = function(...){
      kwargs=list(...)
      environment = list()
      environment = c(environment, self$primary_container[["Environment"]])
      environment[["SAGEMAKER_BATCH"]] = "True"
      if ("MaxPayloadInMB" %in% names(kwargs))
        environment[["SAGEMAKER_MAX_PAYLOAD_IN_MB"]] = as.character(kwargs[["MaxPayloadInMB"]])

      if ("BatchStrategy" %in% names(kwargs)){
        if (kwargs[["BatchStrategy"]] == "SingleRecord"){
          strategy_env_value = "SINGLE_RECORD"
        } else if (kwargs[["BatchStrategy"]] == "MultiRecord"){
          strategy_env_value = "MULTI_RECORD"
        } else {
          ValueError$new("Invalid BatchStrategy, must be 'SingleRecord' or 'MultiRecord'")}
        environment[["SAGEMAKER_BATCH_STRATEGY"]] = strategy_env_value
      }
      # we only do 1 max concurrent transform in Local Mode
      if ("MaxConcurrentTransforms" %in% names(kwargs)
          && as.integer(kwargs[["MaxConcurrentTransforms"]]) > 1){
        LOGGER$warn(paste(
          "Local Mode only supports 1 ConcurrentTransform. Setting MaxConcurrentTransforms",
          "to 1"))
      }
      environment[["SAGEMAKER_MAX_CONCURRENT_TRANSFORMS"]] = "1"

      # if there were environment variables passed to the Transformer we will pass them to the
      # container as well.
      if ("Environment" %in% names(kwargs))
        environment = modifyList(environment, kwargs[["Environment"]])
      return(environment)
    },

    # Return the default values.
    # The values might be anything that was not provided by either the user or the container
    # Args:
    #   **kwargs: current transform arguments
    # Returns:
    #   dict: key/values for the default parameters that are missing.
    .get_required_defaults = function(...){
      kwargs = list(...)
      defaults = list()
      if (!("BatchStrategy" %in% names(kwargs)))
        defaults[["BatchStrategy"]] = "MultiRecord"

      if (!("MaxPayloadInMB" %in% names(kwargs)))
        defaults[["MaxPayloadInMB"]] = 6

      return(defaults)
    },

    .get_working_directory = function(){
      # Root dir to use for intermediate data location. To make things simple we will write here
      # regardless of the final destination. At the end the files will either be moved or
      # uploaded to S3 and deleted.
      root_dir = get_config_value("local.container_root", self$local_session$config)
      if (!is.null(root_dir))
        root_dir = fs::path_abs(root_dir)

      working_dir = tempfile(tmpdir=root_dir)
      return(working_dir)
    },

    # Prepares the data for transformation.
    # Args:
    #   input_data: Input data source.
    # batch_strategy: Strategy for batch transformation to get.
    # Returns:
    #   A (data source, batch provider) pair.
    .prepare_data_transformation = function(input_data,
                                            batch_strategy){
      input_path = input_data[["DataSource"]][["S3DataSource"]][["S3Uri"]]
      data_source = get_data_source_instance(input_path, self$local_session)

      split_type = if ("SplitType" %in% names(input_data)) input_data[["SplitType"]] else NULL
      splitter = get_splitter_instance(split_type)

      batch_provider = get_batch_strategy_instance(batch_strategy, splitter)
      return(list(data_source, batch_provider))
    },

    # Perform batch inference on the given input data.
    # Transforms the input data to feed the serving container. It first gathers
    # the files from S3 or Local FileSystem. It then splits the files as required
    # (Line, RecordIO, None), and finally, it batch them according to the batch
    # strategy and limit the request size.
    # Args:
    #   input_data: Input data source.
    # output_data: Output data source.
    # **kwargs: Additional configuration arguments.
    .perform_batch_inference = function(input_data,
                                        output_data,
                                        ...){
      kwargs = list(...)
      batch_strategy = kwargs[["BatchStrategy"]]
      max_payload = as.integer(kwargs[["MaxPayloadInMB"]])
      ll = private$.prepare_data_transformation(input_data, batch_strategy)
      names(ll) = c("data_source", "batch_provider")
      # Output settings
      accept =  if ("Accept" %in% names(output_data)) output_data[["Accept"]] else NULL

      working_dir = private$.get_working_directory()
      dataset_dir = ll$data_source$get_root_dir()
      for (fn in data_source.get_file_list()){
        relative_path = dirname(fs::path_rel(fn, dataset_dir))
        filename = basename(fn)
        copy_directory_structure(working_dir, relative_path)
        destination_path = fs::path(working_dir, relative_path, paste0(filename, ".out"))

        con = file(destination_path, "wb")
        for(item in ll$batch_provider$pad(fn, max_payload)){
          # call the container and add the result to inference.
          response = self$local_session$sagemaker_runtime$invoke_endpoint(
            item, "", input_data[["ContentType"]], accept)

          response_body = response[["Body"]]
          writeBin(response_body, con)
          if ("AssembleWith" %in% names(output_data) && output_data[["AssembleWith"]] == "Line")
            writeBin("\n", con)
        }
        close(con)
      }

      move_to_destination(working_dir, output_data[["S3OutputPath"]], self$name, self$local_session)
      self$container$stop_serving()
    }
  ),
  lock_objects = F
)

.LocalModel = R6Class(".LocalModel",
  public = list(
    initialize = function(model_name,
                          primary_container){
      self$model_name = model_name
      self$primary_container = primary_container
      self$creation_time = Sys.time()
    },

    describe = function(){
      response = list(
        "ModelName"=self$model_name,
        "CreationTime"=self$creation_time,
        "ExecutionRoleArn"=.UNUSED_ARN,
        "ModelArn"=.UNUSED_ARN,
        "PrimaryContainer"=self$primary_container
        )
      return(response)
    }
  ),
  lock_objects = F
)


.LocalEndpointConfig = R6Class(".LocalEndpointConfig",
  public = list(
    initialize = function(config_name,
                          production_variants,
                          tags=NULL){
      self$name = config_name
      self$production_variants = production_variants
      self$tags = tags
      self$creation_time = Sys.time()
    },

    describe = function(){
      response = list(
        "EndpointConfigName"=self.name,
        "EndpointConfigArn"=.UNUSED_ARN,
        "Tags"=self$tags,
        "CreationTime"=self$creation_time,
        "ProductionVariants"=self$production_variants
      )
      return(response)
    }
  ),
  lock_objects=F
)


.LocalEndpoint = R6Class(".LocalEndpoint",
  public = list(

    initialize = function(endpoint_name,
                          endpoint_config_name,
                          tags=NULL,
                          local_session=NULL){
      self$local_session = local_session %||% LocalSession$new()
      local_client = self$local_session$sagemaker_client

      self$name = endpoint_name
      self$endpoint_config = local_client$describe_endpoint_config(endpoint_config_name)
      self$production_variant = self$endpoint_config[["ProductionVariants"]][[1]]
      self$tags = tags

      model_name = self$production_variant["ModelName"]
      self$primary_container = local_client$describe_model(model_name)[["PrimaryContainer"]]

      self$container = NULL
      self$create_time = NULL
      self$state = private$.CREATING
    },

    serve = function(){
      image = self$primary_container[["Image"]]
      instance_type = self$production_variant[["InstanceType"]]
      instance_count = self$production_variant[["InitialInstanceCount"]]

      accelerator_type = self$production_variant[["AcceleratorType"]]
      if (accelerator_type == "local_sagemaker_notebook")
        self$primary_container[["Environment"]][[
          "SAGEMAKER_INFERENCE_ACCELERATOR_PRESENT"
        ]] = "true"

      self$create_time = Sys.time()
      self$container = .SageMakerContainer$new(
        instance_type, instance_count, image, self$local_session
      )
      self$container$serve(
        self$primary_container[["ModelDataUrl"]], self$primary_container["Environment"]
      )

      serving_port = get_config_value("local.serving_port", self$local_session$config) %||% 8080
      .wait_for_serving_container(serving_port)
      # the container is running and it passed the healthcheck status is now InService
      self$state = private$.IN_SERVICE
    },

    stop = function(){
      if (!is.null(self$container))
        self$container$stop_serving()
    },

    describe = function(){
      response = list(
        "EndpointConfigName"=self$endpoint_config[["EndpointConfigName"]],
        "CreationTime"=self$create_time,
        "ProductionVariants"=self$endpoint_config[["ProductionVariants"]],
        "Tags"=self$tags,
        "EndpointName"=self$name,
        "EndpointArn"=.UNUSED_ARN,
        "EndpointStatus"=self$state
      )
      return(response)
    }
  ),
  private = list(
    .CREATING = "Creating",
    .IN_SERVICE = "InService",
    .FAILED = "Failed"
  ),
  lock_objects=F
)

.wait_for_serving_container = function(serving_port){
  i = 0
  endpoint_url = sprintf("http://localhost:%s/ping",serving_port)
  handler = httr::handle(endpoint_url)
  while(TRUE){
    i = i+5
    if (i >= HEALTH_CHECK_TIMEOUT_LIMIT)
      RuntimeError$new("Giving up, endpoint didn't launch correctly")
    LOGGER$info("Checking if serving container is up, attempt: %s", i)
    ll = .perform_request(endpoint_url, handler)
    if(ll$code!=200){
      LOGGER$info("Container still not up, got: %s", ll$code)
    } else {
      return(NULL)
    }
    Sys.sleep(5)
  }
}

.perform_request = function(endpoint_url, hander=NULL){
  handler = handler %||% httr::handle(endpoint_url)
  tryCatch({
    r = httr::GET(handle = handler)
    list(response = r, code = r$status)
  },
  error = function(e){
    list(response=NULL, code=-1)
  })
}
