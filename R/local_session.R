# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/local/local_session.py

#' @include r_utils.R
#' @include local_image.R
#' @include local_entities.R

#' @import R6
#' @import R6sagemaker.common
#' @import fs
#' @import httr

#' @title A SageMakerClient that implements the API calls locally.
#' @description Used for doing local training and hosting local endpoints. It still needs access to
#'              a boto client to interact with S3 but it won't perform any SageMaker call.
#'              Implements the methods with the same signature as the boto SageMakerClient.
#' @keywords internal
LocalSagemakerClient = R6Class("LocalSagemakerClient",
  public = list(

    #' @description Initialize a LocalSageMakerClient.
    #' @param sagemaker_session (sagemaker.session.Session): a session to use to read configurations
    #'              from, and use its boto client.
    initialize = function(sagemaker_session=NULL){
      self$sagemaker_session = sagemaker_session %||% LocalSession$new()
    },

    #' @description Creates a processing job in Local Mode
    #' @param ProcessingJobName (str): local processing job name.
    #' @param AppSpecification (dict): Identifies the container and application to run.
    #' @param ProcessingResources (dict): Identifies the resources to use for local processing.
    #' @param Environment (dict, optional): Describes the environment variables to pass
    #'              to the container. (Default value = None)
    #' @param ProcessingInputs (dict, optional): Describes the processing input data.
    #'              (Default value = None)
    #' @param ProcessingOutputConfig (dict, optional): Describes the processing output
    #'              configuration. (Default value = None)
    #' @param ... : Keyword arguments
    create_processing_job = function(ProcessingJobName,
                                     AppSpecification,
                                     ProcessingResources,
                                     Environment=NULL,
                                     ProcessingInputs=NULL,
                                     ProcessingOutputConfig=NULL,
                                     ...){
      kwargs=list(...)
      Environment = Environment %||% list()
      ProcessingInputs = ProcessingInputs %||% c()
      ProcessingOutputConfig = ProcessingOutputConfig %||% list()

      container_entrypoint = NULL
      if ("ContainerEntrypoint" %in% names(AppSpecification))
        container_entrypoint = AppSpecification[["ContainerEntrypoint"]]

      container_arguments = NULL
      if ("ContainerArguments" %in% names(AppSpecification))
        container_arguments = AppSpecification[["ContainerArguments"]]

      if ("ExperimentConfig" %in% names(kwargs))
        LOGGER$warn("Experiment configuration is not supported in local mode.")
      if ("NetworkConfig" %in% names(kwargs))
        LOGGER$warn("Network configuration is not supported in local mode.")
      if ("StoppingCondition" %in% names(kwargs))
        LOGGER$warn("Stopping condition is not supported in local mode.")

      container = .SageMakerContainer$new(
        ProcessingResources[["ClusterConfig"]][["InstanceType"]],
        ProcessingResources[["ClusterConfig"]][["InstanceCount"]],
        AppSpecification[["ImageUri"]],
        sagemaker_session=self$sagemaker_session,
        container_entrypoint=container_entrypoint,
        container_arguments=container_arguments
      )
      processing_job = .LocalProcessingJob$new(container)
      LOGGER$info("Starting processing job")
      processing_job$start(
        ProcessingInputs, ProcessingOutputConfig, Environment, ProcessingJobName
      )

      private$.processing_jobs[[ProcessingJobName]] = processing_job
    },

    #' @description Describes a local processing job.
    #' @param ProcessingJobName (str): Processing job name to describe.
    #' @return (dict) DescribeProcessingJob Response.
    describe_processing_job = function(ProcessingJobName){
      if (!(ProcessingJobName %in% names(private$.processing_jobs))){
        SagemakerError$new("Could not find local processing job")
      }
      return(private$.processing_jobs[[ProcessingJobName]]$describe())
    },

    #' @description Create a training job in Local Mode.
    #' @param TrainingJobName (str): local training job name.
    #' @param AlgorithmSpecification (dict): Identifies the training algorithm to use.
    #' @param InputDataConfig (dict, optional): Describes the training dataset and the location where
    #'              it is stored. (Default value = None)
    #' @param OutputDataConfig (dict): Identifies the location where you want to save the results of
    #'              model training.
    #' @param ResourceConfig (dict): Identifies the resources to use for local model training.
    #' @param HyperParameters (dict) [optional]: Specifies these algorithm-specific parameters to
    #'              influence the quality of the final model.
    #' @param ... : Variables added to hyperparameters
    create_training_job = function(TrainingJobName,
                                   AlgorithmSpecification,
                                   OutputDataConfig,
                                   ResourceConfig,
                                   InputDataConfig=NULL,
                                   ...){
      kwargs=list(...)
      InputDataConfig = InputDataConfig %||% list()
      container = .SageMakerContainer$new(
        ResourceConfig[["InstanceType"]],
        ResourceConfig[["InstanceCount"]],
        AlgorithmSpecification[["TrainingImage"]],
        sagemaker_session=self$sagemaker_session
      )
      training_job = .LocalTrainingJob$new(container)
      hyperparameters = kwargs[["HyperParameters"]] %||% list()
      LOGGER$info("Starting training job")
      training_job$start(InputDataConfig, OutputDataConfig, hyperparameters, TrainingJobName)

      private$.training_jobs[[TrainingJobName]] = training_job
    },

    #' @description Describe a local training job.
    #' @param TrainingJobName (str): Training job name to describe.
    #' @return (dict) DescribeTrainingJob Response.
    describe_training_job = function(TrainingJobName){
      if (!(TrainingJobName %in% names(private$.training_jobs))){
        Sagemaker$new("Could not find local training job")
      }
      return(private$.training_jobs[[TrainingJobName]]$describe())
    },

    #' @description Create the transform job.
    #' @param TransformJobName (str):
    #' @param ModelName (str):
    #' @param TransformInput (str):
    #' @param TransformOutput (str):
    #' @param TransformResources (str):
    #' @param ... (obj):
    create_transform_job = function(TransformJobName,
                                    ModelName,
                                    TransformInput,
                                    TransformOutput,
                                    TransformResources,
                                    ...){
      transform_job = .LocalTransformJob$new(TransformJobName, ModelName, self$sagemaker_session)
      private$.transform_jobs[[TransformJobName]] = transform_job
      transform_job$start(TransformInput, TransformOutput, TransformResources, ...)
    },

    #' @description Describe the transform job.
    #' @param TransformJobName (str):
    describe_transform_job = function(TransformJobName){
      if (!(TransformJobName %in% names(private$.transform_jobs)))
        SagemakerError$new("Could not find local transform job")
      return(private$.transform_jobs[[TransformJobName]]$describe())
    },

    #' @description Create a Local Model Object.
    #' @param ModelName (str): the Model Name
    #' @param PrimaryContainer (dict): a SageMaker primary container definition
    #' @param ... (obj):
    create_model = function(ModelName,
                            PrimaryContainer,
                            ...){  # pylint: disable=unused-argument
      private$.models[[ModelName]] = .LocalModel$new(ModelName, PrimaryContainer)
    },

    #' @description Describe the model.
    #' @param ModelName (str):
    describe_model = function(ModelName){
      if (!(ModelName %in% names(private$.models)))
        SagemakerError$new("Could not find local model")
      return(private$.models[[ModelName]]$describe())
    },

    #' @description Describe the endpoint configuration.
    #' @param EndpointConfigName (str):
    describe_endpoint_config = function(EndpointConfigName){
      if (!(EndpointConfigName %in% names(private$.endpoint_configs))){
        SagemakerError$new("Could not find local endpoint config")
      }
      return(private$.endpoint_configs[[EndpointConfigName]]$describe())
    },

    #' @description Create the endpoint configuration.
    #' @param EndpointConfigName (str):
    #' @param ProductionVariants (str):
    #' @param Tags : (Default value = NULL)
    create_endpoint_config = function(EndpointConfigName,
                                      ProductionVariants,
                                      Tags=NULL){
      private$.endpoint_configs[[EndpointConfigName]] = .LocalEndpointConfig$new(
        EndpointConfigName, ProductionVariants, Tags
      )
    },

    #' @description Describe the endpoint.
    #' @param EndpointName (str):
    describe_endpoint = function(EndpointName){
      if (!(EndpointName %in% names(private$.endpoints)))
        SagemakerError$new("Could not find local endpoint")
      return(private$.endpoints[[EndpointName]]$describe())
    },

    #' @description Create the endpoint.
    #' @param EndpointName (str):
    #' @param EndpointConfigName (str):
    #' @param Tags : (Default value = None)
    create_endpoint = function(EndpointName,
                               EndpointConfigName,
                               Tags=NULL){
      endpoint = .LocalEndpoint$new(EndpointName, EndpointConfigName, Tags, self$sagemaker_session)
      private$.endpoints[{EndpointName}] = endpoint
      endpoint$serve()
    },

    #' @description Update the endpoint.
    #' @param EndpointName (str):
    #' @param EndpointConfigName (str):
    update_endpoint = function(EndpointName,
                               EndpointConfigName){ # pylint: disable=unused-argument
      NotImplementedError$new("Update endpoint name is not supported in local session.")
    },

    #' @description Delete the endpoint.
    #' @param EndpointName (str):
    delete_endpoint = function(EndpointName){
      if (EndpointName %in% names(private$.endpoints))
        private$.endpoints[[EndpointName]]$stop()
    },

    #' @description Delete the endpoint configuration.
    #' @param EndpointConfigName (str):
    delete_endpoint_config = function(EndpointConfigName){
      if (EndpointConfigName %in% names(private$.endpoint_configs))
        private$.endpoint_configs[[EndpointConfigName]] = NULL
    },

    #' @description Delete the model.
    #' @param ModelName (str):
    delete_model = function(ModelName){
      if (ModelName %in% names(private$.models))
        private$.models[[ModelName]] = NULL
    }
  ),
  private = list(
    .processing_jobs = list(),
    .training_jobs = list(),
    .transform_jobs = list(),
    .models = list(),
    .endpoint_configs = list(),
    .endpoints = list()
  ),
  lock_objects=F
)

#' @title A SageMaker Runtime client that calls a local endpoint only.
#' @export
LocalSagemakerRuntimeClient = R6Class("LocalSagemakerRuntimeClient",
  public = list(

    #' @description Initializes a LocalSageMakerRuntimeClient.
    #' @param config (list): Optional configuration for this client. In particular only
    #'              the local port is read.
    initialize = function(config=NULL){
      # self.http = urllib3.PoolManager()
      self$serving_port = 8080
      self$config = config
      self$serving_port = get_config_value("local.serving_port", config) %||% 8080
    },

    #' @description Invoke the endpoint.
    #' @param Body : Input data for which you want the model to provide inference.
    #' @param EndpointName : The name of the endpoint that you specified when you
    #'              created the endpoint using the CreateEndpoint API.
    #' @param ContentType : The MIME type of the input data in the request body (Default value = None)
    #' @param Accept : The desired MIME type of the inference in the response (Default value = None)
    #' @param CustomAttributes : Provides additional information about a request for an inference
    #'              submitted to a model hosted at an Amazon SageMaker endpoint (Default value = None)
    #' @param TargetModel : The model to request for inference when invoking a multi-model endpoint
    #'              (Default value = None)
    #' @param TargetVariant : Specify the production variant to send the inference request to when
    #'              invoking an endpoint that is running two or more variants (Default value = None)
    #' @param InferenceId : If you provide a value, it is added to the captured data when you enable
    #'              data capture on the endpoint (Default value = None)
    #' @return object: Inference for the given input.
    invoke_endpoint = function(Body,
                               EndpointName,  # pylint: disable=unused-argument
                               ContentType=NULL,
                               Accept=NULL,
                               CustomAttributes=NULL,
                               TargetModel=NULL,
                               TargetVariant=NULL,
                               InferenceId=NULL){
      url = sprintf("http://localhost:%s/invocations", self$serving_port)
      # set up pool of handles
      h = handle(url)
      headers = list()

      headers[["Content-type"]] = ContentType
      headers[["Accept"]] = Accept
      headers[["X-Amzn-SageMaker-Custom-Attributes"]] = CustomAttributes
      headers[["X-Amzn-SageMaker-Target-Model"]] = TargetModel
      headers[["X-Amzn-SageMaker-Target-Variant"]] = TargetVariant
      headers[["X-Amzn-SageMaker-Inference-Id"]] = InferenceId
      headers = do.call(add_headers, headers)

      r = POST(handle = h, body = Body, config = headers)
      return(list("Body"=r, "ContentType"=Accept))
    }
  ),
  lock_objects=F
)

#' @title A SageMaker ``Session`` class for Local Mode.
#' @export
LocalSession = R6Class("LocalSession",
  inherit = R6sagemaker.common::Session,
  public = list(

    #' @description This class provides alternative Local Mode implementations for the functionality of
    #'              :class:`~sagemaker.session.Session`.
    #' @param paws_session (PawsSession): The underlying AWS credentails passed to paws SDK.
    #' @param s3_endpoint_url (str): Override the default endpoint URL for Amazon S3, if set
    #'              (default: None).
    #' @param disable_local_code (bool): Set ``True`` to override the default AWS configuration
    #'              chain to disable the ``local.local_code`` setting, which may not be supported for
    #'              some SDK features (default: False).
    initialize = function(paws_session=NULL,
                          s3_endpoint_url=NULL,
                          disable_local_code=FALSE){
      self$s3_endpoint_url = s3_endpoint_url
      # We use this local variable to avoid disrupting the __init__->_initialize API of the
      # parent class... But overwriting it after constructor won't do anything, so prefix _ to
      # discourage external use:
      private$.disable_local_code = disable_local_code
      super$initialize(paws_session)
      if(Sys.info()[["sysname"]] == "Windows")
        LOGGER$warn("Windows Support for Local Mode is Experimental")
    },

    #' @description A no-op method meant to override the sagemaker client.
    #' @param job_name (str):
    #' @param wait (boolean): (Default value = False)
    #' @param poll (int): (Default value = 5)
    #' @param log_type (str):
    logs_for_job = function(job_name,
                            wait=FALSE,
                            poll=5,
                            log_type="All"){
      return(invisible(NULL))
    }
  ),
  private = list(
    .disable_local_code = NULL,
    .initialize = function(paws_session,
                           sagemaker_client,
                           sagemaker_runtime_client,
                           ...){
      self$paws_session = if(inherits(paws_session, "PawsSession")) paws_session else PawsSession$new()

      if (is.null(self$paws_session$credentials$region))
        ValueError$new(
          "Must setup local AWS configuration with a region supported by SageMaker.")

      self$sagemaker = LocalSagemakerClient$new(self)
      self$sagemaker_runtime_client = LocalSagemakerRuntimeClient$new(self$config)
      self$local_mode = TRUE

      if (!is.null(self$s3_endpoint_url)){
        cred = self$paws_session$credentials
        cred$endpoint = self$s3_endpoint_url
        self$s3 = paws::s3(config = cred)
      }
      sagemaker_config_file = fs::path_join(c(fs::path_expand_r("~"), ".sagemaker", "config.yaml"))
      if (fs::file_exists(sagemaker_config_file)){
        read_yaml = pkg_method("read_yaml", "yaml")
        self$config = read_yaml(sagemaker_config_file)
        if(private$.disable_local_code && "local" %in% names(self$config))
          self$config[["local"]][["local_code"]] = FALSE
      }
    }
  ),
  lock_objects=F
)

#' @title Amazon SageMaker channel configuration for FILE data sources, used in local mode.
#' @keywords internal
file_input = R6Class("file_input",
  public = list(

    #' @field config
    #' Place holder
    config = NULL,

    #' @description Create a definition for input data used by an SageMaker training job in local mode.
    #' @param fileUri (str):
    #' @param content_type (str):
    initialize = function(fileUri,
                          content_type=NULL){
      self$config = list(
        "DataSource"=list(
          "FileDataSource"=list(
            "FileDataDistributionType"="FullyReplicated",
            "FileUri"=fileUri
          )
        )
      )

      if (!is.null(content_type))
        self$config[["ContentType"]] = content_type
    }
  )
)
