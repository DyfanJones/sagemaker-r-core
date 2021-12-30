# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/local/image.py

#' @include local_session.R
#' @include r_utils.R

#' @import processx
#' @importFrom urltools url_parse
#' @importFrom jsonlite write_json fromJSON toJSON
#' @import fs
#' @import paws
#' @import R6
#' @import sagemaker.common

CONTAINER_PREFIX = "algo"
DOCKER_COMPOSE_FILENAME = "docker-compose.yaml"
DOCKER_COMPOSE_HTTP_TIMEOUT_ENV = "COMPOSE_HTTP_TIMEOUT"
DOCKER_COMPOSE_HTTP_TIMEOUT = "120"

# Environment variables to be set during training
REGION_ENV_NAME = "AWS_REGION"
TRAINING_JOB_NAME_ENV_NAME = "TRAINING_JOB_NAME"
S3_ENDPOINT_URL_ENV_NAME = "S3_ENDPOINT_URL"

#' @title Handle the lifecycle and configuration of a local container execution.
#' @description This class is responsible for creating the directories and configuration
#'              files that the docker containers will use for either training or serving.
.SageMakerContainer = R6Class(".SageMakerContainer",
  public = list(

    #' @description Initialize a SageMakerContainer instance
    #'              It uses a :class:`sagemaker.session.Session` for general interaction
    #'              with user configuration such as getting the default sagemaker S3 bucket.
    #'              However this class does not call any of the SageMaker APIs.
    #' @param instance_type (str): The instance type to use. Either 'local' or
    #'              'local_gpu'
    #' @param instance_count (int): The number of instances to create.
    #' @param image (str): docker image to use.
    #' @param sagemaker_session (sagemaker.session.Session): a sagemaker session
    #'              to use when interacting with SageMaker.
    #' @param container_entrypoint (str): the container entrypoint to execute
    #' @param container_arguments (str): the container entrypoint arguments
    initialize = function(instance_type,
                          instance_count,
                          image,
                          sagemaker_session=NULL,
                          container_entrypoint=NULL,
                          container_arguments=NULL){

      # check if docker-compose is installed
      tryCatch({
        processx::run("docker-compose", "--version")
        },
        error = function(e) {
          ImportError$new(
            "'docker-compose' is not installed. ",
            "Local Mode features will not work without docker-compose. ",
            "For more information on how to install 'docker-compose', please, see ",
            "https://docs.docker.com/compose/install/"
          )
      })

      self$sagemaker_session = sagemaker_session %||% LocalSession$new()
      self$instance_type = instance_type
      self$instance_count = instance_count
      self$image = image
      self$container_entrypoint = container_entrypoint
      self$container_arguments = container_arguments
      # Since we are using a single docker network, Generate a random suffix to attach to the
      # container names. This way multiple jobs can run in parallel.
      suffix = paste0(sample(c(letters, 0:9), 5, TRUE), collapse = "")
      self$hosts = lapply(
        seq_len(self$instance_count+1),
        function(i) sprintf("%s-%s-%s", CONTAINER_PREFIX, i, suffix))
      self$container_root = NULL
      self$container = NULL
    },

    #' @description Run a processing job locally using docker-compose.
    #' @param processing_inputs (dict): The processing input specification.
    #' @param processing_output_config (dict): The processing output configuration specification.
    #' @param environment (dict): The environment collection for the processing job.
    #' @param processing_job_name (str): Name of the local processing job being run.
    process = function(processing_inputs,
                       processing_output_config,
                       environment,
                       processing_job_name){
      self$container_root = private$.create_tmp_folder()

      # A shared directory for all the containers;
      # it is only mounted if the processing script is Local.
      shared_dir = fs::path_join(c(self$container_root, "shared"))
      fs::dir_create(shared_dir)

      data_dir = private$.create_tmp_folder()
      volumes = private$.prepare_processing_volumes(
        data_dir, processing_inputs, processing_output_config
      )

      # Create the configuration files for each container that we will create.
      for (host in self$hosts){
        .create_processing_config_file_directories(self$container_root, host)
        self$write_processing_config_files(
          host, environment, processing_inputs, processing_output_config, processing_job_name
        )
      }

      private$.generate_compose_file(
        "process", additional_volumes=volumes, additional_env_vars=environment
      )
      compose_command = private$.compose()

      if (.ecr_login_if_needed(self$sagemaker_session$paws_session, self$image))
        .pull_image(self$image)

      process = processx::process$new(
        compose_command$command, compose_command$args, stdout="|", stderr="|")

      tryCatch({
        .stream_output(process)
      },
      error = function(e){
        # _stream_output() doesn't have the command line. We will handle the exception
        # which contains the exit code and append the command line to it.
        msg = sprintf("Failed to run: %s", compose_command)
        RuntimeError$new(msg)
      },
      finally = function(f){
        # Uploading processing outputs back to Amazon S3.
        private$.upload_processing_outputs(data_dir, processing_output_config)
        tryCatch({
          # Deleting temporary directories.
          dirs_to_delete = list(shared_dir, data_dir)
          private$.cleanup(dirs_to_delete)
        },
        error = function(e){
          NULL
        })
      })

      # Print our Job Complete line to have a similar experience to training on SageMaker where
      # you see this line at the end.
      writeLines("===== Job Complete =====")
    },

    #' @description Run a training job locally using docker-compose.
    #' @param input_data_config (dict): The Input Data Configuration, this contains data such as the
    #'              channels to be used for training.
    #' @param output_data_config : The configuration of the output data.
    #' @param hyperparameters (dict): The HyperParameters for the training job.
    #' @param job_name (str): Name of the local training job being run.
    #' @return (str): Location of the trained model.
    train = function(input_data_config,
                     output_data_config,
                     hyperparameters,
                     job_name){
      self$container_root = private$.create_tmp_folder()
      fs::dir_create(fs::path_join(c(self$container_root, "output")))
      # create output/data folder since sagemaker-containers 2.0 expects it
      fs::dir_create(fs::path_join(c(self$container_root, "output", "data")))
      # A shared directory for all the containers. It is only mounted if the training script is
      # Local.
      shared_dir = fs::path_join(c(self$container_root, "shared"))
      fs::dir_create(shared_dir)

      data_dir = private$.create_tmp_folder()
      volumes = private$.prepare_training_volumes(
        data_dir, input_data_config, output_data_config, hyperparameters
      )
      # If local, source directory needs to be updated to mounted /opt/ml/code path
      hyperparameters = private$.update_local_src_path(
        hyperparameters, key=model_parameters$DIR_PARAM_NAME
      )

      # Create the configuration files for each container that we will create
      # Each container will map the additional local volumes (if any).
      for (host in self$hosts){
        .create_config_file_directories(self$container_root, host)
        self$write_config_files(host, hyperparameters, input_data_config)
        fs::dir_copy(data_dir, fs::path_join(c(self$container_root, host, "input", "data")))
      }
      training_env_vars = list(
        REGION_ENV_NAME=self$sagemaker_session$paws_region_name,
        TRAINING_JOB_NAME_ENV_NAME=job_name
      )

      if (!is.null(self$sagemaker_session$s3_resource)){
        training_env_vars[[
          S3_ENDPOINT_URL_ENV_NAME
        ]] = sprintf("https://%s", regional_hostname("s3", self$sagemaker$paws_region_name))
      }
      compose_data = private$.generate_compose_file(
        "train", additional_volumes=volumes, additional_env_vars=training_env_vars
      )
      compose_command = private$.compose()

      if (.ecr_login_if_needed(self$sagemaker_session$paws_session, self$image))
        .pull_image(self$image)

      process = processx::process$new(
        compose_command$command, compose_command$args, stdout="|", stderr="|")

      tryCatch({
        .stream_output(process)
      },
      error = function(e){
        # _stream_output() doesn't have the command line. We will handle the exception
        # which contains the exit code and append the command line to it.
        msg = sprintf("Failed to run: %s, %s", compose_command, as.character(e))
        RuntimeError$new(msg)
      },
      finally = function(f){
        artifacts = self$retrieve_artifacts(compose_data, output_data_config, job_name)

        # free up the training data directory as it may contain
        # lots of data downloaded from S3. This doesn't delete any local
        # data that was just mounted to the container.
        dirs_to_delete = list(data_dir, shared_dir)
        private$.cleanup(dirs_to_delete)
      })

      # Print our Job Complete line to have a similar experience to training on SageMaker where
      # you see this line at the end.
      writeLines("===== Job Complete =====")
      return(artifacts)
    },

    #' @description Host a local endpoint using docker-compose.
    #' @param model_dir (str): pointing to a file or s3:// location.
    #' @param environment a dictionary of environment variables to be passed to the
    #'               hosting container.
    serve = function(model_dir,
                     environment){
      LOGGER$info("serving")

      self$container_root = private$.create_tmp_folder()
      LOGGER$info("creating hosting dir in %s", private$container_root)

      volumes = private$.prepare_serving_volumes(model_dir)

      # If the user script was passed as a file:// mount it to the container.
      if (toupper(model_parameters$DIR_PARAM_NAME) %in% names(environment))
        script_dir = environment[[toupper(model_parameters$DIR_PARAM_NAME)]]
      parsed_uri = url_parse(script_dir)
      if (parsed_uri$scheme == "file")
        volumes = c(volumes, .Volume$new(parsed_uri$path, "/opt/ml/code"))
      # Update path to mount location
      environment[[toupper(model_parameters$DIR_PARAM_NAME)]] = "/opt/ml/code"

      if (.ecr_login_if_needed(self$sagemaker_session$paws_session, self$image))
        .pull_image(self$image)

      private$.generate_compose_file(
        "serve", additional_env_vars=environment, additional_volumes=volumes
      )
      compose_command = private$.compose()

      self$container = .HostingContainer$new(compose_command)
      self$container$start()
    },

    #' @description Stop the serving container.
    #'              The serving container runs in async mode to allow the SDK to do other
    #'              tasks.
    stop_serving = function(){
      if (!is.null(self$container)){
        self$container$down()
        self$container$join()
        private$.cleanup()
      }
      # for serving we can delete everything in the container root.
      .delete_tree(self$container_root)
    },

    #' @description Get the model artifacts from all the container nodes.
    #'              Used after training completes to gather the data from all the
    #'              individual containers. As the official SageMaker Training Service, it
    #'              will override duplicate files if multiple containers have the same file
    #'              names.
    #' @param compose_data (list): Docker-Compose configuration in dictionary
    #'              format.
    #' @param output_data_config : The configuration of the output data.
    #' @param job_name : The name of the job.
    #' @return Local path to the collected model artifacts.
    retrieve_artifacts = function(compose_data,
                                   output_data_config,
                                   job_name){
      # We need a directory to store the artfiacts from all the nodes
      # and another one to contained the compressed final artifacts
      artifacts = fs::path(self$container_root, "artifacts")
      compressed_artifacts = fs::path(self$container_root, "compressed_artifacts")
      fs::dir_create(artifacts)

      model_artifacts = fs::path(artifacts, "model")
      output_artifacts = fs::path(artifacts, "output")

      artifact_dirs = c(model_artifacts, output_artifacts, compressed_artifacts)
      fs::dir_create(artifact_dirs)

      # Gather the artifacts from all nodes into artifacts/model and artifacts/output
      for (host in self$hosts){
        volumes = compose_data["services"][str(host)]["volumes"]
        for (volume in volumes){
          if (grepl(volume, "^[A-Za-z]:")){
            ll = split_str(volume.split, ":")
            names(ll) = c("unit", "host_dir", "container_dir")
            host_dir = paste0(unit, ":", host_dir)
          } else {
            ll = volume.split(":")
            names(ll) = c("host_dir", "container_dir")}
          if (container_dir == "/opt/ml/model"){
            recursive_copy(host_dir, model_artifacts)
          } else if (container_dir == "/opt/ml/output")
            recursive_copy(host_dir, output_artifacts)
        }
      }
      # Tar Artifacts -> model.tar.gz and output.tar.gz
      model_files = fs::dir_ls(model_artifacts)
      output_files = fs::dir_ls(output_artifacts)
      create_tar_file(
        model_files, fs::path(compressed_artifacts, "model.tar.gz")
      )
      create_tar_file(
        output_files, fs::path(compressed_artifacts, "output.tar.gz")
      )

      if (output_data_config["S3OutputPath"] == ""){
        output_data = sprintf("file://%s", compressed_artifacts)
      } else {
        # Now we just need to move the compressed artifacts to wherever they are required
        output_data = move_to_destination(
          compressed_artifacts,
          output_data_config[["S3OutputPath"]],
          job_name,
          self$sagemaker_session)
      }
      .delete_tree(model_artifacts)
      .delete_tree(output_artifacts)

      return(fs::path(output_data, "model.tar.gz"))
    },

    #' @description Write the config files for the processing containers.
    #'              This method writes the hyperparameters, resources and input data
    #'              configuration files.
    #' @param host (str): Host to write the configuration for
    #' @param environment (dict): Environment variable collection.
    #' @param processing_inputs (dict): Processing inputs.
    #' @param processing_output_config (dict): Processing output configuration.
    #' @param processing_job_name (str): Processing job name.
    write_processing_config_files = function(host,
                                             environment,
                                             processing_inputs,
                                             processing_output_config,
                                             processing_job_name){
      config_path = fs::path(self$container_root, host, "config")

      resource_config = list("current_host"=host, "hosts"=self$host)
      write_json(resource_config, fs::path(config_path, "resourceconfig.json"))

      processing_job_config = list(
        "ProcessingJobArn"=processing_job_name,
        "ProcessingJobName"=processing_job_name,
        "AppSpecification"=list(
          "ImageUri"=self$image,
          "ContainerEntrypoint"=self$container_entrypoint,
          "ContainerArguments"=self$container_arguments),
        "Environment"=environment,
        "ProcessingInputs"=processing_inputs,
        "ProcessingOutputConfig"=processing_output_config,
        "ProcessingResources"=list(
          "ClusterConfig"=list(
            "InstanceCount"=self$instance_count,
            "InstanceType"=self$instance_type,
            "VolumeSizeInGB"=30,
            "VolumeKmsKeyId"=NULL)
        ),
        "RoleArn"="<no_role>",
        "StoppingCondition"=list("MaxRuntimeInSeconds"=86400)
      )

      write_json(processing_job_config,
        fs::path(config_path, "processingjobconfig.json")
      )
    },

    #' @description Write the config files for the training containers.
    #'              This method writes the hyperparameters, resources and input data
    #'              configuration files.
    #' @param host (str): Host to write the configuration for
    #' @param hyperparameters (dict): Hyperparameters for training.
    #' @param input_data_config (dict): Training input channels to be used for
    #'              training.
    #' @return NULL
    write_config_files = function(host,
                                  hyperparameters,
                                  input_data_config){
      config_path = fs::path(self$container_root, host, "input", "config")

      resource_config = list("current_host"=host, "hosts"=self$hosts)

      json_input_data_config = list()
      for (config in input_data_config){
        channel_name = config[["ChannelName"]]
        json_input_data_config[[channel_name]] = list("TrainingInputMode"="File")
        if ("ContentType" %in% names(config))
          json_input_data_config[[channel_name]][["ContentType"]] = config[["ContentType"]]
      }
      write_json(hyperparameters, fs::path(config_path, "hyperparameters.json"))
      write_json(resource_config, fs::path(config_path, "resourceconfig.json"))
      write_json(json_input_data_config, fs::path(config_path, "inputdataconfig.json"))
    }
  ),
  private = list(

    # Prepares the training volumes based on input and output data configs.
    # Args:
    #   data_dir:
    #   input_data_config:
    #   output_data_config:
    #   hyperparameters:
    .prepare_training_volumes = function(data_dir,
                                         input_data_config,
                                         output_data_config,
                                         hyperparameters){
      shared_dir = fs::path(self$container_root, "shared")
      model_dir = fs::path(self$container_root, "model")
      volumes = list()

      volumes = c(volumes, .Volume$new(model_dir, "/opt/ml/model"))

      # Mount the metadata directory if present.
      # Only expected to be present on SM notebook instances.
      # This is used by some DeepEngine libraries
      metadata_dir = "/opt/ml/metadata"
      if (fs::is_dir(metadata_dir))
        volumes = c(volumes, .Volume$new(metadata_dir, metadata_dir))

      # Set up the channels for the containers. For local data we will
      # mount the local directory to the container. For S3 Data we will download the S3 data
      # first.
      for (channel in input_data_config){
        uri = channel[["DataUri"]]
        channel_name = channel[["ChannelName"]]
        channel_dir = fs::path(data_dir, channel_name)
        fs::dir_create(channel_dir)

        data_source = get_data_source_instance(uri, self$sagemaker_session)
        volumes = c(volumes, .Volume$new(data_source$get_root_dir(), channel=channel_name))
      }

      # If there is a training script directory and it is a local directory,
      # mount it to the container.
      if (model_parameters$DIR_PARAM_NAME %in% names(hyperparameters)){
        training_dir = hyperparameters[[model_parameters$DIR_PARAM_NAME]]
        parsed_uri = urltools::url_parse(training_dir)
        if (parsed_uri$scheme == "file"){
          volumes = c(volumes, .Volume$new(parsed_uri$path, "/opt/ml/code"))
          # Also mount a directory that all the containers can access.
          volumes = c(volumes, .Volume$new(shared_dir, "/opt/ml/shared"))
        }
      }

      parsed_uri = urltools::url_parse(output_data_config[["S3OutputPath"]])
      if (parsed_uri$scheme == "file"
          && model_parameters$SAGEMAKER_OUTPUT_LOCATION %in% names(hyperparameters)){
        intermediate_dir = fs::path(parsed_uri$path, "output", "intermediate")
        if (!fs::dir_exists(intermediate_dir))
          fs::dir_create(intermediate_dir)
        volumes = c(volumes, .Volume$new(intermediate_dir, "/opt/ml/output/intermediate"))
      }
      return(volumes)
    },

    # Prepares local container volumes for the processing job.
    # Args:
    #   data_dir: The local data directory.
    # processing_inputs: The configuration of processing inputs.
    # processing_output_config: The configuration of processing outputs.
    # Returns:
    #   The volumes configuration.
    .prepare_processing_volumes = function(data_dir,
                                           processing_inputs,
                                           processing_output_config){
      shared_dir = fs::path(self$container_root, "shared")
      volumes = list()

      # Set up the input/outputs for the container.

      for (item in processing_inputs){
        uri = item[["DataUri"]]
        input_container_dir = item[["S3Input"]][["LocalPath"]]

        data_source = get_data_source_instance(uri, self$sagemaker_session)
        volumes = c(volumes, .Volume$new(data_source$get_root_dir(), input_container_dir))
      }

      if (!missing(processing_output_config) && "Outputs" %in% names(processing_output_config)){
        for (item in processing_output_config[["Outputs"]]){
          output_name = item[["OutputName"]]
          output_container_dir = item[["S3Output"]][["LocalPath"]]

          output_dir = fs::path(data_dir, "output", output_name)
          fs::dir_create(output_dir)

          volumes = c(volumes, .Volume$new(output_dir, output_container_dir))
        }
      }
      volumes = c(volumes, .Volume$new(shared_dir, "/opt/ml/shared"))

      return(volumes)
    },

    # Uploads processing outputs to Amazon S3.
    # Args:
    #   data_dir: The local data directory.
    # processing_output_config: The processing output configuration.
    .upload_processing_outputs = function(data_dir,
                                          processing_output_config){
      if (!missing(processing_output_config) && "Outputs" %in% names(processing_output_config)){
        for (item in processing_output_config["Outputs"]){
          output_name = item[["OutputName"]]
          output_s3_uri = item[["S3Output"]][["S3Uri"]]
          output_dir = fs::path(data_dir, "output", output_name)

          move_to_destination(
            output_dir, output_s3_uri, "", self$sagemaker_session
          )
        }
      }
    },

    # Updates the local path of source code.
    # Args:
    #   params: Existing configuration parameters.
    # key: Lookup key for the path of the source code in the configuration parameters.
    # Returns:
    #   The updated parameters.
    .update_local_src_path = function(params, key){
      if (key %in% names(params)){
        src_dir = jsonlite::fromJSON(params[key])
        parsed_uri = urltools::url_parse(src_dir)
        if (parsed_uri$scheme == "file"){
          new_params = params
          new_params[[key]] = jsonlite::toJSON("/opt/ml/code", auto_unbox = T)
          return(new_params)
        }
      }
      return(params)
    },

    # Prepares the serving volumes.
    # Args:
    #   model_location: Location of the models.
    .prepare_serving_volumes = function(model_location){
      volumes = list()
      host = self$hosts[[1]]
      # Make the model available to the container. If this is a local file just mount it to
      # the container as a volume. If it is an S3 location, the DataSource will download it, we
      # just need to extract the tar file.
      host_dir = fs::path(self$container_root, host)
      fs::dir_create(host_dir)
      model_data_source = get_data_source_instance(
        model_location, self$sagemaker_session
      )
      for (filename in model_data_source$get_file_list()){
        if (is_tarfile(filename)){
          untar(tarfile = filename, exdir = model_data_source$get_root_dir())
        }
      }
      volumes = c(volumes, .Volume$new(model_data_source$get_root_dir(), "/opt/ml/model"))

      return(volumes)
    },

    # Writes a config file describing a training/hosting environment.
    # This method generates a docker compose configuration file, it has an
    # entry for each container that will be created (based on self.hosts). it
    # calls
    # :meth:~sagemaker.local_session.SageMakerContainer._create_docker_host to
    # generate the config for each individual container.
    # Args:
    #   command (str): either 'train' or 'serve'
    # additional_volumes (list): a list of volumes that will be mapped to
    # the containers
    # additional_env_vars (list): a dictionary with additional environment
    # variables to be passed on to the containers.
    # Returns: (dict) A dictionary representation of the configuration that was written.
    .generate_compose_file = function(command,
                                      additional_volumes=NULL,
                                      additional_env_vars=NULL){
      additional_volumes = additional_volumes %||% list()
      additional_env_vars = additional_env_vars %||% list()
      environment = list()
      optml_dirs = list()

      aws_creds = .aws_credentials(self$sagemaker_session)
      if (!is.null(aws_creds))
        environment = c(environment, aws_creds)

      additional_env_var_list = sprintf("%s=%s", names(env_list), unname(env_list))
      environment = c(environment, additional_env_var_list)

      if(!nzchar(Sys.getenv(DOCKER_COMPOSE_HTTP_TIMEOUT_ENV)))
        sys_set_env(DOCKER_COMPOSE_HTTP_TIMEOUT_ENV, DOCKER_COMPOSE_HTTP_TIMEOUT)

      if(commond == "train"){
        optml_dirs = list("output", "output/data", "input")
      } else if (command == "process"){
        optml_dirs = list("output", "config")
      }

      services = lapply(self$hosts, function(h){
        private$.create_docker_host(h, environment, optml_dirs, command, additional_volumes)
      })
      names(services) <- unname(self$hosts)

      content = list(
        # Use version 2.3 as a minimum so that we can specify the runtime
        "version"="2.3",
        "services"=services,
        "networks"=list("sagemaker-local"=list("name"="sagemaker-local")),
      )

      docker_compose_path = fs::path(self$container_root, DOCKER_COMPOSE_FILENAME)

      as.yaml = pkg_method("as.yaml", "yaml")
      yaml_content = as.yaml(content)
      masked_content = content
      for (k in seq_along(masked_content[["services"]])){
        masked_content[["services"]][[k]][["environment"]] = as.list(
          rep("[Masked]", length(masked_content[["services"]][[k]][["environment"]]))
        )
      }

      masked_content_for_logging = as.yaml(masked_content)
      LOGGER$info("docker compose file: \n%s", masked_content_for_logging)
      writeLines(yaml_content, docker_compose_path)
      return(content)
    },

    # Invokes the docker compose command.
    # Args:
    #   detached
    .compose = function(detached = FALSE){
      compose_cmd = "docker-compose"
      command = list(
        command = compose_cmd,
        args = c("-f",
        fs::path_join(c(self$container_root, DOCKER_COMPOSE_FILENAME)),
        "up",
        "--build",
        if (!detached) "--abort-on-container-exit" else "--detach")  # mutually exclusive
      )
      LOGGER$info("docker command: %s", paste(unlist(command), collapse=" "))
      return(command)
    },

    # Creates the docker host configuration.
    # Args:
    #   host:
    #   environment:
    #   optml_subdirs:
    #   command:
    #   volumes:
    .create_docker_host = function(host,
                                   environment,
                                   optml_subdirs,
                                   command,
                                   volumes){
      optml_volumes = private$.build_optml_volumes(host, optml_subdirs)
      optml_volumes = c(optml_volumes, volumes)

      container_name_prefix = paste0(sample(c(letters, 0:9), 10, TRUE), collapse = "")

      host_config = list(
        "image"=self$image,
        "container_name"=sprintf("%s-%s",container_name_prefix, host),
        "stdin_open"=TRUE,
        "tty"=TRUE,
        "volumes"= lapply(optml_volumes, function(v) v$map),
        "environment"=environment,
        "networks"=list("sagemaker-local"=list("aliases"=list(host)))
      )

      if(command != "process"){
        host_config[["command"]] = command
      } else {
        if(!is.null(self$container_entrypoint))
          host_config[["entrypoint"]] = self$container_entrypoint
        if(!is.null(self$container_arguments))
          host_config[["entrypoint"]] = c(host_config[["entrypoint"]], self$container_arguments)
      }
      # for GPU support pass in nvidia as the runtime, this is equivalent
      # to setting --runtime=nvidia in the docker commandline.
      if (self$instance_type == "local_gpu")
        host_config[["runtime"]] = "nvidia"

      if (command == "serve"){
        serving_port = (
          get_config_value(
            "local.serving_port", self$sagemaker_session$config) %||% 8080
        )
        host_config = modifyList(host_config, list("ports"= list(sprintf("%s:8080", serving_port))))
      }
      return(host_config)
    },

    .create_tmp_folder = function(){
      root_dir = get_config_value(
        "local.container_root", self$sagemaker_session$config
      )
      if (!is.null(root_dir))
        root_dir =  fs::path_abs(root_dir)

      working_dir = tempfile(tmpdir=root_dir)
      # Docker cannot mount Mac OS /var folder properly see
      # https://forums.docker.com/t/var-folders-isnt-mounted-properly/9600
      # Only apply this workaround if the user didn't provide an alternate storage root dir.
      if (is.null(root_dir) && Sys.info()[["sysname"]] == "Darwin")
        working_dir = sprintf("/private%s", working_dir)

      return(fs::path_abs(working_dir))
    },

    # Generate a list of :class:`~sagemaker.local_session.Volume`.
    # These are required for the container to start. It takes a folder with
    # the necessary files for training and creates a list of opt volumes
    # that the Container needs to start.
    # Args:
    #   host (str): container for which the volumes will be generated.
    # subdirs (list): list of subdirectories that will be mapped. For
    # example: ['input', 'output', 'model']
    # Returns: (list) List of :class:`~sagemaker.local_session.Volume`
    .build_optml_volumes = function(host, subdirs){
      volumes = lapply(subdirs, function(subdir){
        host_dir = fs::path(self$container_root, host, subdir)
        container_dir = sprintf("/opt/ml/%s",subdir)
        .Volume$new(host_dir, container_dir)
      })
      return(volumes)
    },

    # Cleans up directories and the like.
    # Args:
    #   dirs_to_delete:
    .cleanup = function(dirs_to_delete=NULL){
      if (!is.null(dirs_to_delete))
        lapply(dirs_to_delete, .delete_tree)

      # Free the container config files.
      for (host in self.hosts){
        container_config_path = fs::path(self$container_root, host)
        .delete_tree(container_config_path)
      }
    }
  ),
  lock_objects=F
)

# Note: unable to inherit from python class Thread.
# Not 100% sure how to create threaded hosting container
# possibly need to investigate future::makeClusterPSOCK/future::makeClusterMPI
.HostingContainer = R6Class(".HostingContainer",
  public = list(
    # Creates a new threaded hosting container.
    # Args:
    #   command:
    initialize = function(command){
      self$command = command$command
      self$command_args = command$args
      self$process = NULL
    },

    run = function(){
      self$process = processx::process$new(
        self$command, self$command_args, stdout="|", stderr="|")

      tryCatch({
        .stream_output(self$process)
      },
      error = function(e){
        msg = sprintf("Failed to run: %s, %s", paste(self$command,self$command_args, collapse=" "),
                      as.character(e))
        RuntimeError$new(msg)
      })
    },

    down = function(){
      self$process$kill()
    }
  ),
  lock_objects=F
)

# Represent a Volume that will be mapped to a container.
.Volume = R6Class(".Volume",
  public = list(

    # Create a Volume instance.
    # The container path can be provided as a container_dir or as a channel name but not both.
    # Args:
    #   host_dir (str): path to the volume data in the host
    # container_dir (str): path inside the container that host_dir will be mapped to
    # channel (str): channel name that the host_dir represents. It will be mapped as
    # /opt/ml/input/data/<channel> in the container.
    initialize = function(host_dir,
                          container_dir=NULL,
                          channel=NULL){
      if (is.null(container_dir) && is.null(channel))
        ValueError$new("Either container_dir or channel must be declared.")

      if (!is.null(container_dir) && !is.null(channel))
        ValueError$new("container_dir and channel cannot be declared together.")

      self$container_dir = (
        if (!is.null(container_dir)) container_dir else paste0("/opt/ml/input/data/", channel)
      )
      self$host_dir = host_dir
      if (Sys.info()[["sysname"]] == "Darwin" && startsWith(host_dir, "/var"))
        self$host_dir = fs::path("/private", host_dir)
    }
  ),
  lock_objects=F
)

# Stream the output of a process to stdout
# This function takes an existing process that will be polled for output.
# Only stdout will be polled and sent to sys.stdout.
# Args:
#   process (subprocess.Popen): a process that has been started with
# stdout=PIPE and stderr=STDOUT
# Returns (int): process exit code
.stream_output = function(pr){
  exit_code = NULL
  while (is.null(exit_code)){
    stdout = pr$read_output_lines()
    writeLines(stdout)
    exit_code = pr$get_exit_status()
  }
  if (exit_code != 0)
    RuntimeError$new(sprintf("Process exited with code: %s", exit_code))
  return(exit_code)
}

# Makes a call to `subprocess.check_output` for the given command and args.
# Args:
#   cmd:
#   *popenargs:
#   **kwargs:
.check_output = function(cmd, args){
  success = TRUE
  output=processx::run(cmd, split_str(args, " "))

  if(output$status != 0) success = FALSE
  if (!success){
    LOGGER$error("Command output: %s", output$stderr)
    SagemakerError$new(sprintf("Failed to run %s", paste(cmd, paste(args, collapse=" "))))
  }

  return(output$stdout)
}

# Creates the directory for the processing config files.
# Args:
#   root: The root path.
# host: The current host.
.create_processing_config_file_directories=function(root,host){
  fs::dir_create(fs::path(root, host, "config"))
}

# Creates the directories for the config files.
.create_config_file_directories = function(root, host){
  for (d in c("input/config", "output", "model")){
    fs::dir_create(fs::path(root, host, d))
  }
}

# Makes a call to `fs::dir_delete` for the given path.
# Args:
#   path:
.delete_tree = function(path){
  tryCatch({
    fs::dir_delete(path)},
    error = function(e){
      LOGGER$error("Failed to delete %s", path)
      stop(e)
  })
}

# Provides the AWS credentials of the session as a paired list of strings.
# These can be used to set environment variables on command execution.
# Args:
#   session:
.aws_credentials = function(session){
  cred_provider = as.list(session$sagemaker$.internal$config$credentials)
  get_credentials <- pkg_method("get_credentials", "paws.common")
  tryCatch({
    creds = get_credentials(cred_provider)$creds
    access_key = creds$access_key_id
    secret_key = creds$secret_access_key
    token = creds$session_token

    # The presence of a token indicates the credentials are short-lived and as such are risky
    # to be used as they might expire while running.
    # Long-lived credentials are available either through
    # 1. boto session
    # 2. EC2 Metadata Service (SageMaker Notebook instances or EC2 instances with roles
    #       attached them)
    # Short-lived credentials available via boto session are permitted to support running on
    # machines with no EC2 Metadata Service but a warning is provided about their danger
    if(nzchar(token)){
      LOGGER$info("Using the long-lived AWS credentials found in session")
      return(list(
        sprintf("AWS_ACCESS_KEY_ID=%s", access_key),
        sprintf("AWS_SECRET_ACCESS_KEY=%s",secret_key))
      )
    }

    meta_creds <- .aws_credentials_available_in_metadata_service()
    if(!is.null(meta_creds)){
      LOGGER$warn(paste(
        "Using the short-lived AWS credentials found in session. They might expire while",
        "running.")
      )
      return(c(
        sprintf("AWS_ACCESS_KEY_ID=%s", meta_creds$AccessKeyId),
        sprintf("AWS_SECRET_ACCESS_KEY=%s", meta_creds$SecretAccessKey),
        sprintf("AWS_SESSION_TOKEN=%s", meta_creds$Token))
      )
    }
    LOGGER$info(paste(
      "No AWS credentials found in session but credentials from EC2 Metadata Service are",
      "available.")
    )
  }, error = function(e){
    LOGGER$info("Could not get AWS credentials: %s", e)
  })
  return(NULL)
}

.aws_credentials_available_in_metadata_service <- function(){
  get_container_credentials <- pkg_method("get_container_credentials", "paws.common")
  raw_to_utf8 <- pkg_method("raw_to_utf8", "paws.common")
  credentials <- get_container_credentials()
  if(is.null(credentials))
    return(NULL)
  credentials_response_body <- jsonlite::fromJSON(
    raw_to_utf8(get_container_credentials()$body))
  return(credentials_response_body)
}

# Log into ECR, if needed.
# Of note, only ECR images need login.
# Args:
#   boto_session:
#   image:
.ecr_login_if_needed = function(paws_sess=NULL, image=NULL){
  sm_regex = regexec(ECR_URI_PATTERN, image)
  sagemaker_match = unlist(regmatches(image, sm_regex))
  if (islistempty(sagemaker_match))
    return(FALSE)

  # do we have the image?
  if (length(.check_output("docker", sprintf("images -q %s", image))) == 0)
    return(FALSE)

  if (is.null(paws_sess))
    RuntimeError$new(
      "PawsSession is required to login to ECR. ",
      sprintf("Please pull the image: %s manually.", image)
    )

  ecr = paws_sess$client("ecr")
  auth = ecr$get_authorization_token(registryIds=list(split_str(image, "\\.")[1]))
  authorization_data = auth[["authorizationData"]][[1]]

  raw_token = processx::base64_decode(authorization_data[["authorizationToken"]])
  token = split_str(rawToChar(raw_token), "AWS:")
  ecr_url = auth[["authorizationData"]][[1]][["proxyEndpoint"]]

  cmd = sprintf("login -u AWS -p %s %s", token, ecr_url)
  processx::run("docker", split_str(cmd, " "))

  return(TRUE)
}

# Invokes the docker pull command for the given image.
# Args:
#   image:
.pull_image = function(image){
  pull_image_command = trimws(sprintf("pull %s", image))
  LOGGER$info("docker command: docker %s", pull_image_command)

  processx::run("docker", split_str(pull_image_command, " "))
  LOGGER$info("image pulled: %s", image)
}
