% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/image_uris.R
\name{ImageUris}
\alias{ImageUris}
\title{ImageUris Class}
\description{
Class to create and format sagemaker docker images stored in ECR
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ImageUris-retrieve}{\code{ImageUris$retrieve()}}
\item \href{#method-ImageUris-get_training_image_uri}{\code{ImageUris$get_training_image_uri()}}
\item \href{#method-ImageUris-format}{\code{ImageUris$format()}}
\item \href{#method-ImageUris-clone}{\code{ImageUris$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ImageUris-retrieve"></a>}}
\if{latex}{\out{\hypertarget{method-ImageUris-retrieve}{}}}
\subsection{Method \code{retrieve()}}{
Retrieves the ECR URI for the Docker image matching the given arguments of inbuilt AWS Sagemaker models.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ImageUris$retrieve(
  framework,
  region,
  version = NULL,
  py_version = NULL,
  instance_type = NULL,
  accelerator_type = NULL,
  image_scope = NULL,
  container_version = NULL,
  distribution = NULL,
  base_framework_version = NULL,
  training_compiler_config = NULL,
  model_id = NULL,
  model_version = NULL,
  tolerate_vulnerable_model = FALSE,
  tolerate_deprecated_model = FALSE,
  sdk_version = NULL,
  inference_tool = NULL,
  serverless_inference_config = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{framework}}{(str): The name of the framework or algorithm.}

\item{\code{region}}{(str): The AWS region.}

\item{\code{version}}{(str): The framework or algorithm version. This is required if there is
more than one supported version for the given framework or algorithm.}

\item{\code{py_version}}{(str): The Python version. This is required if there is
more than one supported Python version for the given framework version.}

\item{\code{instance_type}}{(str): The SageMaker instance type. For supported types, see
https://aws.amazon.com/sagemaker/pricing/instance-types. This is required if
there are different images for different processor types.}

\item{\code{accelerator_type}}{(str): Elastic Inference accelerator type. For more, see
https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html.}

\item{\code{image_scope}}{(str): The image type, i.e. what it is used for.
Valid values: "training", "inference", "eia". If ``accelerator_type`` is set,
``image_scope`` is ignored.}

\item{\code{container_version}}{(str): the version of docker image}

\item{\code{distribution}}{(dict): A dictionary with information on how to run distributed training
(default: None).}

\item{\code{base_framework_version}}{(str):}

\item{\code{training_compiler_config}}{(:class:`~sagemaker.training_compiler.TrainingCompilerConfig`):
A configuration class for the SageMaker Training Compiler
(default: None).}

\item{\code{model_id}}{(str): The JumpStart model ID for which to retrieve the image URI
(default: None).}

\item{\code{model_version}}{(str): The version of the JumpStart model for which to retrieve the
image URI (default: None).}

\item{\code{tolerate_vulnerable_model}}{(bool): ``True`` if vulnerable versions of model specifications
should be tolerated without an exception raised. If ``False``, raises an exception if
the script used by this version of the model has dependencies with known security
vulnerabilities. (Default: False).}

\item{\code{tolerate_deprecated_model}}{(bool): True if deprecated versions of model specifications
should be tolerated without an exception raised. If False, raises an exception
if the version of the model is deprecated. (Default: False).}

\item{\code{sdk_version}}{(str): the version of python-sdk that will be used in the image retrieval.
(default: None).}

\item{\code{inference_tool}}{(str): the tool that will be used to aid in the inference.
Valid values: "neuron, None" (default: None).}

\item{\code{serverless_inference_config}}{(\code{sagemaker.core::ServerlessInferenceConfig}):
Specifies configuration related to serverless endpoint. Instance type is
not provided in serverless inference. So this is used to determine processor type.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
str: the ECR URI for the corresponding SageMaker Docker image.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ImageUris-get_training_image_uri"></a>}}
\if{latex}{\out{\hypertarget{method-ImageUris-get_training_image_uri}{}}}
\subsection{Method \code{get_training_image_uri()}}{
Retrieves the image URI for training.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ImageUris$get_training_image_uri(
  region,
  framework,
  framework_version = NULL,
  py_version = NULL,
  image_uri = NULL,
  distribution = NULL,
  compiler_config = NULL,
  tensorflow_version = NULL,
  pytorch_version = NULL,
  instance_type = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{region}}{(str): The AWS region to use for image URI.}

\item{\code{framework}}{(str): The framework for which to retrieve an image URI.}

\item{\code{framework_version}}{(str): The framework version for which to retrieve an
image URI (default: NULL).}

\item{\code{py_version}}{(str): The python version to use for the image (default: NULL).}

\item{\code{image_uri}}{(str): If an image URI is supplied, it is returned (default: NULL).}

\item{\code{distribution}}{(dict): A dictionary with information on how to run distributed
training (default: NULL).}

\item{\code{compiler_config}}{(:class:`~sagemaker.training_compiler.TrainingCompilerConfig`):
A configuration class for the SageMaker Training Compiler
(default: NULL).}

\item{\code{tensorflow_version}}{(str): The version of TensorFlow to use. (default: NULL)}

\item{\code{pytorch_version}}{(str): The version of PyTorch to use. (default: NULL)}

\item{\code{instance_type}}{(str): The instance type to use. (default: NULL)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
str: The image URI string.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ImageUris-format"></a>}}
\if{latex}{\out{\hypertarget{method-ImageUris-format}{}}}
\subsection{Method \code{format()}}{
format class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ImageUris$format()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ImageUris-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ImageUris-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ImageUris$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
