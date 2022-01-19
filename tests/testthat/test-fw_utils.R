# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/tests/unit/test_fw_utils.py

library(lgr)

LOGGER = lgr::get_logger("sagemaker")

test_that("test framework name_from_image_tf_scriptmode", {
  image_uri = "123.dkr.ecr.us-west-2.amazonaws.com/sagemaker-tensorflow-scriptmode:1.12-cpu-py3"
  test_fw1 = framework_name_from_image(image_uri)
  exp_fw1 = list("tensorflow", "py3", "1.12-cpu-py3", "scriptmode")

  image_uri = "123.dkr.ecr.us-west-2.amazonaws.com/tensorflow-training:1.13-cpu-py3"
  test_fw2 = framework_name_from_image(image_uri)
  exp_fw2 = list("tensorflow", "py3", "1.13-cpu-py3", "training")

  expect_equal(test_fw1, exp_fw1)
  expect_equal(test_fw2, exp_fw2)
})

test_that("test framework_name_from_image rl", {
  image_uri = "123.dkr.ecr.us-west-2.amazonaws.com/sagemaker-rl-mxnet:toolkit1.1-gpu-py3"
  test_fw1 = framework_name_from_image(image_uri)
  exp_fw1 = list("mxnet", "py3", "toolkit1.1-gpu-py3", NULL)

  expect_equal(test_fw1, exp_fw1)
})

test_that("test framework_name_from_image python versions", {
  image_uri = "123.dkr.ecr.us-west-2.amazonaws.com/tensorflow-training:2.2-cpu-py37"
  test_fw1 = framework_name_from_image(image_uri)
  exp_fw1 = list("tensorflow", "py37", "2.2-cpu-py37", "training")

  image_uri = "123.dkr.ecr.us-west-2.amazonaws.com/tensorflow-training:1.15.2-cpu-py36"
  exp_fw2 = list("tensorflow", "py36", "1.15.2-cpu-py36", "training")
  test_fw2 = framework_name_from_image(image_uri)

  expect_equal(test_fw1, exp_fw1)
  expect_equal(test_fw2, exp_fw2)
})

test_that("test legacy name_from_framework_image", {
  image_uri = "123.dkr.ecr.us-west-2.amazonaws.com/sagemaker-mxnet-py3-gpu:2.5.6-gpu-py2"
  test_fw1 = framework_name_from_image(image_uri)
  exp_fw1 = list("mxnet", "py3", "2.5.6-gpu-py2", NULL)

  expect_equal(test_fw1, exp_fw1)
})

test_that("test legacy name_from_framework_image wrong python", {
  image_uri = "123.dkr.ecr.us-west-2.amazonaws.com/sagemaker-myown-py4-gpu:1"
  test_fw1 = framework_name_from_image(image_uri)
  exp_fw1 = list(NULL, NULL, NULL, NULL)

  expect_equal(test_fw1, exp_fw1)
})

test_that("test legacy name_from_framework_image wrong device", {
  image_uri = "123.dkr.ecr.us-west-2.amazonaws.com/sagemaker-myown-py4-gpu:1"
  test_fw1 = framework_name_from_image(image_uri)
  exp_fw1 = list(NULL, NULL, NULL, NULL)

  expect_equal(test_fw1, exp_fw1)
})

test_that("test framework version from tag", {
  tags = list(
    "1.5rc-keras-cpu-py2",
    "1.5rc-keras-gpu-py2",
    "1.5rc-keras-cpu-py3",
    "1.5rc-keras-gpu-py36",
    "1.5rc-keras-gpu-py37"
  )
  exp_tag = "1.5rc-keras"

  for(tag in tags){
    test_tag = framework_version_from_tag(tag)
    expect_equal(test_tag, exp_tag)
  }
})

test_that("test framework version from tag other", {
  test_tag = framework_version_from_tag("weird-tag-py2")
  exp_tag = NULL

  expect_equal(test_tag, exp_tag)
})

test_that("test model code key prefix with all values present", {
  key_prefix = model_code_key_prefix("prefix", "model_name", "image_uri")
  expect_equal(key_prefix, "prefix/model_name")
})

test_that("test model code key prefix with no prefix and all other values present", {
  key_prefix = model_code_key_prefix(NULL, "model_name", "image_uri")
  expect_equal(key_prefix, "model_name")
})


test_that("test model code key prefix with only image present", {
  key_prefix = model_code_key_prefix(NULL, NULL, "image_uri")
  expect_true(grepl("image_uri-[0-9-]",key_prefix))
})

test_that("test model code key prefix and image present", {
  key_prefix = model_code_key_prefix("prefix", NULL, "image_uri")
  expect_true(grepl("prefix/image_uri-[0-9-]",key_prefix))
})

test_that("test model code key prefix with prefix present and others none fail", {
  expect_error(model_code_key_prefix("prefix", NULL, NULL))
})

test_that("test model code key prefix with all none fail", {
  expect_error(model_code_key_prefix(NULL, NULL, NULL))
})

test_that("test region supports debugger feature returns true for supported regions", {
  expect_true(.region_supports_debugger("us-west-2"))
  expect_true(.region_supports_debugger("us-east-2"))
})

test_that("test region supports debugger feature returns false for unsupported regions", {
  expect_false(.region_supports_debugger("us-iso-east-1"))
})

test_that("test warn if parameter server with multi gpu", {
  instance_type = "ml.p2.8xlarge"
  distribution = list("parameter_server"=list("enabled"=TRUE))

  warn_if_parameter_server_with_multi_gpu(
    training_instance_type=instance_type, distribution=distribution
  )

  expect_equal(sagemaker.common:::PARAMETER_SERVER_MULTI_GPU_WARNING, LOGGER$last_event$msg)
})

test_that("test warn if parameter server with local multi gpu", {
  instance_type = "local_gpu"
  distribution = list("parameter_server"=list("enabled"=TRUE))

  warn_if_parameter_server_with_multi_gpu(
    training_instance_type=instance_type, distribution=distribution
  )
  expect_equal(sagemaker.common:::PARAMETER_SERVER_MULTI_GPU_WARNING, LOGGER$last_event$msg)
})

test_that("test validate version or image args not raises", {
  good_args = list(list("1.0", "py3", NULL), list(NULL, "py3", "my:uri"), list("1.0", NULL, "my:uri"))
  for (ll in good_args){expect_null(validate_version_or_image_args(ll[[1]], ll[[2]], ll[[3]]))}
})

test_that("test validate version or image args raises", {
  bad_args = list(list( NULL,  NULL,  NULL), list( NULL, "py3",  NULL), list("1.0",  NULL,  NULL))
  for (ll in bad_args){
    expect_error(
      validate_version_or_image_args(ll[[1]], ll[[2]], ll[[3]])
  )}
})

test_that("test validate smdistributed not raises", {
  smdataparallel_enabled = list("smdistributed"=list("dataparallel"=list("enabled"=TRUE)))
  smdataparallel_enabled_custom_mpi = list(
    "smdistributed"=list("dataparallel"=list("enabled"=TRUE, "custom_mpi_options"="--verbose"))
  )
  smdataparallel_disabled = list("smdistributed"=list("dataparallel"=list("enabled"=FALSE)))
  instance_types = sagemaker.common:::SM_DATAPARALLEL_SUPPORTED_INSTANCE_TYPES

  good_args =list(
    list(smdataparallel_enabled, "custom-container"),
    list(smdataparallel_enabled_custom_mpi, "custom-container"),
    list(smdataparallel_disabled, "custom-container")
  )
  frameworks = list("tensorflow", "pytorch")

  for(framework in frameworks){
    for(instance_type in instance_types){
      for (ll in good_args){
        expect_null(validate_smdistributed(
          instance_type=instance_type,
          framework_name=framework,
          framework_version=NULL,
          py_version=NULL,
          distribution=ll[[1]],
          image_uri=ll[[2]])
        )
      }
    }
  }
})

test_that("test validate smdistributed raises", {
  bad_args = list(
    list("smdistributed"="dummy"),
    list("smdistributed"=list("dummy")),
    list("smdistributed"=list("dummy"="val")),
    list("smdistributed"=list("dummy"=list("enabled"=TRUE)))
  )
  instance_types = sagemaker.common:::SM_DATAPARALLEL_SUPPORTED_INSTANCE_TYPES
  frameworks = list("tensorflow", "pytorch")

  for(framework in frameworks){
    for(distribution in bad_args){
      for(instance_type in instance_types){
        expect_error(validate_smdistributed(
          instance_type=instance_type,
          framework_name=framework,
          framework_version=NULL,
          py_version=NULL,
          distribution=distribution,
          image_uri="custom-container")
        )
      }
    }
  }
})

test_that("test validate smdataparallel args raises", {
  smdataparallel_enabled = list("smdistributed"=list("dataparallel"=list("enabled"=TRUE)))

  # Cases {PT|TF2}
  # 1. None instance type
  # 2. incorrect instance type
  # 3. incorrect python version
  # 4. incorrect framework version

  bad_args = list(
    list(NULL, "tensorflow", "2.3.1", "py3", smdataparallel_enabled),
    list("ml.p3.2xlarge", "tensorflow", "2.3.1", "py3", smdataparallel_enabled),
    list("ml.p3dn.24xlarge", "tensorflow", "2.3.1", "py2", smdataparallel_enabled),
    list("ml.p3.16xlarge", "tensorflow", "1.3.1", "py3", smdataparallel_enabled),
    list(NULL, "pytorch", "1.6.0", "py3", smdataparallel_enabled),
    list("ml.p3.2xlarge", "pytorch", "1.6.0", "py3", smdataparallel_enabled),
    list("ml.p3dn.24xlarge", "pytorch", "1.6.0", "py2", smdataparallel_enabled),
    list("ml.p3.16xlarge", "pytorch", "1.5.0", "py3", smdataparallel_enabled)
  )

  for (ll in bad_args){
    expect_error(
      sagemaker.common:::.validate_smdataparallel_args(
        ll[[1]], ll[[2]], ll[[3]], ll[[4]], ll[[5]])
    )
  }
})

test_that("test validate smdataparallel args not raises", {
  smdataparallel_enabled = list("smdistributed"=list("dataparallel"=list("enabled"=TRUE)))
  smdataparallel_enabled_custom_mpi = list(
    "smdistributed"=list("dataparallel"=list("enabled"=TRUE, "custom_mpi_options"="--verbose"))
  )
  smdataparallel_disabled = list("smdistributed"=list("dataparallel"=list("enabled"=FALSE)))

  # Cases {PT|TF2}
  # 1. SM Distributed dataparallel disabled
  # 2. SM Distributed dataparallel enabled with supported args

  good_args = list(
    list(NULL, NULL, NULL, NULL, smdataparallel_disabled),
    list("ml.p3.16xlarge", "tensorflow", "2.3.1", "py3", smdataparallel_enabled),
    list("ml.p3.16xlarge", "tensorflow", "2.3.2", "py3", smdataparallel_enabled),
    list("ml.p3.16xlarge", "tensorflow", "2.3", "py3", smdataparallel_enabled),
    list("ml.p3.16xlarge", "tensorflow", "2.4.1", "py3", smdataparallel_enabled),
    list("ml.p3.16xlarge", "tensorflow", "2.4", "py3", smdataparallel_enabled),
    list("ml.p3.16xlarge", "pytorch", "1.6.0", "py3", smdataparallel_enabled),
    list("ml.p3.16xlarge", "pytorch", "1.6", "py3", smdataparallel_enabled),
    list("ml.p3.16xlarge", "pytorch", "1.7.1", "py3", smdataparallel_enabled),
    list("ml.p3.16xlarge", "pytorch", "1.7", "py3", smdataparallel_enabled),
    list("ml.p3.16xlarge", "pytorch", "1.8.0", "py3", smdataparallel_enabled),
    list("ml.p3.16xlarge", "pytorch", "1.8.1", "py3", smdataparallel_enabled),
    list("ml.p3.16xlarge", "pytorch", "1.8", "py3", smdataparallel_enabled),
    list("ml.p3.16xlarge", "tensorflow", "2.4.1", "py3", smdataparallel_enabled_custom_mpi),
    list("ml.p3.16xlarge", "pytorch", "1.8.0", "py3", smdataparallel_enabled_custom_mpi)
  )

  for( ll in good_args){
    expect_null(sagemaker.common:::.validate_smdataparallel_args(
      ll[[1]], ll[[2]], ll[[3]], ll[[4]], ll[[5]])
    )
  }
})
