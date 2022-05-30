# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/jumpstart/constants.py

#' @include jumpstart_enums.R
#' @include jumpstart_types.R
#' @include r_utils.R

JUMPSTART_LAUNCHED_REGIONS = list(
  JumpStartLaunchedRegionInfo$new(
    region_name="us-west-2",
    content_bucket="jumpstart-cache-prod-us-west-2"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="us-east-1",
    content_bucket="jumpstart-cache-prod-us-east-1"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="us-east-2",
    content_bucket="jumpstart-cache-prod-us-east-2"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="eu-west-1",
    content_bucket="jumpstart-cache-prod-eu-west-1"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="eu-central-1",
    content_bucket="jumpstart-cache-prod-eu-central-1"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="eu-north-1",
    content_bucket="jumpstart-cache-prod-eu-north-1"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="me-south-1",
    content_bucket="jumpstart-cache-prod-me-south-1"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="ap-south-1",
    content_bucket="jumpstart-cache-prod-ap-south-1"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="eu-west-3",
    content_bucket="jumpstart-cache-prod-eu-west-3"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="af-south-1",
    content_bucket="jumpstart-cache-prod-af-south-1"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="sa-east-1",
    content_bucket="jumpstart-cache-prod-sa-east-1"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="ap-east-1",
    content_bucket="jumpstart-cache-prod-ap-east-1"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="ap-northeast-2",
    content_bucket="jumpstart-cache-prod-ap-northeast-2"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="eu-west-2",
    content_bucket="jumpstart-cache-prod-eu-west-2"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="eu-south-1",
    content_bucket="jumpstart-cache-prod-eu-south-1"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="ap-northeast-1",
    content_bucket="jumpstart-cache-prod-ap-northeast-1"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="us-west-1",
    content_bucket="jumpstart-cache-prod-us-west-1"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="ap-southeast-1",
    content_bucket="jumpstart-cache-prod-ap-southeast-1"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="ap-southeast-2",
    content_bucket="jumpstart-cache-prod-ap-southeast-2"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="ca-central-1",
    content_bucket="jumpstart-cache-prod-ca-central-1"
  ),
  JumpStartLaunchedRegionInfo$new(
    region_name="cn-north-1",
    content_bucket="jumpstart-cache-prod-cn-north-1"
  )
)

JUMPSTART_REGION_NAME_SET = lapply(
  JUMPSTART_LAUNCHED_REGIONS, function(region){
    region$region_name
})
JUMPSTART_REGION_NAME_TO_LAUNCHED_REGION_DICT = setNames(
  JUMPSTART_LAUNCHED_REGIONS,
  JUMPSTART_REGION_NAME_SET
)
JUMPSTART_BUCKET_NAME_SET = lapply(
  JUMPSTART_LAUNCHED_REGIONS, function(region){
    region$content_bucket
})

JUMPSTART_DEFAULT_REGION_NAME = function() tryCatch(get_region(NULL), error = function(e) "us-west-2")

JUMPSTART_DEFAULT_MANIFEST_FILE_S3_KEY = "models_manifest.json"

INFERENCE_ENTRY_POINT_SCRIPT_NAME = "inference.py"
TRAINING_ENTRY_POINT_SCRIPT_NAME = "transfer_learning.py"

SUPPORTED_JUMPSTART_SCOPES = as.list(JumpStartScriptScope)

ENV_VARIABLE_JUMPSTART_CONTENT_BUCKET_OVERRIDE = "AWS_JUMPSTART_CONTENT_BUCKET_OVERRIDE"

JUMPSTART_RESOURCE_BASE_NAME = "sagemaker-jumpstart"
