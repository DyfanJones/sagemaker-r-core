# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/jumpstart/cache.py

#' @include r_utils.R

#' @import R6

import boto3
import botocore
from packaging.version import Version
from packaging.specifiers import SpecifierSet
from sagemaker.jumpstart.constants import (
  JUMPSTART_DEFAULT_MANIFEST_FILE_S3_KEY,
  JUMPSTART_DEFAULT_REGION_NAME,
)
from sagemaker.jumpstart.parameters import (
  JUMPSTART_DEFAULT_MAX_S3_CACHE_ITEMS,
  JUMPSTART_DEFAULT_MAX_SEMANTIC_VERSION_CACHE_ITEMS,
  JUMPSTART_DEFAULT_S3_CACHE_EXPIRATION_HORIZON,
  JUMPSTART_DEFAULT_SEMANTIC_VERSION_CACHE_EXPIRATION_HORIZON,
)
from sagemaker.jumpstart.types import (
  JumpStartCachedS3ContentKey,
  JumpStartCachedS3ContentValue,
  JumpStartModelHeader,
  JumpStartModelSpecs,
  JumpStartS3FileType,
  JumpStartVersionedModelId,
)
from sagemaker.jumpstart import utils
from sagemaker.utilities.cache import LRUCache


JumpStartModelsCache = R6Class("JumpStartModelsCache",
  public = list(

    initialize = function(
      region: str = JUMPSTART_DEFAULT_REGION_NAME,
      max_s3_cache_items: int = JUMPSTART_DEFAULT_MAX_S3_CACHE_ITEMS,
      s3_cache_expiration_horizon: datetime.timedelta =
        JUMPSTART_DEFAULT_S3_CACHE_EXPIRATION_HORIZON,
      max_semantic_version_cache_items: int =
        JUMPSTART_DEFAULT_MAX_SEMANTIC_VERSION_CACHE_ITEMS,
      semantic_version_cache_expiration_horizon: datetime.timedelta =
        JUMPSTART_DEFAULT_SEMANTIC_VERSION_CACHE_EXPIRATION_HORIZON,
      manifest_file_s3_key: str =
        JUMPSTART_DEFAULT_MANIFEST_FILE_S3_KEY,
      s3_bucket_name=NULL){

    }
  )
)
