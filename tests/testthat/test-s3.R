# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/tests/unit/test_s3.py

BUCKET_NAME = "mybucket"
REGION = "us-west-2"
CURRENT_JOB_NAME = "currentjobname"
SOURCE_NAME = "source"
KMS_KEY = "kmskey"

# paws_mock = Mock$new("PawsSession", region_name = "us-east-1")
# paws_mock$call_args("client")
# sagemaker_sesion = Session$new(paws_session=paws_mock, sagemaker_client=Mock$new())
# s3_mock = Mock$new("s3")
# s3_mock$call_args("get_object", list(Body = charToRaw("dummy")))
# s3_mock$call_args("put_object")
# sagemaker_sesion$s3 = s3_mock


sagemaker_session = Mock$new()
sagemaker_session$.call_args("upload_data")
sagemaker_session$.call_args("download_data")


test_that("test upload", {
  desired_s3_uri = file.path("s3:/", BUCKET_NAME, CURRENT_JOB_NAME, SOURCE_NAME)
  S3Uploader$new()$upload(
    local_path="/path/to/app.jar",
    desired_s3_uri=desired_s3_uri,
    sagemaker_session=sagemaker_session
  )

  expect_equal(sagemaker_session$upload_data(..return_value = T), list(
      path = "/path/to/app.jar",
      bucket = BUCKET_NAME,
      key_prefix = file.path(CURRENT_JOB_NAME, SOURCE_NAME)
    )
  )
})

test_that("test upload with kms_key", {
  desired_s3_uri = file.path("s3:/", BUCKET_NAME, CURRENT_JOB_NAME, SOURCE_NAME)
  S3Uploader$new()$upload(
    local_path="/path/to/app.jar",
    desired_s3_uri=desired_s3_uri,
    kms_key=KMS_KEY,
    sagemaker_session=sagemaker_session
  )

  expect_equal(sagemaker_session$upload_data(..return_value = T), list(
      path = "/path/to/app.jar",
      bucket = BUCKET_NAME,
      key_prefix = file.path(CURRENT_JOB_NAME, SOURCE_NAME),
      SSEKMSKeyId = KMS_KEY,
      ServerSideEncryption = "aws:kms"
    )
  )
})

test_that("test upload with kms_key", {
  desired_s3_uri = file.path("s3:/", BUCKET_NAME, CURRENT_JOB_NAME, SOURCE_NAME)
  S3Uploader$new()$upload(
    local_path="/path/to/app.jar",
    desired_s3_uri=desired_s3_uri,
    kms_key=KMS_KEY,
    sagemaker_session=sagemaker_session
  )

  expect_equal(sagemaker_session$upload_data(..return_value = T), list(
    path = "/path/to/app.jar",
    bucket = BUCKET_NAME,
    key_prefix = file.path(CURRENT_JOB_NAME, SOURCE_NAME),
    SSEKMSKeyId = KMS_KEY,
    ServerSideEncryption = "aws:kms"
    )
  )
})

test_that("test download", {
  s3_uri = file.path("s3:/", BUCKET_NAME, CURRENT_JOB_NAME, SOURCE_NAME)
  S3Downloader$new()$download(
    s3_uri=s3_uri, local_path="/path/for/download/", sagemaker_session=sagemaker_session
  )

  expect_equal(sagemaker_session$download_data(..return_value = T), list(
    path = "/path/for/download/",
    bucket = BUCKET_NAME,
    key_prefix = file.path(CURRENT_JOB_NAME, SOURCE_NAME)
    )
  )
})

test_that("test download with kms key", {
  s3_uri = file.path("s3:/", BUCKET_NAME, CURRENT_JOB_NAME, SOURCE_NAME)
  S3Downloader$new()$download(
    s3_uri=s3_uri,
    local_path="/path/for/download/",
    kms_key=KMS_KEY,
    sagemaker_session=sagemaker_session
  )

  expect_equal(sagemaker_session$download_data(..return_value = T), list(
    path = "/path/for/download/",
    bucket = BUCKET_NAME,
    key_prefix = file.path(CURRENT_JOB_NAME, SOURCE_NAME),
    SSECustomerKey=KMS_KEY
    )
  )
})

test_that("test parse s3 url", {
  ll = parse_s3_url("s3://bucket/code_location")
  expect_equal(ll$bucket, "bucket")
  expect_equal(ll$key, "code_location")
})

test_that("test parse s3 url fail", {
  expect_error(parse_s3_url("t3://code_location"))
})

test_that("def test path join", {
  test_cases = list(
    list(expected="foo/bar", args=list("foo", "bar")),
    list(expected="foo/bar", args=list("foo/", "bar")),
    list(expected="foo/bar", args=list("/foo/", "bar")),
    list(expected="s3://foo/bar", args=list("s3://", "foo", "bar")),
    list(expected="s3://foo/bar", args=list("s3://", "/foo", "bar")),
    list(expected="s3://foo/bar", args=list("s3://foo", "bar"))
  )

  for (test in test_cases){
    expect_equal(test$expected, as.character(do.call(s3_path_join, test$args)))
  }
})
