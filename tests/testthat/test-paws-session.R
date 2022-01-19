
test_that("test paws credentials without profile", {
  paws_sess = PawsSession$new(
    aws_access_key_id = "made-up-1",
    aws_secret_access_key = "made-up-2",
    aws_session_token = "made-up-3",
    region_name = "made-up-4",
    endpoint = "made-up-5"
  )

  expect_equal(paws_sess$aws_access_key_id, "made-up-1")
  expect_equal(paws_sess$aws_secret_access_key, "made-up-2")
  expect_equal(paws_sess$aws_session_token, "made-up-3")
  expect_equal(paws_sess$region_name, "made-up-4")
  expect_equal(paws_sess$profile_name, NULL)
  expect_equal(paws_sess$endpoint, "made-up-5")
  expect_equal(paws_sess$credentials, list(
    credentials = list(
      creds = list(
        access_key_id = "made-up-1",
        secret_access_key = "made-up-2",
        session_token = "made-up-3"
      )
    ),
    endpoint = "made-up-5",
    region = "made-up-4"
  ))

  # check if credentials are passed to paws sdk correctly
  s3 = paws_sess$client("s3")

  expect_equal(s3$.internal$config$credentials$creds$access_key_id[[1]], "made-up-1")
  expect_equal(s3$.internal$config$credentials$creds$secret_access_key[[1]], "made-up-2")
  expect_equal(s3$.internal$config$credentials$creds$session_token[[1]], "made-up-3")
  expect_equal(s3$.internal$config$region[[1]], "made-up-4")
  expect_equal(s3$.internal$config$endpoint[[1]], "made-up-5")

  # check if configuration of credentials is correctly modified
  s3 = paws_sess$client("s3", config = list(endpoint = "dummy"))

  expect_equal(s3$.internal$config$endpoint[[1]], "dummy")
})

test_that("test paws credentials with profile", {
  paws_sess = PawsSession$new(
    profile_name = "made-up-1",
    region_name = "made-up-2",
    endpoint = "made-up-3"
  )

  expect_equal(paws_sess$aws_access_key_id, NULL)
  expect_equal(paws_sess$aws_secret_access_key, NULL)
  expect_equal(paws_sess$aws_session_token, NULL)
  expect_equal(paws_sess$region_name, "made-up-2")
  expect_equal(paws_sess$profile_name, "made-up-1")
  expect_equal(paws_sess$endpoint, "made-up-3")
  expect_equal(paws_sess$credentials, list(
    credentials = list(
      profile = "made-up-1"
    ),
    endpoint = "made-up-3",
    region = "made-up-2"
  ))

  # check if credentials are passed to paws sdk correctly
  s3 = paws_sess$client("s3")

  expect_equal(s3$.internal$config$credentials$profile[[1]], "made-up-1")
  expect_equal(s3$.internal$config$region[[1]], "made-up-2")
  expect_equal(s3$.internal$config$endpoint[[1]], "made-up-3")
})

test_that("test if error is raise when unknown paws service is entered", {
  paws_sess = PawsSession$new(
    profile_name = "default",
    region_name = "eu-west-1"
  )

  expect_error(paws_sess$client("made-up"),"Unknown service:", class="UnknownServiceError")
})
