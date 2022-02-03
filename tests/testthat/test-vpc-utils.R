# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/tests/unit/test_vpc_utils.py

subnets = list("subnet")
security_groups = list("sg")
good_vpc_config = list(subnets, security_groups)
foo_vpc_config = list(subnets, security_groups, 1)

names(good_vpc_config) <- c(vpc_configuration_env$SUBNETS_KEY, vpc_configuration_env$SECURITY_GROUP_IDS_KEY)
names(foo_vpc_config) <- c(vpc_configuration_env$SUBNETS_KEY, vpc_configuration_env$SECURITY_GROUP_IDS_KEY, "foo")


test_that("test to list", {
  expect_null(vpc_to_list(NULL, NULL))
  expect_null(vpc_to_list(subnets, NULL))
  expect_null(vpc_to_list(NULL, security_groups))
  expect_equal(vpc_to_list(subnets, security_groups), list(
      Subnets=subnets,
      SecurityGroupIds=security_groups
    )
  )
})

test_that("test from list", {
  expect_equal(vpc_from_list(good_vpc_config), list(
      Subnets=subnets,
      SecurityGroupIds=security_groups
    )
  )
  expect_equal(vpc_from_list(foo_vpc_config), list(
      Subnets=subnets,
      SecurityGroupIds=security_groups
    )
  )
  expect_equal(vpc_from_list(NULL), list(
      Subnets=NULL,
      SecurityGroupIds=NULL
    )
  )
  expect_equal(vpc_from_list(NULL, do_sanitize = TRUE), list(
      Subnets=NULL,
      SecurityGroupIds=NULL
    )
  )

  expect_error(vpc_from_list(list()), class = "ValueError")
  expect_error(vpc_from_list(list(Subnets=subnets)), class = "ValueError")
  expect_error(vpc_from_list(list(SecurityGroupIds=security_groups)), class = "ValueError")
  expect_error(vpc_from_list(list(), do_sanitize = TRUE), class = "ValueError")
})

test_that("test sanitize", {
  expect_equal(vpc_sanitize(good_vpc_config), list(
      Subnets=subnets,
      SecurityGroupIds=security_groups
    )
  )
  expect_equal(vpc_sanitize(foo_vpc_config), list(
      Subnets=subnets,
      SecurityGroupIds=security_groups
    )
  )
  expect_null(vpc_sanitize(NULL))
  expect_error(vpc_sanitize(""), class = "ValueError")
  expect_error(vpc_sanitize(list()), class = "ValueError")

  expect_error(vpc_sanitize(list(Subnets=1)), class = "ValueError")
  expect_error(vpc_sanitize(list(Subnets=list())), class = "ValueError")
  expect_error(vpc_sanitize(list(SecurityGroupIds=1, Subnets = subnets)), class = "ValueError")
  expect_error(vpc_sanitize(list(SecurityGroupIds=list(), Subnets = subnets)), class = "ValueError")
})
