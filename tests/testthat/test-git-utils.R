# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/tests/unit/test_git_utils.py

REPO_DIR = "/tmp/repo_dir"
PUBLIC_GIT_REPO = "https://github.com/aws/sagemaker-python-sdk.git"
PUBLIC_BRANCH = "test-branch-git-config"
PUBLIC_COMMIT = "ae15c9d7d5b97ea95ea451e4662ee43da3401d73"
PRIVATE_GIT_REPO_SSH = "git@github.com:testAccount/private-repo.git"
PRIVATE_GIT_REPO = "https://github.com/testAccount/private-repo.git"
PRIVATE_BRANCH = "test-branch"
PRIVATE_COMMIT = "329bfcf884482002c05ff7f44f62599ebc9f445a"
CODECOMMIT_REPO = "https://git-codecommit.us-west-2.amazonaws.com/v1/repos/test-repo/"
CODECOMMIT_REPO_SSH = "ssh://git-codecommit.us-west-2.amazonaws.com/v1/repos/test-repo/"
CODECOMMIT_BRANCH = "master"

test_that("test git clone repo succeed", {
  git_config = list("repo"=PUBLIC_GIT_REPO, "branch"=PUBLIC_BRANCH, "commit"=PUBLIC_COMMIT)
  entry_point = "entry_point"
  source_dir = "source_dir"
  dependencies = list("foo", "bar")

  with_mock(
    `fs::is_file` = mock_fun(TRUE),
    `fs::is_dir` = mock_fun(TRUE),
    `fs::file_temp` = mock_fun(REPO_DIR),
    `processx::run` = mock_fun(),
    {
      ret = sagemaker.common::git_clone_repo(git_config, entry_point, source_dir, dependencies)
      expect_equal(ret$entry_point, entry_point)
      expect_equal(ret$source_dir, "/tmp/repo_dir/source_dir")
      expect_equal(ret$dependencies, list("/tmp/repo_dir/foo", "/tmp/repo_dir/bar"))
    }
  )
})

test_that("test git clone repo repo not provided", {
  git_config = list("branch"=PUBLIC_BRANCH, "commit"=PUBLIC_COMMIT)
  entry_point = "entry_point_that_does_not_exist"
  source_dir = "source_dir"
  dependencies = list("foo", "bar")
  expect_error(
    git_clone_repo(git_config, entry_point, source_dir, dependencies),
    "Please provide a repo for git_config."
  )
})

test_that("test git clone repo git argument wrong format", {
  git_config = list(
    "repo"=PUBLIC_GIT_REPO,
    "branch"=PUBLIC_BRANCH,
    "commit"=PUBLIC_COMMIT,
    "token"=42
  )
  entry_point = "entry_point"
  source_dir = "source_dir"
  dependencies = list("foo", "bar")
  expect_error(
    git_clone_repo(git_config, entry_point, source_dir, dependencies),
    "'token' must be a string."
  )
})

test_that("test git clone repo branch not exist", {
  git_config = list("repo"=PUBLIC_GIT_REPO, "branch"="banana", "commit"=PUBLIC_COMMIT)
  entry_point = "entry_point"
  source_dir = "source_dir"
  dependencies = list("foo", "bar")

  with_mock(
    `processx::run` = mock_fun(side_effect = function(...) stop()),
    {
      expect_error(sagemaker.common::git_clone_repo(git_config, entry_point, source_dir, dependencies))
    }
  )
})

test_that("test git clone repo commit not exist", {
  git_config = list("repo"=PUBLIC_GIT_REPO, "branch"=PUBLIC_BRANCH, "commit"="banana")
  entry_point = "entry_point"
  source_dir = "source_dir"
  dependencies = list("foo", "bar")
  with_mock(
    `processx::run` = mock_fun(side_effect = function(...) stop()),
    {
      expect_error(sagemaker.common::git_clone_repo(git_config, entry_point, source_dir, dependencies))
    }
  )
})

test_that("test git clone repo entry point not exist", {
  git_config = list("repo"=PUBLIC_GIT_REPO, "branch"=PUBLIC_BRANCH, "commit"=PUBLIC_COMMIT)
  entry_point = "entry_point_that_does_not_exist"
  source_dir = "source_dir"
  dependencies = list("foo", "bar")

  with_mock(
    `fs::is_file` = mock_fun(FALSE),
    `fs::is_dir` = mock_fun(TRUE),
    `fs::dir_exists` = mock_fun(TRUE),
    `fs::file_temp` = mock_fun(REPO_DIR),
    `processx::run` = mock_fun(),
    {
      expect_error(
        sagemaker.common::git_clone_repo(git_config, entry_point, source_dir, dependencies),
        "Entry point does not exist in the repo."
      )
    }
  )
})

test_that("test git clone repo source dir not exist", {
  git_config = list("repo"=PUBLIC_GIT_REPO, "branch"=PUBLIC_BRANCH, "commit"=PUBLIC_COMMIT)
  entry_point = "entry_point"
  source_dir = "source_dir_that_does_not_exist"
  dependencies = list("foo", "bar")

  with_mock(
    `fs::is_file` = mock_fun(TRUE),
    `fs::is_dir` = mock_fun(FALSE),
    `fs::dir_exists` = mock_fun(TRUE),
    `fs::file_temp` = mock_fun(REPO_DIR),
    `processx::run` = mock_fun(),
    {
      expect_error(
        sagemaker.common::git_clone_repo(git_config, entry_point, source_dir, dependencies),
        "Source directory does not exist in the repo."
      )
    }
  )
})

test_that("test git clone repo dependencies not exist", {
  git_config = list("repo"=PUBLIC_GIT_REPO, "branch"=PUBLIC_BRANCH, "commit"=PUBLIC_COMMIT)
  entry_point = "entry_point"
  source_dir = "source_dir"
  dependencies = list("foo", "dep_that_does_not_exist")

  with_mock(
    `fs::is_file` = mock_fun(TRUE),
    `fs::is_dir` = mock_fun(TRUE),
    `fs::dir_exists` = mock_fun(side_effect = iter(TRUE, FALSE)),
    `fs::file_temp` = mock_fun(REPO_DIR),
    `processx::run` = mock_fun(),
    {
      expect_error(
        git_clone_repo(git_config, entry_point, source_dir, dependencies),
        "does not exist in the repo."
      )
    }
  )
})

test_that("test_git_clone_repo_with_username_password_no_2fa", {
  git_config = list(
    "repo"=PRIVATE_GIT_REPO,
    "branch"=PRIVATE_BRANCH,
    "commit"=PRIVATE_COMMIT,
    "username"="username",
    "password"="passw0rd!"
  )
  entry_point = "entry_point"
  source_dir = "source_dir"
  dependencies = list("foo", "dep_that_does_not_exist")

  env = c("current", "GIT_TERMINAL_PROMPT" = 0)
  mock_run = mock_fun()
  with_mock(
    `fs::is_file` = mock_fun(TRUE),
    `fs::is_dir` = mock_fun(TRUE),
    `fs::dir_exists` = mock_fun(side_effect = iter(TRUE, FALSE)),
    `fs::file_temp` = mock_fun(REPO_DIR),
    `processx::run` = mock_run,
    {
      ret = sagemaker.common::git_clone_repo(git_config=git_config, entry_point=entry_point)
      args = mock_run(..return_value_all = T)
      expect_equal(args[[1]], list(
        "git",
        args = c(
          "clone",
          "https://username:passw0rd%21@github.com/testAccount/private-repo.git",
          REPO_DIR
          ),
        env = env
      ))
      expect_equal(args[[2]], list("git", args = c("checkout", PRIVATE_BRANCH), wd = REPO_DIR))
      expect_equal(args[[3]], list("git", args = c("checkout", PRIVATE_COMMIT), wd = REPO_DIR))
      expect_equal(ret$entry_point, "/tmp/repo_dir/entry_point")
      expect_null(ret$source_dir)
      expect_null(ret$dependencies)
    }
  )
})

test_that("test_git_clone_repo_with_token_no_2fa", {
  git_config = list(
    "repo"=PRIVATE_GIT_REPO,
    "branch"=PRIVATE_BRANCH,
    "commit"=PRIVATE_COMMIT,
    "token"="my-token",
    "2FA_enabled"=FALSE
  )
  entry_point = "entry_point"
  source_dir = "source_dir"
  dependencies = list("foo", "dep_that_does_not_exist")

  env = c("current", "GIT_TERMINAL_PROMPT" = 0)
  mock_run = mock_fun()
  with_mock(
    `fs::is_file` = mock_fun(TRUE),
    `fs::file_temp` = mock_fun(REPO_DIR),
    `processx::run` = mock_run,
    {
      ret = sagemaker.common::git_clone_repo(git_config=git_config, entry_point=entry_point)
      args = mock_run(..return_value_all = T)
      expect_equal(args[[1]], list(
        "git",
        args = c(
          "clone",
          "https://my-token@github.com/testAccount/private-repo.git",
          REPO_DIR
        ),
        env = env
      ))
      expect_equal(args[[2]], list("git", args = c("checkout", PRIVATE_BRANCH), wd = REPO_DIR))
      expect_equal(args[[3]], list("git", args = c("checkout", PRIVATE_COMMIT), wd = REPO_DIR))
      expect_equal(ret$entry_point, "/tmp/repo_dir/entry_point")
      expect_null(ret$source_dir)
      expect_null(ret$dependencies)
    }
  )
})

test_that("test_git_clone_repo_with_token_2fa", {
  git_config = list(
    "repo"=PRIVATE_GIT_REPO,
    "branch"=PRIVATE_BRANCH,
    "commit"=PRIVATE_COMMIT,
    "2FA_enabled"=TRUE,
    "username"="username",
    "token"="my-token"
  )
  entry_point = "entry_point"
  source_dir = "source_dir"
  dependencies = list("foo", "dep_that_does_not_exist")

  env = c("current", "GIT_TERMINAL_PROMPT" = 0)
  mock_run = mock_fun()
  with_mock(
    `fs::is_file` = mock_fun(TRUE),
    `fs::file_temp` = mock_fun(REPO_DIR),
    `processx::run` = mock_run,
    {
      expect_warning(
        {ret = sagemaker.common::git_clone_repo(git_config=git_config, entry_point=entry_point)},
        "Using token for authentication, other credentials will be ignored."
      )
      args = mock_run(..return_value_all = T)
      expect_equal(args[[1]], list(
        "git",
        args = c(
          "clone",
          "https://my-token@github.com/testAccount/private-repo.git",
          REPO_DIR
        ),
        env = env
      ))
      expect_equal(args[[2]], list("git", args = c("checkout", PRIVATE_BRANCH), wd = REPO_DIR))
      expect_equal(args[[3]], list("git", args = c("checkout", PRIVATE_COMMIT), wd = REPO_DIR))
      expect_equal(ret$entry_point, "/tmp/repo_dir/entry_point")
      expect_null(ret$source_dir)
      expect_null(ret$dependencies)
    }
  )
})

test_that("test_git_clone_repo_ssh", {
  git_config = list("repo"=PRIVATE_GIT_REPO_SSH, "branch"=PRIVATE_BRANCH, "commit"=PRIVATE_COMMIT)
  entry_point = "entry_point"

  mock_chmod = mock_fun()
  with_mock(
    `processx::run` = mock_fun(),
    `fs::file_chmod` = mock_chmod,
    `fs::file_temp` = mock_fun(REPO_DIR),
    `fs::is_file` = mock_fun(TRUE),
    {
      ret = sagemaker.common::git_clone_repo(git_config, entry_point)
      expect_equal(mock_chmod(..return_value = T)$mode, "511")
      expect_equal(ret$entry_point, "/tmp/repo_dir/entry_point")
      expect_null(ret$source_dir)
      expect_null(ret$dependencies)
    }
  )
})

test_that("test_git_clone_repo_with_token_no_2fa_unnecessary_creds_provided", {
  git_config = list(
    "repo"=PRIVATE_GIT_REPO,
    "branch"=PRIVATE_BRANCH,
    "commit"=PRIVATE_COMMIT,
    "username"="username",
    "password"="passw0rd!",
    "token"="my-token"
  )
  entry_point = "entry_point"
  env = c("current", "GIT_TERMINAL_PROMPT" = 0)
  mock_run = mock_fun()
  with_mock(
    `processx::run` = mock_run,
    `fs::file_temp` = mock_fun(REPO_DIR),
    `fs::is_file` = mock_fun(TRUE),
    {
      expect_warning(
        {ret = sagemaker.common::git_clone_repo(git_config, entry_point)},
        "Using token for authentication, other credentials will be ignored."
      )
      args = mock_run(..return_value_all = T)
      expect_equal(args[[1]],list(
        "git",
        args = c("clone", "https://my-token@github.com/testAccount/private-repo.git", REPO_DIR),
        env = env
      ))
      expect_equal(args[[2]], list("git", args = c("checkout", PRIVATE_BRANCH), wd = REPO_DIR))
      expect_equal(args[[3]], list("git", args = c("checkout", PRIVATE_COMMIT), wd = REPO_DIR))
      expect_equal(ret$entry_point, "/tmp/repo_dir/entry_point")
      expect_null(ret$source_dir)
      expect_null(ret$dependencies)
    }
  )
})

test_that("test_git_clone_repo_with_token_2fa_unnecessary_creds_provided", {
  git_config = list(
    "repo"=PRIVATE_GIT_REPO,
    "branch"=PRIVATE_BRANCH,
    "commit"=PRIVATE_COMMIT,
    "2FA_enabled"=TRUE,
    "username"="username",
    "token"="my-token"
  )
  entry_point = "entry_point"
  env = c("current", "GIT_TERMINAL_PROMPT" = 0)
  mock_run = mock_fun()
  with_mock(
    `processx::run` = mock_run,
    `fs::file_temp` = mock_fun(REPO_DIR),
    `fs::is_file` = mock_fun(TRUE),
    {
      expect_warning(
        {ret = sagemaker.common::git_clone_repo(git_config, entry_point)},
        "Using token for authentication, other credentials will be ignored."
      )
      args = mock_run(..return_value_all = T)
      expect_equal(args[[1]],list(
        "git",
        args = c("clone", "https://my-token@github.com/testAccount/private-repo.git", REPO_DIR),
        env = env
      ))
      expect_equal(args[[2]], list("git", args = c("checkout", PRIVATE_BRANCH), wd = REPO_DIR))
      expect_equal(args[[3]], list("git", args = c("checkout", PRIVATE_COMMIT), wd = REPO_DIR))
      expect_equal(ret$entry_point, "/tmp/repo_dir/entry_point")
      expect_null(ret$source_dir)
      expect_null(ret$dependencies)
    }
  )
})

test_that("test_git_clone_repo_codecommit_https_with_username_and_password", {
  git_config = list(
    "repo"=CODECOMMIT_REPO,
    "branch"=CODECOMMIT_BRANCH,
    "username"="username",
    "password"="my-codecommit-password"
  )
  entry_point = "entry_point"
  env = c("current", "GIT_TERMINAL_PROMPT" = 0)
  mock_run = mock_fun()
  with_mock(
    `processx::run` = mock_run,
    `fs::file_temp` = mock_fun(REPO_DIR),
    `fs::is_file` = mock_fun(TRUE),
    {
      ret = sagemaker.common::git_clone_repo(git_config, entry_point)
      args = mock_run(..return_value_all = T)
      expect_equal(args[[1]],list(
        "git",
        args = c(
          "clone",
          "https://username:my-codecommit-password@git-codecommit.us-west-2.amazonaws.com/v1/repos/test-repo/",
          REPO_DIR
        ),
        env = env
      ))
      expect_equal(args[[2]], list("git", args = c("checkout", CODECOMMIT_BRANCH), wd = REPO_DIR))
      expect_equal(ret$entry_point, "/tmp/repo_dir/entry_point")
      expect_null(ret$source_dir)
      expect_null(ret$dependencies)
    }
  )
})
