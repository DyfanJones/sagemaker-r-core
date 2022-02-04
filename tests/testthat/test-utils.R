# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/tests/unit/test_utils.py

BUCKET_WITHOUT_WRITING_PERMISSION = "s3://bucket-without-writing-permission"

NAME = "base_name"
BUCKET_NAME = "some_bucket"

test_that("test get config value",{
  config = list("local"=list("region_name"="us-west-2", "port"="123"), "other"=list("key"=1))

  expect_equal(get_config_value("local.region_name", config), "us-west-2")
  expect_equal(get_config_value("local", config), list(
    "region_name"="us-west-2",
    "port"="123")
  )

  expect_null(get_config_value("does_not.exist", config))
  expect_null(get_config_value("other.key", NULL))
})

test_that("test get short version", {
  expect_equal(get_short_version("1.13.1"), "1.13")
  expect_equal(get_short_version("1.13"), "1.13")
})

test_that("test name from image", {
  image = "image:latest"
  max_length = 32
  out = name_from_image(image, max_length=max_length)
  expect_true(nchar(out) < max_length)
})

test_that("test name from base", {
  out = name_from_base(NAME, short=F)
  expect_true(grepl("^(.+)-(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}-\\d{2}-\\d{3})", out))
})

test_that("test name from base short", {
  out = name_from_base(NAME, short=T)
  expect_true(grepl("^(.+)-(\\d{6}-\\d{4})", out))
})

test_that("test unique name from base", {
  expect_true(grepl("base-\\d{10}-[a-f0-9]{4}", unique_name_from_base("base")))
})

test_that("test unique name from base truncated", {
  expect_true(grepl("real-\\d{10}-[a-f0-9]{4}",
    unique_name_from_base("really-long-name", max_length=20)))
})

test_that("test base from name", {
  name = "mxnet-training-2020-06-29-15-19-25-475"
  expect_equal(base_from_name(name), "mxnet-training")

  name = "sagemaker-pytorch-200629-1611"
  expect_equal(base_from_name(name), "sagemaker-pytorch")
})

MESSAGE = "message"
STATUS = "status"
TRAINING_JOB_DESCRIPTION_1 = list(
  "SecondaryStatusTransitions"=list(list("StatusMessage"=MESSAGE, "Status"=STATUS))
)
TRAINING_JOB_DESCRIPTION_2 = list(
  "SecondaryStatusTransitions"=list(list("StatusMessage"="different message", "Status"=STATUS))
)

TRAINING_JOB_DESCRIPTION_EMPTY = list("SecondaryStatusTransitions"=list())

test_that("test secondary training status changed true", {
  changed = secondary_training_status_changed(
    TRAINING_JOB_DESCRIPTION_1, TRAINING_JOB_DESCRIPTION_2
  )
  expect_true(changed)
})

test_that("test secondary training status changed false", {
  changed = secondary_training_status_changed(
    TRAINING_JOB_DESCRIPTION_1, TRAINING_JOB_DESCRIPTION_1
  )
  expect_false(changed)
})

test_that("test secondary training status changed prev missing", {
  changed = secondary_training_status_changed(TRAINING_JOB_DESCRIPTION_1, list())
  expect_true(changed)
})

test_that("test secondary training status changed prev none", {
  changed = secondary_training_status_changed(TRAINING_JOB_DESCRIPTION_1, NULL)
  expect_true(changed)
})

test_that("test secondary training status changed current missing", {
  changed = secondary_training_status_changed(list(), TRAINING_JOB_DESCRIPTION_1)
  expect_false(changed)
})

test_that("test secondary training status changed empty", {
  changed = secondary_training_status_changed(
    TRAINING_JOB_DESCRIPTION_EMPTY, TRAINING_JOB_DESCRIPTION_1
  )
  expect_false(changed)
})

test_that("test secondary training status message status changed", {
  now = Sys.time()
  TRAINING_JOB_DESCRIPTION_1[["LastModifiedTime"]] = now
  expected = sprintf("%s %s - %s",
    strftime(now, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
    STATUS,
    MESSAGE
  )
  expect_equal(
    secondary_training_status_message(
      TRAINING_JOB_DESCRIPTION_1, TRAINING_JOB_DESCRIPTION_EMPTY
    ),
    expected
  )
})

test_that("test secondary training status message status not changed",{
  now = Sys.time()
  TRAINING_JOB_DESCRIPTION_1[["LastModifiedTime"]] = now
  expected = sprintf("%s %s - %s",
    strftime(now, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
    STATUS,
    MESSAGE
  )
  expect_equal(
    secondary_training_status_message(
      TRAINING_JOB_DESCRIPTION_1, TRAINING_JOB_DESCRIPTION_2
    ),
    expected
  )
})

test_that("test secondary training status message prev missing", {
  now = Sys.time()
  TRAINING_JOB_DESCRIPTION_1[["LastModifiedTime"]] = now
  expected = sprintf("%s %s - %s",
    strftime(now, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
    STATUS,
    MESSAGE
  )
  expect_equal(
    secondary_training_status_message(
      TRAINING_JOB_DESCRIPTION_1, list()
    ),
    expected
  )
})

test_that("test download folder", {
  paws_mock = Mock$new("PawsSession", region_name = "us-east-1")
  paws_mock$.call_args("client")
  session = Session$new(paws_session=paws_mock, sagemaker_client=Mock$new())
  s3_mock = Mock$new("s3")
  s3_mock$.call_args("get_object", list(Body = charToRaw("dummy")))
  s3_mock$.call_args("list_objects_v2", list(
      Contents = list(
        list(Key = "prefix/train/train_data.csv"),
        list(Key = "prefix/train/validation_data.csv")
      ),
      ContinuationToken = character(0)
    )
  )
  session$s3 = s3_mock

  download_folder(BUCKET_NAME, "/prefix","/tmp", session)

  expect_true(file.exists(file.path("/tmp", "prefix")))

  download_folder(BUCKET_NAME, "/prefix/","/tmp", session)

  expect_true(file.exists(file.path("/tmp", "train", "train_data.csv")))
  expect_true(file.exists(file.path("/tmp", "train", "validation_data.csv")))

  fs::file_delete(c(
    file.path("/tmp", "prefix"),
    file.path("/tmp", "train","train_data.csv"),
    file.path("/tmp", "train","validation_data.csv")
    )
  )
})

test_that("test download folder points to single file", {
  paws_mock = Mock$new("PawsSession", region_name = "us-east-1")
  paws_mock$.call_args("client")
  session = Session$new(paws_session=paws_mock, sagemaker_client=Mock$new())
  s3_mock = Mock$new("s3")
  s3_mock$.call_args("get_object", list(Body = charToRaw("dummy")))
  session$s3 = s3_mock

  download_folder(BUCKET_NAME, "/prefix/train/train_data.csv","/tmp", session)

  expect_true(file.exists(file.path("/tmp", "train_data.csv")))

  fs::file_delete(file.path("/tmp", "train_data.csv"))
})

test_that("test download file", {
  paws_mock = Mock$new("PawsSession", region_name = "us-east-1")
  paws_mock$.call_args("client")
  session = Session$new(paws_session=paws_mock, sagemaker_client=Mock$new())
  s3_mock = Mock$new("s3")
  s3_mock$.call_args("get_object", list(Body = charToRaw("dummy")))
  session$s3 = s3_mock

  tmp_file_tar = file.path(tempdir(), "file.tar.gz")

  download_file(
    BUCKET_NAME, "/prefix/path/file.tar.gz", tmp_file_tar, session
  )

  expect_true(file.exists(tmp_file_tar))

  fs::file_delete(tmp_file_tar)
})

test_that("test create tar file with provided path", {
  temp_dir = "dummy"
  dir.create(temp_dir)
  writeLines("dummy", file.path(temp_dir, "file_a"))
  writeLines("dummy", file.path(temp_dir, "file_b"))
  writeLines("dummy", file.path(temp_dir, "file_c"))

  file_list = c(file.path(temp_dir, "file_a"), file.path(temp_dir, "file_b"))

  out = create_tar_file(file_list, target="my/custom/path.tar.gz")

  expect_true(file.exists("my/custom/path.tar.gz"))

  expect_equal(untar(out, list = T), basename(file_list))

  fs::dir_delete(c(file.path(getwd(), "my"), "dummy"))
})

test_that("test create tar file with auto generated path", {
  temp_dir = "dummy"
  dir.create(temp_dir)
  writeLines("dummy", file.path(temp_dir, "file_a"))
  writeLines("dummy", file.path(temp_dir, "file_b"))

  file_list = c(file.path(temp_dir, "file_a"), file.path(temp_dir, "file_b"))

  out = create_tar_file(file_list)

  expect_true(file.exists(out))

  expect_equal(untar(out, list = T), basename(file_list))

  fs::dir_delete("dummy")
  fs::file_delete(out)
})

create_file_tree <- function(tmp, tree){
  file_path = fs::path(tmp, tree)
  fs::dir_create(dirname(file_path))
  fs::file_touch(file_path)
}

tar_and_raw = function(File, tar_file="file.tar.gz"){
  fs::dir_create("dummy")
  tar_file = file.path("dummy", tar_file)
  create_tar_file(File, target=tar_file)
  readBin(tar_file, "raw", n = file.size(tar_file))
}

tmp = file.path(getwd(), "temp")

fake_s3_session <- function(tar=T, model_dir = file.path(tmp, "model-dir")){
  paws_mock = Mock$new("PawsSession", region_name = "us-east-1")
  paws_mock$.call_args("client")
  session = Session$new(paws_session=paws_mock, sagemaker_client=Mock$new())
  s3_mock = Mock$new("s3")
  if(tar){
    obj = list(Body = tar_and_raw(model_dir))
    s3_mock$.call_args("get_object", return_value = obj)
  }
  s3_mock$.call_args("list_objects_v2", list(
      Contents = list(
        list(Key = "prefix/train/train_data.csv"),
        list(Key = "prefix/train/validation_data.csv")
      ),
      ContinuationToken = character(0)
    )
  )
  s3_mock$.call_args("put_object")

  session$s3 = s3_mock
  return(session)
}

test_that("test repack model without source dir", {
  create_file_tree(
    tmp,
    c(
      "model-dir/model",
      "dependencies/a",
      "dependencies/some/dir/b",
      "aa",
      "bb",
      "source-dir/inference.py",
      "source-dir/this-file-should-not-be-included.py"
    )
  )
  session = fake_s3_session()
  repack_model(
    inference_script=file.path(tmp, "source-dir/inference.py"),
    source_directory=NULL,
    dependencies=c(
      file.path(tmp, "dependencies/a"),
      file.path(tmp, "dependencies/some/dir"),
      file.path(tmp, "aa"),
      file.path(tmp, "bb")
    ),
    model_uri="s3://fake/location",
    repacked_model_uri="s3://destination-bucket/model.tar.gz",
    sagemaker_session=session
  )

  obj = session$s3$put_object(..return_value = T)
  writeBin(obj$Body,"dummy/temp.tar.gz")

  file_list = untar("dummy/temp.tar.gz", list = T)

  expect_equal(
    sort(file_list[!grepl("/$", file_list)]),
    c("./code/inference.py", "./code/lib/a", "./code/lib/aa", "./code/lib/bb",
      "./code/lib/dir/b", "./model")
  )
  fs::dir_delete(c("temp","dummy"))
})

test_that("test repack model with entry point without path without source dir", {
  create_file_tree(
    tmp,
    c(
      "model-dir/model",
      "source-dir/inference.py",
      "source-dir/this-file-should-not-be-included.py"
    )
  )
  session = fake_s3_session()

  cwd = getwd()
  setwd(file.path(tmp, "source-dir"))

  repack_model(
    "inference.py",
    NULL,
    NULL,
    "s3://fake/location",
    "s3://destination-bucket/model.tar.gz",
    session
  )

  setwd(cwd)

  obj = session$s3$put_object(..return_value = T)
  writeBin(obj$Body,"dummy/temp.tar.gz")

  file_list = untar("dummy/temp.tar.gz", list = T)

  expect_equal(
    sort(file_list[!grepl("/$", file_list)]),
    c("./code/inference.py", "./model")
  )
  fs::dir_delete(c("temp","dummy"))
})

test_that("test repack model with entry point without path without source dir", {
  create_file_tree(
    tmp,
    c(
      "model-dir/model",
      "source-dir/inference.py",
      "source-dir/this-file-should-be-included.py"
    )
  )
  session = fake_s3_session()

  repack_model(
    "inference.py",
    file.path(tmp, "source-dir"),
    NULL,
    "s3://fake/location",
    "s3://destination-bucket/model.tar.gz",
    session
  )

  obj = session$s3$put_object(..return_value = T)
  writeBin(obj$Body,"dummy/temp.tar.gz")

  file_list = untar("dummy/temp.tar.gz", list = T)

  expect_equal(
    sort(file_list[!grepl("/$", file_list)]),
    c("./code/inference.py", "./code/this-file-should-be-included.py", "./model")
  )
  fs::dir_delete(c("temp","dummy"))
})

test_that("test repack model from s3 to s3", {
  create_file_tree(
    tmp,
    c(
      "model-dir/model",
      "source-dir/inference.py",
      "source-dir/this-file-should-be-included.py"
    )
  )
  session = fake_s3_session()

  repack_model(
    "inference.py",
    file.path(tmp, "source-dir"),
    NULL,
    "s3://fake/location",
    "s3://destination-bucket/model.tar.gz",
    session
  )

  obj = session$s3$put_object(..return_value = T)
  writeBin(obj$Body,"dummy/temp.tar.gz")

  file_list = untar("dummy/temp.tar.gz", list = T)

  expect_equal(
    sort(file_list[!grepl("/$", file_list)]),
    c("./code/inference.py", "./code/this-file-should-be-included.py", "./model")
  )
  fs::dir_delete(c("temp","dummy"))
})

test_that("test repack model from file to file", {
  create_file_tree(tmp, c("model", "dependencies/a", "source-dir/inference.py"))

  model_tar_path = file.path(getwd(), "dummy", "model.tar.gz")
  create_tar_file(file.path(tmp, "model"), model_tar_path)

  session = fake_s3_session(F)

  file_mode_path = sprintf("file://%s", model_tar_path)
  destination_path = sprintf("file://%s", file.path("dummy", "repacked-model.tar.gz"))

  repack_model(
    "inference.py",
    file.path(tmp, "source-dir"),
    file.path(tmp, "dependencies/a"),
    file_mode_path,
    destination_path,
    session
  )

  file_list = untar(gsub("file://", "",destination_path), list = T)

  expect_equal(
    sort(file_list[!grepl("/$", file_list)]),
    c("./code/inference.py", "./code/lib/a", "./model")
  )
  fs::dir_delete(c("temp","dummy"))
})

test_that("test repack mode with inference code should replace the code", {
  create_file_tree(
    tmp, c("model-dir/model", "source-dir/new-inference.py", "model-dir/code/old-inference.py")
  )

  session = fake_s3_session()

  repack_model(
    "inference.py",
    file.path(tmp, "source-dir"),
    NULL,
    "s3://fake/location",
    "s3://destination-bucket/repacked-model",
    session
  )

  obj = session$s3$put_object(..return_value = T)
  writeBin(obj$Body,"dummy/temp.tar.gz")

  file_list = untar("dummy/temp.tar.gz", list = T)

  expect_equal(
    sort(file_list[!grepl("/$", file_list)]),
    c("./code/new-inference.py", "./model" )
  )
  fs::dir_delete(c("temp","dummy"))
})

test_that("test repack model from file to folder", {
  create_file_tree(tmp, c("model", "source-dir/inference.py"))
  session = fake_s3_session(F)

  model_tar_path = file.path(tmp, "model.tar.gz")
  create_tar_file(file.path(tmp, "model"), model_tar_path)

  file_mode_path = sprintf("file://%s", model_tar_path)


  repack_model(
    "inference.py",
    file.path(tmp, "source-dir"),
    list(),
    file_mode_path,
    sprintf("file://%s/repacked-model.tar.gz", tmp),
    session
  )

  file_list = untar(sprintf("%s/repacked-model.tar.gz", tmp), list = T)

  expect_equal(
    sort(file_list[!grepl("/$", file_list)]),
    c("./code/inference.py", "./model")
  )
  fs::dir_delete(c("temp"))
})


################################################################
# HERE!!!!!
################################################################
test_that("test repack model with inference code and requirements", {
  create_file_tree(
    tmp,
    c(
      "new-inference.py",
      "model-dir/model",
      "model-dir/code/old-inference.py",
      "model-dir/code/requirements.txt"
    )
  )
  session = fake_s3_session(model_dir = file.path(tmp, "model-dir"))

  repack_model(
    file.path(tmp, "new-inference.py"),
    NULL,
    NULL,
    "s3://fake/location",
    "s3://destination-bucket/repacked-model",
    session
  )

  obj = session$s3$put_object(..return_value = T)

  writeBin(obj$Body,"dummy/temp.tar.gz")

  file_list = untar("dummy/temp.tar.gz", list = T)

  expect_equal(
    sort(file_list[!grepl("/$", file_list)]),
    c("./code/new-inference.py",
      "./code/old-inference.py",
      "./code/requirements.txt",
      "./model")
  )

  fs::dir_delete(c("temp", "dummy"))
})

test_that("test repack model with same inference file name", {
  create_file_tree(
    tmp,
    c(
      "inference.py",
      "model-dir/model",
      "model-dir/code/inference.py",
      "model-dir/code/requirements.txt"
    )
  )
  session = fake_s3_session()

  repack_model(
    file.path(tmp, "inference.py"),
    NULL,
    NULL,
    "s3://fake/location",
    "s3://destination-bucket/repacked-model",
    session
  )

  obj = session$s3$put_object(..return_value = T)
  writeBin(obj$Body,"dummy/temp.tar.gz")

  file_list = untar("dummy/temp.tar.gz", list = T)

  expect_equal(
    sort(file_list[!grepl("/$", file_list)]),
    c("./code/inference.py", "./code/requirements.txt", "./model")
  )
  fs::dir_delete(c("temp", "dummy"))
})

test_that("test sts regional endpoint", {
  endpoint = sts_regional_endpoint("us-west-2")
  expect_equal(endpoint, "https://sts.us-west-2.amazonaws.com")

  endpoint = sts_regional_endpoint("us-iso-east-1")
  expect_equal(endpoint, "https://sts.us-iso-east-1.c2s.ic.gov")
})

test_that("test partition by region", {
  expect_equal(.aws_partition("us-west-2"), "aws")
  expect_equal(.aws_partition("cn-north-1"), "aws-cn")
  expect_equal(.aws_partition("us-gov-east-1"), "aws-us-gov")
  expect_equal(.aws_partition("us-iso-east-1"), "aws-iso")
  expect_equal(.aws_partition("us-isob-east-1"), "aws-iso-b")
})
