#' r6 sagemaker: this is just a placeholder
#'
#' @import R6
#' @import paws
#' @import jsonlite
#' @import R6sagemaker.common
#' @import lgr
#' @import utils
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  # set package logs and don't propagate root logs
  .logger = lgr::get_logger(name = "R6sagemaker")

  # set package logger
  assign(
    "LOGGER",
    .logger,
    envir = parent.env(environment())
  )
}
