#' r6 sagemaker: this is just a placeholder
#'
#' @import R6
#' @import paws
#' @import jsonlite
#' @import sagemaker.common
#' @import lgr
#' @importFrom utils object.size
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  # set package logs and don't propagate root logs
  .logger = lgr::get_logger(name = "sagemaker")

  # set package logger
  assign(
    "LOGGER",
    .logger,
    envir = parent.env(environment())
  )
}
