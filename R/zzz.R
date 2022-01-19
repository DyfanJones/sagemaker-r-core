#' r6 sagemaker: this is just a placeholder
#'
#' @import lgr
#' @importFrom utils getFromNamespace
"_PACKAGE"

#' @include logs.R

.onLoad <- function(libname, pkgname) {
  # set package logs and don't propagate root logs
  .logger = lgr::get_logger(name = "sagemaker")$set_propagate(FALSE)

  # set logging layout
  .logger$add_appender(
    lgr::AppenderConsole$new(
      layout=sagemaker_log_layout()
    )
  )

  # set package logger
  assign(
    "LOGGER",
    .logger,
    envir = parent.env(environment())
  )

  readr_methods()
}

readr_methods <- function(){
  pkg_env$readr$available <- base::requireNamespace("readr", quietly = TRUE)

  if(pkg_env$readr$available) {
    pkg_env$readr$methods$write_file <- utils::getFromNamespace("write_file", "readr")
  } else {
    LOGGER$info("For extra speed please install `readr`.")
  }
}

# Package cache
pkg_env = new.env(emptyenv())

# package cache structure
# pkg_env:
#   - readr:
#     - available:
#       - TRUE|FALSE
#     - methods
#       - write_file
#       - ...
