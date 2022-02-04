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
}
