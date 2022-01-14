#' @import R6

`%||%` <- function(x, y) if (is.null(x)) return(y) else return(x)

# Check the magic bytes at offset 257. If they match "ustar" including the null terminator, the file is probably a tar.
# https://stackoverflow.com/questions/32180215/how-to-check-whether-a-file-is-in-tar-format
# http://www.gnu.org/software/tar/manual/html_node/Standard.html
is_tarfile <- function(path){
  con <- gzfile(path.expand(path), "rb")
  on.exit(close(con))
  magic <- readBin(con, "raw", n = 262L)
  rawToChar(magic[258:262]) == "ustar"
}

sys_set_env <- function(key, value){
  config_env = list(value)
  names(config_env) = key
  do.call(Sys.setenv, config_env)
}

pkg_name = function(){
  env <- topenv(environment())
  get0(".packageName", envir = env, inherits = FALSE)
}
