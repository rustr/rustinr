#' Clean Rust build target
#'
#' \code{rustr_clean_build} cleans the build target of \code{cargo build}.
#'
#' @param pkgdir package path
#' @export
rustr_clean_build <- function(pkgdir = ".") {
  if(!dir.exists(file.path(pkgdir, "src", "rustlib", "target"))) {
    warning(paste(as.character(file.path(pkgdir, "src", "rustlib", "target")), "is not exist."))
  }
  unlink(file.path(pkgdir, "src", "rustlib", "target"), recursive = T)
}
