#' Clean Rust build target
#'
#' This function cleans
#'
#' @param pkgdir package path
#' @export
rustr_clean_build <- function(pkgdir = ".") {
  if(!dir.exists(file.path(pkgdir, "src", "rustlib", "target"))) {
    warning(paste(as.character(file.path(pkgdir, "src", "rustlib", "target")), "is not exist."))
  }
  unlink(file.path(pkgdir, "src", "rustlib", "target"), recursive = T)
}
