#' Clean Rust build target
#'
#' \code{rustr_clean_build} cleans the build target of \code{cargo build}.
#'
#' @param pkgdir package path
#' @param lib_only only remove builded static library
#' @export
rustr_clean_build <- function(pkgdir = ".", lib_only = FALSE) {
  if(lib_only){

      static_lib = file.path(pkgdir, "src", "rustlib", "target", "release", "librustlib.a")
      if(!dir.exists(static_lib)) {
          warning(paste(as.character(static_lib), "does not exist."))
      }
      unlink(static_lib)

  } else{

      target_dir = file.path(pkgdir, "src", "rustlib", "target")
      if(!dir.exists(target_dir)) {
          warning(paste(as.character(target_dir), "does not exist."))
      }
      unlink(target_dir, recursive = T)

  }

}
