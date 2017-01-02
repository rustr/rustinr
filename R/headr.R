#' Header of Rust file
#'
#' These lines should be in the head of lib.rs file.
#'
#' @examples
#' \dontrun{
#'
#' #[macro_use]
#' extern crate rustr;
#' pub mod export;
#' pub use rustr::*;
#'
#' // #[rustr_export]
#' pub fn say_hi()->RResult<String>{
#'     Ok("hello world".into())
#' }
#' }
#' @export
headr = function() {
    cat(
        '#[macro_use]\nextern crate rustr;\npub mod export;\npub use rustr::*;\n\n// #[rustr_export]\npub fn say_hi()->RResult<String>{\n   Ok("hello world".into())\n}'
    )
}
