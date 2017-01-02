#' Header of Rust file
#'
#' These lines should be in the head of lib.rs file.
#'
#' @export
headr = function() {
    cat(
        '#[macro_use]\nextern crate rustr;\npub mod export;\npub use rustr::*;\n\n// #[rustr_export]\npub fn say_hi()->RResult<String>{\n   Ok("hello world".into())\n}'
    )
}
