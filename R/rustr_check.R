#' @title Check rustinr status
#' @description \code{rustr_check} checks the status of rustinr, rustc, and cargo installation.
#'
#' @param detail print detail info, FALSE by default.
#'
#' @export
rustr_check = function(detail = FALSE) {
    origin_verbose = getOption("verbose")
    checked = FALSE
    res = NULL
    tryCatch({
        if(origin_verbose){
            message("Running: library(rustinr)\n")
            message('Running:
rust(code = \'
// #[rustr_export]
pub fn say_hi() -> String{
    "Hello World".into()
}
\')\n')
        }


        rust(code = '
             // #[rustr_export]
             pub fn say_hi() -> String{
             "Hello World".into()
             }
             ')
        res = eval(quote(say_hi()))
        if (is.null(res) || res != "Hello World")
        {
            checked = TRUE
            find_info(res, OK = F)
            return(invisible(FALSE))
        } else
        {
            checked = TRUE
            if (detail == T) {
                message("\nMore info:\n")
                find_info(res)
            }
            message("\nGreat! It works!")
            return(invisible(TRUE))
        }

    },
    finally = {
        if (!checked) find_info(res,OK=F)
        options(verbose = origin_verbose)
    })
}

find_info = function(output, OK = T) {
    if (is.null(output) || output != "Hello World") {
        message("\nSomething is not working correctly. Getting more detail.\n")
    }
    options(verbose = TRUE)
    message('Running:\nlibrary(rustinr)\n')
    try(library(rustinr))
    message(
'Running:
rust(code = \'
// #[rustr_export]
pub fn say_hi() -> String{
    "Hello World".into()
}
\')\n')
    message("Running: Rust code parsing")
    try(rust(code = '
             // #[rustr_export]
             pub fn say_hi() -> String{
             "Hello World".into()
             }
             '))
    # find cargo
    message("\nFind cargo:")
    info = try(system("which cargo"))
    if (info != 0) {
        message("cargo is not in PATH\n")
        message("Find CARGO_HOME:")
        info_2 = Sys.getenv('CARGO_HOME', unset = NA)


        if (is.na(info_2)){
            message("CARGO_HOME is not set.\n")
        } else{
            message(paste("CARGO_HOME =", info_2,"\n"))
        }
    } else{
        try(system("cargo --version"))
        try(system("rustc --version"))
        message("\n")
    }

    # R session info
    message("sessionInfo:\n")
    print(sessionInfo())
    message("\n")

    # check loaded dlls
    message("Loaded DLLs:\n")
    print(getLoadedDLLs())
    message("\n")

    # Rust temporary  source
    message("Rust temporary source:")
    message(SOURCE_RUST_PATH$obj)
    message("\n")

    # Rust build result
    message("Rust build result:")
    try(print(list.files(
        file.path(SOURCE_RUST_PATH$obj, "src/rustlib/target/release")
    )))
    message("\n")

    # check lib.rs
    message("Check lib.rs:")
    try(cat(readLines(
        file.path(SOURCE_RUST_PATH$obj, "src/rustlib/src/lib.rs")
    ), sep = "\n"))
    message("\n")

    # R build result
    message("R build result")
    try(print(list.files(file.path(SOURCE_RUST_PATH$obj, "src/"))))
    if (!OK) {
        message("\n")
        message("Something is not working correctly. rustr is not ready.")
    }

}