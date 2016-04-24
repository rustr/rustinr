#' Check rustr installation
#' @param detail show more detail
#' @export
check_rustr = function(detail = FALSE){
    origin_verbose = getOption("verbose")

    tryCatch({

        message('Running:
library(rustinr)\n')
        library(rustinr)
        message('Running:
rust(code = \'
// #[rustr_export]
pub fn say_hi() -> String{
    "Hello World".into()
}
\')\n')
        rust(code = '
// #[rustr_export]
pub fn say_hi() -> String{
    "Hello World".into()
}
')
        res = say_hi()
        if (res != "Hello World") {
            warning("Something is not working correctly. Let's read more detail.")
            find_info()
            return(invisible(FALSE))
        } else{
            if (detail == T){
                message("\nLet's find more info:\n")
                find_info()
            }
            message("\nGreat! It works!")
            return(invisible(TRUE))
        }

    },
    error = function(e) stop(e),
    finally = {
        options(verbose = origin_verbose)
    })
}

find_info = function(){
    options(verbose = TRUE)
    message('Running:
library(rustinr)\n')
    library(rustinr)
    message('Running:
rust(code = \'
// #[rustr_export]
pub fn say_hi() -> String{
    "Hello World".into()
}
\')\n')
    rust(code = '
// #[rustr_export]
pub fn say_hi() -> String{
    "Hello World".into()
}
')
    # find cargo
    message("\nFind cargo:")
    res = system("which cargo")
    message("\n")
    if (res != 0){
        warning("cargo is not in PATH")
        message("Find CARGO_HOME:")
        res = Sys.getenv('CARGO_HOME', unset = NA)
        message(paste("CARGO_HOME =", res))

        if (is.na(res)) {
            warning("CARGO_HOME is not set.")
        }
    } else{
        system("cargo --version")
        system("rustc --version")
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
    print(list.files(file.path(SOURCE_RUST_PATH$obj,"src/rustlib/target/release")))
    message("\n")

    # check lib.rs
    message("Check lib.rs:")
    cat(readLines(file.path(SOURCE_RUST_PATH$obj,"src/rustlib/src/lib.rs")),sep="\n")
    message("\n")

    # R build result
    message("R build result")
    print(list.files(file.path(SOURCE_RUST_PATH$obj,"src/")))
}