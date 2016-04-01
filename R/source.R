#' Source rust file
#'
#' @param depend rustr version
#' @param rebuild rebuild cache
#' @param header add header
#' @param path package path
#' @param code rust code
#' @param env  an environment, determining where the export R functions are evaluated
#' @export
rust <- function(code, path, depend = NULL, header = TRUE, rebuild = FALSE, env = globalenv()) {
    if (!missing(code)) {
        file <- tempfile(fileext = ".rs")
        con <- file(file, open = "w")
        if (header){
            writeLines(c("#[macro_use]",
                         "extern crate rustr;",
                         "pub mod export;",
                         "pub use rustr::*;"
            ),con)
        }
        writeLines(code, con)
        close(con)
        path2 = normalizePath(file)
        if (getOption("verbose")){
            cat(paste0("tempfile : ", path2, "\n"))
        }
    } else {
        stopifnot(length(path) == 1)
        path2 = normalizePath(path)
    }

    cwd <- getwd()

    if (is.null(SOURCE_RUST_PATH$obj) || rebuild == TRUE) {
        rss = random_string()
        pathdir = suppressWarnings(normalizePath(file.path(tempdir(), rss)))
        SOURCE_RUST_PATH$obj = pathdir
        SOURCE_RUST_PATH$rss = rss
        reboot = T
        rss2 = rss
    } else{
        pathdir = SOURCE_RUST_PATH$obj
        reboot = F
        rss = SOURCE_RUST_PATH$rss
        rss2 =  random_string()
    }

    envRestore = suppressWarnings(setup(file.path(pathdir, "src", "REXPORT.c")))
    tryCatch({
        succeeded <- FALSE

        if (reboot == T) {
            if (dir.exists(pathdir)) {
                stop(paste("can not create tempdir : ", pathdir))
            }
            suppressMessages(rustr_init(rss, path = tempdir()))
            suppressWarnings(file.remove(file.path(pathdir, "src", "Makevars.win")))
            suppressWarnings(file.remove(file.path(pathdir, "man", paste0(rss, "-package.Rd"))))
        } else{
            x = readLines(file.path(pathdir, "DESCRIPTION"))
            x[1] = paste0("Package: ", rss2)
            writeLines(x, file.path(pathdir, "DESCRIPTION"))
        }

        if (!file.exists(path2)) {
            stop(paste("can not find : ", path2))
        }

        file.copy(path2,
                  file.path(pathdir, "src", "rustlib", "src", "lib.rs"),
                  overwrite = T)

        if (!is.null(depend)) {
            dest = file.path(pathdir, "src", "rustlib", "Cargo.toml")
            file.copy(
                file.path(system.file("init", package = "rustinr"),"Cargo_deps.toml"),
                dest,
                overwrite = T)
            message("updating cached Cargo.toml.")
            destfile = file(dest,"a")
            writeLines(depend,destfile)
            close(destfile)
            if (getOption("verbose")) {
                cat(readLines(dest),sep = "\n")
            }
        }

        rustrize(pathdir)

        setwd(file.path(pathdir, "src", "rustlib"))
        cargo = ifelse(Sys.getenv("CARGO_HOME") != "",
                       Sys.getenv("CARGO_HOME"),
                       "cargo")
        if(.Platform$OS.type == "windows"){
            cmd = paste(cargo, "build --release ")
        } else{
            cmd = paste(cargo, "build --release 2>&1")
        }

        result <-
            suppressWarnings(system(cmd, intern = !getOption("verbose")))
        status <- attr(result, "status")
        if (!is.null(status)) {
            cat(result, sep = "\n")
            succeeded <- FALSE
            stop("Error ",
                 status,
                 " occurred building shared library.")
        }
        setwd(file.path(pathdir, "src"))

        linklib = ifelse(
            .Platform$OS.type == "windows",
            "-lrustlib -lws2_32 -luserenv -lshell32 -ladvapi32",
            "-lrustlib"
        )
        linksearch = "-Lrustlib/target/release/"

        if (!.callBuildHook(path2, FALSE,
                            getOption("verbose"))) {
            return(invisible(NULL))
        }

        cmd <- paste(
            R.home(component = "bin"),
            .Platform$file.sep,
            "R ",
            "CMD SHLIB ",
            shQuote(linksearch),
            " ",
            shQuote(linklib),
            " ",
            "-o ",
            shQuote(paste0(rss2, .Platform$dynlib.ext)),
            " ",

            " ",
            "",
            shQuote("REXPORT.c"),
            " ",
            sep = ""
        )

        result <-
            suppressWarnings(system(cmd, intern = !getOption("verbose")))
        status <- attr(result, "status")
        if (!is.null(status)) {
            cat(result, sep = "\n")
            succeeded <- FALSE
            stop("Error ",
                 status,
                 " occurred building shared library.")
        }
        dyn.load(file.path(paste0(rss2, .Platform$dynlib.ext)))
        source("../R/REXPORT.R", local = env)
        succeeded <- TRUE
    }, finally = {
        if (!succeeded)
            .showBuildFailureDiagnostics()
        setwd(cwd)
        restore(envRestore)
        setwd(cwd)
    })
}

random_string <- function(n = 1, lenght = 12)
{
    randomString <- c(1:n)
    for (i in 1:n)
    {
        randomString[i] <- paste(sample(c(letters, LETTERS),
                                        lenght, replace = TRUE),
                                 collapse = "")
    }
    return(randomString)
}

## Learn from Rcpp and Rcpp11

restore = function(env) {
    setVars <- env[!is.na(env)]
    if (length(setVars))
        do.call(Sys.setenv, setVars)
    removeVars <- names(env[is.na(env)])
    if (length(removeVars))
        Sys.unsetenv(removeVars)
}

setup = function(sourceFile) {
    buildEnv <- list()

    mergeEnv <- function(name, value) {
        if (is.null(value) || !nzchar(value))
            return

        if (is.null(buildEnv[[name]])) {
            buildEnv[[name]] <<- value
        }
        else if (!identical(buildEnv[[name]], value)) {
            buildEnv[[name]] <<- paste(buildEnv[[name]], value)

        }

    }

    if (length(buildEnv) == 0) {
        buildEnv <- list(PKG_LIBS = "")
    }

    for (name in names(buildEnv))
        mergeEnv(name, Sys.getenv(name))

    buildEnv$CYGWIN = "nodosfilewarning"

    if (.Platform$OS.type == "windows" &&
        !nzchar(Sys.getenv("RSTUDIO"))) {
        env <- EnvRtools()
        for (var in names(env))
            buildEnv[[var]] <- env[[var]]
    }

    restore <- list()

    for (name in names(buildEnv))
        restore[[name]] <- Sys.getenv(name, unset = NA)

    do.call(Sys.setenv, buildEnv)

    return(restore)
}

build_path = function(path) {
    if (.Platform$OS.type == "windows") {
        path <- normalizePath(path)
        if (grepl(" ", path, fixed = TRUE))
            path <- utils::shortPathName(path)
        path <- gsub("\\\\", "/", path)
    }
    return(path)
}



EnvRtools <- function() {
    hasRtools <-
        nzchar(Sys.which("ls.exe")) && nzchar(Sys.which("gcc.exe"))
    if (!hasRtools) {
        key <- NULL
        try(key <- utils::readRegistry("SOFTWARE\\R-core\\Rtools",
                                       hive = "HLM",
                                       view = "32-bit"),
            silent = TRUE)

        if (!is.null(key)) {
            ver <- key$`Current Version`
            if (as.numeric(ver) >=3.3) {
                rToolsPath <- key$`InstallPath`
                if (!is.null(rToolsPath)) {
                    path <- file.path(rToolsPath, "bin", fsep = "\\")

                    if (all(file.exists(path))) {
                        env <- list()
                        path <-
                            paste(path, collapse = .Platform$path.sep)
                        env$PATH <-
                            paste(path,
                                  Sys.getenv("PATH"),
                                  sep = .Platform$path.sep)
                        env$RTOOLS <- .rtoolsPath(rToolsPath)
                        env$BINPREF <-  file.path(env$RTOOLS,"mingw_$(WIN)/bin//",fsep = "/")
                        return(env)
                    }
                }
            }
        }
    }

    return(NULL)
}


.rtoolsPath <- function(path) {
    path <- gsub("\\\\", "/", path)
    .localsub <- function(re, x)
        sub(re, "", x, perl = TRUE)
    path <- .localsub("[ \t\r\n]+$", .localsub("^[ \t\r\n]+", path))
    if (substring(path, nchar(path)) != "/")
        path <- paste(path, "/", sep = "")
    path
}

.showBuildFailureDiagnostics <- function() {
    # RStudio does it's own diagnostics so only do this for other environments
    if (nzchar(Sys.getenv("RSTUDIO")))
        return()


    # if we can't call R CMD SHLIB then notify the user they should
    # install the appropriate development tools
    if (!.checkDevelTools()) {
        msg <-
            paste(
                "\nWARNING: The tools required to build C++ code for R ",
                "were not found.\n\n",
                sep = ""
            )
        sysName <- Sys.info()[['sysname']]
        if (identical(sysName, "Windows")) {
            msg <- paste(
                msg,
                "Please download and install the appropriate ",
                "version of Rtools:\n\n",
                "http://cran.r-project.org/bin/windows/Rtools/\n",
                sep = ""
            )


        } else if (identical(sysName, "Darwin")) {
            msg <- paste(
                msg,
                "Please install Command Line Tools for XCode ",
                "(or equivalent).\n",
                sep = ""
            )
        } else {
            msg <- paste(
                msg,
                "Please install GNU development tools ",
                "including a C++ compiler.\n",
                sep = ""
            )
        }
        message(msg)
    }
}

# check if R development tools are installed (cache successful result)
.hasDevelTools <- FALSE
.checkDevelTools <- function() {
    if (!.hasDevelTools) {
        # create temp source file
        tempFile <- file.path(tempdir(), "foo.c")
        cat("void foo() {}\n", file = tempFile)
        on.exit(unlink(tempFile))

        # set working directory to tempdir (revert on exit)
        oldDir <- setwd(tempdir())
        on.exit(setwd(oldDir), add = TRUE)

        # attempt the compilation and note whether we succeed
        cmd <-
            paste(R.home(component = "bin"),
                  .Platform$file.sep,
                  "R ",
                  "CMD SHLIB foo.c",
                  sep = "")
        result <- suppressWarnings(system(cmd,
                                          ignore.stderr = TRUE,
                                          intern = TRUE))
        utils::assignInMyNamespace(".hasDevelTools", is.null(attr(result, "status")))

        if (.hasDevelTools) {
            lib <- file.path(tempdir(),
                             paste("foo", .Platform$dynlib.ext, sep = ''))
            unlink(lib)
        }
    }
    .hasDevelTools
}

.getHooksList <- function(name) {
    hooks <- getHook(name)
    if (!is.list(hooks))
        hooks <- list(hooks)
    hooks
}

.callBuildHook <- function(file, fromCode, showOutput) {
    for (fun in .getHooksList("sourceCpp.onBuild")) {
        if (is.character(fun))
            fun <- get(fun)

        # allow the hook to cancel the build (errors in the hook explicitly
        # do not cancel the build since they are unexpected bugs)
        continue <- tryCatch(
            fun(file, fromCode, showOutput),
            error = function(e)
                TRUE
        )

        if (!continue)
            return (FALSE)
    }

    return (TRUE)
}


#' Temporary Folder for source_rust()
#'
#' @export
SOURCE_RUST_PATH = new.env(parent = emptyenv())

SOURCE_RUST_PATH$obj = NULL