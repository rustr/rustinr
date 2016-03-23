#' Create a Skeleton for a New Source Package
#'
#' @param name character string: the package name and directory name for your package.
#' @param path path to put the package directory in.
#' @param force If FALSE will not overwrite an existing directory.
#' @param code_files a character vector with the paths to R code files to build the package around.
#' @param rust_files rust files
#' @param author author
#' @param maintainer maintainer
#' @param email email
#' @param license license
#' @export
rustr_init <- function(name,
                       path = ".",
                       force = FALSE,
                       code_files = character(),
                       rust_files = character(),
                       author = "Your Name",
                       maintainer = if (missing(author))
                           "Your Name"
                       else
                           author,
                       email = "your@email.com",
                       license = "MIT") {
    if (!is.character(rust_files))
        stop("'rust_files' must be a character vector")

    hienv = new.env(parent = emptyenv())
    hienv$hi = function() {
        "hi"
    }
    tryCatch(
        package.skeleton(
            name = name,
            environment = hienv,
            path = path,
            force = force,
            code_files = code_files
        ),
        error = function(e) {
            stop(sprintf(
                "error while calling `package.skeleton` : %s",
                conditionMessage(e)
            ))
        }
    )

    root <- file.path(path, name)

    DESCRIPTION <- file.path(root, "DESCRIPTION")
    if (file.exists(DESCRIPTION)) {
        x <- read.dcf(DESCRIPTION)
        x[, "Author"] <- author
        x[, "Maintainer"] <- sprintf("%s <%s>", maintainer, email)
        x[, "License"] <- license
        x = cbind(x, SystemRequirements = "cargo, rustc")
        write.dcf(x, file = DESCRIPTION)
    }

    NAMESPACE <- file.path(root, "NAMESPACE")
    lines <- readLines(NAMESPACE)
    ns <- file(NAMESPACE, open = "w")
    if (!grepl("useDynLib", lines)) {
        lines <- c(sprintf("useDynLib(%s)", name), lines)
        writeLines(lines, con = ns)
        message("added useDynLib to NAMESPACE")
    }
    close(ns)


    package_help_page <-
        file.path(root, "man", sprintf("%s-package.Rd", name))
    if (file.exists(package_help_page)) {
        lines <- readLines(package_help_page)
        lines <-
            gsub("What license is it under?", license, lines, fixed = TRUE)
        lines <-
            gsub(
                "Who to complain to <yourfault@somewhere.net>",
                sprintf("%s <%s>", maintainer, email),
                lines,
                fixed = TRUE
            )
        lines <- gsub("Who wrote it", author, lines, fixed = TRUE)
        writeLines(lines, package_help_page)
    }

    src <- file.path(root, "src")
    if (!file.exists(src)) {
        dir.create(src)
    }
    rustlib_src = file.path(root, "src", "rustlib")
    if (!file.exists(rustlib_src)) {
        dir.create(rustlib_src)
    }
    rust_src_src = file.path(rustlib_src, "src")
    if (!file.exists(rust_src_src)) {
        dir.create(rust_src_src)
    }
    skeleton <- system.file("init", package = "rustinr")

    if (length(rust_files) > 0L) {
        for (file in rust_files) {
            file.copy(file, src)
            message(" >> copied ", file, " to src directory")
        }
    }

    file.copy(file.path(skeleton, "Makevars.win"),
              file.path(src, "Makevars.win"))
    file.copy(file.path(skeleton, "Makevars"),
              file.path(src, "Makevars"))

    file.copy(file.path(skeleton, "Cargo.toml"),
              file.path(rustlib_src, "Cargo.toml"))

    file.copy(file.path(skeleton, "lib.rs"),
              file.path(rust_src_src, "lib.rs"))

    manu <- file.path(root, "man")
    suppressWarnings(file.remove(file.path(manu, "hi.Rd")))
    suppressWarnings(file.remove(file.path(manu, paste0(name,"-package.Rd"))))
    rustrize(root)

    invisible(NULL)
}
