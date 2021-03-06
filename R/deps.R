
#' Recursive (hard) dependencies of CRAN packages
#'
#' @param packages Character vector of package names.
#' @return A character vector of package names and versions,
#'   separated by a single dash.
#'
#' @keywords internal

get_cran_deps <- function(packages) {

  if (length(packages) == 0) {
    character()
  }

  db <- utils::available.packages()

  deps <- tools::package_dependencies(
    packages,
    recursive = TRUE,
    which = c("Depends", "Imports", "LinkingTo"),
    db = db
  )

  # Get rid of base packages
  deps <- unique(c(packages, unlist(deps, use.names = FALSE)))
  base <- rownames(utils::installed.packages(priority = "base"))
  deps <- setdiff(deps, base)

  deps
}

#' Recursive (hard) dependencies of remote packages
#'
#' @param remote_packages Character vector of package names.
#' @return A character vector of package names and versions,
#'   separated by a single dash.
#'
#' @keywords internal
get_remote_deps <- function(remote_packages, process = process_remotes()) {
  remote_deps <- data.frame(
    type    = character(),
    package = character(),
    version = character()
  )
  if (!length(remote_packages)) {
    return(remote_deps)
  }
  for (remote_package in remote_packages) {
    remote_package <- utils::tail(strsplit(remote_package, "::", fixed = TRUE)[[1]], 1)
    repo_spec <- remotes::parse_repo_spec(remote_package)
    repo_spec <- repo_spec[nchar(repo_spec) > 0L]
    dsc <- desc::desc(text = do.call(remotes:::github_DESCRIPTION, repo_spec))
    remote_deps <- rbind(
      remote_deps,
      dsc$get_deps(),
      get_remote_deps(
        process$unprocessed(
          dsc$get("Package"),
          dsc$get_remotes()
        ),
        process
      )
    )
  }
  remote_deps
}

process_remotes <- function() {
  processed_remotes <- character()
  list(
    unprocessed = function(pkg, rmt) {
      on.exit({
        unq <- unique(rmt)
        processed_remotes <<- c(
          processed_remotes,
          setNames(unq, rep(pkg, length(unq)))
        )
      })
      if (any(processed_remotes %in% rmt)) {
        warning("Recursive remotes. Printing remotes dependency tree. \n",
          paste0("package : ", names(processed_remotes),
                 " // remote : ", processed_remotes
                 , collapse = "\n"),
          immediate. = TRUE
        )
      }
      rmt[!rmt %in% processed_remotes]
    },
    processed = function() {
      processed_remotes
    }
  )
}
