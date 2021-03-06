% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.R, R/sysreqs.R
\docType{package}
\name{sysreqs}
\alias{sysreqs}
\title{Install SystemRequirements of Packages}
\usage{
sysreqs(
  desc,
  platform = current_platform(),
  soft = TRUE,
  remotes = FALSE,
  pkgs = character()
)
}
\arguments{
\item{desc}{Path to a \code{DESCRIPTION} file.}

\item{platform}{Platform string, defaults to the current platform.}

\item{soft}{Whether to include soft dependencies.}

\item{remotes}{Whether to include remotes dependencies.}

\item{pkgs}{Additional package names.}
}
\value{
All system dependencies on the current or the specified
  platform.
}
\description{
Automatically download and install system requirements of R packages.
}
