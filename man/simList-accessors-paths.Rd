% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simList-accessors.R
\docType{methods}
\name{paths}
\alias{cachePath}
\alias{cachePath,.simList-method}
\alias{cachePath<-}
\alias{cachePath<-,.simList-method}
\alias{inputPath}
\alias{inputPath,.simList-method}
\alias{inputPath<-}
\alias{inputPath<-,.simList-method}
\alias{modulePath}
\alias{modulePath,.simList-method}
\alias{modulePath<-}
\alias{modulePath<-,.simList-method}
\alias{outputPath}
\alias{outputPath,.simList-method}
\alias{outputPath<-}
\alias{outputPath<-,.simList-method}
\alias{paths}
\alias{paths,.simList-method}
\alias{paths<-}
\alias{paths<-,.simList-method}
\alias{simList-accessors-paths}
\title{Specify paths for modules, inputs, and outputs}
\usage{
paths(object)

\S4method{paths}{.simList}(object)

paths(object) <- value

\S4method{paths}{.simList}(object) <- value

cachePath(object)

\S4method{cachePath}{.simList}(object)

cachePath(object) <- value

\S4method{cachePath}{.simList}(object) <- value

inputPath(object)

\S4method{inputPath}{.simList}(object)

inputPath(object) <- value

\S4method{inputPath}{.simList}(object) <- value

outputPath(object)

\S4method{outputPath}{.simList}(object)

outputPath(object) <- value

\S4method{outputPath}{.simList}(object) <- value

modulePath(object)

\S4method{modulePath}{.simList}(object)

modulePath(object) <- value

\S4method{modulePath}{.simList}(object) <- value
}
\arguments{
\item{object}{A \code{simList} simulation object.}

\item{value}{The object to be stored at the slot.}
}
\value{
Returns or sets the value of the slot from the \code{simList} object.
}
\description{
Accessor functions for the \code{paths} slot in a \code{simList} object.
}
\details{
These are ways to add or access the file paths used by \code{\link{spades}}.
There are four file paths: \code{cachePath}, \code{modulePath},
\code{inputPath}, and \code{outputPath}.
Each has a function to get or set the value in a \code{simList} object.
When not otherwise specified, the default is to set the path values to the
current working directory.

\tabular{lll}{
   \code{cachePath} \tab \code{NA} \tab Global simulation cache path.\cr
   \code{modulePath} \tab \code{NA} \tab Global simulation module path.\cr
   \code{inputPath} \tab \code{NA} \tab Global simulation input path.\cr
   \code{outputPath} \tab \code{NA} \tab Global simulation output path.\cr
   \code{paths} \tab \code{NA} \tab Global simulation paths (cache, modules, inputs, outputs).\cr
}
}
\seealso{
\code{\link{simList-class}},
         \code{\link{simList-accessors-envir}},
         \code{\link{simList-accessors-events}},
         \code{\link{simList-accessors-inout}},
         \code{\link{simList-accessors-modules}},
         \code{\link{simList-accessors-objects}},
         \code{\link{simList-accessors-params}},
         \code{\link{simList-accessors-times}}.
}

