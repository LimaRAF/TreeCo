% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/refString.R
\name{refString}
\alias{refString}
\title{Build TreeCo Reference String}
\usage{
refString(
  ref.data = NULL,
  col.names = NULL,
  default.ref.type = "article",
  to.upper = FALSE
)
}
\arguments{
\item{ref.data}{the data frame with reference database}

\item{col.names}{a named list conating the name of the columns needed (see
details)}

\item{default.ref.type}{a character containing the default type of reference
to be assumed when the reference type is missing. Defaults to 'article'.}

\item{to.upper}{should the names of the authors be capitalized. Defaults to
FALSE}
}
\value{
the data frame \code{ref.data} with and extra column
  containing the reference string in the standard TreeCo format.
}
\description{
Function to build the standard TreeCo reference string based on
reference information (i.e. metadata)
}
\details{
The standard TreeCo format for the references is adapted
  from the Chicago citation style. Currently, this is the only
  citation format available. The general format for an article is:
 
  AUTHORS(s). Year. Title. Publication name volume (issue): start page-end page.

  The separation between multiple authors is done using semi-colons.
}
\seealso{
\link[TreeCo]{refInfo}
}
\author{
Renato A. Ferreira de Lima
}
