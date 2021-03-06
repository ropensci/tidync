% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidync_data.R
\name{print.tidync_data}
\alias{print.tidync_data}
\title{Print tidync data}
\usage{
\method{print}{tidync_data}(x, ...)
}
\arguments{
\item{x}{'tidync_data' object (from \code{\link[=hyper_array]{hyper_array()}})}

\item{...}{reserved args}
}
\value{
the input object invisibly
}
\description{
Print method for the 'tidync_data' list of arrays returned by \code{\link[=hyper_array]{hyper_array()}}.
}
\details{
The output lists the variables and their dimensions of an object from a
previous call to \code{\link[=tidync]{tidync()}}, and possibly \code{\link[=hyper_filter]{hyper_filter()}}. The available
data will differ from the source in terms of variables (via \code{select_var} in
\link{hyper_array}) and the lengths of each dimension (via named expressions in
\code{\link[=hyper_filter]{hyper_filter()}}).
}
\examples{
argofile <- system.file("extdata/argo/MD5903593_001.nc", package = "tidync")
argodata <- tidync(argofile) \%>\% hyper_filter(N_LEVELS = index < 5) \%>\% 
              hyper_array(select_var = c("TEMP_ADJUSTED", "PRES"))
print(argodata)
}
\seealso{
tidync_data
}
