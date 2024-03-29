% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mgl_to_meq.R
\name{mgl_to_meq}
\alias{mgl_to_meq}
\title{Convert analyses in mg/L to meq/L.}
\usage{
mgl_to_meq(data, ions)
}
\arguments{
\item{data}{A dataframe with ions as columns.}

\item{ions}{A string vector of ions to convert.}
}
\value{
A dataframe with a new columns of each ion in meq/L.
}
\description{
Convert analyses in mg/L to meq/L.
}
\examples{
data(pc_data)

mgl_to_meq(pc_data, ions = c("Ca", "Mg", "Na", "K", "SO4", "Cl", "HCO3"))
}
