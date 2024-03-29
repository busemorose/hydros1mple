% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/km_date_index.R
\name{km_date_index}
\alias{km_date_index}
\title{Get the corresponding KarstMod index to a date}
\usage{
km_date_index(date_begin, date_end)
}
\arguments{
\item{date_begin}{Date object in \%Y-\%m-\%d format.}

\item{date_end}{Date object in \%Y-\%m-\%d format.}
}
\value{
A value corresponding to the index.
}
\description{
Get the corresponding KarstMod index to a date
}
\examples{
km_date_index(as.Date("2021-01-01"), as.Date("2022-01-01"))
}
