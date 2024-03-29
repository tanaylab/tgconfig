% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tgconfig.R
\name{register_params}
\alias{register_params}
\title{Register parameters from config file}
\usage{
register_params(config_file, package = NULL, override = FALSE)
}
\arguments{
\item{config_file}{yaml file with parameters and values}

\item{package}{package}

\item{override}{override current loaded parameters}
}
\description{
Register parameters from config file
}
\examples{
config_file <- example_config_file()
register_params(config_file, 'tgconfig')
get_package_params('tgconfig')

}
