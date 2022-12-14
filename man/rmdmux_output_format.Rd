% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmdmux.R
\name{rmdmux_get_output_format}
\alias{rmdmux_get_output_format}
\alias{rmdmux_is_paged}
\alias{rmdmux_is_plain}
\title{Get the actual \verb{output format} when knitting the document}
\usage{
rmdmux_get_output_format()

rmdmux_is_paged()

rmdmux_is_plain()
}
\description{
The following function can be executed inside of an Rmd chunk in order to
specify the output format which is currently rendered:
\itemize{
\item \code{rmdmux_get_output_format()}: Returns a detailed string holding either the
value \code{"html_rmdmux_paged"} or \code{"html_rmdmux_plain"} (or \code{NULL} if neither
applies).
\item \code{rmdmux_is_paged()}: Returns a logical flag, defining if the current output
format is \code{html_rmdmux_paged}
\item \code{rmdmux_is_paged()}: Returns a logical flag, defining if the current output
format is \code{html_rmdmux_plain}
}
}
