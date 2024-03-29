% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_window_suffix.R
\name{get_window_suffix}
\alias{get_window_suffix}
\title{Get window suffix}
\source{
\code{
dat <- echodata::BST1
window_suffix <- echoplot:::get_window_suffix(dat=dat, zoom=1000)
window_suffix <- echoplot:::get_window_suffix(dat=dat, zoom=NULL)
window_suffix <- echoplot:::get_window_suffix(dat=dat, zoom="all")
window_suffix <- echoplot:::get_window_suffix(dat=dat, zoom="2x")
}
}
\usage{
get_window_suffix(dat, zoom, verbose = TRUE)
}
\description{
Determine the plot suffix indicating its window size.
}
\seealso{
Other plot: 
\code{\link{add_multitrack_lines}()},
\code{\link{dot_summary_plot}()}
}
\concept{plot}
\keyword{internal}
