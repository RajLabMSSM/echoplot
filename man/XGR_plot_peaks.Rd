% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XGR_plot_peaks.R
\name{XGR_plot_peaks}
\alias{XGR_plot_peaks}
\title{Plot XGR peaks}
\usage{
XGR_plot_peaks(
  gr.lib,
  dat,
  fill_var = "Assay",
  facet_var = "Source",
  geom = "density",
  locus = NULL,
  adjust = 0.2,
  show_plot = TRUE,
  legend = TRUE,
  as_ggplot = TRUE,
  trim_xlims = FALSE
)
}
\arguments{
\item{gr.lib}{\code{GRanges} object of annotations.}

\item{dat}{Data.frame with at least the following columns:
\describe{
\item{SNP}{SNP RSID}
\item{CHR}{chromosome}
\item{POS}{position}
}}

\item{fill_var}{Fill variable.}

\item{facet_var}{Row facet variable.}

\item{geom}{Plot type ("density", or "histogram").}

\item{locus}{[Optional] Locus name.}

\item{adjust}{The granularity of the peaks.}

\item{show_plot}{Print the plot.}

\item{legend}{
    A logical value indicates whether to show legend or not. Default is
    \code{TRUE}
  }

\item{as_ggplot}{Return plot as \code{ggplot2} 
(\code{TRUE}) or \code{Tracks} (\code{FALSE}) object.}

\item{trim_xlims}{Trim the x-axis limits.}
}
\value{
\code{ggbio} track plot.
}
\description{
Plots the distribution of annotations across a genomic region (x-axis).
}
\examples{
#### Import example query ####
gr.lib <- echoannot::xgr_example

#### Filter query ####
gr.filt <- echoannot::XGR_filter_sources(gr.lib = gr.lib)
gr.filt <- echoannot::XGR_filter_assays(gr.lib = gr.filt)

#### Plot query #####
XGR_track <- echoplot::XGR_plot_peaks(
    gr.lib = gr.filt,
    dat = echodata::BST1,
    fill_var = "Assay",
    facet_var = "Source")
}
\concept{XGR}
