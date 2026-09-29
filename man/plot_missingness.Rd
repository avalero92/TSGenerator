\name{plot_missingness}
\alias{plot_missingness}
\title{Plot observed and modelled temporal missingness}
\usage{plot_missingness(x, sos = NULL, max_doy = NULL, eos = NULL, facet = TRUE)}
\arguments{\item{x}{A tsg_missingness_model or compatible data frame.}\item{sos,max_doy,eos}{Optional DOY markers.}\item{facet}{Facet by year.}}
\value{A ggplot object.}
\description{Plots observed missingness proportions and GAM predictions without printing automatically.}
