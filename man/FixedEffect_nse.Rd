% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FixedEffectModels.R
\name{FixedEffect_nse}
\alias{FixedEffect_nse}
\title{Use FixedEffectModels.jl to run large fixed effect models in julia on multiple models}
\usage{
FixedEffect_nse(dt, formula, fe = NULL, weights = NULL, vcov = NULL,
  ...)
}
\arguments{
\item{dt}{dataset of interest}

\item{formula}{formula of the y ~ x1 + x2 type}

\item{fe}{expression of fixed effects id1 + id2:id3}

\item{weights}{expression of weights}

\item{vcov}{error types, expression either robust or cluster(id1)}

\item{...}{Variables that we pass by to FixedEffect function like print and save_res}
}
\value{
The return value will be a list which contains two elements at this point
  results: includes most of the observation from the julia call
  summary: includes information that is of importance to write a table
}
\description{
\code{FixedEffect_nse} returns the results of a linear fixed effect regression
}
\examples{
# See vignettes and readme
\dontrun{
  FixedEffect_nse(y ~ x1 + x2, id1 + id2, cluster(id1), save = T)
}

}
