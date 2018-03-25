#' Use FixedEffectModels.jl to run large fixed effect models in julia
#'
#' \code{FixedEffect} returns the results of a linear fixed effect regression
#'
#' @param dt        dataset of interest
#' @param lhs       String, Y regression variable
#' @param rhs       String, X formula
#' @param fe        Fixed effects
#' @param weights   Regression weights
#' @param vcov      Specification for the error
#' @param save_res  Do we save the residuals
#'
#' @return The return value will be a list which contains two elements at this point
#'   results: includes most of the observation from the julia call
#'   summary: includes information that is of importance to write a table
#'
#' @examples See vignettes and readme
#'
#' @export
#####################################################################################################################
FixedEffectIV <- function(dt,
                          lhs,
                          rhs,
                          iv,
                          fe       = NULL,
                          weights  = NULL,
                          vcov     = NULL,
                          save_res = FALSE,    # do we save residuals and fixed effects
                          print    = TRUE
){




}
