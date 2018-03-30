.FixedEffect <- new.env(parent = emptyenv())

#' Do initial setup for FixedEffectjlr package.
#'
#' \code{FixedEffect_setup} does the initial setup for ipoptjlr package.
#'
#' @param ... arguments passed to \code{JuliaCall::julia_setup}.
#'
#' @examples
#' \dontrun{
#' # FixedEffect_setup()
#' }
#'
#' @export
FixedEffect_setup <- function(...) {
  .FixedEffect$julia <- JuliaCall::julia_setup(...)
  .FixedEffect$julia$install_package_if_needed("FixedEffectModels")
  .FixedEffect$julia$library("FixedEffectModels")
  .FixedEffect$julia$install_package_if_needed("InteractiveFixedEffectModels")
  .FixedEffect$julia$library("InteractiveFixedEffectModels")
  .FixedEffect$julia$library("StatsModels")
  .FixedEffect$julia$library("Distributions")
  .FixedEffect$julia$library("DataFrames")
}
