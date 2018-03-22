.FixedEffect <- new.env(parent = emptyenv())



#' Use Ipopt solver to solve optimization problems.
#'
#' \code{IPOPT} returns the solution to the optimization problem found by Ipopt solver.
#'
#' @param dt        dataset of interest
#' @param lhs       String, Y regression variable
#' @param rhs       String, X formula
#' @param fe        Fixed effects
#' @param weights   Regression weights (not yet coded)
#' @param vcov      Specification for the error
#' @param save_res  Do we save the residuals (not yet coded)
#'
#' @return The return value will be a list which contains two elements at this point
#'   results: includes most of the observation from the julia call
#'   summary: includes information that is of importance to write a table
#'
#' @examples See vignettes and readme
#'
#' @export
#####################################################################################################################
FixedEffect <- function(dt,
                        lhs,
                        rhs,
                        fe       = NULL,
                        weights  = NULL,
                        vcov     = NULL,
                        save_res = FALSE,    # do we save residuals
                        print    = TRUE
){


  # 1. convert the dataset
  dt_julia <- JuliaObject(dt)
  julia_assign("dt_julia", dt_julia)

  # parse the fixed effects and convert to pooled
  fe_split <- unlist( stringr::str_split(fe, "\\+") )
  fe_split <- unlist( stringr::str_split(fe_split, "\\:") )
  fe_split <- unique( stringr::str_replace_all(fe_split, " ", "") )
  # create pooled fe variables in the dataset
  for (iter in seq(1, length(fe_split))){
    pool_cmd = paste0("dt_julia[:", fe_split[iter], "]",
                      " = categorical(dt_julia[:",
                      fe_split[iter], "]);")
    julia_command(pool_cmd)
  }

  cluster_split <- gsub("cluster", "",
                     gsub("[()]", "",
                       gsub("robust", "", vcov) ) )
  cluster_split <- gsub(" ", "",
                     unlist( stringr::str_split(cluster_split, "\\+") ) )
  if ( stringr::str_length(paste(cluster_split, collapse="")) > 0 ){
    for (iter in seq(1, length(cluster_split))){
      pool_cmd = paste0("dt_julia[:", cluster_split[iter], "]",
                        " = categorical(dt_julia[:",
                        cluster_split[iter], "]);")
      julia_command(pool_cmd)
    }
  }


  # set the formula for julia
  julia_formula  = paste(lhs, "~", rhs)

  # set the options
  julia_reg_fe   = paste("fe =", stringr::str_replace_all(fe, "\\:", "\\&"))
  julia_reg_wg   = paste("weights =", weights)
  julia_reg_vcov = paste("vcov = ", vcov)
  julia_reg_save = paste("save = ", ifelse(save_res, "true", "false"))
  julia_reg_opt  = paste(c(julia_reg_fe, julia_reg_wg, julia_reg_vcov, julia_reg_save),
                         collapse = ", ")

  julia_regcall = paste("reg_res = reg(dt_julia, @model(",
                        paste(c(julia_formula, julia_reg_opt), collapse = ", "),
                        ") );")

  # Run the regression
  julia_command(julia_regcall)

  # Return some useful information "close to lm"
  z <- list()

  # coefficients
  jl_coefficients    = julia_eval("reg_res.coef")
  names(jl_coefficients) = julia_eval("reg_res.coefnames")
  z$coefficients = jl_coefficients
  # residuals
  ## z$residuals = julia_eval("residuals(reg_res, dt_julia_mp)") # only if necessary
  # effects
  ## z$effects = c(NA)
  # rank
  z$rank = julia_eval("reg_res.nobs - reg_res.df_residual")
  # fitted.values
  ## z$fitted.values = c(NA)
  # assign
  ## z$assign = c(NA)
  # qr
  ## z$qr = c(NA)
  # df.residual
  jl_df.residual = julia_eval("reg_res.df_residual")
  z$df.residual  = jl_df.residual
  # xlevels
  ## z$xlevels <- list()
  ## names(z$xlevels) <- c()
  # call
  if (grep("cluster", vcov)){
    cluster_formula <- gsub("cluster", "",
                         gsub("[()]", "", vcov) )
  } else {
    cluster_formula <- "0"
  }
  R_call = paste(julia_formula, "|", fe, "| 0 |", cluster_formula)
  z$call = list(R_call = as.formula(R_call), julia_call = julia_regcall)
  # terms

  # other stuff
  z$nobs = julia_eval("reg_res.nobs")   # number of observations

  z$r2    = list(r2 = julia_eval("reg_res.r2"),
                 r2_adjusted = julia_eval("reg_res.r2_a"),
                 r2_within = julia_eval("reg_res.r2_within"))
  z$statistics = list(F_stat = julia_eval("reg_res.F"),
                      pvalue = julia_eval("reg_res.p"))

  z$convergence = list(julia_eval("reg_res.iterations"),
                       julia_eval("reg_res.converged"))

  z$se = julia_eval("stderr(reg_res)")
  z$ci = julia_eval("confint(reg_res)")

  class(z) <- 'lm'

  # Return somthing else close to summary
  if (print == TRUE){
    julia_eval("show(reg_res);")
  }

  # BUILD COEFFICIENT TABLE LIST
  ct <- list()   # CoefTable2




  julia_eval("jl_table = coeftable(reg_res);")

  ct$ctitle   = julia_eval("title(reg_res)")
  ct$ctop     = julia_eval("top(reg_res)")
  ct$cc       = julia_eval("coef(reg_res)")             # coefficients
  ct$se       = julia_eval("stderr(reg_res)")
  ct$coefnms  = julia_eval("coefnames(reg_res)")
  ct$conf_int = julia_eval("confint(reg_res)")
  ct$tt       = julia_eval("coef(reg_res) ./ stderr(reg_res)")

  ct$mat = julia_eval("jl_table.mat")
  ct$colnms  = julia_eval("jl_table.colnms")
  ct$rownms  = julia_eval("jl_table.rownms")
  ct$pvalcol = julia_eval("jl_table.pvalcol")

  # ---------------------------------------------------------------------------------
  return(list(results = z,
              summary = ct))


}







