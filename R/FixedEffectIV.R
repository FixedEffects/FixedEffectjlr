#' Use FixedEffectModelsIV.jl to run large fixed effect models in julia
#'
#' \code{FixedEffectIV_nse} returns the results of a 2 stage linear fixed effect regression
#'
#' @param dt        dataset of interest
#' @param formula   formula of the y ~ x1 + x2 type
#' @param iv        formula endogenous on exogenous variable
#' @param fe        expression of fixed effects id1 + id2:id3
#' @param vcov      error types, expression either robust or cluster(id1)
#' @param weights   expression of weights
#' @param save_res  Do we save the residuals
#' @param print     Do we print the results (default is yes)
#'
#' @return The return value will be a list which contains two elements at this point
#'   results: includes most of the observation from the julia call
#'   summary: includes information that is of importance to write a table
#'
#' @examples
#'   # See vignettes and readme
#'
#' @export
#####################################################################################################################
FixedEffectIV_nse <- function(
  dt,
  formula,
  iv,
  fe       = NULL,
  weights  = NULL,
  vcov     = NULL,
  save_res = FALSE,    # do we save residuals and fixed effects
  print    = TRUE
){

  # Parse the formulas
  lhs = as.character(formula[[2]])
  rhs <- unlist(stringr::str_split(as.character(formula[[3]]), "\\+"))
  rhs <- paste(gsub(" ", "", rhs)[stringr::str_length(rhs)>0], collapse = " + ")
  fe  <- deparse(substitute(fe))
  weights = deparse(substitute(weights))
  if (weights == "NULL"){ weights = NULL }
  vcov    = deparse(substitute(vcov))
  if (vcov == "NULL"){ vcov = NULL }

  # parse the rhs, fe and cluster
  rhs_split <- unlist( stringr::str_split(rhs, "\\+") )
  rhs_split <- unique( stringr::str_replace_all(rhs_split, " ", "") )

  fe_split <- unlist( stringr::str_split(fe, "\\+") )
  n_fe = length(fe_split)
  fe_split <- unlist( stringr::str_split(fe_split, "\\:") )
  fe_split <- unique( stringr::str_replace_all(fe_split, " ", "") )

  cluster_split <- gsub("cluster", "",
                        gsub("[()]", "",
                             gsub("robust", "", vcov) ) )
  cluster_split <- gsub(" ", "",
                        unlist( stringr::str_split(cluster_split, "\\+") ) )

  # set the formula for julia
  julia_formula  = paste(lhs, "~", rhs)

  # set the iv formula
  endogenous_split =  unlist(stringr::str_split(as.character(iv[[2]]), "\\+"))
  endogenous_split = gsub(" ", "", endogenous_split)[stringr::str_length(endogenous_split)>0]
  endogenous_var   = paste(endogenous_split, collapse = " + ")
  instrument_split = unlist(stringr::str_split(as.character(iv[[3]]), "\\+"))
  instrument_split = gsub(" ", "", instrument_split)[stringr::str_length(instrument_split)>0]
  instrument_var   = paste(instrument_split, collapse = " + ")
  julia_iv         = paste("(", endogenous_var, "~", instrument_var, ")")
  julia_formula    = paste(julia_formula, "+", julia_iv)

  # set the options
  julia_reg_fe   = paste("fe =", stringr::str_replace_all(fe, "\\:", "\\&"))
  if (!is.null(weights)){
    if (stringr::str_length(weights)>0){
      julia_reg_fe   = paste(julia_reg_fe, ", weights =", weights)
    } }
  if (is.null(vcov)){  # default to robust
    vcov <- "robust"
  } else if (!stringr::str_detect(vcov, "cluster")) {
    vcov <- "robust"
  }
  julia_reg_vcov = paste("vcov = ", vcov)
  julia_reg_save = paste("save = ", ifelse(save_res, "true", "false"))
  julia_reg_opt  = paste(c(julia_reg_fe, julia_reg_vcov, julia_reg_save),
                         collapse = ", ")

  julia_regcall = paste("reg_res = reg(df_julia, @model(",
                        paste(c(julia_formula, julia_reg_opt), collapse = ", "),
                        ") );")

  # move only the right amount of data into julia (faster)
  col_keep = unique(intersect(
    names(dt),
    c(lhs, rhs_split, endogenous_split, instrument_split, fe_split, cluster_split, weights)))
  dt <- data.table(dt)[, c(col_keep), with = F ]
  # fill NAs on lhs, sometimes object passed onto julia ends having NaN instead of missing
  for ( lhs_iter in seq(1, length(lhs)) ){
    dt[ !is.finite(get(lhs[lhs_iter])), c(lhs[lhs_iter]) := NA ]
    # dt_tmp[ !is.finite(dt_tmp[[lhs[lhs_iter]]]), c(lhs[lhs_iter]) := NA ] # Alternative writing in data.table
  }
  dt_julia <- JuliaObject(dt)
  julia_assign("df_julia", dt_julia)

  # create pooled fe variables in the dataset
  for (iter in seq(1, length(fe_split))){
    pool_cmd = paste0("df_julia[:", fe_split[iter], "]",
                      " = categorical(df_julia[:",
                      fe_split[iter], "]);")
    julia_command(pool_cmd)
  }
  if ( stringr::str_length(paste(cluster_split, collapse="")) > 0 ){
    for (iter in seq(1, length(cluster_split))){
      pool_cmd = paste0("df_julia[:", cluster_split[iter], "]",
                        " = categorical(df_julia[:",
                        cluster_split[iter], "]);")
      julia_command(pool_cmd)
    }
  }

  # Run the regression
  julia_command(julia_regcall)

  # BUILD COEFFICIENT TABLE LIST
  ct <- list()   # CoefTable2
  julia_eval("jl_table = coeftable(reg_res);")
  ct$ctitle   = julia_eval("title(reg_res)")
  ct$ctop     = julia_eval("top(reg_res)")
  ct$cc       = julia_eval("coef(reg_res)")             # coefficients
  ct$se       = julia_eval("stderror(reg_res)")
  ct$tt       = julia_eval("coef(reg_res) ./ stderror(reg_res)")
  ct$pvalues  = julia_eval("coeftable(reg_res).mat[:,4]")
  ct$coefnms  = julia_eval("coefnames(reg_res)")
  ct$conf_int = julia_eval("confint(reg_res)")
  ct$mat      = julia_eval("jl_table.mat")
  ct$colnms   = julia_eval("jl_table.colnms")
  ct$rownms   = julia_eval("jl_table.rownms")
  ct$pvalcol  = julia_eval("jl_table.pvalcol")

  # Coeftest class is easy: only need coefficients
  Rcoef = matrix(c(ct$cc, ct$se, ct$tt, ct$pvalues),
                 nrow=length(ct$coefnms), ncol=4, byrow = F)
  colnames(Rcoef) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(Rcoef) = ct$coefnms
  class(Rcoef) = "coeftest"
  ct$coeftest = Rcoef

  # OUTPUT
  if (print == T){
    julia_command("reg_res")
  }

  # RETURN julia reg
  jl_obj = julia_eval("reg_res")

  return(list(results = jl_obj, summary = ct))

}
