#' Use InteractiveFixedEffectModels.jl to run large fixed effect models in julia
#'
#' \code{FixedEffectInteract} returns the results of a 2 stage linear fixed effect regression
#'
#' @param dt        dataset of interest
#' @param lhs       String: lhs variable
#' @param rhs       String: rhs variables
#' @param ife       String of interacted fixed effects (id1 + id2)
#' @param rank_ife  Integer: number of factors to be estimated
#' @param fe        expression of fixed effects id1 + id2:id3
#' @param weights   expression of weights
#' @param vcov      Specification for the error
#' @param save_res  Save the results of the model
#' @param print     do we print output or not
#'
#' @return The return value will be a list which contains two elements at this point
#'   results: includes most of the observation from the julia call
#'   summary: includes information that is of importance to write a table
#'
#' @examples
#' # See vignettes and readme
#' # Short example
#' \dontrun{
#'   df = Ecdat::Cigar
#'   FixedEffectInteract(df, "sales", "price", "state+year", 2, "state", vcov = "robust")
#' }
#'
#' @export
#####################################################################################################################
FixedEffectInteract <- function(
  dt,
  lhs,
  rhs,
  ife,
  rank_ife,
  fe       = NULL,
  weights  = NULL,
  vcov     = NULL,
  save_res = TRUE,    # do we save PCs, loadings and fixed effects
  print    = TRUE
){

  # parse the rhs, fe and cluster
  if (is.null(rhs)){ rhs <- "0" }
  rhs_split <- unlist( stringr::str_split(rhs, "\\+") )
  rhs_split <- unique( stringr::str_replace_all(rhs_split, " ", "") )

  fe_split <- unlist( stringr::str_split(fe, "\\+") )
  n_fe = length(fe_split)
  fe_split <- unlist( stringr::str_split(fe_split, "\\:") )
  fe_split <- unique( stringr::str_replace_all(fe_split, " ", "") )

  ife_split <- unlist( stringr::str_split(ife, "\\+") )
  n_ife = length(ife_split)
  ife_split <- unlist( stringr::str_split(ife_split, "\\:") )
  ife_split <- unique( stringr::str_replace_all(ife_split, " ", "") )



  cluster_split <- gsub("cluster", "",
                        gsub("[()]", "",
                             gsub("robust", "", vcov) ) )
  cluster_split <- gsub(" ", "",
                        unlist( stringr::str_split(cluster_split, "\\+") ) )

  # set the formula for julia
  julia_formula  = paste(lhs, "~", rhs)

  # set the ife
  julia_ife = paste0("ife = ( ", ife, ", ", rank_ife, " )")

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


  julia_regcall = paste("reg_res = regife(df_julia, @model(",
                        paste(c(julia_formula, julia_ife, julia_reg_opt), collapse = ", "),
                        ") );")

  # move only the right amount of data into julia (faster)
  col_keep = intersect(names(dt), c(lhs, rhs_split, ife_split, fe_split, cluster_split, weights))
  dt <- data.table(dt)[, c(col_keep), with = F ]
  # fill NAs on lhs, sometimes object passed onto julia ends having NaN instead of missing
  for ( lhs_iter in seq(1, length(lhs)) ){
    dt[ !is.finite(get(lhs[lhs_iter])), c(lhs[lhs_iter]) := NA ]
    # dt_tmp[ !is.finite(dt_tmp[[lhs[lhs_iter]]]), c(lhs[lhs_iter]) := NA ] # Alternative writing in data.table
  }
  dt_julia <- JuliaObject(dt)
  julia_assign("df_julia", dt_julia)

  # create pooled fe variables in the dataset
  for (iter in seq(1, length(ife_split))){
    pool_cmd = paste0("df_julia[:", ife_split[iter], "]",
                      " = categorical(df_julia[:",
                      ife_split[iter], "]);")
    julia_command(pool_cmd)
  }
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


  # ----------------------------
  # Run the regression
  julia_command(julia_regcall)
  # ----------------------------


  # ----------------------------
   # return results in R: coefficient results if there is a rhs
  if (rhs != "0"){

    list_tmp <- list() # initialize result is a list

    jl_coefficients    = julia_eval(paste0("reg_res.coef"))
    names(jl_coefficients) = julia_eval(paste0("reg_res.coefnames"))
    list_tmp$coefficients = jl_coefficients

    list_tmp$lhs        = julia_eval(paste0("reg_res.yname"))
    list_tmp$julia_call = julia_regcall
    list_tmp$se         = julia_eval(paste0("stderror(reg_res)"))
    list_tmp$tt         = julia_eval(paste0("coef(reg_res) ./ stderror(reg_res)"))
    list_tmp$pvalues    = julia_eval(paste0("coeftable(reg_res).mat[:,4]"))
    list_tmp$ci         = julia_eval(paste0("confint(reg_res)"))
    list_tmp$coefnms    = julia_eval(paste0("coefnames(reg_res)"))
    list_tmp$nobs       = julia_eval(paste0("reg_res.nobs"))   # number of observations
    list_tmp$r2         = list(r2          = julia_eval(paste0("reg_res.r2")),
                               r2_adjusted = julia_eval(paste0("reg_res.r2_a")),
                               r2_within   = julia_eval(paste0("reg_res.r2_within")) )
    list_tmp$statistics = list(F_stat = NA,
                               pvalue = list_tmp$pvalues)

    # Coeftest class is easy: only need coefficients
    Rcoef = matrix(c(jl_coefficients, list_tmp$se, list_tmp$tt, list_tmp$pvalues),
                   nrow=length(list_tmp$coefnms), ncol=4, byrow = F)
    colnames(Rcoef) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    rownames(Rcoef) = list_tmp$coefnms
    class(Rcoef) = "coeftest"
    list_tmp$coeftest = Rcoef
    coef_list <- list_tmp

  }

  # output dataset (that keeps loading and PCs)
  augment <- julia_eval("getfield(reg_res, :augmentdf)")
  setDT(augment)

  # ----------------------------
  # output results (nothing saved yet)
  if (print == TRUE){
    reg_list  <- julia_eval(paste0("reg_res"))
    message(reg_list)
  }

  # ----------------------------
  # return both all of the regression and coefficient subset
  if (rhs != "0"){
    return(list(statistics = coef_list,
                summary    = coef_list$coeftest,
                dt_augment = augment     # NB empty is save_res = FALSE
           ))
  } else if (rhs == "0"){
    return(list(dt_augment = augment))
  }


} # end of FixedEffectInteract
# ----------------------------------------------------------------------------------------------

