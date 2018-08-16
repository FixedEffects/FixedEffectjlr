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
#' @param print     Do we print the results
#' @param verbose   Do we print status report on what's going on
#'
#' @return The return value will be a list which contains two elements at this point
#'   results: includes most of the observation from the julia call
#'   summary: includes information that is of importance to write a table
#'
#' @examples
#' # See vignettes and readme
#'
#' @export
#####################################################################################################################
FixedEffect <- function(dt,
                        lhs,
                        rhs,
                        fe       = NULL,
                        weights  = NULL,
                        vcov     = NULL,
                        save_res = FALSE,    # do we save residuals and fixed effects
                        print    = TRUE,
                        verbose  = FALSE
){

  # parse the rhs, fe and cluster
  rhs       <- stringr::str_replace_all(rhs, "\\:", "\\&")
  rhs_split <- unlist( stringr::str_split(rhs, "\\+") )
  rhs_split <- unlist( stringr::str_split(rhs_split, "\\&") )
  rhs_split <- unlist( stringr::str_split(rhs_split, "\\*") )
  rhs_split <- unique( stringr::str_replace_all(rhs_split, " ", "") )

  # N.B. on interaction term: do make sure variables have right type already in R!
  fe_interact <- stringr::str_detect(fe, "\\*") | stringr::str_detect(fe, "\\:")   # check if there is an interaction term
  fe_split    <- unlist( stringr::str_split(fe, "\\+") )
  n_fe = length(fe_split)                             # usefule for getfe function in julia
  fe_split <- unlist( stringr::str_split(fe_split, "\\*") )
  fe_split <- unlist( stringr::str_split(fe_split, "\\:") )
  fe_split <- unique( stringr::str_replace_all(fe_split, " ", "") )

  # split all the clustering variables
  cluster_split <- NULL
  if (!is.null(vcov)){
    cluster_split <- gsub("cluster", "",
                       gsub("[()]", "",
                         gsub("robust", "", vcov) ) )
    cluster_split <- gsub(" ", "",
                       unlist( stringr::str_split(cluster_split, "\\+") ) )
  }

  # set the formula for julia
  julia_formula  = paste(lhs, "~", rhs)

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
  col_keep = intersect(names(dt), c(lhs, rhs_split, fe_split, cluster_split, weights))
  dt <- data.table(dt)[, c(col_keep), with = F ]

  # get the types of all the variables: important to define categoricals in julia
  classes <- data.table(colclass = as.character(sapply(dt, class)) )
  classes <- cbind(name = colnames(dt), classes)

  # fill NAs on lhs, sometimes object passed onto julia ends having NaN instead of missing
  for ( lhs_iter in seq(1, length(lhs)) ){
    dt[ !is.finite(get(lhs[lhs_iter])), c(lhs[lhs_iter]) := NA ]
  }
  # for rhs too
  for ( rhs_iter in seq(1, length(rhs_split)) ){
    if (as.logical( sum(rhs_split != "1") ) ){
      if (classes[ name == rhs_split[rhs_iter] ][["colclass"]] %in% c("numeric", "integer") ){
       dt[ !is.finite(get(rhs_split[rhs_iter])), c(rhs_split[rhs_iter]) := NA ]
      }
    }
  }
 # for continuous variables remove the NaN
 for ( fe_iter in seq(1, length(fe_split))){
   if (classes[ name == fe_split[fe_iter]][["colclass"]] %in% c("numeric", "integer") ){
     dt[ !is.finite(get(fe_split[fe_iter])),  c(fe_split[fe_iter]) := NA ]
     if (verbose == T & (nrow(dt[ !is.finite(get(fe_split[fe_iter] ))])>0) ) {
       message("# removing NA for continuous fe variable ... ", fe_split[fe_iter])
     }
   }
 }

  # send the R data.table to julia
  dt_julia <- JuliaObject(dt)
  julia_assign("df_julia", dt_julia)


  # create pooled fe variables in the julia dataset
  for (iter in seq(1, length(fe_split))){
    # convert to categorical if no fe with continuous interaction
    if (!fe_interact | (classes[ name == fe_split[iter]]$colclass == "factor")){
      if (verbose == T){ message("# converting ", fe_split[iter], " to categorical variable") }
      pool_cmd = paste0("df_julia[:", fe_split[iter], "]",
                        " = categorical(df_julia[:",
                        fe_split[iter], "]);")
      julia_command(pool_cmd)
    }
  }
  if ( stringr::str_length(paste(cluster_split, collapse="")) > 0 ){
    for (iter in seq(1, length(cluster_split))){
      if (verbose == T){ message("# converting ", fe_split[iter], " to categorical variable") }
      pool_cmd = paste0("df_julia[:", cluster_split[iter], "]",
                        " = categorical(df_julia[:",
                        cluster_split[iter], "]);")
      julia_command(pool_cmd)
    }
  }

  # Run the regression
  if (verbose == T){
    message("# running regression in FixedEffectModels.jl\n",
            "# julia call is ...\n",
            julia_regcall)
  }
  julia_command(julia_regcall)

  # Return some useful information "close to lm"
  z <- list()

  # coefficients
  jl_coefficients    = julia_eval("reg_res.coef")
  names(jl_coefficients) = julia_eval("reg_res.coefnames")
  z$coefficients = jl_coefficients
  if (save_res == TRUE){
    z$residuals = julia_eval("reg_res.augmentdf[:residuals]")
    z$fitted.values = julia_eval(paste0("df_julia[:", lhs, "] - reg_res.augmentdf[:residuals]"))
  }
  # effects
  ## z$effects = c(NA)
  z$rank = julia_eval("reg_res.nobs - reg_res.df_residual")
  # assign
  ## z$assign = c(NA)
  # qr
  ## z$qr = c(NA)
  jl_df.residual = julia_eval("reg_res.df_residual")
  z$df.residual  = jl_df.residual
  # xlevels
  ## z$xlevels <- list()
  ## names(z$xlevels) <- c()

  if (stringr::str_detect(vcov, "cluster")){
    cluster_formula <- gsub("cluster", "",
                         gsub("[()]", "", vcov) )
  } else {
    cluster_formula <- "0"
  }
  R_call = paste(julia_formula, "|", fe, "| 0 |", cluster_formula)
  z$call = list(R_call = as.formula(R_call), julia_call = julia_regcall)
  # terms
  z$terms = terms(as.formula(R_call))

  # other stuff
  z$nobs = julia_eval("reg_res.nobs")   # number of observations

  z$r2    = list(r2 = julia_eval("reg_res.r2"),
                 r2_adjusted = julia_eval("reg_res.r2_a"),
                 r2_within = julia_eval("reg_res.r2_within"))
  z$statistics = list(F_stat = julia_eval("reg_res.F"),
                      pvalue = julia_eval("reg_res.p"))

  z$convergence = list(julia_eval("reg_res.iterations"),
                       julia_eval("reg_res.converged"))

  z$se = julia_eval("stderror(reg_res)")
  z$ci = julia_eval("confint(reg_res)")

  if (save_res == TRUE){
    z$fe = julia_eval(paste0("reg_res.augmentdf[:, 2:", n_fe+1, "]"))
  }

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

  # -------------------------------------------------------------
  return(list(results = z,
              summary = ct))


} # end of FixedEffect
# ----------------------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------------------
#' Use FixedEffectModels.jl to run large fixed effect models in julia on multiple models
#'
#' \code{FixedEffect_nse} returns the results of a linear fixed effect regression
#'
#' @param dt        dataset of interest
#' @param formula   formula of the y ~ x1 + x2 type
#' @param fe        expression of fixed effects id1 + id2:id3
#' @param weights   expression of weights
#' @param vcov      error types, expression either robust or cluster(id1)
#' @param ...       Variables that we pass by to FixedEffect function like print and save_res
#'
#' @return The return value will be a list which contains two elements at this point
#'   results: includes most of the observation from the julia call
#'   summary: includes information that is of importance to write a table
#'
#' @examples
#' # See vignettes and readme
#' \dontrun{
#'   FixedEffect_nse(y ~ x1 + x2, id1 + id2, cluster(id1), save = T)
#' }
#'
#' @export
#####################################################################################################################
FixedEffect_nse <- function(dt,
                            formula,
                            fe       = NULL,
                            weights  = NULL,
                            vcov     = NULL,
                            ...
){

  lhs = as.character(formula[[2]])
  rhs <- unlist(stringr::str_split(as.character(formula[[3]]), "\\+"))
  rhs <- paste(gsub(" ", "", rhs)[stringr::str_length(rhs)>0], collapse = " + ")

  f = deparse(substitute(fe))
  w = deparse(substitute(weights))
  if (w == "NULL"){ w = NULL }
  v    = deparse(substitute(vcov))
  if (v == "NULL"){ v = NULL }

  FixedEffect(
    dt,
    lhs,
    rhs,
    f,
    weights = w,
    vcov    = v,
    ...
  )
} # End of FixedEffect_nse
# ----------------------------------------------------------------------------------------------




# ----------------------------------------------------------------------------------------------
#' Use FixedEffectModels.jl to run large fixed effect models in julia on multiple models
#'
#' \code{FixedEffect_models} returns the results of a linear fixed effect regression
#'
#' @param dt        dataset of interest
#' @param lhs       String, a vector of Y regression variables
#' @param rhs       String, a vector of X dependent variables
#' @param fe        String, a vector of Fixed effects
#' @param weights   Vector of regression weights (not yet coded)
#' @param vcov      Vector of specification for the error
#'
#' @return The return value will be a list which contains two elements at this point
#'   results: includes most of the observation from the julia call
#'   summary: includes information that is of importance to write a table
#'
#' @examples
#'   # See vignettes and readme
#' \dontrun{
#' lhs <- c("log_ewemt", "f1_log_ewemt")
#' rhs <- c("MP + retail_index + retail_index_MP",
#'         "MP + discount_rate + discount_rate_MP")
#' fe  <- c("date_y",
#'          "date_y:fed_district + quarter:fed_district + industry_code_num:quarter")
#' vcov <- c("robust", "cluster(date_y)")
#' }
#' @export
#####################################################################################################################
FixedEffect_models <- function(
  dt,
  lhs,
  rhs,
  fe       = NULL,
  weights  = NULL,
  vcov     = NULL
){

  #########################################################################
  # 1. clean all the variables; parse the rhs, fe and cluster
  rhs_split <- unlist( stringr::str_split(rhs, "\\+") )
  rhs_split <- unique( stringr::str_replace_all(rhs_split, " ", "") )

  # parse the fixed effects and convert to pooled
  fe_split <- stringr::str_split(fe, "\\+")
  fe_split <- purrr::map(fe_split, ~ stringr::str_split(., "\\:") )
  fe_split <- purrr::map_chr(unlist(fe_split), ~ stringr::str_replace_all(., " ", "") )
  fe_split <- unique(fe_split)
  fe_julia <- purrr::map(fe, ~ stringr::str_replace_all(., "\\:", "\\&") )
  # get the cluster variables for categorical
  if (is.null(vcov)){  # default to robust
    vcov <- "robust"
  }
  cluster_split <- gsub("robust", "", vcov)
  cluster_split <- gsub("[()]", "", gsub("cluster", "", cluster_split))
  cluster_split <- gsub(" ", "", stringr::str_split(cluster_split, "\\+", simplify = T))
  cluster_split <- as.vector(cluster_split)[ stringr::str_length(cluster_split)>0 ]

  #########################################################################
  # 2. move only the right amount of data into julia (faster)
  col_keep = intersect(names(dt), c(lhs, rhs_split, fe_split, cluster_split, weights))
  dt <- data.table(dt)[, c(col_keep), with = F ]
  # fill NAs on lhs, sometimes object passed onto julia ends having NaN instead of missing
  for ( lhs_iter in seq(1, length(lhs)) ){
    dt[ !is.finite(get(lhs[lhs_iter])), c(lhs[lhs_iter]) := NA ]
    # dt_tmp[ !is.finite(dt_tmp[[lhs[lhs_iter]]]), c(lhs[lhs_iter]) := NA ] # Alternative writing in data.table
  }
  # pass the data to julia
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


  #########################################################################
  # 3. list of formulas
  r_prelim <- purrr::cross2(.x = lhs, .y = rhs)
  r_prelim <- purrr::map2(.x = r_prelim, .y = purrr::map(r_prelim, ~ paste(.x[[2]], collapse = " + ")),
                         ~ paste(.x[[1]], "~", .y))
  r_prelim

  r_fe <- purrr::cross2(r_prelim, fe_julia)
  r_fe <- purrr::map(.x=r_fe, ~ paste0(.x[[1]], ", fe = ", .x[[2]]))
  r_fe

  r_vcov <- purrr::cross2(r_fe, vcov)
  r_vcov <- purrr::map(.x=r_vcov, ~ paste0(.x[[1]], ", vcov = ", .x[[2]]))
  r_vcov

  if (!is.null(weights)){
    r_weights <- purrr::cross2(r_vcov, weights)
    r_final <- purrr::map(.x=r_weights,
                          ~ ifelse(stringr::str_length(.x[[2]])>0,
                                   paste0(.x[[1]], ", weights = ", .x[[2]]),
                                   .x[[1]]) )
  } else {
    r_final <- r_vcov
  }

  julia_reg <- purrr::map(r_final, ~ paste("reg(df_julia, @model(", ., ") );") )
  julia_reg <- paste0("reg_res", seq(1, length(julia_reg)), " = ", julia_reg)
  n_reg <- length(julia_reg)
  message("Running ... ", n_reg, " fixed effects regressions")
  # Run the regression and keep coefficients
  coef_list <- list()
  reg_list <- list()

  for (reg_iter in seq(1, length(julia_reg))){

    reg_msg <- paste0("\n\nRegression ... ", reg_iter, " ...\n",
                      gsub(", ", ",\n  ", r_final[[reg_iter]]))
    julia_command(paste0('print_with_color(:green, "', reg_msg, '\n")'))
    julia_command(paste(julia_reg[[reg_iter]]))
    julia_command(paste0("reg_res", reg_iter))

    list_tmp <- list()
    jl_coefficients    = julia_eval(paste0("reg_res", reg_iter, ".coef"))
    names(jl_coefficients) = julia_eval(paste0("reg_res", reg_iter, ".coefnames"))
    list_tmp$lhs =  julia_eval(paste0("reg_res", reg_iter, ".yname"))
    list_tmp$coefficients = jl_coefficients
    list_tmp$julia_call = r_final[[reg_iter]]
    list_tmp$se = julia_eval(paste0("stderror(reg_res", reg_iter, ")"))
    list_tmp$tt = julia_eval(paste0("coef(reg_res", reg_iter, ") ./ stderror(reg_res", reg_iter, ")"))
    list_tmp$pvalues = julia_eval(paste0("coeftable(reg_res", reg_iter, ").mat[:,4]"))
    list_tmp$ci      = julia_eval(paste0("confint(reg_res", reg_iter, ")"))
    list_tmp$coefnms= julia_eval(paste0("coefnames(reg_res", reg_iter, ")"))
    list_tmp$nobs = julia_eval(paste0("reg_res", reg_iter, ".nobs"))   # number of observations
    list_tmp$r2    = list(r2 = julia_eval(paste0("reg_res", reg_iter, ".r2")),
                          r2_adjusted = julia_eval(paste0("reg_res", reg_iter, ".r2_a")),
                          r2_within = julia_eval(paste0("reg_res", reg_iter, ".r2_within")) )
    list_tmp$statistics = list(F_stat = julia_eval(paste0("reg_res", reg_iter, ".F")),
                               pvalue = julia_eval(paste0("reg_res", reg_iter, ".p")) )

    # Coeftest class is easy: only need coefficients
    Rcoef = matrix(c(jl_coefficients, list_tmp$se, list_tmp$tt, list_tmp$pvalues),
                   nrow=length(list_tmp$coefnms), ncol=4, byrow = F)
    colnames(Rcoef) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    rownames(Rcoef) = list_tmp$coefnms
    class(Rcoef) = "coeftest"
    list_tmp$coeftest = Rcoef

    coef_list[[reg_iter]] <- list_tmp
    reg_list[[reg_iter]]  <- julia_eval(paste0("reg_res", reg_iter))

  }

  # return both all of the regression and coefficient subset
  return(list(statistics = coef_list,
              julia_reg  = reg_list))

} # end of FixedEffect_models
# ----------------------------------------------------------------------------------------------
