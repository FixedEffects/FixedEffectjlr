#' Use FixedEffectModels.jl to run large fixed effect models in julia
#'
#' \code{FixedEffect} returns the results of a linear fixed effect regression
#'
#' @param dt        dataset of interest
#' @param lhs       String, Y regression variable
#' @param rhs       String, X formula
#' @param fe        Fixed effects: c("x", "y"), c("x:y"), c("x:^y")
#' @param weights   Regression weights
#' @param vcov      c("state", "date_y"), "robust, NULL
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


  # --- PARSE THE RIGHT HAND SIDE
  rhs       <- stringr::str_replace_all(rhs, "\\:", "\\&")
  rhs_split <- unlist( stringr::str_split(rhs, "\\+") )
  rhs_split <- unlist( stringr::str_split(rhs_split, "\\&") )
  rhs_split <- unlist( stringr::str_split(rhs_split, "\\*") )
  rhs_split <- unique( stringr::str_replace_all(rhs_split, " ", "") )

  # --- PARSE THE FIXED EFFECTS
  # N.B. on interaction term: do make sure variables have right type already in R!
  list_fe <- purrr::map(fe, ~ stringr::str_split(., "[\\:\\*\\^]") )
  list_fe <- purrr::map_chr(unlist(list_fe), ~ stringr::str_replace_all(., " ", "") )
  list_fe <- unique(list_fe[ stringr::str_length(list_fe)>0])
  n_fe = length(list_fe)                             # useful for getfe function in julia

  # parse the different types
  interaction_1_idx <- unique(c(grep("[\\:]", fe)))
  interaction_all_idx <- unique(c(grep("[\\*]", fe)))
  interaction_c_idx <- unique(c(grep("[\\^]", fe)))
  # group all of the interaction terms
  interaction_idx <- unique(c(interaction_1_idx, interaction_all_idx, interaction_c_idx))
  interaction_idx <- interaction_idx[order(interaction_idx)]

  interaction_1_idx
  interaction_all_idx
  interaction_c_idx

  fe_formula <- ""
    for ( fe_idx in seq(1, length(fe)) ){
    if (!(fe_idx %in% interaction_idx)){
      fe_formula <- c(fe_formula, paste0("fe(", fe[fe_idx], ")"))
    } else if (fe_idx %in% interaction_idx){
      # discrete case first
      if ( !(fe_idx %in% interaction_c_idx) ){
        fe_tmp <- fe[fe_idx]
        fe_tmp <- unlist( stringr::str_split(fe_tmp, "\\*") )
        fe_tmp <- unlist( stringr::str_split(fe_tmp, "\\:") )
        # simple or also levels
        if (fe_idx %in% interaction_1_idx){
          fe_formula <- c(fe_formula, paste0(paste0("fe(", fe_tmp, ")"), collapse="&"))
        } else if ( fe_idx %in% interaction_all_idx){
          fe_tmp_all <- paste0("fe(", fe_tmp, ")")
          fe_formula <- c(fe_formula,
                          paste0(paste0(fe_tmp_all, collapse=" + "), " + ", paste0(fe_tmp_all, collapse="&")) )
        }
        # continuous case
      } else if (fe_idx %in% interaction_c_idx){
        fe_tmp <- fe[fe_idx]
        fe_tmp <- unlist( stringr::str_split(fe_tmp, "\\*") )
        fe_tmp <- unlist( stringr::str_split(fe_tmp, "\\:") )
        fe_tmp1 <- grep("[\\^]", fe_tmp, value = T, invert=T)
        fe_tmp2 <- gsub("[\\^]", "", grep("[\\^]", fe_tmp, value = T))
        #  simple or also levels
        if (fe_idx %in% interaction_1_idx){
          fe_formula <- c(fe_formula, paste0("fe(", fe_tmp1, ")&", fe_tmp2))
        } else if ( fe_idx %in% interaction_all_idx){
          fe_formula <- c(fe_formula,
                          paste0("fe(", fe_tmp1, ") + fe(", fe_tmp1, ")&", fe_tmp2) )
        }
      }
    }
  }
  fe_formula <- fe_formula[str_length(fe_formula)>0]
  fe_formula <- paste(unique(fe_formula), collapse = " + ")
  # set the formula for julia
  julia_formula  = paste(lhs, "~", rhs, "+", fe_formula)

  # --- SET THE OPTIONS
  # split all the clustering variables
  cluster_split <- NULL
  if (!is.null(vcov)){
    cluster_split <- gsub("cluster", "",
                          gsub("[()]", "",
                               gsub("robust", "", vcov) ) )
    cluster_split <- gsub(" ", "",
                          unlist( stringr::str_split(cluster_split, "\\+") ) )
  }
  # weights
  julia_reg_weights <- ""
  if (!is.null(weights)){
    if (stringr::str_length(weights)>0){
      julia_reg_weights   = paste0("weights = :", weights)
    } }
  # covariance
  julia_reg_vcov = " Vcov.robust()" # default to robust
  if (is.null(vcov)){
    julia_reg_vcov = ""
  } else if (sum(vcov %in% colnames(dt))>0) {
    # vcov[vcov %in% colnames(dt)]
    julia_reg_vcov <- paste0("Vcov.cluster(:", paste0(vcov[vcov %in% colnames(dt)], collapse=", :"), ")" )
  } else  {
    julia_reg_vcov = " Vcov.robust()"
  }

  julia_reg_save = paste0("save = ", ifelse(save_res, "true", "false"))

  julia_reg_opt <- c(julia_reg_vcov, julia_reg_save)
  julia_reg_opt <- julia_reg_opt[str_length(julia_reg_opt)>0]
  julia_reg_opt <- paste(julia_reg_opt, collapse = ", ")
  if (str_length(julia_reg_weights) > 0){
    julia_reg_opt  = paste0(julia_reg_opt, ", ", julia_reg_weights)
  }


  julia_regcall = paste0("reg_fe(x) = reg(df_julia, @formula(", julia_formula, "), ",
                         julia_reg_opt, collapse = ", ",
                         ");")

  # move only the right amount of data into julia (faster)
  col_keep = intersect(names(dt), c(lhs, rhs_split, list_fe, cluster_split, weights))
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
 for ( fe_iter in seq(1, length(list_fe)) ){
   if (classes[ name == list_fe[fe_iter]][["colclass"]] %in% c("numeric", "integer") ){
     dt[ !is.finite(get(list_fe[fe_iter])),  c(list_fe[fe_iter]) := NA ]
     if (verbose == T & (nrow(dt[ !is.finite(get(list_fe[fe_iter] ))])>0) ) {
       message("# removing NA for continuous fe variable ... ", list_fe[fe_iter])
     }
   }
 }

  # send the R data.table to julia
  dt_julia <- JuliaObject(dt)
  julia_assign("df_julia", dt_julia)

  # create pooled fe variables in the julia dataset: not necessary anymore
  # commented out!

  # Run the regression
  if (verbose == T){
    message("# running regression in FixedEffectModels.jl\n",
            "# julia call is ...\n",
            julia_regcall)
  }
  julia_command(julia_regcall) # function that executes the regression
  # now catch errors with try/catch in julia
  julia_command("
reg_fe_error = function(x)
    try
        reg_fe(x);
    catch
        0;
    end
end;")
  # run the regression with error catching
  julia_command("reg_res = reg_fe_error(1);")
  # check if regression worked
  julia_command("
reg_fe_test = function(x)
    try
        x.coef[1];
    catch
        1im
    end
end;")
  # making the failed regression imaginary is the best I found (couldn't move missing to NA)
  reg_test <- julia_eval("reg_fe_test(reg_res)")
  reg_test <- !is.complex(reg_test)  # boolean if regression did go through

  # Return some useful information "close to lm"
  z <- list()

  if (reg_test == T){
    # coefficients
    jl_coefficients    = julia_eval("reg_res.coef")
    names(jl_coefficients) = julia_eval("reg_res.coefnames")
    z$coefficients = jl_coefficients
    if (save_res == TRUE){
      z$residuals = julia_eval("reg_res.augmentdf[!, :residuals]")
      z$fitted.values = julia_eval(paste0("df_julia[!, :", lhs, "] - reg_res.augmentdf[!, :residuals]"))
    }
    # effects
    ## z$effects = c(NA)
    z$rank = julia_eval("reg_res.nobs - reg_res.dof_residual")
    # assign
    ## z$assign = c(NA)
    # qr
    ## z$qr = c(NA)
    jl_df.residual = julia_eval("reg_res.dof_residual")  # change in FixedEffectModels interface from df_residual to dof_residual
    z$df.residual  = jl_df.residual
    # xlevels
    ## z$xlevels <- list()
    ## names(z$xlevels) <- c()

    if ( is.null(vcov) ){
      cluster_formula <- "0"
    } else if ( grepl("robust", vcov) ){
      cluster_formula <- "0"
    } else {
      cluster_formula <- gsub("cluster", "",
                              gsub("[()]", "", vcov) )
    }
    R_call = paste(lhs, "~", rhs, "|",
                   paste0(gsub("[\\^]", "", fe), collapse = " + "),
                   "| 0 |", cluster_formula)
    z$call = list(R_call = as.formula(R_call), julia_call = julia_regcall)
    # terms
    z$terms = terms(as.formula(R_call))

    # other stuff
    z$nobs = julia_eval("reg_res.nobs")   # number of observations

    z$r2    = list(r2 = julia_eval("reg_res.r2"),
                   r2_adjusted = julia_eval("reg_res.adjr2"),
                   r2_within = julia_eval("reg_res.r2_within"))
    z$statistics = list(F_stat = julia_eval("reg_res.F"),
                        pvalue = julia_eval("reg_res.p"))

    z$convergence = list(julia_eval("reg_res.iterations"),
                         julia_eval("reg_res.converged"))

    z$se = julia_eval("stderror(reg_res)")
    z$ci = julia_eval("confint(reg_res)")

    if (save_res == TRUE){
      z$fe = julia_eval(paste0("reg_res.augmentdf[:, 2:end]"))
    }

    # cheat a little and force it unto class lm for showing in stargazer
    class(z) <- 'lm'

    # Return something else close to summary
    if (print == TRUE){
      julia_eval("show(reg_res);")
    }

    # BUILD COEFFICIENT TABLE LIST
    ct <- list()   # CoefTable2
    julia_eval("jl_table = coeftable(reg_res);")
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

  } else if (reg_test == F){
    warning("# FE regression failed (see julia for more details)")
    z  = NULL
    ct = NULL
  }

  # -------------------------------------------------------------
  return(list(results = z,
              summary = ct))


} # end of FixedEffect
# ----------------------------------------------------------------------------------------------




