#' Prepare GAM components for running stan model
#'
#' @param orig.preds vector of years to be modeled
#' @param nknots numeric value for number of knots in model
#' @param predpoints values to be predicted. Default is NULL
#' @param npredpoints number of values to be predicted. Default is 100
#' @param sm_name name of model column outputs, default is "year
#'
#' @return a list containing components of  GAM model to be run within STAN
#' @export
#'
#' @examples
#' \dontrun{
#'aa <- prep_hgam(out$year, nknots = 6, predpoints = NULL, npredpoints = 100, sm_name = "year")
#'}
prep_hgam <- function(orig.preds = NA,
                      nknots = 6,#number of internal knots
                      predpoints = NULL,
                      npredpoints = 100,
                      sm_name = "year"){

  if(any(is.na(orig.preds) == T)){
    stop("This GAM formulation cannot handle missing values in the predictor")
  }

  # testing data
    #orig.preds = out$year
    #nknots = n_knots
    #predpoints = NULL
    #npredpoints = 100
    #sm_name = "year"

  dat = data.frame(x = orig.preds,
                   y = stats::rnorm(length(orig.preds),0,0.1))
  if(is.null(predpoints)){
    predpoints = seq(min(orig.preds),max(orig.preds),length.out = npredpoints)
  }else{
    npredpoints <- length(predpoints)
  }

  dat_pred = data.frame(x = predpoints,
                        y = stats::rnorm(length(predpoints),0,0.1))


  M = mgcv::smoothCon(mgcv::s(x,k = nknots+1, bs = "tp"),data = dat,
                absorb.cons=TRUE,#this drops the constant
                diagonal.penalty=TRUE) ## If TRUE then the smooth is reparameterized to turn the penalty into an identity matrix, with the final diagonal elements zeroed (corresponding to the penalty nullspace).

  gamx.basis = M[[1]]$X

  M_pred = mgcv::smoothCon(mgcv::s(x,k = nknots+1, bs = "tp"),data = dat_pred,
                     absorb.cons=TRUE,#this drops the constant
                     diagonal.penalty=TRUE) ## If TRUE then the smooth is reparameterized to turn the penalty into an identity matrix, with the final diagonal elements zeroed (corresponding to the penalty nullspace).

  gamx.basispred = M_pred[[1]]$X


  outlist <- list(gamx.basis = gamx.basis,
                  gamx.basispred = gamx.basispred,
                  orig.preds = orig.preds,
                  predpoints = predpoints,
                  nknots = nknots,
                  npredpoints = npredpoints,
                  M = M,
                  M_pred = M_pred)
  names(outlist) <- c(paste0(sm_name,"_basis"),
                      paste0(sm_name,"_basispred"),
                      "original_predictor_values",
                      paste0(sm_name,"_visualized_predictor_values"),
                      paste0("nknots_",sm_name),
                      paste0("npredpoints_",sm_name),
                      paste0(sm_name,"_smoothCon"),
                      paste0(sm_name,"_smoothCon_pred"))


  return(outlist)

}
