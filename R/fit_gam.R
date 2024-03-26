#' Fit gam models on posterior draws
#'
#' @param indata dataframe of posterior draws. Each column is a year, each row is a posterior draw
#' @param start_yr numeric year at which to start model. Default is the first year available
#' @param end_yr numeric year at which to end model. Default is the last year available
#' @param n_knots number of knots used in the gam model, default is 5
#'
#' @return dataframe with predicted
#' @export
#' @importFrom foreach '%do%'
#' @examples
#' \dontrun{
#'pred_dataset <- fit_gam(indat2, start_yr = 1990, end_yr = 2020, n_knots = 14)
#'}

fit_gam <- function(indata, start_yr = NA, end_yr = NA, n_knots = 5) {

  # perhaps add a default number of knots or some approximate calculation for number of years?
  # questions 1: - do we want to make the gam option flexible (tp vs cs)

  #  # testing lines
  # indata = indat2
  # start_yr = NA
  # end_yr = NA
  # n_knots = 14
  message("hang tight, you are currently running gams.... this might take a minute")

  allyr_seq <- colnames(indata) |>  as.numeric()

  # if years are to be filtered do this here:
  if(is.na(start_yr)) {
    start_year <- min(allyr_seq)
  } else {
    start_year <- start_yr
  }

  if(is.na(end_yr)) {
    end_year <- max(allyr_seq)
  } else {
    end_year <- end_yr
  }

  # create a list of year to use
  year_seq = allyr_seq
  year_seq = year_seq[year_seq >= start_year]
  year_seq = year_seq[year_seq <= end_year]


  # filter per years of interest
  indata <- indata |>
    dplyr::select(dplyr::all_of(as.character(year_seq)))

  # convert to log and create a list
  out <- log(indata) |> split(seq(nrow(indata)))

  # iterate over data frame per row
  preds <- foreach::foreach(i = names(out), .combine = rbind)%do%{
    # testing line
    #i = names(out)[1]

    i_dat <- data.frame(log_y = as.vector(unlist(out[i])), Year = year_seq)

    gam <- gam(log_y~s(Year, k = n_knots, bs = "tp"),
               data = i_dat) ## or

    #gam <- gam(log_y~s(year, bs = 'cs', k = length(knots)),
    #           knots = list(Year = knots),
    #           data = i_dat)


    i_dat$gam_pred <- stats::predict(gam, newdata = i_dat)
    predvals <- exp(i_dat$gam_pred)

    predvals

  }

  pred_df <- data.frame(preds)
  colnames(pred_df) = year_seq
  rownames(pred_df) <- NULL

  return(pred_df)

}


