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
fit_gam <- function(indata, start_yr = NA, end_yr = NA, n_knots = NA) {

   # questions 1: - do we want to make the gam option flexible (tp vs cs)

  #  # testing lines
  #indata = indat2
  #start_yr = NA
  #end_yr = NA
  #n_knots = NA
  #  # testing end

  message("hang tight, you are currently running gams.... this might take a minute")

  allyr_seq <- colnames(indata) |>  as.numeric()

  min_yr <- min(allyr_seq)
  max_yr <- max(allyr_seq)

  # if years are to be filtered do this here:
  if(is.na(start_yr)) {
    start_yr <- min_yr
  } else {
    if(start_yr < min_yr) {
      message("`start_yr` is before the date range, using minimum year of ",
              "the data (", start_yr <- min_yr, ") instead.")
    }
  }

  if (is.na(end_yr)) {
    end_yr <- max_yr
  } else {
    if(end_yr > max_yr) {
      message("`max_year` is beyond the date range, using maximum year of ",
              "the data (", end_yr <- max_year, ") instead.")
    }
  }


  # create a list of year to use
  year_seq = allyr_seq
  year_seq = year_seq[year_seq >= start_yr]
  year_seq = year_seq[year_seq <= end_yr]


  #estimate n_knots if not specified
  n_years <- as.integer(length(year_seq))

  if(is.na(n_knots)){
    n_knots <- as.integer(round(n_years/4))
    message("`n_knots` is not defined, using default of one knot per 4 years of data ",
            "using n_knots = ",  n_knots, ".")
  }

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

    gam <- mgcv::gam(log_y~s(Year, k = n_knots, bs = "tp"),
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

