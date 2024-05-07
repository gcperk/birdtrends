#' Predict generated trends into the future
#'
#' @param proj_output tibble of estimated indices drawn from fit_* function. Columns are draw, year, and proj_y
#' @param trend_output tibble of estimated trends generated from get_trends* function.
#' @param start_yr numeric year of the first projected date
#' @param proj_yr numeric year to which trend will be projected
#'
#' @return datatable with modeled and predicted values into the future
#' @export
#'
#' @examples
#' \dontrun{
#'  trend_sm <- predict_trend(ldf_smooths, trend_sm, start_yr = 2023, proj_yr = 2046)
#'}
predict_trend <- function(proj_output,
                          trend_output,
                          start_yr = NA,
                          proj_yr = 2046){

  ## testing
  # proj_output <- ldf_smooths
  # trend_output <- trend_sm
  # start_yr = 2023
  # proj_yr = 2046

  #proj_output = ldf
  #trend_output  = trend_sm
  #proj_output  = indata1
  #trend_output = tr

  if(is.na(start_yr)){
    start_yr <- max_yr <- max(proj_output$year)+ 1
  }

  if(start_yr - (max(proj_output$year)) > 1){
    stop("Start year of prediction is too far in advance, choose a year no more than 1 year more than maximum year of data")
  }


  # generate a table for inputs
  pred_out <- proj_output[1,] %>% dplyr::mutate(pred_ind = 0)

  pbar <- txtProgressBar()
  message("hold tight, running the numbers!")

  #TODO: update this portion to purr::map..

  for( i in trend_output$draw){

    # testing line
    # i  = trend_output$draw[2]
    # end testing line

    setTxtProgressBar(pbar, i/length(trend_output$draw))

    trend_draw <- trend_output %>% dplyr::filter(draw == i) %>% dplyr::pull(trend_log)

    proj_draw <- proj_output %>% dplyr::filter(draw == i)


    # not sure where to cut of projections here:
    #ie build trend from project from 2014 - 2022
    # project to 2024
    # project to 2046

    proj_draw <- proj_draw %>%
      tidyr::complete(year = seq(start_yr, max(proj_yr), 1)) %>%
      dplyr::mutate(draw = i) %>%
      #filter(year >= proj_start_yr)
      dplyr::mutate(pred_ind = proj_y)%>%
      dplyr::arrange(year)

    #  calculate future projections starting in year of goal

    for (y in seq(start_yr, proj_yr)) {
      proj_draw$pred_ind[proj_draw$year == y] <- proj_draw$pred_ind[proj_draw$year == (y-1)] *exp(trend_draw)
    }

    pred_out <- dplyr::bind_rows(pred_out, proj_draw)

  }

  # clean up the output
  pred_out <- pred_out[-1,]

  return(pred_out)

}

