#' Generate trend based based on multiple draws of annual indices
#'
#' @param proj_data tibble of estimated indices drawn from fit_* function. Columns are draw, year, and proj_y
#' @param start_yr numeric value of the first year in which trend will be calculated. Default is first available year within the dataset
#' @param end_yr numeric vlaue representing the last year in which trend will be calculated. Default is first available year within the dataset
#' @param method character of method used to calculate trend. Two methods available; geometric mean ("gmean" as default )or "lm" linear regression
#'
#' @return tibble with estimated trend and percent_trend for each draw
#' @export
#'
#' @examples
#' \dontrun{
#'  ldf_smooths <- tibble::rowid_to_column(fitted_smooths, "draw") %>%
#'       tidyr::pivot_longer(., cols = !starts_with("d")) %>%
#'       dplyr::rename('year' = name, "proj_y" = value)%>%
#'       dplyr::mutate(year = as.integer(year))
#'  trend_sm <- get_trend(ldf_smooths, start_yr = 2014, end_yr = 2022, method = "gmean")
#'}
get_trend <- function(proj_data, start_yr = NA, end_yr = NA, method = "gmean"){

  #   # testing
  # proj_data <- ldf_smooths
  # start_yr = 1990
  # end_yr = 2000
  # method = "lm"

  min_yr <- min(proj_data$year)
  max_yr <- max(proj_data$year)

  if(is.na(start_yr)) {
    start_yr <-  min_yr
  } else {
    if(start_yr < min_yr) {
      message("`start_yr` is before the date range, using minimum year of ",
              "the data (", start_yr <- min_yr, ") instead.")
    }
  }

  if (is.null(end_yr)) {
    end_yr <- max_yr
  } else if(end_yr > max_yr) {
    message("`max_year` is beyond the date range, using maximum year of ",
            "the data (", end_yr <- max_year, ") instead.")
  }


  # subset data based on the selected years
  trend_dat <- subset(proj_data, year %in% seq(start_yr, end_yr))


  if(method == "gmean") {

    # estimate the trend based on the years of selection for each draw

    trend_sum <- trend_dat %>%
      dplyr::group_by(draw) %>%
      dplyr::summarise(trend_log = mean(diff(log(proj_y)))) %>%
      dplyr::mutate(perc_trend = 100*(exp(trend_log)-1))


  }  else if (method == "lm"){

    lm_mod <- function(df){
      stats::lm(log(proj_y) ~ year, data = df)}

    trend_df <- trend_dat %>%
      dplyr::group_by(draw) %>%
      tidyr::nest()

    trend_lms <- trend_df %>% dplyr::mutate(model = purrr::map(data, lm_mod))

    trend_sum <- trend_lms %>%
      dplyr::mutate(tidy = purrr::map(model, broom::tidy),
             trend_log = broom::tidy %>% purrr::map_dbl(function(x) x$estimate[2])) %>%
      dplyr::select(c(-model, -tidy, -data)) %>%
      tidyr::unnest(cols = c(draw))%>%
      dplyr::mutate(perc_trend = 100*(exp(trend_log)-1))

  }

  return(trend_sum)

}



