#' Calculate probability of meeting targets
#'
#' @param projected_trends tibble with all draws, years, projected trends, output of proj_trend function
#' @param ref_year numeric year in which change is measured from i.e. 2014
#' @param targ_year numeric year in which target will be reached i.e. 2026
#' @param prob_decrease numeric vector of predicted target increase. i.e c(25, 35). Determine the probability that trend will increase 25% to 35% by target year
#' @param prob_increase numeric vector of predicted target decrease. i.e c(25, 35). Determine the probability that trend will decline by 25% to 35% by target year
#'
#' @return tibble with the average percent change in addition to probability of increase or decrease
#' @export
#'
#' @examples
#' \dontrun{
#'   test_st <- calculate_probs(predicted_trends = preds_sm,
#'         ref_year = 2014,
#'         targ_year = 2026,
#'         prob_increase = c(targ$st_pop_pc_lower,targ$st_pop_pc_uppper))
#'}
calculate_probs <- function(
    projected_trends = preds_sm,
    ref_year = 2014,
    targ_year = 2026,
    prob_decrease = NULL,
    prob_increase = NULL){


  # check in input values
  uyrs <-  projected_trends$year
  if(!ref_year %in% uyrs){
    stop("reference year is not within input data, please re-select")
  }
  if(!targ_year %in% uyrs){
    stop("target year is not within input data, please re-select")
  }

  if(!targ_year %in% uyrs){
    stop("target year is not within input data, please re-select")
  }
  if(!is.null(prob_decrease)){
    if(!is.numeric(prob_decrease)){
    stop("prob_decrease must be numeric")
      }
  }
  if(!is.null(prob_increase)){
    if(!is.numeric(prob_increase)){
    stop("prob_increase must be numeric")
    }
  }
  if(is.null(prob_increase) & is.null(prob_decrease)){
    stop("no target increase or decrease included")
  }


  # set up parts of function
  #calc_quantiles = stats::quantile
  #quantiles = c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975)

  # from bbsbayes2 get trend function
  calc_prob_crease <- function(x, p, type = "decrease") {
    if(type == "decrease") f <- function(p) length(x[x < (-1 * p)]) / length(x)
    if(type == "increase") f <- function(p) length(x[x > p]) / length(x)

    vapply(p, FUN = f, FUN.VALUE = 1.1) %>%
      stats::setNames(paste0("prob_", type, "_", p, "_percent"))
  }


  # for each draw subtract target year from ref year (2024) - 2014
  tyears <- dplyr::filter(projected_trends, year %in% c(ref_year, targ_year)) |>
    dplyr::select(-proj_y)


  tyears <-  tidyr::pivot_wider(tyears, values_from = pred_ind, names_from = year)
  colnames(tyears)= c("draw", "ref_yr", "targ_year")

  ltyears <- tyears |>
    dplyr::mutate(ch = targ_year/ref_yr) |>
    dplyr::rowwise() |>
    dplyr::mutate(ch_pc = 100 * (stats::median(ch) -1)) |>
    dplyr::ungroup() #%>%
    # dplyr::mutate(percent_change_ave = 100 * (stats::median(ch)-1),
    #        percent_change_q_0.025 = 100 *  (stats::quantile(ch, probs = 0.025)-1),
    #        percent_change_q_0.05 = 100 *  (stats::quantile(ch, probs = 0.05)-1),
    #        percent_change_q_0.25 = 100 *  (stats::quantile(ch, probs = 0.25)-1),
    #        percent_change_q_0.75 = 100 *  (stats::quantile(ch, probs = 0.75)-1),
    #        percent_change_q_0.95 = 100 *  (stats::quantile(ch, probs = 0.95)-1),
    #        percent_change_q_0.975 = 100 *  (stats::quantile(ch, probs = 0.975)-1))



  # Model conditional probabilities of population change during trends period
  if(!is.null(prob_decrease)) {
    ltyears <- ltyears %>%
      dplyr::mutate(
        pch_pp = purrr::map_df(.data$ch_pc, calc_prob_crease,
                               .env$prob_decrease, type = "decrease")) %>%
      tidyr::unnest("pch_pp")
  }

  if(!is.null(prob_increase)){
    ltyears <- ltyears %>%
      dplyr::mutate(
        pch_pp = purrr::map_df(.data$ch_pc, calc_prob_crease,
                               .env$prob_increase, type = "increase")) %>%
      tidyr::unnest("pch_pp")
  }

  # filter and select only cols needed.

  out <- ltyears |>
    dplyr::mutate(across(starts_with("prob"), ~ sum(.x, na.rm = TRUE)/length(.x)*100)) %>%
    dplyr::select(starts_with("p"))%>%
    dplyr::distinct()%>%
    dplyr::mutate(ref_year = ref_year, target_yr = targ_year)

  return(out)

}

