#' Get target values for plotting
#'
#' @param model_indices tibble of estimated indices drawn from fit_* function. Columns are draw, year, and proj_y
#' @param ref_year numeric year in which targets are compared to, default = 2014
#' @param st_year numeric short term year for which targets will be estimated
#' @param st_lu_target_pc numeric lower confidence interval of percentage of short term population change from reference year to st_year
#' @param st_up_target_pc numeric upper confidence interval of percentage of short term population change from reference year to st_year
#' @param lt_year numeric long term year for which targets will be estimated
#' @param lt_lu_target_pc numeric lower confidence interval of percentage of long term population change from reference year to lt_year
#' @param lt_up_target_pc numeric upper confidence interval of percentage of long term population change from reference year to lt_year
#'
#' @return tibble with upper and lower target values
#' @export
#'
#' @examples
#' \dontrun{
#'   hgams_plot <- get_targets(model_indices = ldf, ref_year = 2014,
#'   st_year = 2026, st_lu_target_pc = -2,st_up_target_pc = 1,
#'    lt_year = 2046,  lt_lu_target_pc = 5,lt_up_target_pc = 15)
#'}
get_targets <- function(model_indices = ldf,
                        ref_year = 2014,
                        st_year = 2026,
                        st_lu_target_pc = -2,
                        st_up_target_pc = 1,
                        lt_year = NA,
                        lt_lu_target_pc = NA,
                        lt_up_target_pc = NA){


  # # ## start testing
  # model_indices = ldf
  # ref_year = 2014
  # st_year = 2026
  # st_lu_target_pc = -2
  # st_up_target_pc = 1
  # lt_year = NA
  # lt_lu_target_pc = NA
  # lt_up_target_pc = NA
  # # end testing/


  # check in input values
  uyrs <-  model_indices$year
  if(!ref_year %in% uyrs){
    stop("reference year is not within input data, please re-select")
  }

   if(st_lu_target_pc > st_up_target_pc){
     stop("st_lower confidence is higher than upper confidence value")
   }


  # get the reference year for change
  index_baseline <- model_indices %>%
    dplyr::filter(year == ref_year)%>%
    dplyr::mutate(ave_ref_index = mean(proj_y))%>%
    dplyr::select(year, ave_ref_index) |>
    dplyr::distinct() |>
    dplyr::mutate(st_year = st_year,
           st_lu_target = (ave_ref_index + (st_lu_target_pc /100) * ave_ref_index),
           st_up_target = (ave_ref_index + (st_up_target_pc /100) * ave_ref_index),
           lt_year = lt_year,
           lt_lu_target = (ave_ref_index + (lt_lu_target_pc /100) * ave_ref_index),
           lt_up_target = (ave_ref_index + (lt_up_target_pc /100) * ave_ref_index))


  return(index_baseline)

}
