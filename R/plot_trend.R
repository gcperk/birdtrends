#' Plot predicted trends
#'
#' @param raw_indices tibble with raw annual index value of observed data columns = year, index, index_q_0.25 = 2.5% confidence interval and index_q_0.975 = 97.5 % confidence interval
#' @param model_indices tibble of estimated indices drawn from fit_* function. Columns are draw, year, and proj_y
#' @param pred_indices datatable with modeled and predicted values into the future
#' @param start_yr numeric value of the first year in which trend will be calculated. Default is first available year within the dataset
#' @param end_yr numeric vlaue representing the last year in which trend will be calculated. Default is first available year within the dataset
#'
#' @return plot
#' @export
#'
#' @examples
#' \dontrun{
#'   hgams_plot <- plot_trend(raw_indices = input_option_1, model_indices = ldf_hgam,
#'   pred_indices = preds_hgam, start_yr = 2014, end_yr = 2022)
#'}
plot_trend <- function(raw_indices = indat1,
                       model_indices = ldf_hgam,
                       pred_indices =  preds_hgam ,
                       start_yr = 2014,
                       end_yr = 2022){

  #indices_summarized <- raw_indices #%>%
  #   tibble::rowid_to_column(., "draw") %>%
  #   tidyr::pivot_longer(., cols = !starts_with("d")) %>%
  #   dplyr::rename('year' = name, "obs_y" = value)%>%
  #   mutate(year = as.integer(year)) %>%
  #   group_by(year) %>%
  #   summarize(Index_q_0.025 = quantile(obs_y,0.025),
  #             Index = quantile(obs_y,0.5),
  #             Index_q_0.975 = quantile(obs_y,0.975))
  #

  gam_summarized <- model_indices %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(gam_q_0.025 = stats::quantile(proj_y,0.025),
              gam_index = stats::quantile(proj_y,0.5),
              gam_q_0.975 = stats::quantile(proj_y,0.975))

  predict_summarized <- pred_indices |>
    dplyr::group_by(year)%>%
    dplyr::summarize(pred_q_0.025 = stats::quantile(pred_ind,0.025),
              pred_index = stats::quantile(pred_ind,0.5),
              pred_q_0.975 = stats::quantile(pred_ind,0.975)) %>%
    dplyr::filter(year >= end_yr)

  #Index_baseline <- pred_indices %>%
  #  filter(year == 2014)

  ref_year = 2014


  baseline_years <- gam_summarized %>%
    dplyr::filter(year %in% c(start_yr, end_yr))


  sp_plot_index <- ggplot2::ggplot() +

    # Vertical line showing the year goals were set
    ggplot2::geom_vline(xintercept =  ref_year, size=2, col = "black", alpha = 0.2)+
    #geom_text(aes(x =  ref_year+1, y = 0.01),
    #          label = "<- reference year", col = "black", alpha = 0.2,
    #          hjust=0, fontface = "bold", size = 2)+

    ggplot2::geom_vline(xintercept = 2024, size=1, col = "black", alpha = 0.4,  linetype="dotted")+
    ggplot2::geom_vline(xintercept = 2046, size=1, col = "black", alpha = 0.4, linetype="dotted")+

    # Observed indices
    ggplot2::geom_errorbar(data = subset(raw_indices, year <= max(raw_indices$year)),aes(x = year, ymin = index_q_0.025, ymax = index_q_0.975), width = 0, col = "gray30")+
    ggplot2::geom_point(data = subset(raw_indices, year <= max(raw_indices$year)),aes(x = year, y = index), col = "gray30")+

    # gam smooth
    ggplot2::geom_ribbon(data = gam_summarized, aes(x = year, ymin = gam_q_0.025, ymax = gam_q_0.975), alpha = 0.4, fill = "gray50")+
    ggplot2::geom_line(data = gam_summarized, aes(x = year, y = gam_index), col = "gray50", linewidth = 1)+

    # predicted projection
    ggplot2::geom_ribbon(data = subset(predict_summarized, year >= end_yr), aes(x = year, ymin = pred_q_0.025, ymax = pred_q_0.975), alpha = 0.2, fill = "orangered")+
    ggplot2::geom_line(data = subset(predict_summarized, year >= end_yr), aes(x = year, y = pred_index), col = "orangered", linewidth = 1)+


    #geom_line(data = baseline_years, aes(x = year, y = gam_index), col = "black", linewidth = 1)+

    ggplot2::ylab("Annual Index of Abundance")+
    ggplot2::xlab("Year")+
    ggplot2::theme_bw()
  #coord_cartesian(ylim=c(0,max(apply(sp_projection$indices,2,function(x) quantile(x, 0.975)))),
  #               xlim=c(1970,2050))+
  #scale_x_continuous(breaks = seq(1970,sp_projection$end_of_projection,10))

  print(sp_plot_index)

}
