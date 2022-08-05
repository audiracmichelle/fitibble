#' plot_wear_heatmap
#'
#' @param minute_data a fitibble
#'
#' @return A heatmap indicating the wear proportion in each hour and weekday for each subject `id` in `minute_data`.
#' @export
#'
#' @examples
plot_wear_heatmap <- function(minute_data) {
  wear_heatmap <- minute_data %>%
    dplyr::mutate(
      hour = factor(lubridate::hour(.data$time), levels = 0:23),
      weekday = lubridate::wday(.data$time, label = T, abbr = T)
    )

  wear_heatmap %>%
    dplyr::group_by(.data$id) %>%
    dplyr::count(.data$hour, .data$weekday, .drop = F) %>%
    dplyr::left_join(
      wear_heatmap %>%
        dplyr::group_by(.data$id, .data$hour, .data$weekday) %>%
        dplyr::summarise(wear = sum(.data$is_wear))
    ) %>%
    dplyr::mutate(wear_prop = .data$wear / .data$n) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$weekday,
                                 y = .data$hour,
                                 fill = .data$wear_prop)) +
    ggplot2::geom_tile() +
    ggplot2::facet_wrap(~id, nrow=length(unique(minute_data$id))) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(fill = "proportion of\n wear")
}
