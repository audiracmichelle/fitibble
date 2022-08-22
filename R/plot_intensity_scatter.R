#' Intensity scatter plot
#'
#' @param minute_data a fitibble.
#' @param sample_size the number of points in the scatter plot
#'
#' @return A scatterplot with a two-dimension representation of the PA intensity levels. For each id a sample of minute HR and step readings is extracted. Colors represent PA intensity levels.
#' @export
#'
#' @examples
plot_intensity_scatter <- function(
    minute_data,
    sample_size = 5e4
) {
  intensity_colname <- attr(minute_data, "intensity_colname")
  intensity_levels <- attr(minute_data, "intensity_levels")

  minute_data$intensity_levels <- factor(minute_data[[intensity_colname]],
                                         exclude = NULL)
  minute_data$intensity_levels <- minute_data$intensity_levels %>%
    forcats::fct_recode(!!!intensity_levels)

  n_id <- length(unique(minute_data$id))

  minute_data %>%
    crop_valid() %>%
    dplyr::group_by(.data$id) %>%
    dplyr::slice_sample(n = sample_size, replace = T) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = .data$steps,
                                     y = .data$HR,
                                     col = .data$intensity_levels),
                        size = 0.5) +
    ggplot2::facet_wrap(~ .data$id,
                        nrow = ceiling(n_id / round(sqrt(n_id))),
                        ncol = round(sqrt(n_id))) +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Minute average HR", x = "Minute total step counts") +
    ggplot2::guides(col = ggplot2::guide_legend("Intensity levels")) +
    ggplot2::theme(legend.position = "bottom")
}
