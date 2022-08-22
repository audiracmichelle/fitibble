#' Plot PA time-use
#'
#' @param minute_data a fitibble.
#' @param id the subject `id`.
#'
#' @return A plot showing the time-use descriptors split in valid and non-valid days.
#' @export
#'
#' @examples
plot_time_use <- function(
    minute_data,
    id
) {
  .id = id
  intensity_levels <- attr(minute_data, "intensity_levels")
  color_palette <- RColorBrewer::brewer.pal(length(intensity_levels), "Oranges")
  names(color_palette) <- names(intensity_levels)

  prep_daily_data(minute_data) %>%
    dplyr::filter(.data$id == .id) %>%
    dplyr::select("id", "date", tidyselect::contains("time_use")) %>%
    tidyr::pivot_longer(cols = tidyselect::contains("time_use")) %>%
    dplyr::mutate(type = "valid") %>%
    dplyr::bind_rows(
      prep_daily_data(minute_data, nonvalid = T) %>%
        dplyr::filter(.data$id == .id) %>%
        dplyr::select("id", "date", tidyselect::contains("time_use")) %>%
        tidyr::pivot_longer(cols = tidyselect::contains("time_use")) %>%
        dplyr::mutate(type = "nonvalid")
    ) %>%
    dplyr::mutate(
      name = gsub("_time_use", "", .data$name),
      name = factor(.data$name,
                    levels = names(sort(intensity_levels, decreasing = T))),
      type = factor(.data$type, c("valid", "nonvalid")),
      type = forcats::fct_recode(
        .data$type,
        !!!c("Valid days" = "valid",
             "Non-valid days" = "nonvalid"))
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x=.data$date, y=.data$value, fill=.data$name),
                      stat="identity") +
    ggplot2::facet_wrap(~.data$type, nrow=2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::labs(x = "", y = "PA time use per intensity level") +
    ggplot2::guides(fill = ggplot2::guide_legend("Intensity levels"))
}
