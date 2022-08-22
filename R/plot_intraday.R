#' Plot intraday
#'
#' @param minute_data a fitibble.
#' @param id the subject `id`.
#' @param date the date from which fitbit HR and step readings will be plotted.
#' @param add_wear default is `TRUE` to color the background of wear minutes in orange and nonwear minutes in gray.
#'
#' @return Intraday minute-level HR and step time series plot.
#' @export
#'
#' @examples
plot_intraday <- function(
    minute_data,
    id,
    date,
    add_wear = T
) {
  #filter
  .id = id
  .date = as.Date(date)

  minute_data %<>%
    dplyr::mutate(
      date = as.Date(.data$time)
    ) %>%
    dplyr::filter(.data$id == .id, .data$date == .date) %>%
    dplyr::mutate(
      time = hms::hms(hours = lubridate::hour(.data$time),
                      minutes = lubridate::minute(.data$time),
                      seconds = lubridate::second(.data$time))
    )

  #standardize
  min_steps = min(minute_data$steps, na.rm = T)
  max_steps = max(minute_data$steps, na.rm = T)
  min_HR = min(minute_data$HR, na.rm = T)
  max_HR = max(minute_data$HR, na.rm = T)

  #rescale HR and steps
  readings_data <- minute_data %>%
    dplyr::mutate(
      #steps = (steps - min_steps) / (max_steps - min_steps),
      HR = (.data$HR - min_HR) / (max_HR - min_HR) * (max_steps - min_steps) + min_steps
    ) %>%
    dplyr::select(.data$time, .data$steps, .data$HR) %>%
    tidyr::pivot_longer(-.data$time)

  #refactor wear
  wear_data <- minute_data %>%
    dplyr::mutate(
      xmin = .data$time,
      xmax = dplyr::lag(.data$time),
      is_wear = as.factor(.data$is_wear)
    ) %>%
    dplyr::select(.data$xmin, .data$xmax, .data$is_wear)
  wear_data$is_wear <- wear_data$is_wear %>%
    forcats::fct_recode(!!!c("wear" = "TRUE", "nonwear" = "FALSE"))

  #plot
  if(add_wear) {
    fill_colors <- c(wear = "orange", nonwear = "gray80")

    p <- wear_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_rect(
        ggplot2::aes(xmin=.data$xmin, xmax=.data$xmax,
                     ymin=-Inf, ymax=Inf,
                     fill=.data$is_wear),
        alpha = 0.5) +
      ggplot2::geom_line(data = readings_data,
                         ggplot2::aes(x = .data$time,
                                      y = .data$value,
                                      color = .data$name)) +
      ggplot2::geom_point(data = dplyr::filter(readings_data,
                                               .data$name == "HR"),
                          ggplot2::aes(x = .data$time, y = .data$value),
                          color = "red", size = 0.5)  +
      ggplot2::scale_fill_manual(values = fill_colors) +
      ggplot2::scale_color_manual(values = c(HR = "red", steps = "black")) +
      ggplot2::scale_x_time(breaks = hms::hms(hours = c(0,6,12,18,24)),
                            position = "top") +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_continuous(
        name = "Minute step count",
        sec.axis = ggplot2::sec_axis(~ (. - min_steps) / (max_steps - min_steps) * (max_HR - min_HR) + min_HR,
                                     name= "Minute average HR")
      ) +
      ggplot2::labs(x = "") +
      ggplot2::guides(
        fill = ggplot2::guide_legend(""),
        color = ggplot2::guide_legend("")
      ) +
      ggplot2::theme(
        legend.position = "bottom"
      )
  } else {
    p <- readings_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(x = .data$time,
                                      y = .data$value,
                                      color = .data$name)) +
      ggplot2::geom_point(data = dplyr::filter(readings_data,
                                               .data$name == "HR"),
                          ggplot2::aes(x = .data$time, y = .data$value),
                          color = "red", size = 0.5)  +
      ggplot2::scale_color_manual(values = c(HR = "red", steps = "black")) +
      ggplot2::scale_x_time(breaks = hms::hms(hours = c(0,6,12,18,24)),
                            position = "top") +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_continuous(
        name = "Minute step count",
        sec.axis = ggplot2::sec_axis(~ (. - min_steps) / (max_steps - min_steps) * (max_HR - min_HR) + min_HR,
                                     name= "Minute average HR")
      ) +
      ggplot2::labs(x = "") +
      ggplot2::guides(
        fill = ggplot2::guide_legend(""),
        color = ggplot2::guide_legend("")
      ) +
      ggplot2::theme(
        legend.position = "bottom"
      )
  }
  p
}
