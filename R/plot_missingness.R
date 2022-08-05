#' Plot missingness
#'
#' @param minute_data a fitibble
#' @param calendar_time Default is `TRUE`. If set to `FALSE` then date values in the `x` axis are replaced by a numerical index.
#' @param type Either `stacked` or `raster`. The representation of the number of minutes can be stacked or in raster format.
#'
#' @return A time line showing the number of minutes with missing values in a day. Minutes are either `missing HR`, `missing steps`, `missing HR and steps` or `no missing`
#' @export
#'
#' @examples
#' \dontrun{
#' plot_missingness(minute_data, type = "stacked")
#' plot_missingness(minute_data, type = "raster")}

plot_missingness <- function(
    minute_data,
    calendar_time = TRUE,
    type = c("stacked", "raster")
) {
  minute_data %<>%
    dplyr::mutate(
      date = as.Date(.data$time),
      HR_is_missing_ = is.na(.data$HR),
      steps_is_missing_ = is.na(.data$steps),
      miss_steps_and_miss_HR_ = .data$HR_is_missing_ & .data$steps_is_missing_,
      miss_steps_or_miss_HR_ = .data$HR_is_missing_ | .data$steps_is_missing_
    )

  if(!calendar_time) {
    minute_data %<>%
      dplyr::left_join(
        minute_data %>%
          dplyr::distinct(.data$id, .data$date) %>%
          dplyr::group_by(.data$id) %>%
          dplyr::mutate(index = dplyr::row_number()) %>%
          dplyr::ungroup(),
        by = c("id", "date")
      )

    minute_data$date <- minute_data$index
  }

  if(type[1] == "stacked") {
    ylab = "Proportion of missing readings per day"
    p <- minute_data %>%
      dplyr::group_by(.data$id, .data$date) %>%
      dplyr::summarise(
        miss_steps_and_miss_HR = sum(.data$miss_steps_and_miss_HR_) / 60,
        HR_is_missing = sum(.data$HR_is_missing_ * (1 - .data$miss_steps_and_miss_HR_)) / 60,
        steps_is_missing = sum(.data$steps_is_missing_ * (1 - .data$miss_steps_and_miss_HR_)) / 60,
        no_missing = sum(!.data$miss_steps_or_miss_HR_) / 60,
        .groups = 'drop'
      ) %>%
      dplyr::select(
        .data$id,
        .data$date,
        .data$miss_steps_and_miss_HR,
        .data$HR_is_missing,
        .data$steps_is_missing,
        .data$no_missing
      ) %>%
      tidyr::pivot_longer(
        cols = c('miss_steps_and_miss_HR',
                 'HR_is_missing',
                 'steps_is_missing',
                 'no_missing')
      ) %>%
      #dplyr::mutate(name = stringr::str_replace_all(.data$name, "_", " ")) %>%
      ggplot2::ggplot() +
      ggplot2::geom_bar(
        ggplot2::aes(x=.data$date, y=.data$value, fill=as.factor(.data$name)),
        stat="identity")  +
      ggplot2::facet_wrap(~id, nrow=length(unique(minute_data$id)))
  }
  if(type[1] == "raster") {
    ylab = "Minute-by-minute missing readings per day"
    p <- minute_data %>%
      dplyr::mutate(
        hour = lubridate::hour(.data$time),
        minute = lubridate::minute(.data$time),
        minute = .data$hour * 60 + .data$minute,
        miss_steps_and_miss_HR = (.data$miss_steps_and_miss_HR_) * 1,
        HR_is_missing = .data$HR_is_missing_ * (1 - .data$miss_steps_and_miss_HR_),
        steps_is_missing =.data$steps_is_missing_ * (1 - .data$miss_steps_and_miss_HR_),
        no_missing = (!.data$miss_steps_or_miss_HR_) * 1
      ) %>%
      dplyr::select(
        .data$id,
        .data$date,
        .data$minute,
        .data$miss_steps_and_miss_HR,
        .data$HR_is_missing,
        .data$steps_is_missing,
        .data$no_missing
      ) %>%
      tidyr::pivot_longer(
        cols = c('miss_steps_and_miss_HR',
                 'HR_is_missing',
                 'steps_is_missing',
                 'no_missing')
      ) %>%
      #dplyr::mutate(name = stringr::str_replace_all(.data$name, "_", " ")) %>%
      dplyr::filter(.data$value > 0) %>%
      ggplot2::ggplot() +
      ggplot2::geom_raster(
        ggplot2::aes(x=.data$date, y=.data$minute, fill=as.factor(.data$name))) +
      ggplot2::facet_wrap(~id, nrow=length(unique(minute_data$id)))
  }
  fill_colors <- c(miss_steps_and_miss_HR = "grey60",
                   HR_is_missing = "gray80",
                   steps_is_missing = "black",
                   no_missing =  "orange1")

  p +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::labs(fill = "", y = ylab, x = "")
}
