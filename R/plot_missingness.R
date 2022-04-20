#' Plot missing values in Fitbit's HR and steps data
#'
#' @param minute_data Preprocessed fitbit data. It can be the output of `prepare_minute_data()` or a tibble or data.frame with columns  `id` | `time` (POSIXct yyyy-mm-dd HH:MM:SS) |`HR` |`steps` and minute-level values in each row. It is necessary that for each participant the minute sequence has no gaps and  missing records should be entered as `NA`.
#' @param calendar_time Default is `TRUE`. If set to `FALSE` then date values in the `x` axis are replaced by a numerical index.
#' @param type Either `stacked` or `raster`. The representation of the number of minutes that have missing values can be stacked or in raster format.
#'
#' @return A time line showing the number of minutes with missing values in a day. Minutes are either `missing HR`, `missing steps`, `missing HR and steps` or `no missing`
#' @export
#'
#' @examples
#' \dontrun{
#' plot_missingness(minute_data, type = "stacked")
#' plot_missingness(minute_data, type = "raster")}

plot_missingness <- function(minute_data, calendar_time = TRUE, type = c("stacked", "raster")) {
  minute_data %>%
    dplyr::mutate(
      HR_is_missing_ = is.na(.data$HR),
      steps_is_missing_ = is.na(.data$steps),
      miss_steps_and_miss_HR_ = .data$HR_is_missing_ * .data$steps_is_missing_,
      miss_steps_or_miss_HR_ = .data$HR_is_missing | .data$steps_is_missing
    ) %>%
    dplyr::group_by(.data$id, .data$date) %>%
    dplyr::summarise(
      miss_steps_and_miss_HR = sum(.data$miss_steps_and_miss_HR_, na.rm = T) / 60,
      HR_is_missing = sum(.data$HR_is_missing_ * (1 - .data$miss_steps_and_miss_HR_), na.rm = T) / 60,
      steps_is_missing = sum(.data$steps_is_missing_ * (1 - .data$miss_steps_and_miss_HR_), na.rm = T) / 60,
      no_missing = sum(!.data$miss_steps_or_miss_HR_, na.rm = T) / 60
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
    dplyr::mutate(name = stringr::str_replace_all(.data$name, "_", " ")) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x=.data$date, y=.data$value, fill=.data$name),
             stat="identity")  +
    ggplot2::facet_wrap(~id, nrow=23) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_discrete(type = c("grey80", "gray60", "orange1", "black")) +
    ggplot2::labs(fill = "", y = "hours in the day", x = "")
}
