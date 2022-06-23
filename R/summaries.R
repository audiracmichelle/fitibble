#' prep_daily_data
#'
#' @details Prepare daily data.
#'
#' @param .data a fitibble with readings of Fitbit data.
#'
#' @return a tibble containing daily statistics derived from minute-level data.
#' @export
#'
#' @examples
prep_daily_data <- function(
    .data
) {
  intensity_colname <- attr(.data, "intensity_colname")
  intensity_levels <- attr(.data, "intensity_levels")

  .data <- crop_valid(.data)

  daily_data <- .data %>%
    tibble::as_tibble %>%
    dplyr::mutate(
      zero_steps = 1 * (.data$steps == 0)
    ) %>%
    dplyr::select(
      .data$id, .data$date, .data$is_valid, .data$HR, .data$zero_steps
    ) %>%
    dplyr::group_by(.data$id, .data$date) %>%
    dplyr::summarise(
      valid_mins = sum(.data$is_valid, na.rm = T),
      HR = mean(.data$HR, na.rm = T),
      zero_steps_prop = sum(.data$zero_steps, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      zero_steps_prop = .data$zero_steps_prop / .data$valid_mins
    )

  .data$intensity_levels <- factor(.data[[intensity_colname]], exclude = NULL)
  .data$intensity_levels <- .data$intensity_levels %>%
    forcats::fct_recode(!!!intensity_levels) %>%
    as.character()
  .data$value <- 1

  daily_data %>%
    dplyr::left_join(
      .data %>%
        tibble::as_tibble() %>%
        dplyr::filter(.data$is_valid) %>%
        dplyr::select(
          .data$id, .data$date, .data$time, .data$intensity_levels, .data$value
        ) %>%
        tidyr::pivot_wider(
          names_from = .data$intensity_levels,
          values_from = .data$value,
          names_glue = "{intensity_levels}_prop",
          values_fill = 0
        ) %>%
        dplyr::group_by(.data$id, .data$date) %>%
        dplyr::summarise(
          dplyr::across(-c(.data$time), .data$mean, na.rm = T)
        ) %>%
        dplyr::ungroup()
    )
}
