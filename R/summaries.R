#' prep_daily_data
#'
#' @details Prepare daily data.
#'
#' @param .data a fitibble.
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

  daily_data <- crop_valid(.data, mask = T) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      date = as.Date(.data$time),
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
      crop_valid(.data, flag_valid = F) %>%
        tibble::as_tibble() %>%
        dplyr::select(
          .data$id, .data$time, .data$intensity_levels, .data$value
        ) %>%
        tidyr::pivot_wider(
          names_from = .data$intensity_levels,
          values_from = .data$value,
          names_glue = "{intensity_levels}_prop",
          values_fill = 0
        ) %>%
        dplyr::mutate(date = as.Date(.data$time)) %>%
        dplyr::group_by(.data$id, .data$date) %>%
        dplyr::summarise(
          dplyr::across(-c(.data$time), mean, na.rm = T)
        ) %>%
        dplyr::ungroup()
    )
}

prep_wear_summary <- function(.data) {
  .data %>%
    dplyr::mutate(date = as.Date(.data$time)) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise(
      min_date = min(.data$date),
      max_date = max(.data$date),
      adherent_wear_hours = sum(.data$is_wear * .data$is_adherent) / 60,
      valid_hours = sum(.data$is_wear & .data$is_adherent * .data$is_valid_day) / 60,
      nonvalid_hours = sum(.data$is_wear & .data$is_adherent * !.data$is_valid_day) / 60,
    )
}

prep_valid_wear_summary <- function(.data) {
  intensity_colname <- attr(.data, "intensity_colname")
  intensity_levels <- attr(.data, "intensity_levels")

  daily_data <- crop_valid(.data, mask = T) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      pos_steps = dplyr::if_else(.data$steps > 0, .data$steps, as.numeric(NA)),
      zero_steps = 1 * (.data$steps == 0)
    ) %>%
    dplyr::select(
      .data$id, .data$is_valid, .data$HR, .data$pos_steps, .data$zero_steps
    ) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise(
      min_HR__VWT = min(.data$HR, na.rm = T),
      max_HR__VWT = max(.data$HR, na.rm = T),
      mean_HR__VWT = mean(.data$HR, na.rm = T),
      sd_HR__VWT = stats::sd(.data$HR, na.rm = T),
      max_pos_steps__VWT = max(.data$pos_steps, na.rm = T),
      mean_pos_steps__VWT = mean(.data$pos_steps, na.rm = T),
      zero_steps_prop__VWT = sum(.data$zero_steps, na.rm = T) /
        sum(.data$is_valid, na.rm = T)
    ) %>%
    dplyr::ungroup()

  .data$intensity_levels <- factor(.data[[intensity_colname]], exclude = NULL)
  .data$intensity_levels <- .data$intensity_levels %>%
    forcats::fct_recode(!!!intensity_levels) %>%
    as.character()
  .data$value <- 1

  daily_data %>%
    dplyr::left_join(
      crop_valid(.data, flag_valid = F) %>%
        tibble::as_tibble() %>%
        dplyr::select(
          .data$id, .data$time, .data$intensity_levels, .data$value
        ) %>%
        tidyr::pivot_wider(
          names_from = .data$intensity_levels,
          values_from = .data$value,
          names_glue = "{intensity_levels}_prop__VWT",
          values_fill = 0
        ) %>%
        dplyr::group_by(.data$id) %>%
        dplyr::summarise(
          dplyr::across(-c(.data$time), mean, na.rm = T)
        ) %>%
        dplyr::ungroup()
    )
}

prep_nonvalid_wear_summary <- function(.data) {
  intensity_colname <- attr(.data, "intensity_colname")
  intensity_levels <- attr(.data, "intensity_levels")

  daily_data <- crop_nonvalid(.data, mask = T) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      pos_steps = dplyr::if_else(.data$steps > 0, .data$steps, as.numeric(NA)),
      zero_steps = 1 * (.data$steps == 0)
    ) %>%
    dplyr::select(
      .data$id, .data$is_nonvalid, .data$HR, .data$pos_steps, .data$zero_steps
    ) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise(
      min_HR__NVW = min(.data$HR, na.rm = T),
      max_HR__NVW = max(.data$HR, na.rm = T),
      mean_HR__NVW = mean(.data$HR, na.rm = T),
      sd_HR__NVW = stats::sd(.data$HR, na.rm = T),
      max_pos_steps__NVW = max(.data$pos_steps, na.rm = T),
      mean_pos_steps__NVW = mean(.data$pos_steps, na.rm = T),
      zero_steps_prop__NVW = sum(.data$zero_steps, na.rm = T) /
        sum(.data$is_nonvalid, na.rm = T)
    ) %>%
    dplyr::ungroup()

  .data$intensity_levels <- factor(.data[[intensity_colname]], exclude = NULL)
  .data$intensity_levels <- .data$intensity_levels %>%
    forcats::fct_recode(!!!intensity_levels) %>%
    as.character()
  .data$value <- 1

  daily_data %>%
    dplyr::left_join(
      crop_nonvalid(.data, flag_nonvalid = F) %>%
        tibble::as_tibble() %>%
        dplyr::select(
          .data$id, .data$time, .data$intensity_levels, .data$value
        ) %>%
        tidyr::pivot_wider(
          names_from = .data$intensity_levels,
          values_from = .data$value,
          names_glue = "{intensity_levels}_prop__NVW",
          values_fill = 0
        ) %>%
        dplyr::group_by(.data$id) %>%
        dplyr::summarise(
          dplyr::across(-c(.data$time), mean, na.rm = T)
        ) %>%
        dplyr::ungroup()
    )
}

#' prep_patient_summary
#'
#' @param .data a fittible.
#'
#' @return a tibble containing summary statistics for each subject `id` in `data`.
#' @export
#'
#' @examples
prep_patient_summary <- function(.data){
  prep_wear_summary(.data) %>%
    dplyr::full_join(prep_valid_wear_summary(.data)) %>%
    dplyr::full_join(prep_nonvalid_wear_summary(.data))
}
