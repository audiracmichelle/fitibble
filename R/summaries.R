#' mean_sed_bout_length
#'
#' @details Calculates the mean sedentary bout in a day .
#'
#' @param is_sedentary a logic vector with entries for every minute in day. `TRUE` values correspond to those minutes in the day that are considered to be sedentary time.
#'
#' @return a single numeric value, the mean of sedentary bouts in a day.
#' @export
#'
#' @examples
mean_sed_bout_length <- function(is_sedentary) {
  if(!is.logical(is_sedentary)) stop("is_sedentary must be binary")
  consecutive_1s <- rle(is_sedentary)
  consecutive_1s <- consecutive_1s$lengths[consecutive_1s$values]
  mean(consecutive_1s)
}

#' prep_daily_data
#'
#' @details Prepares daily data derived from valid wear in a fitibble.
#'
#' @param .data a fitibble.
#' @param nonvalid determines if nonvalid wear should be cropped instead of valid wear.
#' @param sedentary_level named vector of length one indicating the intensity value and name of the PA intensity level that represents the sedentary behavior.
#'
#' @return a tibble containing daily statistics of valid wear for each date and subject `id` in `.data`.
#' @export
#'
#' @examples
prep_daily_data <- function(
    .data,
    nonvalid = F,
    sedentary_level = c(sedentary = "0")
) {
  intensity_colname <- attr(.data, "intensity_colname")
  intensity_levels <- attr(.data, "intensity_levels")

  if(!identical(intensity_levels[names(sedentary_level)], sedentary_level)) stop("sedentary_level must match an element in intensity_levels")
  .data$is_sedentary <- (.data[[intensity_colname]] == sedentary_level)

  daily_data <- crop_valid(.data, nonvalid = nonvalid, mask = T) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      date = as.Date(.data$time),
      zero_steps = 1 * (.data$steps == 0),
      is_sedentary = dplyr::if_else(is.na(.data$intensity), FALSE, .data$is_sedentary) #modify crop_valid so that all non identifier columns are masked
    ) %>%
    dplyr::select(
      .data$id, .data$date, .data$is_valid, .data$is_nonvalid, .data$HR, .data$zero_steps, .data$steps, .data$is_sedentary
    ) %>%
    dplyr::group_by(.data$id, .data$date) %>%
    dplyr::summarise(
      valid_mins = sum(.data$is_valid),
      nonvalid_mins = sum(.data$is_nonvalid),
      HR = mean(.data$HR, na.rm = T),
      zero_steps_prop = sum(.data$zero_steps, na.rm = T),
      steps = sum(.data$steps, na.rm = T) / min(1, sum(!is.na(.data$steps))), #the lower part of the fraction avoids zero sum if all steps are NA
      sed_bout_length = mean_sed_bout_length(.data$is_sedentary) #think thought the order between sedentary bout length calculation and valid minutes filtering
    ) %>%
    dplyr::ungroup()

  if(nonvalid) {
    daily_data$zero_steps_prop <- daily_data$zero_steps_prop / daily_data$nonvalid_mins
  } else {
    daily_data$zero_steps_prop <- daily_data$zero_steps_prop / daily_data$valid_mins
  }

  .data$intensity_levels <- factor(.data[[intensity_colname]], exclude = NULL)
  .data$intensity_levels <- .data$intensity_levels %>%
    forcats::fct_recode(!!!intensity_levels) %>%
    as.character()
  .data$value <- 1

  daily_data %>%
    dplyr::left_join(
      crop_valid(.data, nonvalid = nonvalid, flag_valid = F) %>%
        tibble::as_tibble() %>%
        dplyr::select(
          .data$id, .data$time, .data$intensity_levels, .data$value
        ) %>%
        tidyr::pivot_wider(
          names_from = .data$intensity_levels,
          values_from = .data$value,
          names_glue = "{intensity_levels}_time_use",
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

#' prep_daily_summary
#'
#' @param .data a fitibble.
#' @param nonvalid determines if nonvalid wear should be cropped instead of valid wear.
#' @param sedentary_level named vector of length one indicating the intensity value and name of the PA intensity level that represents the sedentary behavior.
#'
#' @return a tibble containing summary statistics that describe daily patterns for each subject `id` in `.data`.
#' @export
#'
#' @examples
prep_daily_summary <- function(
    .data,
    nonvalid = F,
    sedentary_level = c(sedentary = "0")
) {
  prep_daily_data(.data, nonvalid = nonvalid, sedentary_level = sedentary_level) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise(
      valid_days = sum(.data$valid_mins > 0),
      mu_HR = mean(.data$HR, na.rm = T),
      sd_HR = stats::sd(.data$HR, na.rm = T),
      mu_steps = mean(.data$steps, na.rm = T),
      sd_steps = stats::sd(.data$steps, na.rm = T),
      mu_sed_bout_length = mean(.data$sed_bout_length, na.rm = T),
      sd_sed_bout_length = stats::sd(.data$sed_bout_length, na.rm = T),
      mu_zero_steps_prop = mean(.data$zero_steps_prop, na.rm = T),
      sd_zero_steps_prop = stats::sd(.data$zero_steps_prop, na.rm = T),
      dplyr::across(
        tidyselect::contains("time_use"),
        list(mu = ~ mean(.x, na.rm = T),
             sd = ~ stats::sd(.x, na.rm = T)),
        .names = "{.fn}_{.col}")
    ) %>%
    dplyr::ungroup()
}

prep_wear_summary <- function(.data) {
  .data %>%
    dplyr::mutate(date = as.Date(.data$time)) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise(
      min_date = min(.data$date),
      max_date = max(.data$date),
      total_hours = dplyr::n() / 60,
      wear_hours = sum(.data$is_wear) / 60,
      adherent_hours = sum(.data$is_wear * .data$is_adherent) / 60,
      valid_hours = sum(.data$is_wear & .data$is_adherent * .data$is_valid_day) / 60,
      nonvalid_hours = sum(.data$is_wear & .data$is_adherent * !.data$is_valid_day) / 60,
    )
}

prep_valid_wear_summary <- function(.data) {
  intensity_colname <- attr(.data, "intensity_colname")
  intensity_levels <- attr(.data, "intensity_levels")

  valid_wear_summary <- crop_valid(.data, mask = T) %>%
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

  valid_wear_summary %>%
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

  nonvalid_wear_summary <- crop_nonvalid(.data, mask = T) %>%
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

  nonvalid_wear_summary %>%
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
