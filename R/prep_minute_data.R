#' prep_minute_data
#'
#' @details Prepare minute data
#'
#' @param fitabase_files the output list of `read_fitabase_files()`.
#'
#' @return A tibble containing `id` | `time` | `HR` | `steps` | `intensity`.
#' @export
#'
#' @examples
#' \dontrun{
#' minute_data <- prepare_minute_data(fitabase_files)
#' minute_data %>%
#' mutate(hour = hour(time),
#'        weekday = wday(as.Date(time), label = TRUE))
#' }
prep_minute_data <- function(fitabase_files) {

  #### ####
  # preprocess HR
  #### ####

  preprocess_raw_HR <- function(x) {
    ts <- seq(min(x$time), max(x$time), by="min")
    ts <- dplyr::tibble(time = ts)

    ts %>%
      dplyr::left_join(x, by = c("time")) %>%
      dplyr::mutate(id = zoo::na.locf(.data$id),
                    label = zoo::na.locf(.data$label)
      )
  }

  fitabase_files[["raw_HR_list"]] <- lapply(fitabase_files[["raw_HR_list"]], preprocess_raw_HR)
  raw_HR <- dplyr::bind_rows(fitabase_files[["raw_HR_list"]])

  #### ####
  # preprocess steps
  #### ####

  preprocess_raw_steps <- function(x) {
    ts <- seq(min(x$time), max(x$time), by="min")
    ts <- dplyr::tibble(time = ts)

    ts %>%
      dplyr::left_join(x, by = c("time")) %>%
      dplyr::mutate(id = zoo::na.locf(.data$id),
                    label = zoo::na.locf(.data$label)
      )
  }

  fitabase_files[["raw_steps_list"]] <- lapply(fitabase_files[["raw_steps_list"]], preprocess_raw_steps)
  raw_steps <- dplyr::bind_rows(fitabase_files[["raw_steps_list"]])

  #### ####
  # preprocess intensities
  #### ####

  preprocess_raw_intensity <- function(x) {
    ts <- seq(min(x$time), max(x$time), by="min")
    ts <- dplyr::tibble(time = ts)

    ts %>%
      dplyr::left_join(x, by = c("time")) %>%
      dplyr::mutate(id = zoo::na.locf(.data$id),
                    label = zoo::na.locf(.data$label)
      )
  }

  fitabase_files[["raw_intensity_list"]] <- lapply(fitabase_files[["raw_intensity_list"]], preprocess_raw_intensity)
  raw_intensity <- dplyr::bind_rows(fitabase_files[["raw_intensity_list"]])

  #### ####
  # join all datasets: minute_data
  #### ####

  raw_HR %<>%
    dplyr::left_join(
      fitabase_files[["time_period"]] %>%
        dplyr::select(.data$id, .data$label, .data$min_time, .data$max_time, .data$is_valid_time_period),
      by = c("id", "label")
    ) %>%
    dplyr::filter(.data$is_valid_time_period,
                  .data$time >= .data$min_time,
                  .data$time <= .data$max_time) %>%
    dplyr::select(-.data$min_time, -.data$max_time, -.data$is_valid_time_period)

  raw_steps %<>%
    dplyr::left_join(
      fitabase_files[["time_period"]] %>%
        dplyr::select(.data$id, .data$label, .data$min_time, .data$max_time, .data$is_valid_time_period),
      by = c("id", "label")
    ) %>%
    dplyr::filter(.data$is_valid_time_period,
                  .data$time >= .data$min_time,
                  .data$time <= .data$max_time) %>%
    dplyr::select(-.data$min_time, -.data$max_time, -.data$is_valid_time_period)

  raw_intensity %<>%
    dplyr::left_join(
      fitabase_files[["time_period"]] %>%
        dplyr::select(.data$id, .data$label, .data$min_time, .data$max_time, .data$is_valid_time_period),
      by = c("id", "label")
    ) %>%
    dplyr::filter(.data$is_valid_time_period,
                  .data$time >= .data$min_time,
                  .data$time <= .data$max_time) %>%
    dplyr::select(-.data$min_time, -.data$max_time, -.data$is_valid_time_period)

  minute_data <- raw_HR %>%
    dplyr::left_join(raw_steps, by = c("time", "id", "label")) %>%
    dplyr::left_join(raw_intensity, by = c("time", "id", "label"))

  minute_data
}
