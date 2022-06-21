#' new_fitibble
#'
#' @param x a tibble containing Fitbit HR and step minute readings for multiple subjects in a cohort.
#' @param intensity_colname a character entry indicating the name of the column that contains the intensity of each minute reading in `x`.
#' @param intensity_levels a named character vector indicating the labels and levels of the intensity categories.
#'
#' @return a fitibble object.
#'
#' @examples
new_fitibble <- function(
    x = tibble::tibble(),
    intensity_colname = character(),
    intensity_levels = character() #named character vector
    ) {
  stopifnot(tibble::is_tibble(x))
  stopifnot(is.character(intensity_colname))
  stopifnot(is.character(intensity_levels))

  tibble::new_tibble(
    x,
    intensity_colname = intensity_colname,
    intensity_levels = intensity_levels,
    class = "fitibble"
  )
}

#' print.fitibble
#'
#' @param x a fitibble.
#'
#' @return print fitibble content and arguments.
#'
#' @examples
print.fitibble <- function(x) {
  NextMethod(x)
  print(
    list(
      "intensity_colname" = attr(x, "intensity_colname"),
      "intensity_levels" = attr(x, "intensity_levels")
    )
  )
  invisible(x)
}

#' validate_fitibble
#'
#' @param x a fitibble-like object.
#'
#' @return Checks a tibble for internal consistency.
#'
#' @examples
validate_fitibble <- function(x) {
  intensity_colname <- attr(x, "intensity_colname")
  intensity_levels <- attr(x, "intensity_levels")

  if(sum(!c("time", "id", "HR", "steps",
            "is_wear", "is_adherent", "is_valid_day") %in% names(x)) > 0) {
    stop("`x` columns does not include must-have fitibble columns",
         call. = FALSE)
  }
  if(!intensity_colname %in% names(x)) {
    stop("Non-valid `intensity_colname`",
         call. = FALSE)
  }
  if (sum(!unique(x[[intensity_colname]]) %in% intensity_levels) > 0) {
    stop(
      "Non-valid `intensity_levels`",
      call. = FALSE
    )
  }

  x
}

#' fitibble
#'
#' @details `fitibble` prepares a fitibble object.
#'
#' @param minute_data A tibble containing `id` | `time` | `HR` | `steps` | `intensity`. The intensity column can have another name, specified by `intensity_col`.
#' @param intensity_colname a character entry indicating the name of the column that contains the intensity of each minute reading in `minute_data`.
#' @param intensity_levels a named character vector indicating the labels and levels of the intensity categories in `intensity_col`.
#' @param nonwear_method one of the following "missing_HR", "missing_HR_zero_steps", "choi_HR" or "choi_steps".
#' @param adherent_method one of the following "adherent_hours_between" (other adherence rule could be integrated into the package).
#' @param valid_day_method one of the following "valid_adherent_hours" or "valid_step_count".
#' @param ... additional parameters that go into the nonwear, adherence or valid day methods.
#'
#' @return a fitibble, an object containing arguments that are useful for generating physical activity summaries from Fitbit data.
#'
#' @export
#'
#' @examples
fitibble <- function(
    minute_data,
    intensity_colname = "intensity",
    intensity_levels = c(sedentary = "0",
                         light = "1",
                         moderate = "2",
                         active = "3"),
    nonwear_method = c("missing_HR",
                       "missing_HR_zero_steps",
                       "choi_HR",
                       "choi_steps"),
    adherent_method = c("adherent_hours_between"),
    valid_day_method = c("valid_adherent_hours",
                         "valid_step_count"),
    ...
) {
  minute_data %<>%
    dplyr::group_by(.data$id) %>%
    dplyr::mutate(
      is_wear = !flag_nonwear(.data$HR,
                              .data$steps,
                              nonwear_method = nonwear_method[1],
                              ...),
      is_adherent = flag_adherent(.data$time,
                                  adherent_method = adherent_method[1],
                                  ...),
      date = as.Date(.data$time)
    )

  minute_data %<>%
    dplyr::group_by(.data$id, .data$date) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      is_valid_day = purrr::map(.data$data, function(x)
        flag_valid_day(x$is_wear,
                       x$is_adherent,
                       steps = x$steps,
                       valid_day_method = valid_day_method[1],
                       ...))
    ) %>%
    tidyr::unnest(cols = c(.data$data, .data$is_valid_day)) %>%
    dplyr::ungroup()

  new_fitibble(
    minute_data,
    intensity_colname = intensity_colname,
    intensity_levels = intensity_levels
  )
}
