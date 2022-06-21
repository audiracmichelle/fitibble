#' flag_valid_adherent_hours
#'
#' @details Flag valid days using minimum adherent hours criteria
#'
#' @param is_wear a logic vector with entries for every minute in day. `TRUE` values correspond to those minutes in the day that are considered to be wear time.
#' @param is_adherent a logic vector indicating minutes in a day that adhere to a study's inclusion criteria.
#' @param minimum_adherent_hours the minimum number of adherent hours required in a day to be considered valid.
#'
#' @return a single boolean indicating whether the minimum adherent hours criteria is satisfied.
#' @export
#'
#' @examples
flag_valid_adherent_hours <- function(
    is_wear,
    is_adherent,
    minimum_adherent_hours = 10
) {
  if(sum(is_wear * is_adherent) >= (minimum_adherent_hours * 60)) {
    valid_day = T
  } else valid_day = F
  return(valid_day)
}

#' flag_valid_step_count
#'
#' @details Flag valid days using minimum step count criteria.
#'
#' @param is_wear a logic vector with entries for every minute in day. `TRUE` values correspond to those minutes in the day that are considered to be wear time.
#' @param is_adherent a logic vector indicating minutes in a day that adhere to a study's inclusion criteria.
#' @param steps a numeric vector containing Fitbit minute step recordings in a day.
#' @param minimum_step_count the minimum number of step counts required in a day to be considered valid.
#'
#' @return a single boolean indicating whether the minimum step counts criteria is satisfied.
#' @export
#'
#' @examples
flag_valid_step_count <- function(
    is_wear,
    is_adherent,
    steps,
    minimum_step_count = 1000
) {
  if(sum(is_wear * is_adherent * steps, na.rm = T) >= minimum_step_count) {
    valid_day = T
  } else valid_day = F
  return(valid_day)
}

#' flag_valid_day
#'
#' @details Flag valid days using either minimum adherent hours or minimum step count criteria.
#'
#' @param is_wear a logic vector with entries for every minute in day. `TRUE` values correspond to those minutes in the day that are considered to be wear time.
#' @param is_adherent a logic vector indicating minutes in a day that adhere to a study's inclusion criteria.
#' @param valid_day_method either "valid_adherent_hours" or "valid_step_count".
#' @param steps a numeric vector containing Fitbit minute step recordings in a day.
#' @param ... additional parameters that go into the selected valid_day_method.
#'
#' @return a single boolean indicating whether the valid day criteria is satisfied.
#' @export
#'
#' @examples
flag_valid_day <- function(
    is_wear,
    is_adherent,
    steps,
    valid_day_method = c("valid_adherent_hours",
                         "valid_step_count"),
    ...
) {
  if(valid_day_method[1] == "valid_adherent_hours") {
    valid_day = flag_valid_adherent_hours(is_wear, is_adherent, ...)
  }
  if(valid_day_method[1] == "valid_step_count") {
    valid_day = flag_valid_step_count(is_wear, is_adherent, steps, ...)
  }
  return(valid_day)
}
