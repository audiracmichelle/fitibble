#' flag_adherent_hours_between
#'
#' @details Flag adherent hours in the day. With this rule, the adherent time is bounded between lower/upper hour values as per the inclusion criteria of the study.
#'
#' @param time a numeric vector containing sequences of minute values in a day.
#' @param hours_between two-length numeric array taking lower/upper hour values between 0 and 24.
#'
#' @return boolean vector, adherent minutes encoded as T, and non-adherent encoded as F.
#' @export
#'
#' @examples
flag_adherent_hours_between <- function(
    time,
    hours_between = c(8, 20)
) {
  adherent <- (lubridate::hour(time) >= hours_between[1]) &
    (lubridate::hour(time) <= hours_between[2])
  return(adherent)
}

#' flag_adherent
#'
#' @details Flag adherent hours in the day. So far only the "adherent_hours_between" rule is implemented.
#'
#' @param time a numeric vector containing sequences of minute values in a day.
#' @param adherent_method one of the following "adherent_hours_between" (other adherence rule could be integrated into the package).
#' @param ... additional parameters required by the selected adherence criteria.
#'
#' @return boolean vector, adherent minutes encoded as T, and non-adherent encoded as F.
#' @export
#'
#' @examples
flag_adherent <- function(
    time,
    adherent_method = c("adherent_hours_between"),
    ...
) {
  if(adherent_method[1] == "adherent_hours_between") {
    adherent = flag_adherent_hours_between(time, ...)
  }
  return(adherent)
}
