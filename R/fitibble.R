#' @title New Fitibble
#'
#' @description
#'
#' @param x a tibble (containing Fitbit minute readings).
#' @param intensity_colname a character entry indicating the name of the column in `x` that contains the intensity of each minute reading.
#' @param intensity_levels a named character vector indicating the labels and levels of the intensity categories in `intensity_col`.
#'
#' @return a fitibble object.
#'
#' @examples
new_fitibble <- function(
    x = tibble::tibble(),
    intensity_colname = character(),
    intensity_levels = character() #named character vector
    ) { #new issue:ideally a fitibble should be a minute-level tsibble
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

#' @export
print.fitibble <- function(x, ...) {
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
#' @return Checks a fitibble for internal consistency.
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
  if (sum(!unique(x[[intensity_colname]]) %in% c(intensity_levels, NA))> 0) {
    stop(
      "Non-valid `intensity_levels`",
      call. = FALSE
    )
  }

  x
}

#' relevel_fitibble
#'
#' @details Redefine `intensity_colname` and `intensity_levels` attributes in a fitibble.
#'
#' @param x a fitibble.
#' @param intensity_colname a character entry indicating the name of the column that contains the intensity of each minute reading in `x`.
#' @param intensity_levels a named character vector indicating the labels and levels of the intensity categories in `intensity_col`.
#'
#' @return a fitibble with redefined intensity attributes.
#'
#' @export
#'
#' @examples
relevel_fitibble <- function( #new issue:make this function a generic in future versions
    x,
    intensity_colname,
    intensity_levels) {
  new_fitibble(
    x,
    intensity_colname = intensity_colname,
    intensity_levels = intensity_levels
  ) %>%
    validate_fitibble()
}

#' fitibble
#'
#' @details Prepares a `fitibble` object.
#'
#' @param .data A tibble containing `id` | `time` | `HR` | `steps` | `intensity`. The intensity column can have another name, specified by `intensity_col`. It can be the output of `prepare_minute_data()` or a tibble or data.frame with columns  `id` (character)| `time` (POSIXct yyyy-mm-dd HH:MM:SS) |`HR` (numeric)|`steps` (numeric) and with minute-level values in each row. It is necessary that for each participant the minute sequence has no gaps and  missing records should be entered as `NA`.
#' @param intensity_colname a character entry indicating the name of the column that contains the intensity of each minute reading in `.data`.
#' @param intensity_levels a named character vector indicating the labels and levels of the intensity categories in `intensity_col`.
#' @param nonwear_method one of the following "missing_HR", "missing_HR_zero_steps", "choi_HR", "choi_steps"  or  "none".
#' @param nonwear_args additional parameters that go into the nonwear method.
#' @param adherent_method one of the following "adherent_hours_between" (other adherence rule could be integrated into the package).
#' @param adherent_args additional parameters that go into the adherent method.
#' @param valid_day_method one of the following "valid_adherent_hours" or "valid_step_count".
#' @param valid_day_args additional parameters that go into the valid day method.
#'
#' @return a fitibble, an object containing arguments that are useful for generating physical activity summaries from Fitbit data.
#'
#' @export
#'
#' @examples
fitibble <- function(
    .data,
    intensity_colname = "intensity",
    intensity_levels = c(sedentary = "0",
                         light = "1",
                         moderate = "2",
                         active = "3"),
    nonwear_method = c("missing_HR",
                       "missing_HR_zero_steps",
                       "choi_HR",
                       "choi_steps", 
                       "none"),
    nonwear_args = list(),
    adherent_method = c("adherent_hours_between"),
    adherent_args = list(),
    valid_day_method = c("valid_adherent_hours",
                         "valid_step_count"),
    valid_day_args = list()
) {
  .data %<>%
    dplyr::group_by(.data$id) %>%
    dplyr::mutate(
      is_wear = !do.call("flag_nonwear",
                         c(list(HR = .data$HR,
                                steps = .data$steps,
                                nonwear_method = nonwear_method[1]),
                           nonwear_args)),
      is_adherent = do.call("flag_adherent",
                            c(list(time = .data$time,
                                   adherent_method = adherent_method[1]),
                              adherent_args)),
      date = as.Date(.data$time)
    )

  .data %<>%
    dplyr::group_by(.data$id, .data$date) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      is_valid_day = purrr::map(
        .data$data,
        function(x)
          do.call("flag_valid_day",
                  c(list(is_wear = x$is_wear,
                         is_adherent = x$is_adherent,
                         steps = x$steps,
                         valid_day_method = valid_day_method[1]),
                    valid_day_args)))
    ) %>%
    tidyr::unnest(cols = c(.data$data, .data$is_valid_day)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$date)

  new_fitibble(
    .data,
    intensity_colname = intensity_colname,
    intensity_levels = intensity_levels
  ) %>%
    validate_fitibble()
}

#' crop_valid
#'
#' @details Crops minutes of valid wear in a fitibble.
#'
#' @param .data a fitibble.
#' @param nonvalid determines if nonvalid wear should be cropped instead of valid wear
#' @param flag_valid indicates whether the `is_valid` flag should be added to `.data`.
#' @param mask indicates whether minutes that are not valid wear should be masked with an `NA` instead of filtered out. Only entries in HR, steps and intensity columns are masked.
#'
#' @return a fitibble of valid wear entries,
#' @export
#'
#' @examples
crop_valid <- function(.data, nonvalid = F, flag_valid = T, mask = F) {
  intensity_colname <- attr(.data, "intensity_colname")
  is_valid <- .data$is_wear &
    .data$is_adherent &
    .data$is_valid_day
  is_nonvalid <- .data$is_wear &
    .data$is_adherent &
    !.data$is_valid_day

  if(flag_valid) {
    .data$is_valid <- is_valid
    .data$is_nonvalid <- is_nonvalid
  }

  if(!nonvalid){
    if(mask){
      .data$HR[!is_valid] <- as.numeric(NA)
      .data$steps[!is_valid] <- as.numeric(NA)
      .data[[intensity_colname]][!is_valid] <- as.numeric(NA)
    } else {
      .data <- .data[is_valid,]
    }
  }

  if(nonvalid) {
    if(mask){
      .data$HR[!is_nonvalid] <- as.numeric(NA)
      .data$steps[!is_nonvalid] <- as.numeric(NA)
      .data[[intensity_colname]][!is_nonvalid] <- as.numeric(NA)
    } else {
      .data <- .data[is_nonvalid,]
    }
  }

  .data
}

#' crop_nonvalid
#'
#' @details Crops minutes of nonvalid wear in a fitibble.
#'
#' @param .data a fitibble.
#' @param flag_nonvalid indicates whether the `is_nonvalid` flag should be added to `.data`
#' @param mask indicates whether minutes that are not nonvalid wear should be masked with an `NA` instead of filtered out. Only entries in HR, steps and intensity columns are masked.
#'
#' @return a fitibble of nonvalid wear entries.
#'
#' @examples
crop_nonvalid <- function(.data, flag_nonvalid = T, mask = F) {
  intensity_colname <- attr(.data, "intensity_colname")
  is_nonvalid <- .data$is_wear &
    .data$is_adherent &
    !.data$is_valid_day

  if(flag_nonvalid) {
    .data$is_nonvalid <- is_nonvalid
  }

  if(mask){
    .data$HR[!is_nonvalid] <- as.numeric(NA)
    .data$steps[!is_nonvalid] <- as.numeric(NA)
    .data[[intensity_colname]][!is_nonvalid] <- as.numeric(NA)
  } else {
    .data <- .data[is_nonvalid,]
  }

  .data
}
