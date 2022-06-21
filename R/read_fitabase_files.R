#' Read Fitabase files
#'
#' @param zip_path a path to a zip file that contains minute HR, steps and intensity files extracted from Fitabase. Filenames suffix contain '_heartrate_1min', '_minuteStepsNarrow', or '_minuteIntensitiesNarrow' followed by 'yyyymmdd_yyyymmdd.csv'.
#'
#' @return A list that contains:
#' * `files`: a tibble that describes the files in zip path. It contains `filename` | `filetype` (HR, steps or intensity) | `lines` (number of lines in the file) | `label` (fitabase participant id label) | `id` (a newly assigned id) | `is_valid_file_length` (minimum file length check).
#' * `raw_HR_list`, `raw_steps_list`, `raw_intensity_list`: lists of tibbles that contain raw minute HR, steps or intensity data for each fitabase participant.
#' * `time_period`: a tibble that summarizes the minimum and maximum time recorded for each fitabase participant and file type (HR, steps and intensity). `is_valid_time_period` corresponds to the minimum time period check (at least one day of overlaping records from 0:00 to 23:59 hrs)
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' fitabase_files <- read_fitabase_files(file.choose(new = FALSE))
#' fitabase_files$files
#' fitabase_files$time_period
#' lapply(fitabase_files$raw_HR_list, function(x) summary(x$HR))
#' lapply(fitabase_files$raw_steps_list, function(x) summary(x$steps))
#' lapply(fitabase_files$raw_intensity_list, function(x) summary(as.factor(x$intensity)))}

read_fitabase_files <- function(zip_path) {

  #### ####
  # read data
  #### ####

  # create temp folder
  if(dir.exists(file.path(tempdir(), "files_path")))
    fs::dir_delete(file.path(tempdir(), "files_path"))
  files_path <- fs::dir_create(file.path(tempdir(), "files_path"))

  # unzip in temp folder
  zip::unzip(zip_path, exdir = files_path, junkpaths = T)

  # list files in zip
  files <- tibble::tibble(filename = list.files(files_path, pattern = "heartrate_1min"),
                          filetype = "HR")
  files <- dplyr::bind_rows(
    files,
    tibble::tibble(filename = list.files(files_path, pattern = "minuteStepsNarrow"),
                   filetype = "steps")
  )
  files <- dplyr::bind_rows(
    files,
    tibble::tibble(filename = list.files(files_path, pattern = "minuteIntensitiesNarrow"),
                   filetype = "intensity"))
  files$lines <- sapply(file.path(files_path, files$filename), R.utils::countLines)
  files %<>%
    dplyr::mutate(label = sub("(_heartrate_1min|_minuteStepsNarrow|_minuteIntensitiesNarrow).*",
                              "", .data$filename)) %>%
    dplyr::group_by(.data$label) %>%
    dplyr::mutate(id = dplyr::cur_group_id(),
                  is_valid_file_length = min(.data$lines) > 1) %>%
    dplyr::ungroup()

  #### ####
  # read HR
  #### ####

  files_ <- dplyr::filter(files, .data$filetype == "HR", .data$is_valid_file_length)
  raw_HR_list <- lapply(1:nrow(files_),
                        function(r) {
                          readr::read_csv(file.path(files_path,
                                                    files_$filename[r]),
                                          show_col_types = FALSE) %>%
                            dplyr::mutate(id = files_$id[r],
                                          label = files_$label[r])
                        })

  read_raw_HR <- function(x) {
    x %>%
      dplyr::rename(time = .data$Time,
                    HR = .data$Value) %>%
      dplyr::mutate(time = lubridate::parse_date_time(.data$time, "%m/%d/%Y %H:%M:%S %Op")) %>%
      dplyr::arrange(.data$time) %>%
      dplyr::select(.data$id, .data$label, .data$time, .data$HR)
  }

  raw_HR_list <- lapply(raw_HR_list, read_raw_HR)

  #### ####
  # read steps
  #### ####

  files_ <- dplyr::filter(files, .data$filetype == "steps", .data$is_valid_file_length)
  raw_steps_list <- lapply(1:nrow(files_),
                           function(r) {
                             readr::read_csv(file.path(files_path,
                                                       files_$filename[r]),
                                             show_col_types = FALSE) %>%
                               dplyr::mutate(id = files_$id[r],
                                             label = files_$label[r])
                           }
  )

  read_raw_steps <- function(x) {
    x %>%
      dplyr::rename(time = .data$ActivityMinute,
                    steps = .data$Steps) %>%
      dplyr::mutate(time = lubridate::parse_date_time(.data$time, "%m/%d/%Y %H:%M:%S %Op")) %>%
      dplyr::arrange(.data$time) %>%
      dplyr::select(.data$id, .data$label, .data$time, .data$steps)
  }

  raw_steps_list <- lapply(raw_steps_list, read_raw_steps)

  #### ####
  # read intensities
  #### ####

  files_ <- dplyr::filter(files, .data$filetype == "intensity", .data$is_valid_file_length)
  raw_intensity_list <- lapply(1:nrow(files_),
                               function(r) {
                                 readr::read_csv(file.path(files_path,
                                                           files_$filename[r]),
                                                 show_col_types = FALSE) %>%
                                   dplyr::mutate(id = files_$id[r],
                                                 label = files_$label[r])
                               }
  )

  read_raw_intensity <- function(x) {
    x %>%
      dplyr::rename(time = .data$ActivityMinute,
                    intensity = .data$Intensity) %>%
      dplyr::mutate(time = lubridate::parse_date_time(.data$time, "%m/%d/%Y %H:%M:%S %Op")) %>%
      dplyr::arrange(.data$time) %>%
      dplyr::select(.data$id, .data$label, .data$time, .data$intensity)
  }

  raw_intensity_list <- lapply(raw_intensity_list, read_raw_intensity)

  #### ####
  # deriving time period
  #### ####
  time_period <- raw_HR_list %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(.data$id, .data$label) %>%
    dplyr::summarise(min_HR_time = min(.data$time),
                     max_HR_time = max(.data$time)) %>%
    dplyr::left_join(
      raw_steps_list %>%
        dplyr::bind_rows() %>%
        dplyr::group_by(.data$id, .data$label) %>%
        dplyr::summarise(min_steps_time = min(.data$time),
                         max_steps_time = max(.data$time)),
      by = c("id", "label")
    ) %>%
    dplyr::left_join(
      raw_intensity_list %>%
        dplyr::bind_rows() %>%
        dplyr::group_by(.data$id, .data$label) %>%
        dplyr::summarise(min_intensity_time = min(.data$time),
                         max_intensity_time = max(.data$time)),
      by = c("id", "label")
    ) %>%
    dplyr::ungroup()

  time_period %<>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      min_time = as.Date(max(.data$min_HR_time,
                             .data$min_steps_time,
                             .data$min_intensity_time)) + 1,
      max_time = as.Date(min(.data$max_HR_time,
                             .data$max_steps_time,
                             .data$max_intensity_time)) - 1 +
        lubridate::hours(23) + lubridate::minutes(59),
      is_valid_time_period = (.data$max_time > .data$min_time)
    )

  return(list(
    'files' = files,
    'raw_HR_list' = raw_HR_list,
    'raw_steps_list' = raw_steps_list,
    'raw_intensity_list' = raw_intensity_list,
    'time_period' = time_period
  ))
}
