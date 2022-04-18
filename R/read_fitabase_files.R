#' Read Fitabase files
#'
#' @param zip_path a path to a zip file that contains minute HR, steps and intensity files extracted from Fitabase. Filenames suffix contain '_heartrate_1min', '_minuteStepsNarrow', or '_minuteIntensitiesNarrow' followed by 'yyyymmdd_yyyymmdd.csv'.
#'
#' @return A list that contains: 1) Files: a tibble that describes the files in zip path. It contains | filename | filetype (HR, steps or intensity) | lines (number of lines in the file) | label (fitabase participant id label) | id (a newly assigned id) | min_lines (a file minimum length check), 2) raw_HR_list, 3) raw_steps_list, 4) raw_intensity_list: lists of tibbles that contain raw minute HR, steps or intensity data for each fitabase participant.
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' fitabase_files <- read_fitabase_files(file.choose(new = FALSE))
#' lapply(fitabase_files$raw_HR_list,
#'        function(x) summary(table(x$time)))
#' lapply(fitabase_files$raw_HR_list, function(x) summary(x$HR))
#' lapply(fitabase_files$raw_steps_list,
#'        function(x) summary(table(x$time)))
#' lapply(fitabase_files$raw_steps_list, function(x) summary(x$steps))
#' lapply(fitabase_files$raw_intensity_list,
#'        function(x) summary(table(x$time)))
#' lapply(fitabase_files$raw_intensity_list, function(x) summary(as.factor(x$intensity)))
#' }

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
  files <- dplyr::bind_rows(files,
                     tibble::tibble(filename = list.files(files_path, pattern = "minuteStepsNarrow"),
                            filetype = "steps"))
  files <- dplyr::bind_rows(files,
                     tibble::tibble(filename = list.files(files_path, pattern = "minuteIntensitiesNarrow"),
                            filetype = "intensity"))
  files$lines <- sapply(file.path(files_path, files$filename), R.utils::countLines)
  files %<>%
    dplyr::mutate(label = sub("(_heartrate_1min|_minuteStepsNarrow|_minuteIntensitiesNarrow).*", "", .data$filename)) %>%
    dplyr::group_by(.data$label) %>%
    dplyr::mutate(id = dplyr::cur_group_id(),
           min_lines = min(.data$lines)) %>%
    dplyr::ungroup()

  #### ####
  # preprocessing HR
  #### ####

  files_ <- dplyr::filter(files, .data$filetype == "HR", .data$min_lines > 1)
  raw_HR_list <- lapply(1:nrow(files_),
                        function(r) {
                          readr::read_csv(file.path(files_path, files_$filename[r])) %>%
                            dplyr::mutate(id = files_$id[r],
                                   label = files_$label[r])
                        })

  read_raw_HR <- function(x) {
    x %>%
      dplyr::rename(time = .data$Time,
             HR = .data$Value) %>%
      dplyr::mutate(time = lubridate::parse_date_time(.data$time, "%m/%d/%Y %H:%M:%S %Op")) %>%
      dplyr::arrange(.data$time)
  }

  raw_HR_list <- lapply(raw_HR_list, read_raw_HR)

  #### ####
  # preprocessing steps
  #### ####

  files_ <- dplyr::filter(files, .data$filetype == "steps", .data$min_lines > 1)
  raw_steps_list <- lapply(1:nrow(files_),
                           function(r) {
                             readr::read_csv(file.path(files_path, files_$filename[r])) %>%
                               dplyr::mutate(id = files_$id[r],
                                      label = files_$label[r])
                           }
  )

  read_raw_steps <- function(x) {
    x %>%
      dplyr::rename(time = .data$ActivityMinute,
             steps = .data$Steps) %>%
      dplyr::mutate(time = lubridate::parse_date_time(.data$time, "%m/%d/%Y %H:%M:%S %Op")) %>%
      dplyr::arrange(.data$time)
  }

  raw_steps_list <- lapply(raw_steps_list, read_raw_steps)

  #### ####
  # preprocessing intensities
  #### ####

  files_ <- dplyr::filter(files, .data$filetype == "intensity", .data$min_lines > 1)
  raw_intensity_list <- lapply(1:nrow(files_),
                               function(r) {
                                 readr::read_csv(file.path(files_path, files_$filename[r])) %>%
                                   dplyr::mutate(id = files_$id[r],
                                          label = files_$label[r])
                               }
  )

  read_raw_intensity <- function(x) {
    x %>%
      dplyr::rename(time = .data$ActivityMinute,
             intensity = .data$Intensity) %>%
      dplyr::mutate(time = lubridate::parse_date_time(.data$time, "%m/%d/%Y %H:%M:%S %Op")) %>%
      dplyr::arrange(.data$time)
  }

  raw_intensity_list <- lapply(raw_intensity_list, read_raw_intensity)

  return(list(
    'files' = files,
    'raw_HR_list' = raw_HR_list,
    'raw_steps_list' = raw_steps_list,
    'raw_intensity_list' = raw_intensity_list
  ))
}
