#' flag_nonwear
#'
#' @param HR a numeric vector representing HR readings of a Fitbit.
#' @param steps a numeric vector representing step readings of a Fitbit. Depending on the nonwear method, either HR or steps or both are used.
#' @param nonwear_method a `character` entry, the name of one of the non wear method options: "missing_HR", "missing_HR_zero_steps", "choi_HR", or "choi_steps".
#' @param ... additional parameters that go into the selected nonwear_method.
#'
#' @return nonwear vector, non wear time encoded as T, and wear time encoded as F.
#' @export
#'
#' @examples
#' \dontrun{
#' minute_data %>%
#'   group_by(id) %>%
#'   mutate(
#'     missing_HR = flag_nonwear(HR, steps, "missing_HR"),
#'     missing_HR_zero_steps = flag_nonwear(HR, steps, "missing_HR_zero_steps"),
#'     choi_HR = flag_nonwear(HR, steps, "choi_HR"),
#'     choi_steps = flag_nonwear(HR, steps, "choi_steps")
#'   )
#' }
flag_nonwear <- function(
    HR,
    steps,
    nonwear_method = c("missing_HR",
                       "missing_HR_zero_steps",
                       #"claudel",
                       "choi_HR",
                       "choi_steps"),
    ...
) {
  if(nonwear_method[1] == "missing_HR") {
    nonwear = is.na(HR)
  }
  if(nonwear_method[1] == "missing_HR_zero_steps") {
    nonwear = (is.na(HR) & (steps == 0)) | is.na(steps)
  }
  if(nonwear_method[1] == "choi_HR") {
    nonwear = flag_choi_HR(HR, ...)
  }
  if(nonwear_method[1] == "choi_steps") {
    nonwear = flag_choi_steps(steps, ...)
  }
  return(nonwear)
}
