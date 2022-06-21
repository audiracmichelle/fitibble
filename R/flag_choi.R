#' Apply Choi et al nonwear algorithm
#'
#' @details
#' Implements non-wear algorithm closely following that of Choi et al. (2011).
#'
#' Med Sci Sports Exerc. 2011 Feb;43(2):357-64. doi: 10.1249/MSS.0b013e3181ed61a3.
#' Validation of accelerometer wear and nonwear time classification algorithm.
#' Choi L1, Liu Z, Matthews CE, Buchowski MS.
#'
#' Code implementation from:https://github.com/shaheen-syed/ActiGraph-ActiWave-Analysis/
#'
#' @param x a numeric vector, no NAs. Values correspond to timestamps for each epoch, note that 1 epoch is 60s.
#' @param activity_threshold values below `activity_threshold` are classified as nonwear as per Choi et al. (2011). The activity threshold is the value of the count that is considered "zero", since we are searching for a sequence of zero counts. Default threshold is 0.
#' @param min_period_len the minimum length of the consecutive zeros that can be considered valid non wear time. Default value is 90 (since we have 60sec epoch data, this equals 90 mins).
#' @param spike_tolerance any count that is above the activity threshold is considered a spike. The tolerence defines the number of spikes that are acceptable within a sequence of zeros. The default is 2, meaning that we allow for 2 spikes in the data, i.e. artifactual movement.
#' @param min_window_len minimum length of upstream or downstream time window (referred to as window2 in the paper) for consecutive zero counts required before and after the artifactual movement interval to be considered a nonwear time interval.
#' @param window_spike_tolerance accepted number of spikes in upstream and downstream windows.
#' @param print_output if set to True, then print the output of the non wear sequence, start index, end index, duration, start time, end time and epoch values. Default is False.
#'
#' @return nonwear vector, non wear time encoded as T, and wear time encoded as F.
#' @export
#'
#' @examples
#' \dontrun{
#' minute_data %>%
#'   group_by(id) %>%
#'   mutate(nonwear = flag_choi(1 - is.na(HR)))
#' }
flag_choi <- function(
    x,
    activity_threshold = 0,
    min_period_len = 90,
    spike_tolerance = 2,
    min_window_len = 30,
    window_spike_tolerance = 0,
    print_output = F
) {

  # check if data contains at least min_period_len of data
  if(length(x) < min_period_len)
    stop("Epoch data contains fewer entries than the  minimum required")

  # create non-wear vector as an array with Fs. Now we only need to add Ts which are the non-wear time segments
  non_wear_vector = rep(F, length(x))

  ## ---- ---- ----
  #	VARIABLES USED TO KEEP TRACK OF NON WEAR PERIODS
  ## ---- ---- ----

  # indicator for resetting and starting over
  reset = F
  # indicator for stopping the non-wear period
  stopped = F
  # indicator for starting to count the non-wear period
  start = F
  # second window validation
  window_2_invalid = F
  # starting minute for the non-wear period
  strt_nw = 0
  # ending minute for the non-wear period
  end_nw = 0
  # counter for the number of minutes with activity
  cnt_non_zero = 0
  # keep track of non wear sequences
  ranges = list()

  ## ---- ---- ----
  #	FIND NON WEAR PERIODS IN DATA
  ## ---- ---- ----

  # loop over the data
  for(paxn in 1:length(x)){
    # get the value
    paxinten = x[paxn]

    # reset counters if reset or stopped
    if(reset | stopped) {
      strt_nw = 0
      end_nw = 0
      start = F
      reset = F
      stopped = F
      window_2_invalid = F
      cnt_non_zero = 0
    }

    # the non-wear period starts with a zero count (smaller or equal to activity threshold)
    if((paxinten <= activity_threshold) & !start) {
      # assign the starting minute of non-wear
      strt_nw = paxn
      # set start boolean to true so we know that we started the period
      start = T
    }

    # only do something when the non-wear period has started
    if(start) {
      # keep track of the number of minutes with intensity that is not a 'zero' count (> activity threshold)
      if(paxinten > activity_threshold) {
        # increase the spike counter
        cnt_non_zero <- cnt_non_zero + 1
      }

      # when there is a non-zero count (greather than activity threshold), check the upstream and downstream window for counts
      # only when the upstream and downstream window have zero counts, then it is a valid non wear sequence
      if(paxinten > activity_threshold) {

        if((paxn + spike_tolerance - 1) < length(x)) {
          # check upstream window if there are counts, note that we skip the count right after the spike, since we allow for 2 minutes of spikes (spike_tolerance)
          upstream = x[(paxn + spike_tolerance - 1):min(paxn + min_window_len, length(x))]

          # check if upstream has non zero counts (greather than activity threshold), if so, then the window is invalid
          if(sum(upstream > activity_threshold) > window_spike_tolerance)
            window_2_invalid = T
        }

        if((paxn - spike_tolerance - 1) > 0) {
          # check downstream window if there are counts, again, we skip the count right before since we allow for 2 minutes of spikes (spike_tolerance)
          downstream = x[max(1, paxn - min_window_len):(paxn - spike_tolerance - 1)]

          # check if downstream has non zero counts (greather than activity threshold), if so, then the window is invalid
          if(sum(downstream > activity_threshold) > window_spike_tolerance)
            window_2_invalid = T
        }

        # if the second window is invalid, we need to reset the sequence for the next run
        if(window_2_invalid) reset = T
      }

      # reset counter if value is "zero" again
      if(paxinten <= activity_threshold) cnt_non_zero = 0

      # the sequence ends when there are 3 consecutive spikes (spike_tolerance + 1), or an invalid second window (upstream or downstream), or the last value of the sequence
      if(cnt_non_zero == (spike_tolerance + 1) || window_2_invalid || paxn == length(x)) {
        # define the end of the period
        if(paxn == length(x)) end_nw = paxn else end_nw = paxn - 1

        # check if the sequence is sufficient in length
        if(length(x[strt_nw:end_nw]) < min_period_len) {
          # lenght is not sufficient, so reset values in next run
          reset = T
        } else{
          # length of sequence is sufficient, set stopped to True so we save the sequence start and end later on
          stopped = T
        }
      }

      # if stopped is True, the sequence stopped and is valid to include in the ranges
      if(stopped) {
        # add ranges start and end non wear time
        ranges <- append(ranges, list(c(strt_nw, end_nw)))
      }
    }
  }
  # convert ranges into non-wear sequence vector
  for(row in ranges) {
    # if set to True, then print output to console/log
    if(print_output) {
      print(paste(
        "start index:", row[1],
        "end index:", row[2],
        "length:", row[2] - row[1]))
    }

    # set the non wear vector according to start and end
    non_wear_vector[row[1]:row[2]] <- T
  }
  return(non_wear_vector)
}


#' Apply Choi et al nonwear algorithm to steps
#'
#' @param steps a numeric vector containing Fitbit steps for a subject.
#' @param activity_threshold same as in flag_choi(), the difference is that the default activity threshold is specifically set for Fitbit steps to 1, however modifying the value is still optional.
#' @param min_period_len same as in flag_choi().
#' @param spike_tolerance same as in flag_choi().
#' @param min_window_len same as in flag_choi().
#' @param window_spike_tolerance same as in flag_choi().
#'
#' @return nonwear vector, non wear time encoded as T, and wear time encoded as F.
#' @export
#'
#' @examples
#' \dontrun{
#' minute_data %>%
#'   group_by(id) %>%
#'   mutate(nonwear = flag_choi_steps(steps))
#' }
flag_choi_steps <- function(
    steps,
    activity_threshold = 1, #default activity threshold is specifically set for fitbit steps, however modifying is still optional
    min_period_len = 90,
    spike_tolerance = 2,
    min_window_len = 30,
    window_spike_tolerance = 0
) {
  steps[is.na(steps)] <- 0
  flag_choi(
    steps,
    activity_threshold = activity_threshold,
    min_period_len = min_period_len,
    spike_tolerance = spike_tolerance,
    min_window_len = min_window_len,
    window_spike_tolerance = window_spike_tolerance
  )
}

#' Apply Choi et al nonwear algorithm to HR
#'
#' @details
#' Modifying activity_threshold param is not optional because the presence of HR is binary.
#'
#' @param HR a numeric vector containing Fitbit HR for a subject.
#' @param min_period_len same as in flag_choi().
#' @param spike_tolerance same as in flag_choi().
#' @param min_window_len same as in flag_choi().
#' @param window_spike_tolerance same as in flag_choi().
#'
#' @return nonwear vector, non wear time encoded as T, and wear time encoded as F.
#' @export
#'
#' @examples
#' \dontrun{
#' minute_data %>%
#'   group_by(id) %>%
#'   mutate(HR = flag_choi_HR(HR))
#' }
flag_choi_HR <- function(
    HR,
    min_period_len = 90,
    spike_tolerance = 2,
    min_window_len = 30,
    window_spike_tolerance = 0
) {
  flag_choi(
    1 - is.na(HR), #the presence of HR
    activity_threshold = 0, #modifying activity_threshold param is not optional because the presence of HR is binary
    min_period_len = min_period_len,
    spike_tolerance = spike_tolerance,
    min_window_len = min_window_len,
    window_spike_tolerance = window_spike_tolerance
  )
}
