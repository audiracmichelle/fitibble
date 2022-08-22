#' Plot confidence intervals
#'
#' @param daily_summary a tibble, ideally the output of `prep_daily_summary()`.
#' @param mu_varname a character entry indicating the column name that contains the sample means.
#' @param sd_varname a character entry indicating the column name that contains the sample standard deviations.
#' @param type the variable type, it can be `"continuous"` or `"categorical"`
#' @param critical_value the default value is a z-critical value with 95%  confidence (assuming a normal distribution of the sample mean).
#'
#' @return A plot containing the confidence intervals of a PA descriptor in daily_summary. Each point range in the plot corresponds to a participant in a cohort.
#' @importFrom stats qnorm
#'
#' @examples
plot_confidence_intervals <- function(
    daily_summary,
    mu_varname = "mu_HR",
    sd_varname = "sd_HR",
    type = c("continuous", "categorical"),
    critical_value = qnorm(p = .05 / 2, lower.tail=FALSE)
) {
  daily_summary$mu <- daily_summary[[mu_varname]]
  daily_summary$sd <- daily_summary[[sd_varname]]

  if(type[1] == "continuous") {
    daily_summary$sd_error <- daily_summary$sd / sqrt(daily_summary$valid_days)
  }
  if(type[1] == "categorical") {
    daily_summary$sd_error <- sqrt(daily_summary$mu * (1 - daily_summary$mu)) / sqrt(daily_summary$valid_days)
  }

  daily_summary %>%
    dplyr::mutate(
      id = as.factor(.data$id),
      lo = .data$mu - critical_value * .data$sd_error,
      hi = .data$mu + critical_value * .data$sd_error
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_pointrange(ggplot2::aes(x=.data$id,
                                          y=.data$mu,
                                          ymin=.data$lo,
                                          ymax=.data$hi)) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw()
}

#' Plot confidence intervals
#'
#' @param daily_summary a tibble, ideally the output of `prep_daily_summary()`.
#' @param type the plot type, it can be `"basic"` or `"sedentary_behavior"` or `"time_use"`
#' @param critical_value the default value is a z-critical value with 95%  confidence (assuming a normal distribution of the sample mean).
#'
#' @return A plot containing the confidence intervals of a physical activity PA descriptor in daily_summary. Each point range in the plot corresponds to a participant in a cohort.
#' @export
#'
#' @examples
plot_ci <- function(
    daily_summary,
    type = c("basic", "sedentary_behavior", "time_use"),
    critical_value = qnorm(p = .05 / 2, lower.tail=FALSE)
) {

  if(type[1] == "basic") {
    ci_summary <- daily_summary %>%
      dplyr::select("id", "valid_days", "mu_HR", "mu_steps") %>%
      tidyr::pivot_longer(
        cols =  tidyselect::contains("mu"),
        names_transform = list(name = function(x) gsub("mu_", "", x)),
        values_to = "mu") %>%
      dplyr::left_join(
        daily_summary %>%
          dplyr::select("id", "valid_days", "sd_HR", "sd_steps") %>%
          tidyr::pivot_longer(
            cols = tidyselect::contains("sd"),
            names_transform = list(name = function(x) gsub("sd_", "", x)),
            values_to = "sd")
      ) %>%
      dplyr::mutate(
        id = as.factor(.data$id),
        sd_error = .data$sd / sqrt(.data$valid_days),
        lo = .data$mu - critical_value * .data$sd_error,
        hi = .data$mu + critical_value * .data$sd_error
      )

    p1 <- ci_summary %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(
        ggplot2::aes(x=.data$mu,
                     fill = .data$name)
      ) +
      ggplot2::facet_wrap(~.data$name, scales = "free") +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "") +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        legend.position = "none"
      )

    p2 <- ci_summary %>%
      ggplot2::ggplot() +
      ggplot2::geom_pointrange(
        ggplot2::aes(x=.data$id,
                     y=.data$mu,
                     ymin=.data$lo,
                     ymax=.data$hi,
                     color = .data$name),
        size = 0.3
      ) +
      ggplot2::facet_wrap(~.data$name, scales = "free_x", ) +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "none",
        strip.text.x = ggplot2::element_blank()
      )

    p <- cowplot::plot_grid(p1, p2, nrow = 2, rel_heights = c(0.3, 0.7), align = "v")
  }

  if(type[1] == "sedentary_behavior") {
    ci_summary <- daily_summary %>%
      dplyr::select("id", "valid_days", "mu_sed_bout_length", "mu_zero_steps_prop") %>%
      tidyr::pivot_longer(
        cols =  tidyselect::contains("mu"),
        names_transform = list(name = function(x) gsub("mu_", "", x)),
        values_to = "mu") %>%
      dplyr::left_join(
        daily_summary %>%
          dplyr::select("id", "valid_days", "sd_sed_bout_length", "sd_zero_steps_prop") %>%
          tidyr::pivot_longer(
            cols = tidyselect::contains("sd"),
            names_transform = list(name = function(x) gsub("sd_", "", x)),
            values_to = "sd")
      ) %>%
      dplyr::mutate(
        id = as.factor(.data$id),
        sd_error = .data$sd / sqrt(.data$valid_days),
        lo = .data$mu - critical_value * .data$sd_error,
        hi = .data$mu + critical_value * .data$sd_error
      )

    p1 <- ci_summary %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(
        ggplot2::aes(x=.data$mu,
                     fill = .data$name)
      ) +
      ggplot2::facet_wrap(~.data$name, scales = "free") +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "") +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        legend.position = "none"
      )

    p2 <- ci_summary %>%
      ggplot2::ggplot() +
      ggplot2::geom_pointrange(
        ggplot2::aes(x=.data$id,
                     y=.data$mu,
                     ymin=.data$lo,
                     ymax=.data$hi,
                     color = .data$name),
        size = 0.3
      ) +
      ggplot2::facet_wrap(~.data$name, scales = "free_x", ) +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "none",
        strip.text.x = ggplot2::element_blank()
      )

    p <- cowplot::plot_grid(p1, p2, nrow = 2, rel_heights = c(0.3, 0.7), align = "v")
  }

  if(type[1] == "time_use") {
    ci_summary <- daily_summary %>%
      dplyr::select("id", "valid_days", tidyselect::contains("mu")) %>%
      dplyr::select("id", "valid_days", tidyselect::contains("time_use")) %>%
      tidyr::pivot_longer(
        cols =  tidyselect::contains("mu"),
        names_transform = list(name = function(x) gsub("mu_", "", x)),
        values_to = "mu") %>%
      dplyr::left_join(
        daily_summary %>%
          dplyr::select("id", "valid_days", tidyselect::contains("sd")) %>%
          dplyr::select("id", "valid_days", tidyselect::contains("time_use")) %>%
          tidyr::pivot_longer(
            cols = tidyselect::contains("sd"),
            names_transform = list(name = function(x) gsub("sd_", "", x)),
            values_to = "sd")
      ) %>%
      dplyr::mutate(
        id = as.factor(.data$id),
        sd_error = .data$sd / sqrt(.data$valid_days),
        lo = .data$mu - critical_value * .data$sd_error,
        hi = .data$mu + critical_value * .data$sd_error
      )

    p1 <- ci_summary %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(
        ggplot2::aes(x=.data$mu,
                     fill = .data$name)
      ) +
      ggplot2::facet_wrap(~.data$name, ncol = 1, scales = "free_y") +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "") +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        legend.position = "none",
        strip.text.x = ggplot2::element_blank()
      )

    p2 <- ci_summary %>%
      ggplot2::ggplot() +
      ggplot2::geom_pointrange(
        ggplot2::aes(x=.data$id,
                     y=.data$mu,
                     ymin=.data$lo,
                     ymax=.data$hi,
                     color = .data$name),
        size = 0.3
      ) +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "bottom"
      )

    p <- cowplot::plot_grid(p1, p2, nrow = 2, rel_heights = c(0.35, 0.65), align = "v")
  }

  return(p)
}
