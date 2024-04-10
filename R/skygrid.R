#' Create a data.frame for plotting a skygrid model.
#'
#' @param height numeric vector; root height parameter.
#' @param pop_size data.frame; population size parameters.
#' @param grid_height numeric; grid height (i.e. cutoff).
#' @param trajectory character string; Trajectory of the population size.
#' Either mean or median.
#' @param ci numeric vector; the default c(0.025, 0.075) corresponds to the
#'   95\% HPD of the population size.
#' @param age_of_youngest numeric; sampling time of the youngest taxon.
#' @param max_height numeric; Either lower, upper, median or mean.
#' @param range_time numeric vector; time range to plot. -1 if automatic.
#' @returns A data.frame
#' @importFrom stats quantile
#' @export
prepare_skygrid <-
  function(height,
           pop_size,
           grid_height,
           trajectory = c("median", "mean"),
           ci = c(0.025, 0.975),
           age_of_youngest = 0,
           max_height = c("lower", "upper", "median", "mean"),
           range_time = NULL) {
    if (is.matrix(pop_size)) {
      pop_size <- as.data.frame(pop_size)
    }
    if (is.data.frame(height)) {
      height <- as.vector(unlist(height))
    }

    trajectory <- match.arg(trajectory)
    max_height <- match.arg(max_height)

    traj <- switch(trajectory,
      mean = sapply(pop_size, mean),
      median = sapply(
        pop_size,
        quantile,
        simplify = T,
        probs = 0.5,
        name = F,
        na.rm = T
      )
    )

    traj <- unname(traj)

    traj_ci <- unname(sapply(
      pop_size,
      quantile,
      probs = ci,
      name = F,
      na.rm = T
    ))

    if (max_height == "lower") {
      max_height <- quantile(height,
        probs = 0.025,
        name = F,
        na.rm = T
      )
    } else if (max_height == "median") {
      max_height <- quantile(height,
        probs = 0.5,
        name = F,
        na.rm = T
      )
    } else if (max_height == "mean") {
      max_height <- mean(height, na.rm = T)
    } else if (max_height == "upper") {
      max_height <- quantile(height,
        probs = 0.975,
        name = F,
        na.rm = T
      )
    } else {
      stop("max_height should be either lower, median, mean, or upper")
    }

    if (!is.null(range_time)) {
      min_time <- range_time[1]
      max_time <- range_time[2]
    } else {
      if (age_of_youngest > 0) {
        min_time <- age_of_youngest - max_height
        max_time <- age_of_youngest
      } else {
        min_time <- 0
        max_time <- max_height
      }
    }

    x <- seq(0, grid_height, length.out = ncol(pop_size))

    if (age_of_youngest > 0) {
      x <- age_of_youngest - x
    }

    res <- as.data.frame(cbind(
      time = x,
      pop_size = traj,
      pop_size_low = traj_ci[1, ],
      pop_size_high = traj_ci[2, ]
    ))

    if (max_height > grid_height) {
      res <- rbind(
        res,
        list(
          max_time,
          res$pop_size[nrow(res)],
          res$pop_size_low[nrow(res)],
          res$pop_size_high[nrow(res)]
        )
      )
    }

    res[res$time >= min_time & res$time <= max_time, ]
  }
