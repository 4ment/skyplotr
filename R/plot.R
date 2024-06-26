#' Plotting a skyline model.
#'
#' @param data data.frame; data.frame generated by prepare_sky* function.
#' @param y_log logical; if TRUE, y axis is in log10 space.
#' @param alpha numeric; opacity of ribbon
#' @param color character string; color for line.
#' @param fill character string; color for ribbons.
#' @param color_by character string; name of column for coloring the line.
#' @param fill_by character string; name of column for filling the ribbon.
#' @param group_by character string; name of column for grouping in facet.
#' @param facet list or character string; named list of arguments passed
#'   to [ggplot2::facet_wrap()] or [ggplot2::facet_grid()]. If facet is a
#'   character string, facet_wrap is used with facet as the only argument.
#'   Sub-plots are superimposed if NULL.
#' @returns ggplot2 object.
#' @import ggplot2 scales
#' @importFrom rlang .data
#' @export
skyplot <-
  function(data,
           y_log = T,
           alpha = 0.5,
           color = NULL,
           fill = NULL,
           color_by = NULL,
           fill_by = NULL,
           facet = NULL) {
    aes_line <- aes(
      x = .data$time,
      y = .data$trajectory
    )

    aes_ribbon <- aes(
      ymin = .data$trajectory_low,
      ymax = .data$trajectory_high
    )

    if (!is.null(color_by)) {
      aes_line$color <- substitute(.data[[color_by]])
    }
    if (!is.null(fill_by)) {
      aes_ribbon$fill <- substitute(.data[[fill_by]])
    }

    graph <- ggplot(data, aes_line) +
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.title = element_blank()
      )

    if (!is.null(color)) {
      graph <- graph + geom_line(color = color)
    } else {
      graph <- graph + geom_line()
    }

    if (!is.null(fill)) {
      graph <- graph + geom_ribbon(aes_ribbon, fill = fill, alpha = alpha)
    } else {
      graph <- graph + geom_ribbon(aes_ribbon, alpha = alpha)
    }

    if (y_log) {
      graph <- graph + scale_y_log10(
        breaks = trans_breaks("log10", function(x) {
          10^x
        }),
        labels = trans_format("log10", math_format(10^.x))
      )
    }

    if (is.character(facet)) {
      graph <- graph + do.call("facet_wrap", list(facets = vars(.data[[facet]])))
    } else if (!is.null(facet)) {
      if ("facets" %in% names(facet)) {
        graph <- graph + do.call("facet_wrap", facet)
      } else {
        facet_args <- list(
          rows = vars(.data[[facet[["rows"]]]]),
          cols = vars(.data[[facet[["cols"]]]])
        )
        graph <- graph + do.call("facet_grid", facet_args)
      }
    }

    graph + xlab("Time")
  }
