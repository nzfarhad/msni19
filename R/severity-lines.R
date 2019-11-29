#' Create line graph to show households by # of sector severity scores 3 and over
#'
#' @param df MSNA data
#' @param lsg character vector of all column names of LSG indices
#' @param lsg_labels character vector of all labels for LSG indices
#' @param group column name to group data by, e.g. population type
#' @param group_order character vector of how to order the graph, defaults to alphabetical ordering
#' @param weighting_function weighting function used to weight MSNA dataset
#' @param print_plot logical column indicating whether or not to save the plot to PDF
#' @param plot_name name to save plot with
#' @param path path to save plot to if not in current working directory
#' @param language value of "en" or "fr" to indicate if the text on the graph is in English or French
#'
#' @importFrom dplyr mutate group_by summarize filter
#' @importFrom stringr str_sub
#' @importFrom rlang !! sym
#' @importFrom ggplot2 ggplot geom_line theme_minimal theme scale_y_continuous scale_x_continuous scale_color_manual aes ggsave element_blank
#' @importFrom tidyr gather
#'
#' @export
severity_lines <- function(df,
                           lsg = c("education_lsg",
                                   "shelter_lsg",
                                   "fsl_lsg",
                                   "health_lsg",
                                   "nutrition_lsg",
                                   "protection_lsg",
                                   "wash_lsg"),
                           group = NULL,
                           group_order = NULL,
                           group_labels = NULL,
                           weighting_function = NULL,
                           print_plot = F,
                           plot_name = "severity_lines",
                           path = NULL,
                           language = "en") {

  if (is.null(weighting_function)) {
    df$weights <- rep(1, nrow(df))
  } else {
    df$weights <- weighting_function(df)
  }

  language <- match.arg(language, c("en", "fr"))
  if (language == "en") {
    x_axis_labs <- "# of sectors"
  } else if (language == "fr") {
    x_axis_labs <- "# de secteurs"
  }

  data <- df %>%
    mutate(val_count = rowSums(select(., lsg) >= 3, na.rm = T)) %>%
    group_by(!! sym(group)) %>%
    summarize(lsg_1 = sum((val_count == 1) * weights) / sum(weights),
              lsg_2 = sum((val_count == 2) * weights) / sum(weights),
              lsg_3 = sum((val_count == 3) * weights) / sum(weights),
              lsg_4 = sum((val_count == 4) * weights) / sum(weights),
              lsg_5 = sum((val_count == 5) * weights) / sum(weights),
              lsg_6 = sum((val_count == 6) * weights) / sum(weights),
              lsg_7 = sum((val_count == 7) * weights) / sum(weights),
              lsg_8 = sum((val_count == 8) * weights) / sum(weights),
              lsg_9 = sum((val_count == 9) * weights) / sum(weights)) %>%
    gather("severity", "percent", -group) %>%
    mutate(severity = as.numeric(str_sub(severity, start = 5))) %>%
    filter(severity <= length(lsg))

  if (!is.null(group)) {
    if (!is.null(group_order)) {
      data <- mutate(data, !!sym(group) := factor(!!sym(group), levels = group_order))
    }

    p <- ggplot(data, aes(x = severity, y = percent, group = !!sym(group))) +
      geom_line(aes(color = !!sym(group)), size = 1.3) +
      theme_minimal() +
      theme(legend.title = element_blank()) +
      scale_y_continuous("", labels = function(x) scales::percent(x, accuracy = 1)) +
      scale_x_continuous(message, labels = data$severity, breaks = data$severity)

    if (!is.null(group_labels)) {
      p <- p + scale_color_manual(labels = group_labels,
                                    values = c("#EE5859",
                                               "#58585A",
                                               "#D1D3D4",
                                               "#D2CBB8",
                                               "#A9C5A1",
                                               "#FFF67A",
                                               "#F69E61",
                                               "#95A0A9",
                                               "#56B3CD"))
    } else {
      p <- p + scale_color_manual(values = c("#EE5859",
                                            "#58585A",
                                            "#D1D3D4",
                                            "#D2CBB8",
                                            "#A9C5A1",
                                            "#FFF67A",
                                            "#F69E61",
                                            "#95A0A9",
                                            "#56B3CD"))
    }

  } else {
    p <- ggplot(data, aes(x = severity, y = percent)) +
      geom_line(size = 1.3) +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous("", labels = function(x) scales::percent(x, accuracy = 1)) +
      scale_x_continuous("# of sectors", labels = data$severity, breaks = data$severity) +
      scale_color_manual(values = c("#EE5859",
                                    "#58585A",
                                    "#D1D3D4",
                                    "#D2CBB8",
                                    "#A9C5A1",
                                    "#FFF67A",
                                    "#F69E61",
                                    "#95A0A9",
                                    "#56B3CD"))
  }

  if (print_plot) {
    ggsave(paste0(plot_name, ".pdf"), plot = p, path = path, height = 4, width = 6)
  }

  p
}
