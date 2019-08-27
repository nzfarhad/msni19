
#' Create sunburst for MSNI components
#'
#' @param df MSNA data
#' @param group column name to group data by, e.g. population type
#' @param group_order character vector of values to order groups by in graph, if NULL, alphabetical
#' @param group_labels character vector of labels for graph
#' @param index column name for scoring index to graph with
#' @param index_max max value index can take, either 4 or 5
#' @param index_type character vector specifying either "msni" or "lsg", to determine labels on scores
#' @param weights column name for weights column in dataset
#' @param plot_name name to save plot with
#' @param path path to save plot to if not in current working directory
#'
#' @importFrom dplyr filter group_by summarize mutate n
#' @importFrom rlang !! sym
#' @importFrom forcats fct_rev
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes theme_minimal labs scale_fill_manual scale_y_continuous scale_x_discrete ggsave coord_flip geom_bar
#'
#' @export
severity_bar_chart <- function(df,
                           group = "group",
                           group_order = NULL,
                           group_labels = NULL,
                           index = "msni",
                           index_max = 4,
                           index_type = "msni",
                           weights = NULL,
                           plot_name = "severity_bar_chart",
                           path = NULL) {
  if (is.null(weights)) {
    df$weights <- rep(1, nrow(df))
  } else {
    df$weights <- df[[weights]]
  }

  data <- df %>%
    group_by(!!sym(group)) %>%
    filter(!is.na(!!sym(index))) %>%
    summarize(index_1 = 100 * sum((!!sym(index) == 1) * weights) / sum(weights),
              index_2 = 100 * sum((!!sym(index) == 2) * weights) / sum(weights),
              index_3 = 100 * sum((!!sym(index) == 3) * weights) / sum(weights),
              index_4 = 100 * sum((!!sym(index) == 4) * weights) / sum(weights),
              index_4_plus = sum((!!sym(index) > 4) * weights) / sum(weights)) %>%
    gather("score", "percent", -!!sym(group))

  if (!is.null(group_order)) {
    data <- mutate(data, !!sym(group) := factor(!!sym(group), levels = group_order))
  }

  index_fill <- c("#F7ACAC", "#FACDCD", "#A7A9AC", "#58585A")
  if (index_type == "msni") {
    index_labels <- c("Extreme (4)", "Severe (3)", "Stress (2)", "Minimal (1)")
  } else {
    index_labels <- c("Extreme", "Severe", "Stress", "Minimal")
  }


  if (index_max == 5) {
    index_fill <- c("#EE5A59", index_fill)
    if (index_type == "msni") {
      index_labels <- c("Extreme+ (4+)", index_labels)
    } else {
      index_labels <- c("Catastrophic", index_labels)
    }

  } else {
    data <- filter(data, score != "index_4_plus")
  }

  p <- ggplot(data = data, aes(x = !!sym(group), y = percent)) +
    geom_bar(aes(fill = forcats::fct_rev(score)), stat = "identity") +
    theme_minimal() +
    labs(fill = "", x = "", y = "") +
    scale_fill_manual(values = index_fill,
                      labels = index_labels) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1))

  if (!is.null(group_labels)) {
    p <- p + scale_x_discrete(labels = group_labels)
  }

  p + coord_flip()
  ggsave(paste0(plot_name, ".pdf"), plot = p, path = path, height = length(unique(df[[group]])))
}
