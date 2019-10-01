#' Create radar graph for LSG scores
#'
#' @param df MSNA data
#' @param lsg character vector of all column names of LSG indices
#' @param lsg_labels character vector of all labels for LSG indices
#' @param group column name to group data by, e.g. population type
#' @param group_order character vector of how to order the graph, defaults to alphabetical ordering
#' @param group_labels character vector of labels for graph, make sure they match the order for the data
#' @param weighting_function weighting function used to weight MSNA dataset
#' @param legend_position character specifying legend location, either "top", "left", "right", or "bottom"
#' @param legend_text_size integer specifying size of legend text
#' @param label_text_size integer specifying size of label text
#' @param print_plot logical column indicating whether or not to save the plot to PDF
#' @param plot_name name to save plot with
#' @param path path to save plot to if not in current working directory
#' @param index_threshold index threshold to graph
#'
#' @importFrom dplyr filter transmute mutate mutate_at summarize_at rename bind_cols group_by arrange
#' @importFrom tibble add_column
#' @importFrom purrr map_df
#' @importFrom rlang !! sym
#' @importFrom ggradar ggradar
#' @importFrom ggplot2 ggsave
#' @importFrom tidyr nest
#'
#' @export
radar_graph <- function(df,
                        lsg = c("education_lsg",
                                "shelter_lsg",
                                "fsl_lsg",
                                "health_lsg",
                                "nutrition_lsg",
                                "protection_lsg",
                                "wash_lsg"),
                        lsg_labels = c("Education in\nEmergency",
                                       "Emergency Shelter\nand NFI",
                                       "Food Security and\nAgriculture",
                                       "Health",
                                       "Nutrition",
                                       "Protection",
                                       "WASH"),
                        group = NULL,
                        group_order = NULL,
                        group_labels = NULL,
                        weighting_function = NULL,
                        legend_position = "right",
                        legend_text_size = 10,
                        label_text_size = 4,
                        print_plot = F,
                        plot_name = "radar_graph",
                        path = NULL,
                        index_threshold=3) {

  if (!is.null(group)) {
    df <- group_by(df, !! sym(group)) %>% nest()
    data <- map_df(df[[2]], function(x) summarize_at(x, lsg, function(y) index_percent(x, as.character(substitute(y)), weighting_function,index_threshold)))
    data <- bind_cols(select(df, !! sym(group)), data) %>%
      rename(group = !! sym(group))

    if (!is.null(group_order)) {
      data <- arrange(data, match(group_order, group))
    } else {
      data <- arrange(data, group)
    }

    if (!is.null(group_labels)) {
      data <- mutate(data, group = group_labels)
    }
    data <- mutate(data, group = factor(group, levels = group))

  } else {
    data <- summarize_at(df, lsg, function(x) index_percent(df, as.character(substitute(x)), weighting_function)) %>%
      add_column(group = "", .before = 1)
  }

  p <- ggradar(data,
               axis.labels = lsg_labels,
               axis.label.size = label_text_size,
               background.circle.transparency = 0.1,
               group.point.size = 3,
               legend.position = legend_position,
               legend.text.size = legend_text_size,
               group.colours = c("#EE5859",
                                 "#58585A",
                                 "#D1D3D4",
                                 "#D2CBB8",
                                 "#A9C5A1",
                                 "#FFF67A",
                                 "#F69E61",
                                 "#95A0A9",
                                 "#56B3CD"))

  if (print_plot) {
    ggsave(paste0(plot_name, ".pdf"), p, path = path)
  }
  p
}

#' Function to calculate percent for index with custom weighting
#'
#' @importFrom dplyr filter summarize pull
#' @importFrom rlang !! sym
#'
#' @noRd
index_percent <- function(df, index, weighting_function,index_threshold) {
  df <- filter(df, !is.na(!! sym(index)))
  if (!is.null(weighting_function)) {
    df$weights <- weighting_function(df)
  } else {
    df$weights <- rep(1, nrow(df))
  }

  df <- summarize(df, !! sym(index) := sum((!! sym(index) >= index_threshold) * weights) / sum(weights))
  pull(df, !! sym(index))
}
