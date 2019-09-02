#' Create intersection graph for indices in certain range of values
#'
#' @param df data frame
#' @param lsg character vector of all column names of LSG indices
#' @param lsg_labels character vector of all labels for LSG indices
#' @param index_filter numeric vector where index will get 1s if in the filter, and 0s otherwise
#' @param weighting_function function for weighting the data frame
#' @param nintersects number of intersections to include in the plot
#' @param exclude_unique whether the set intersections should include singular sete. Note that if this is set to True, the total set size on the left will be wrong
#' @param print_plot logical column indicating whether or not to save the plot to PDF
#' @param plot_name name to save plot with
#' @param path path to save plot to if not in current working directory
#'
#' @importFrom Setviz plot_set_percentages
#' @importFrom dplyr mutate_at rename_at
#'
#' @export
index_intersections <- function(df,
                                lsg = c("education_lsg",
                                "shelter_lsg",
                                "fsl_lsg",
                                "health_lsg",
                                "nutrition_lsg",
                                "protection_lsg",
                                "wash_lsg"),
                                lsg_labels = c("Education",
                                               "Shelter",
                                               "Food",
                                               "Health",
                                               "Nutrition",
                                               "Protection",
                                               "WASH"),
                                y_label = "% in need per combination of sectors",
                                index_filter = c(3,4,5),
                                weighting_function = NULL,
                                nintersects = 12,
                                exclude_unique = T,
                                print_plot = F,
                                plot_name = "intersection",
                                path = NULL) {

  df <- mutate_at(df, lsg, ~ .x %in% index_filter)
  if (!is.null(lsg_labels) & !is.na(lsg_labels)) {
    df <- rename_at(df, lsg, ~ lsg_labels)
    lsg <- lsg_labels
  }
  if (print_plot) {
    if (!is.null(path)) {
      plot_name <- paste(path, plot_name, sep = "/")
    }
    pdf(paste0(plot_name, ".pdf"))
    p <- plot_set_percentages(df,
                              lsg,
                              weight_function = weighting_function,
                              nintersects = nintersects,
                              exclude_unique = exclude_unique,
                              nsets = length(lsg),
                              label = y_label)
    print(p)
    dev.off()
    p
  } else {
    plot_set_percentages(df,
                         lsg,
                         weight_function = weighting_function,
                         nintersects = nintersects,
                         exclude_unique = exclude_unique,
                         nsets = length(lsg),
                         label = y_label)
  }
}
