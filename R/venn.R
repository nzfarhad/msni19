#' Create venn diagram of living standards gaps >= 3 and capacity gaps >= 3
#'
#' @param df MSNA data
#' @param lsg character vector of all column names of LSG indices
#' @param capacity_gaps column name for capacity/coping index
#' @param weighting_function function for weighting the data frame
#' @param print_plot logical column indicating whether or not to save the plot to PDF
#' @param plot_name name to save plot with
#' @param path path to save plot to if not in current working directory
#'
#' @importFrom dplyr summarize_all mutate_at transmute select
#' @importFrom tidyr replace_na
#' @importFrom rlang !! sym
#' @importFrom eulerr euler
#'
#' @export
venn_msni <- function(df,
                      lsg = c("education_lsg",
                              "shelter_lsg",
                              "fsl_lsg",
                              "health_lsg",
                              "nutrition_lsg",
                              "protection_lsg",
                              "wash_lsg"),
                      capacity_gaps = "capacity_gaps",
                      weighting_function = NULL,
                      print_plot = F,
                      plot_name = "venn",
                      path = NULL) {
  data <- df %>%
    transmute(lsg_over_3 = pmin(1, rowSums(select(., lsg) >= 3, na.rm = T)),
              cg_over_3 = replace_na(as.numeric(!!sym(capacity_gaps) >= 3), 0),
              lsg_cg_over_3 = as.numeric(lsg_over_3 + cg_over_3 > 1),
              lsg_over_3 = lsg_over_3 - lsg_cg_over_3,
              cg_over_3 = cg_over_3 - lsg_cg_over_3) %>%
    select(lsg_over_3, cg_over_3, lsg_cg_over_3)

  if (is.null(weighting_function)) {
    data$weights <- rep(1, nrow(df))
  } else {
    data$weights <- weighting_function(df)
  }

  data <- data %>%
    mutate_at(c("lsg_over_3", "cg_over_3", "lsg_cg_over_3"), ~ .x * weights) %>%
    select(-weights) %>%
    summarize_all(sum)

  data <- as.numeric(unlist(data))
  cat(paste0(
    "LSG over 3 is ", round(data[1]/sum(data)), "%\n",
    "CG over 3 is ", round(data[2]/sum(data)), "%\n",
    "Overlap is ", round(data[3]/sum(data)), "%\n"
  ))
  fit <- euler(c("A" = data[1], "B" = data[2], "A&B" = data[3]))
  if (print_plot) {
    if (!is.null(path)) {
      plot_name <- paste(path, plot_name, sep = "/")
    }
    pdf(paste0(plot_name, ".pdf"))
    p <- plot(fit,
         fills = list(fill = c("#EE5859", "#D1D3D4")),
         labels = F)
    print(p)
    dev.off()
    p
  } else {
    plot(fit,
         fills = list(fill = c("#EE5859", "#D1D3D4")),
         labels = F)
  }
}
