#' Create venn diagram of living standards gaps >= 3 and capacity gaps >= 3
#'
#' @param df MSNA data
#' @param fsl_lsg column name for FSL index
#' @param health_lsg column name for health index
#' @param protection_lsg column name for protection index
#' @param shelter_lsg column name for shelter index
#' @param wash_lsg column name for wash index
#' @param education_lsg column name for education index
#' @param capacity_gaps column name for capacity/coping index
#' @param weights column name for weights column in dataset
#' @param print_plot logical column indicating whether or not to save the plot to PDF
#' @param plot_name name to save plot with
#' @param path path to save plot to if not in current working directory
#'
#' @importFrom dplyr summarize_all mutate_all transmute select
#' @importFrom tidyr replace_na
#' @importFrom rlang !! sym
#' @importFrom eulerr euler
#'
#' @export
venn_msni <- function(df,
                      fsl_lsg = "fsl_lsg",
                      health_lsg = "health_lsg",
                      protection_lsg = NULL,
                      shelter_lsg = "shelter_lsg",
                      wash_lsg = "wash_lsg",
                      education_lsg = "education_lsg",
                      capacity_gaps = "capacity_gaps",
                      weights = NULL,
                      print_plot = F,
                      plot_name = "venn",
                      path = NULL) {

  lsg_cols <- c(fsl_lsg, health_lsg, protection_lsg, shelter_lsg, wash_lsg, education_lsg)
  data <- df %>%
    transmute(lsg_over_3 = pmin(1, rowSums(select(., lsg_cols) >= 3, na.rm = T)),
              cg_over_3 = replace_na(as.numeric(!!sym(capacity_gaps) >= 3), 0),
              lsg_cg_over_3 = as.numeric(lsg_over_3 + cg_over_3 > 1),
              lsg_over_3 = lsg_over_3 - lsg_cg_over_3,
              cg_over_3 = cg_over_3 - lsg_cg_over_3) %>%
    select(lsg_over_3, cg_over_3, lsg_cg_over_3)

  if (!is.null(weights)) {
    data <- data %>%
      mutate_all(~ .x * !!sym(weights))
  }

  data <- summarize_all(data, sum)
  data <- as.numeric(unlist(data))
  fit <- euler(c("A" = data[1], "B" = data[2], "A&B" = data[3]))
  if (print_plot) {
    if (!is.null(path)) {
      plot_name <- paste(path, plot_name, sep = "/")
    }
    pdf(paste0(plot_name, ".pdf"))
    plot(fit,
         fills = list(fill = c("#EE5859", "#D1D3D4")),
         labels = F)
    dev.off()
  } else {
    plot(fit,
         fills = list(fill = c("#EE5859", "#D1D3D4")),
         labels = F)
  }
}
