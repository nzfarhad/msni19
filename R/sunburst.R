#' Creates sunburst from binary columns
#'
#' @importFrom plotly plot_ly layout
#' @importFrom dplyr summarize_at
#'
#' @param df data frame
#' @param cols character vector of binary columns to calculate groups from
#' @param labels character vector of labels for each column name
#' @param parents character vector of the parent column for each column. The top value should have blank value for parent, ""
#' @param weighting_function weighting function used to weight MSNA dataset
#' @param colors character vector of hex colors for branches, should ideally be a color specified for each 2nd level branch
#' @param na.rm if TRUE, ignore NAs in summation of binary columns
#'
#' @return plot_ly plot object
sunburst <- function(df, cols, labels, parents, weighting_function = NULL, colors, na.rm = T) {
  if (is.null(weighting_function)) {
    df$weights <- rep(1, nrow(df))
  } else {
    df$weights <- weighting_function(df)
  }

  data <- summarize_at(df, cols, ~ sum(.x * weights, na.rm = na.rm))
  data <- as.numeric(unlist(data, use.names = F))
  center <- which(parents == "")
  percents <- round(100 * (data / data[center]), digits = 0)
  labels[-center] <- paste0(labels[-center], "<br>", percents[-center], "%")
  plot_ly(ids = cols, parents = parents, values = data, labels = labels, type = "sunburst", branchvalues = "total") %>%
    layout(colorway = colors)
}

#' Create sunburst for MSNI components
#'
#' @param df MSNA data
#' @param msni column name for MSNI
#' @param fsl_lsg column name for FSL index
#' @param health_lsg column name for health index
#' @param protection_lsg column name for protection index
#' @param shelter_lsg column name for shelter index
#' @param wash_lsg column name for wash index
#' @param capacity_gaps column name for capacity/coping index
#' @param impact column name for impact index
#' @param msni_filter filter dataset for households with only specific MSNI scores
#' @param weighting_function weighting function used to weight MSNA dataset
#' @param fsl_wash_branch if TRUE, displays 3rd branch layer of FSL/WASH scores
#' @param health_prot_shelter_branch if TRUE, displays 3rd branch layer for H/P/S scores
#' @param impact_branch if TRUE, displays 3rd layer of impact branch
#' @param print_plot logical column indicating whether or not to save the plot to PDF
#' @param plot_name name to save plot with
#' @param path path to save plot to if not in current working directory
#'
#' @importFrom dplyr filter transmute mutate mutate_at
#' @importFrom rlang !! sym
#' @importFrom plotly orca
#'
#' @export
sunburst_msni <- function(df,
                          msni = "msni",
                          fsl_lsg = "fsl_lsg",
                          health_lsg = "health_lsg",
                          protection_lsg = "protection_lsg",
                          shelter_lsg = "shelter_lsg",
                          wash_lsg = "wash_lsg",
                          capacity_gaps = "capacity_gaps",
                          impact = "impact",
                          msni_filter = c(3, 4, 5),
                          weighting_function = NULL,
                          fsl_wash_branch = F,
                          health_prot_shelt_branch = F,
                          impact_branch = F,
                          print_plot = F,
                          plot_name = "sunburst",
                          path = NULL) {

  data <- df %>% filter(!!sym(msni) %in% msni_filter) %>%
    transmute(msni = rep(1, nrow(.)),
              fsl_cause = ifelse(!!sym(fsl_lsg) %in% msni_filter, 1, 0),
           wash_cause = ifelse(!!sym(wash_lsg) %in% msni_filter, 1, 0),
           health_cause = ifelse(!!sym(health_lsg) %in% msni_filter, 1, 0),
           protection_cause = ifelse(!!sym(protection_lsg) %in% msni_filter, 1, 0),
           shelter_cause = ifelse(!!sym(shelter_lsg) %in% msni_filter, 1, 0),
           capacity_total = ifelse(!!sym(capacity_gaps) %in% msni_filter, 1, 0),
           impact_cause = ifelse(!!sym(impact) %in% msni_filter, 1, 0)) %>%
    mutate(fsl_wash_cause = ifelse(fsl_cause + wash_cause == 2, 1, 0),
           fsl_cause = fsl_cause - fsl_wash_cause,
           wash_cause = wash_cause - fsl_wash_cause,
           fsl_wash_total = fsl_cause + wash_cause + fsl_wash_cause) %>%
    mutate(capacity_total = ifelse(fsl_wash_total == 1, 0, capacity_total)) %>%
    mutate(health_prot_cause = ifelse(health_cause + protection_cause == 2, 1, 0),
           health_shelter_cause = ifelse(health_cause + shelter_cause == 2, 1, 0),
           prot_shelter_cause = ifelse(protection_cause + shelter_cause == 2, 1, 0),
           health_prot_shelter_cause = ifelse(health_prot_cause + health_shelter_cause + prot_shelter_cause == 3, 1, 0),
           health_prot_cause = health_prot_cause - health_prot_shelter_cause,
           health_shelter_cause = health_shelter_cause - health_prot_shelter_cause,
           prot_shelter_cause = prot_shelter_cause - health_prot_shelter_cause,
           health_prot_shelter_total = health_prot_cause + health_shelter_cause + prot_shelter_cause + health_prot_shelter_cause) %>%
    mutate_at(c("health_prot_cause", "health_shelter_cause", "prot_shelter_cause", "health_prot_shelter_cause", "health_prot_shelter_total"),
              ~ ifelse(fsl_wash_total + capacity_total >= 1, 0, .x)) %>%
    mutate(impact_health_cause = ifelse(health_cause + impact_cause == 2, 1, 0),
           impact_prot_cause = ifelse(protection_cause + impact_cause == 2, 1, 0),
           impact_shelter_cause = ifelse(shelter_cause + impact_cause == 2, 1, 0),
           impact_total = impact_health_cause + impact_prot_cause + impact_shelter_cause) %>%
    mutate_at(c("impact_health_cause", "impact_prot_cause", "impact_shelter_cause", "impact_total"),
              ~ ifelse(fsl_wash_total + capacity_total + health_prot_shelter_total >= 1, 0, .x))

  cols <- c("msni", "fsl_wash_total", "capacity_total", "health_prot_shelter_total", "impact_total")
  labels <- c("", "FSL / WASH", "Capacity gaps", "H / P / S", "Impact")
  parents <- c("", "msni", "msni", "msni", "msni")
  colors <- c("#EE5859", "#D2CBB8", "#D1D3D4", "#585859")

  if (fsl_wash_branch) {
    cols <- c(cols, "fsl_cause", "wash_cause", "fsl_wash_cause")
    labels <- c(labels, "FSL", "WASH", "FSL & WASH")
    parents <- c(parents, rep("fsl_wash_total", 3))
  }

  if (health_prot_shelt_branch) {
    cols <- c(cols, "health_shelter_cause", "prot_shelter_cause", "health_prot_cause", "health_prot_shelter_cause")
    labels <- c(labels, "H + S", "P + S", "H + P", "H + P + S")
    parents <- c(parents, rep("health_prot_shelter_total", 4))
  }

  if (impact_branch) {
    cols <- c(cols, "impact_health_cause", "impact_prot_cause", "impact_shelter_cause")
    labels <- c(labels, "I + H", "I + P", "I + S")
    parents <- c(parents, rep("impact_total", 3))
  }

  p <- sunburst(data,
           cols = cols,
           labels = labels,
           parents = parents,
           weighting_function = weighting_function,
           colors = colors,
           na.rm = T)

  if (print_plot) {
    if (!is.null(path)) {
      plot_name <- paste(path, plot_name, sep = "/")
    }
    plot_name <- paste0(plot_name, ".pdf")
    orca(p, plot_name)
  }

  p
}
