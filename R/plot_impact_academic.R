#' Plot Results of Impact Analysis
#'
#' This function plots the results of an impact analysis using the ImpactViz package.
#' @importFrom ggplot2 ggplot geom_bar geom_errorbar geom_text theme scale_fill_manual scale_y_continuous  scale_x_continuous
#' @importFrom scales comma
#' @param dataset Data frame containing the dataset.
#' @param outcomes Character vector of outcome variable names.
#' @param treatment_var The name of the treatment variable.
#' @param fixed_effect_var The name of the fixed effect variable (optional).
#' @param cluster_var The name of the cluster variable (optional).
#' @param level The level for confidence intervals.
#' @param colors Character vector of colors for treatment groups.
#' @param treatment_labs Character vector of labels for treatment groups.
#' @param subtitle The subtitle for the plot.
#' @param legend_title The title for the legend.
#' @param outcome_labs Character vector of labels for outcome variables.
#' @return A ggplot object representing the impact analysis results.
#' @export
#' @examples
#' plot_results(dataset = your_dataset,
#'              outcomes = c("outcome1", "outcome2"),
#'              treatment_var = "your_treatment_var",
#'              colors = c("grey", "blue"),
#'              treatment_labs = c("Control", "Treatment"),
#'              subtitle = "Your Subtitle",
#'              legend_title = "Treatment Group")

# Define the plotting function
plot_impact_academic <- function(
    dataset,
    accuracy        = NULL,
    font = "Lato",
    colors         = NULL,
    treatment_labs = NULL,
    subtitle       = NULL,
    legend_title   = NULL,
    outcome_labs   = NULL,
    outcome_pct  = F,
    buffer_adj = 20,
    buffer_diff_adj = 0,
    label_x_adj = 0,
    y_expansion =  c(0, 0.1),
    y_breaks = waiver()) {

  # create result data
  results_long <- dataset

  xlabs <- NULL
  xbreaks <- NULL
  outcomes = unique(results_long$Outcome)


  if(outcome_pct  == F)  {


    ylabels <- scales::comma
    results_long$blab = scales::comma(results_long$Coefficient, accuracy = accuracy)

  } else {


    results_long$Coefficient  = results_long$Coefficient * 100
    results_long$blab = sprintf("%.0f%%", results_long$Coefficient)

    ylabels <- NULL


  }



  print(results_long)


  # Data manipulation
  results_long <- results_long %>%

    mutate(Outcome = factor(Outcome, levels = outcomes),
           Treatment    = factor(Treatment)) %>%
    arrange(Outcome, Treatment) %>%
    group_by(Outcome) %>%
    mutate(IntraOutcome_Pos = (row_number() - 1) * 1.2, # Increase spacing between Types
           Outcome_Pos = (as.numeric(Outcome) - 1) * 3, # Adjust the multiplier for spacing between Outcomes
           X_position = 1 + Outcome_Pos + IntraOutcome_Pos) %>%
    ungroup()

  if(length(outcomes) > 1) {

    xlabs <- outcome_labs

    xbreaks <- as.numeric(unlist(results_long[results_long$Treatment == 0, "X_position"]))

  }

  unique_treatments <- sort(unique(results_long$Treatment))

  # create label positions with buffer
  buffer = max(results_long$Coefficient)/buffer_adj


  results_long$x = min(results_long$X_position)
  results_long$y = max(data$Coefficient)

  results_long <- results_long %>%
    group_by(Outcome) %>%
    mutate(diff = Coefficient - Coefficient[Treatment == 0],
           pct_diff = diff/Coefficient * 100,
           max = max(pct_diff))

  results_long <- results_long %>%
    mutate(coef_buffer = case_when(Coefficient > 0 ~ Coefficient + buffer,
                                   Coefficient < 0 ~ Coefficient - buffer),
           coef_buffer = case_when(max > 0  & Treatment != 0 ~ coef_buffer +  buffer_diff_adj,
                                   max == 0 & Treatment == 0 ~ coef_buffer +  buffer_diff_adj,
                                   TRUE           ~ coef_buffer))



  # If the order of unique_treatments is not 0, 1, 2, match colors accordingly
  # For a dynamic approach, especially when treatment arms are not known in advance or are non-numeric:
  color_map <- setNames(colors, unique_treatments) # Creates a named vector

  # Example usage:
  # Assuming 'Type' is a factor, get colors for plotting
  plot_colors <- color_map[as.character(results_long$Treatment)]

  if (subtitle != "") {
    subtitle = paste(subtitle, "\n\n\n")
  }

 results_long <- results_long %>% mutate(y_increment = seq(0, by = 0.2*max(results_long$Coefficient) ,length.out = nrow(results_long)),
         y = y + y_increment,
         x_label = (x + X_position)/2,
         y_label = y * 1.05,
         diff= round(diff, 2),
         diff = scales::comma(diff, accuracy= accuracy),
         diff = as.character(diff),
         diff = case_when(Pval < 0.01 ~ paste(diff,  "***", sep = ""),
                          Pval < 0.05 ~ paste(diff,  "**", sep = ""),
                          Pval < 0.10 ~ paste(diff,  "*", sep = ""),
                          TRUE        ~ diff
         ))



  # Plotting
  p <- ggplot(data = results_long) +


    geom_bar(aes(x = X_position,
                 y = Coefficient,
                 fill = Treatment),
             alpha = 1,
             stat = "identity",
             position = "identity",
             width = 1) +


    geom_text(data = results_long[results_long$Treatment == 0,],
              aes(x = X_position + label_x_adj,
                  y = coef_buffer,
                  label = blab),
              size = 10, family = font) +


    scale_x_continuous(breaks = xbreaks,
                       labels = xlabs) +

    scale_y_continuous(expand = expansion(mult = y_expansion),
                       position = "right",
                       labels = ylabels,
                       breaks = y_breaks
    ) +

    scale_fill_manual(values = plot_colors,
                      name = legend_title,
                      labels = treatment_labs) +

    labs(subtitle = subtitle,
         x = NULL,
         y = "") +


    theme_economist_white(gray_bg = FALSE, horizontal = TRUE) +

    theme(axis.text.x = element_text(family = font, size = 39, hjust = 0.2),
          axis.title.x = element_text(family = font, size = 39, hjust = 0),
          axis.title.y = element_text(family = font, size = 25),
          plot.title = element_text(family = font, size = 28, lineheight = 0.2),
          plot.subtitle = element_text(family = font, face = "bold", size = 41, hjust = 0, lineheight = 0.6),
          legend.text = element_text(size = 41, family = font),
          plot.caption = element_text(hjust = 0, family = font, size = 21, color = "#808080"),
          legend.position = c(0.48, 1.12),
          legend.title = element_text(family = font, size = 41, hjust = 0),
          legend.direction = "vertical",
          axis.text.y = element_text(family = font, size = 23),
          axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(),
          axis.line.y.right = element_line(),
          axis.line.x.bottom = element_line(),
          panel.grid.major.y = element_blank(),

          )




    p <- p + geom_text(data = results_long[results_long$Treatment != 0,],
                       aes(x = X_position,
                           y =  coef_buffer,
                           label = blab),
                       size = 10, family = font) + theme(panel.background = element_rect(fill = "white")) +
      geom_segment(results_long[2:nrow(results_long), ], mapping = aes(x = x,
                                                       xend = X_position,y = y, yend = y), lineend = "round",
                   linejoin="bevel",
                   size = 1) +

            geom_segment(results_long[2:nrow(results_long), ], mapping = aes(x = x, xend = x, y =y, yend = y - (max(results_long$y) * 0.05)), size = 1) +

      geom_segment(results_long[2:nrow(results_long), ], mapping = aes(x = X_position, xend = X_position, y =y, yend = y - (max(results_long$y) * 0.05)), size = 1)+

              geom_text(results_long[2:nrow(results_long),],
                                           size = 10,
                                           family = font,
                                           mapping = aes(x = x_label, y = y_label, label = diff))



  return(p)

}

