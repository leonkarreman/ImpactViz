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
#' @param color bar color
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

#--------------------------------
# Define the plotting function
# --------------------------------
plot_impact_academic <- function(
    dataset,
    pair_data = NULL,
    accuracy        = 2,
    font = "Lato",
    color        = "blue",
    treatment_labs = NULL,
    subtitle       = NULL,
    legend_title   = NULL,
    outcome_labs   = NULL,
    label_adjust = 0.5,
    label_size = 39,
    outcome_pct  = F,
    buffer_adj = 1,
    buffer_diff_adj = 0,
    label_x_adj = 0,
    y_breaks = waiver()) {



  #--------------------------------
  # Some label creation
  # --------------------------------

  #rename dataset
  results_long <- dataset

  # get unique outcomes
  outcomes = unique(results_long$Outcome)


  if(outcome_pct  == F)  {


    ylabels <- scales::comma

    results_long$blab = round(results_long$Coefficient, accuracy)

    if (max(results_long$Coefficient, na.rm = T) > 1000) {


    results_long$blab = scales::comma(results_long$Coefficient)

    }

  } else {


    results_long$Coefficient  = results_long$Coefficient * 100
    results_long$blab = sprintf("%.0f%%", results_long$Coefficient)

    ylabels <- NULL


  }

  min_coef= min(results_long$Coefficient)

  if (0 %in% results_long$Coefficient) {

    results_long <- results_long %>%
                    mutate(blab = as.character(blab),
                      blab = case_when(Pval < 0.01 ~ paste(blab,  "***", sep = ""),
                                     Pval < 0.05 ~ paste(blab,  "**", sep = ""),
                                     Pval < 0.10 ~ paste(blab,  "*", sep = ""),
                                     TRUE        ~ blab
                    ))
  }



if (min_coef < 0) {
  y_breaks <- pretty(c(0, min(results_long$Coefficient)) , n = 4)
  y_expansion =  c(0.1, 0.1)

}

else{
  y_breaks <- pretty(c(0, max(results_long$Coefficient)) , n = 4)
  y_expansion =  c(0, 0.1)
}



  #--------------------------------
  # Main Outcome Dataset
  # --------------------------------

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




  results_long$x = min(results_long$X_position)
  results_long$y = max(data$Coefficient)

  results_long <- results_long %>%
    group_by(Outcome) %>%
    mutate(diff = Coefficient - Coefficient[Treatment == 0],
           pct_diff = diff/Coefficient * 100,
           max = max(pct_diff)) %>%
       filter(Coefficient != 0)


  results_long <- results_long %>%
    mutate(buffer = Coefficient * .05,
           buffer = min(buffer),
           coef_buffer = Coefficient + buffer)

     print(results_long)

           # coef_buffer = case_when(max > 0  & Treatment != 0 ~ coef_buffer +  buffer_diff_adj,
           #                         max == 0 & Treatment == 0 ~ coef_buffer +  buffer_diff_adj,
           #                         TRUE           ~ coef_buffer))

     # case_when(Coefficient > 0 ~ Coefficient + buffer,
     #           Coefficient < 0 ~ Coefficient - buffer)

  #--------------------------------
  # Create Plot
  # --------------------------------

  # Plotting
  p <- ggplot(data = results_long) +


    geom_bar(aes(x = X_position,
                 y = Coefficient),

             color = color,
             alpha = 0.6,
             fill = color,
             stat = "identity",
             position = "identity",
             width = 1) +


    geom_text(data = results_long,
              aes(x = X_position + label_x_adj,
                  y = coef_buffer,
                  label = blab),
              size = 10, family = font) +


    scale_x_continuous(breaks = results_long$X_position,
                       labels = treatment_labs) +

    scale_y_continuous(expand = expansion(mult = y_expansion),
                       position = "right",
                       labels = ylabels,
                       breaks = y_breaks
    ) +

    labs(subtitle = subtitle,
         x = NULL,
         y = "") +


    theme_economist_white(gray_bg = FALSE, horizontal = TRUE) +

    theme(axis.text.x = element_text(family = font, size = label_size, hjust = label_adjust),
          axis.title.x = element_text(family = font, size = 39, hjust = 0),
          axis.title.y = element_text(family = font, size = 25),
          plot.title = element_text(family = font, size = 28, lineheight = 0.2),
          plot.subtitle = element_text(family = font, face = "bold", size = 41, hjust = 0, lineheight = 0.6),
          legend.text = element_text(size = 41, family = font),
          plot.caption = element_text(hjust = 0, family = font, size = 21, color = "#808080"),
          legend.position = c(0.48, 1.12),
          legend.title = element_text(family = font, size = 41, hjust = 0),
          legend.direction = "vertical",
          axis.text.y = element_text(family = font, size = 28),
          axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(),
          axis.line.y.right = element_blank(),
          axis.line.x.bottom = element_line(),
          panel.grid.major.y = element_blank(),

    )

  #--------------------------------
  # Pairs-level data
  # --------------------------------

if (!is.null(pair_data)) {

 pairs$id  <- 1:nrow(pairs)


 pairs <- merge(pairs, results_long[, c("Treatment", "X_position")], by.x = "Coefficient1", by.y= "Treatment", all.x = T)


 pairs <- merge(pairs, results_long[, c("Treatment", "X_position")], by.x = "Coefficient2", by.y= "Treatment", all.x = T)

 pairs <- pairs[order(pairs$id), ]

 pairs <- pairs %>% mutate(y_increment =
                          case_when(min_coef > 0 ~ seq(0.15*max(results_long$Coefficient), by = 0.15*max(results_long$Coefficient) ,length.out = nrow(pairs)),
                                    min_coef < 0 ~  seq(0.15*min(results_long$Coefficient), by = -0.15*min(results_long$Coefficient) ,length.out = nrow(pairs))),
                           y = case_when( min_coef > 0 ~ max(results_long$Coefficient),
                                          min_coef < 0 ~ min(results_long$Coefficient)),

                           y = y + y_increment,
                           x_label = (X_position.x + X_position.y)/2,
                           y_label = y * 1.05,
                           diff= round(Diff, accuracy),
                           diff = case_when(diff > 1000 ~ scales::comma(diff),
                                            TRUE ~ as.character(diff)),

                           diff = case_when(Pval < 0.01 ~ paste(diff,  "***", sep = ""),
                                            Pval < 0.05 ~ paste(diff,  "**", sep = ""),
                                            Pval < 0.10 ~ paste(diff,  "*", sep = ""),
                                            TRUE        ~ diff
                           ))




p <- p  +  geom_segment(pairs, mapping = aes(x = X_position.x,
                                               xend = X_position.y,
                                               y = y, yend = y), lineend = "round",
                          linejoin="bevel",
                          size = 1) + geom_segment(pairs, mapping = aes(x = X_position.x, xend = X_position.x, y = y, yend = y - (max(y) * 0.05)), size = 1) +
      geom_segment(pairs, mapping = aes(x = X_position.y, xend = X_position.y, y = y, yend = y - (max(y) * 0.05)), size = 1) +

      geom_text(pairs,
                size = 10, family= font,
                mapping = aes(x = x_label, y = y_label, label = diff))

    }



  return(p)

}

