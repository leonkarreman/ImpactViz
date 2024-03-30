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
plot_impact <- function(  
                         dataset, 
                         outcomes,
                         treatment_var, 
                         fixed_effect_var = NULL, 
                         cluster_var      = NULL,
                         level            = 5, 
                         
                        
                         colors         = NULL, 
                         treatment_labs = NULL, 
                         subtitle       = NULL, 
                         legend_title   = NULL,
                         outcome_labs   = NULL) {
  
  # create result data 
  results_long <- ImpactViz::impact_regression(dataset = dataset, 
                                               outcomes =outcomes,
                                                treatment_var = treatment_var,
                                               fixed_effect_var =fixed_effect_var,
                                               cluster_var = cluster_var,
                                               level =level
                                        )
  xlabs <- NULL
  xbreaks <- NULL
  

  
  if(max(results_long$Coefficient) > 1)  { 
    
    
    ylabels <- scales::comma
    results_long$blab = scales::comma(results_long$Coefficient)

     } else {
       
       
       results_long$Coefficient  = results_long$Coefficient * 100 
       results_long$UpperCI      = results_long$UpperCI * 100
       results_long$LowerCI      = results_long$LowerCI * 100
       results_long$blab = sprintf("%.0f%%", results_long$Coefficient)
       
       ylabels <- NULL
       
     } 
  
  

  
  
  
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
  buffer = max(results_long$Coefficient)/20

  
  # If the order of unique_treatments is not 0, 1, 2, match colors accordingly
  # For a dynamic approach, especially when treatment arms are not known in advance or are non-numeric:
  color_map <- setNames(colors, unique_treatments) # Creates a named vector
  
  # Example usage:
  # Assuming 'Type' is a factor, get colors for plotting
  plot_colors <- color_map[as.character(results_long$Treatment)]
  
  
  subtitle = paste(subtitle, "\n\n\n")
  # Plotting
  p <- ggplot(data = results_long) +
    

    geom_bar(aes(x = X_position, 
                 y = Coefficient, 
                 fill = Treatment), 
                alpha = 1, 
                stat = "identity", 
                position = "identity", 
                width = 1) +
    
    geom_errorbar(aes(x = X_position, 
                      ymin = LowerCI, 
                      ymax = UpperCI), width = 0.4) +
    
    geom_text(data = results_long[results_long$Treatment == 0,], 
              aes(x = X_position, 
                  y = Coefficient + buffer, 
                  label = blab), 
              size = 10, family = "Lato") +
    
    geom_text( 
              aes(x = X_position, 
                  y = UpperCI + buffer, 
                  label = blab), 
              size = 10, family = "Lato") +
    
    scale_x_continuous(breaks = xbreaks,
                       labels = xlabs) +
    
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                       position = "right",
                       labels = ylabels) +
    
    scale_fill_manual(values = plot_colors, 
                      name = legend_title,
                      labels = treatment_labs) +
    
    labs(subtitle = subtitle, 
         x = NULL, 
         y = "") +
    
    theme_economist_white(gray_bg = FALSE, horizontal = TRUE) +
    
    theme(axis.text.x = element_text(family = "Lato", size = 39, hjust = 0.2),
          axis.title.x = element_text(family = "Lato", size = 39, hjust = 0),
          axis.title.y = element_text(family = "Lato", size = 25),
          plot.title = element_text(family = "Lato", size = 28, lineheight = 0.2),
          plot.subtitle = element_text(family = "Lato", face = "bold", size = 41, hjust = 0, lineheight = 0.6),
          legend.text = element_text(size = 41, family = "Lato"),
          plot.caption = element_text(hjust = 0, family = "Lato", size = 21, color = "#808080"),
          legend.position = c(0.48, 1.12),
          legend.title = element_text(family = "Lato", size = 41, hjust = 0),
          legend.direction = "horizontal",
          axis.text.y = element_text(family = "Lato", size = 23),
          axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.y.right = element_blank(),
          axis.line.x.bottom = element_line())
  
  return(p)
  
}

# Example usage:
# plot_results(results_df)
