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
plot_means <- function(data,
                       font = 'Lato',
                       title= NULL,
                       subtitle = NULL,
                       blabel_size = 20,
                       label_adjust = 0,
                       xlabs,
                       colors = c("#e5daa5", "#90310b", "#90315a"),
                       groupvar,
                       outcomevar
                       ){



  data <- data %>% mutate(group  = unlist(data[, groupvar]),
                          outcome = unlist(data[, outcomevar] ))


  data$group <- factor(data$group)


  results <- data %>%
    group_by(group) %>%
    summarise(mean = mean(outcome, na.rm = T)) %>%
    filter(!is.na(group))



  color_map <- setNames(colors, unique(results$group)) # Creates a named vector

  # Example usage:
  # Assuming 'Type' is a factor, get colors for plotting
  plot_colors <- color_map[as.character(results$group)]


 p <-  ggplot(data =results, aes(y = mean, x = group, fill = group, color = group)) +

    geom_bar(,
             alpha = 1,
             stat = "identity",
             width = 0.5,
             position = position_dodge(2)) +

    geom_text(
      aes( label = sprintf("%.1f",round(mean, 1)) ),
      size = 7,
      vjust = -0.5)  +

    labs(subtitle = subtitle,
         title = title,
         x = NULL,
         y = "") +

    scale_y_continuous(expand = expansion(mult = c(0,0.2)),
                       position = "right") +

    scale_fill_manual(values = plot_colors) +
    scale_color_manual(values = plot_colors) +

    scale_x_discrete(,
                     labels = xlabs,
                     expand = expansion(mult = c(0.3, 0.3))) +

    theme_economist_white(gray_bg = F, horizontal = T) +

    theme(axis.text.x = element_text(family = font, size = blabel_size),
          axis.title.x = element_text(family = font, size = 39, hjust = 0),
          axis.title.y = element_text(family = font, size = 25),
          plot.title = element_text(family = font, size = 25, lineheight = 0.2),
          plot.subtitle = element_text(family = font, size = 22, hjust = 0),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y.right = element_blank(),
          axis.line.x.bottom = element_line(),
          panel.grid.major.y = element_blank(),

    )
 print(p)

 return(p)

   }




