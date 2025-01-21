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
                       outcomevar,
                       legendvar = NULL,
                       legendlabs = NULL,
                       horizontal = F
                       ){


  #----- Main data manipulation ----
  data <- data %>% mutate(group  = unlist(data[, groupvar]),
                          outcome = unlist(data[, outcomevar] ))


  data$group <- factor(data$group)


  #----- legend not missing ----
  if (!is.null(legendvar)) {

    data <- data %>% mutate(legend = unlist(data[, legendvar] ))
      data$legend <- factor(data$legend)

      results <- data %>%
        group_by(group, legend) %>%
        summarise(mean = mean(outcome, na.rm = T)) %>%
        filter(!is.na(group))

      color_map <- setNames(colors, unique(results$legend)) # Creates a named vector


      plot_colors <- color_map[as.character(results$legend)]


   p <-   ggplot(data =results, aes(y = mean, x = group, fill = legend, color = legend)) +

        geom_bar(,
                 alpha = 1,
                 stat = "identity",
                 width = 0.5,
                 position = position_dodge(0.7)) +

        labs(subtitle = subtitle,
             title = title,
             x = NULL,
             y = "",
             color = NULL,
             fill = NULL ) +

        scale_y_continuous(expand = expansion(mult = c(0,0.2)),
                           position = "right") +


        theme_economist_white(gray_bg = F, horizontal = T) + theme(legend.position = "top",
                                                                   legend.justification = "left")




  }



  #----- legend missing ----

  if (is.null(legendvar)) {

    results <- data %>%
      group_by(group) %>%
      summarise(mean = mean(outcome, na.rm = T)) %>%
      filter(!is.na(group))

    color_map <- setNames(colors, unique(results$group)) # Creates a named vector

    plot_colors <- color_map[as.character(results$group)]

    p <-  ggplot(data =results, aes(y = mean, x = group, fill = group, color = group)) +

      geom_bar(,
               alpha = 1,
               stat = "identity",
               width = 0.5,
               position = position_dodge(1.5)) +



      labs(subtitle = subtitle,
           title = title,
           x = NULL,
           y = "") +

      scale_y_continuous(expand = expansion(mult = c(0,0.2)),
                         position = "right") +

      theme_economist_white(gray_bg = F, horizontal = T) + theme(legend.position = "none")
  }




  #----- horizontal ----

  if (horizontal == T) {
    p <- p +          geom_text(
      aes( label = sprintf("%.1f",round(mean, 1)) ),
      size = 7,
      hjust = -0.25,
      position =position_dodge(0.7))  +
      scale_x_discrete(,
                       labels = rev(xlabs),
                       expand = expansion(mult = c(0.3, 0.3)),
                       limits = rev(levels(results$group))) +
      scale_fill_manual(breaks =  rev(levels(results$legend)), values = rev(plot_colors), labels = legendlabs) +
      scale_color_manual(guide = "none", breaks =  rev(levels(results$legend)), values = rev(plot_colors), labels = legendlabs) +




      theme(axis.text.y = element_text(family = font, size = blabel_size),
                    axis.title.y = element_text(family = font, size = 39, hjust = 0),
                    axis.title.x = element_text(family = font, size = 25),
                    plot.title = element_text(family = font, size = 25, lineheight = 0.2, hjust = 0),
                    plot.subtitle = element_text(family = font, size = 22, hjust = 0,
                                                 lineheight = 1.2),
                    axis.text.x = element_blank(),
                    axis.line = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.line.x.bottom = element_blank(),
                    axis.line.y.left  = element_line(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.major.y = element_blank(),
            plot.title.position = "plot") +
     coord_flip()
  }

  else {

    p <- p +         geom_text(
      aes( label = sprintf("%.1f",round(mean, 1)) ),
      size = 6.5,
      vjust = -0.5,
      position =position_dodge(1.5))  +
      scale_x_discrete(,
                       labels = xlabs) +

      scale_fill_manual(values = plot_colors, labels = legendlabs) +
      scale_color_manual(values = plot_colors, labels = legendlabs) +

      theme(axis.text.x = element_text(family = font, size = blabel_size),
                    axis.title.x = element_text(family = font, size = 39, hjust = 0),
                    axis.title.y = element_text(family = font, size = 25),
                    plot.title = element_text(family = font, size = 25, lineheight = 0.2),
                    plot.subtitle = element_text(family = font, size = 22, hjust = 0,
                                                 lineheight = 1.2),
                    axis.text.y = element_blank(),
                    axis.line = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.line.y.right = element_blank(),
                    axis.line.x.bottom = element_line(),
                    panel.grid.major.y = element_blank(),

    )

  }


 print(p)

 return(p)

   }




