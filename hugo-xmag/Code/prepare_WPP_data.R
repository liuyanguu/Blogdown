
# data_plot = dt_wpp_world_l[AgeGrpStart %in% c(0)]

plot.wpp.data <- function(data_plot, title0 = "", by_sex = FALSE){

  # sum across AgeGrp
  data_plot[, Pop_million := sum(Pop)/1E3, by = .(Sex, Time)]
  data_plot <- unique(data_plot[,.(Time, Sex, Pop_million)])
  data_plot[, label := paste0(round(Pop_million, 0), "M")]

  if(by_sex){
    data_plot_sex <- data_plot[Sex != "Total"]
    data_plot_sex[, Sex := factor(as.factor(Sex), levels = c("Male", "Female"))]
  } else {
    data_plot_sex <- data_plot[Sex == "Total"]
  }

  # Identify the highest point
  max_value_year <- data_plot_sex[which.max(Pop_million), Time]
  highest_point <- data_plot_sex[Time == max_value_year]
  lowest_point <- data_plot_sex[Time %in% c(1950, 2100)]
  y_max <- max(data_plot_sex$Pop_million)
  value_2024 <- data_plot_sex[Time == CURRENT_YEAR]

  col_red      <- "#E2231A"
  col_orange   <- "#F26A21"
  col_green    <- "#00833D"
  col_blue     <- "#1CABE2"

  text_size1 = 3.5 # for highest red point
  text_size2 = 3   # for low points and current year

  # Plot
  g <- ggplot(data = data_plot_sex, aes(x = Time, y = Pop_million, color = Sex)) +  # Divide y-axis by 1000 for thousands
    geom_line(linewidth = 1) +  # Line color and size
    scale_color_manual(values = c("Total" = col_blue, "Female" = col_orange, "Male" = col_blue)) +  # Set line color

    scale_y_continuous(limits = c(0, y_max * 1.2),
                       expand = c(0, 0),  # Remove padding
                       breaks = scales::pretty_breaks()) +  # Set y-axis limit
    scale_x_continuous(limit = c(1945, 2100), breaks = scales::pretty_breaks()) +  # Set x-axis limit
    # point highlighting the current year
    geom_point(data = value_2024, aes(x = Time, y = Pop_million), size = 3) +

    theme_classic() +  # Classic theme for clean look
    labs(title = title0, x = "Year", y = "Population (in millions)", color = "") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Centered title
      axis.title = element_text(size = 10),  # Axis title size
      axis.text = element_text(size = 10)    # Axis text size
    )

    # labeling scheme by sex
    if(by_sex) {
      # g <- g + ggrepel::geom_text_repel(data = highest_point, aes(x = Time, y = Pop_million, label = label),
      #                                   color = col_red, size = text_size1, direction = "y",
      #                                   box.padding = 0.5, point.padding = 0.5)
      g <- g +
        # labeling
        geom_text(data = lowest_point[Sex=="Male"], aes(x = Time, y = Pop_million,  label = label),
                                 size = text_size2, vjust = -1, show.legend = FALSE) + # Add value label
        geom_text(data = value_2024[Sex=="Male"], aes(x = Time, y = Pop_million, label = paste0(label, "\n(", CURRENT_YEAR, ")")),
                                 size = text_size2, vjust = -1, show.legend = FALSE) +
        geom_text(data = lowest_point[Sex=="Female"], aes(x = Time, y = Pop_million,  label = label),
                                 size = text_size2, vjust = 1.2, show.legend = FALSE) + # Add value label
        geom_text(data = value_2024[Sex=="Female"], aes(x = Time, y = Pop_million, label = paste0(label, "\n(", CURRENT_YEAR, ")")),
                                 size = text_size2, vjust = 1.2, show.legend = FALSE) +

        theme(legend.position = "bottom")
    } else {
      g <- g +
        geom_point(data = highest_point, aes(x = Time, y = Pop_million),
                   color = col_red, size = 3) +                                      # Highlight highest point in red
        geom_vline(data = highest_point, aes(xintercept = Time), color = "gray", linetype = "dashed") + # Vertical line
        geom_text(data = highest_point, aes(x = Time, y = Pop_million, label = label),
                         color = col_red, size = text_size1, vjust = -1.5) +
        # Peak year label
        geom_text(data = highest_point, aes(x = Time, y = 0, label = Time), vjust = -1, color = col_red, size = 4) +

        # label for current and 1950, 2100
        geom_text(data = lowest_point, aes(x = Time, y = Pop_million,  label = label),
                  size = text_size2, vjust = 1.5, show.legend = FALSE) + # Add value label
        geom_text(data = value_2024, aes(x = Time, y = Pop_million, label = paste0(label, "\n(", CURRENT_YEAR, ")")),
                  size = text_size2, vjust = 1.5, show.legend = FALSE) +

        theme(legend.position = "none") # Remove legend if total sex
    }
  g
  return(g)
}
