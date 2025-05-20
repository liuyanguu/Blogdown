

library("data.table")
library("ggplot2")
library("ggforce")
library("cowplot")
library("ggh4x")
library("readxl")
library("plotly")
library("shiny")
library("DT") 
library("curl") 


# general theme for plotting
mytheme1 <- theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.title.x = element_text(size = 14), # Enlarges x-axis label
        axis.title.y = element_text(size = 14), # Enlarges y-axis label
        axis.text = element_text(size = 12), # Enlarges axis text
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold") # Enlarged title size
  )

custom_colors <- c(
  "0-500" = "#FFC20E",
  "500-1000" = "#80BD41",
  "1000-1500" = "#00833D",
  "1500-2000" = "#1CABE2",
  ">2000" = "#0058AB",
  "unrated" = "#777777"
)


combined_data <- fread(here::here("input/2025 Final standing.csv"))

# Shiny UI
# Subset unique sections for user selection

combined_data <- combined_data[Grade!="" & !is.na(Grade) & !grepl("Unrated", section) & RateCategory!="unrated"]
unique_sections <- unique(combined_data$section_full)

unique(combined_data$Grade)
combined_data[, Grade:= factor(as.factor(Grade), levels = c("K", as.character(1:12)))]
combined_data[, Grade_full := paste("Grade", Grade)]
combined_data[, Grade_full := factor(as.factor(Grade_full), levels = paste("Grade", c("K", as.character(1:12))))]


ui <- fluidPage(
  titlePanel("Percentile of Standing vs. Rating by Section!"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_section", "Select Section:", choices = unique_sections, selected = unique_sections[5])
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplot", plotly::plotlyOutput("scatterplot", , height = "500px")),
        tabPanel("Standing", DTOutput("name_table"))
      )
    )
  )
)

# Shiny Server
server <- function(input, output) {
  # Reactive data: Filtered data based on selected section
  filtered_data <- reactive({
    combined_data[combined_data$section_full == input$selected_section, ]
  })
  
  # Render Scatterplot
  # option 1 
  output$scatterplot <- plotly::renderPlotly({
      # g <- ggplot(filtered_data()[, .(`No.`, Pts, Rate, Percentile_standing, RateCategory, Name)], 
      #             aes(x = Rate, y = Percentile_standing, 
      #                 color = RateCategory, 
      #                 text = paste0(Name,
      #                               "<br>Points: ", Pts,
      #                               "<br>Standing (percentile): ", `No.`, " (", round(Percentile_standing), "%)",
      #                               "<br>Rating: ", Rate
      #                              ))) +
      #   geom_point(size = 2, alpha = 0.5) +
      #   scale_y_reverse() +
      #   scale_color_manual(values = custom_colors) +
      #   labs(title = paste(input$selected_section),
      #        x = "Rating", y = "Percentile_standing of standing", color = "Rating") +
      #   mytheme1 +
      #   theme(legend.position = "bottom")
      # return(plotly::ggplotly(g, tooltip = "text"))
      
  # option 2 using plot_ly
  fd <- filtered_data()[, .(`No.`, Grade, Grade_full, Pts, Rate, Percentile_standing, RateCategory, Name)]
  fd <- fd[!is.na(Rate) & !is.na(Percentile_standing)]
  
  # Fit linear model
  lm_fit <- lm(Percentile_standing ~ Rate, data = fd)
  lm_preds <- data.frame(Rate = seq(min(fd$Rate), max(fd$Rate), length.out = 100))
  lm_preds$Percentile_standing <- predict(lm_fit, newdata = lm_preds)
  lm_preds <- lm_preds[lm_preds$Percentile_standing >= 0 & lm_preds$Percentile_standing <= 100,]
  
  # Fit loess model
  loess_fit <- loess(Percentile_standing ~ Rate, data = fd, span = 0.75)
  loess_preds <- data.frame(Rate = seq(min(fd$Rate), max(fd$Rate), length.out = 100))
  loess_preds$Percentile_standing <- predict(loess_fit, newdata = loess_preds)
  loess_preds <- loess_preds[loess_preds$Percentile_standing >= 0 & loess_preds$Percentile_standing <= 100,]
  
  # 13 custom colors (feel free to customize)
  custom_colors <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
    "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", "#98df8a"
  )
  
  # 6 symbols (these will recycle automatically across categories)
  custom_symbols <- c("circle", "square", "diamond", "cross", "x", "triangle-up",
                      "circle", "square", "diamond", "cross", "x", "triangle-up", "circle")
  
  
  # Build the plot
  p <- plot_ly(
    data = fd,
    hoverinfo = "text",
    x = ~Rate,
    y = ~Percentile_standing,
    colors = custom_colors,
    symbol = ~Grade_full,
    symbols = custom_symbols,
    text = ~paste0(
      Name,
      "<br>Grade: ", Grade,
      "<br>Points: ", Pts,
      "<br>Standing (percentile): ", `No.`, " (", round(Percentile_standing), "%)",
      "<br>Rating: ", Rate
    ),
    type = "scatter",
    mode = "markers",
    marker = list(size = 6, opacity = 0.7)
  ) %>% 
    add_lines(
      data = lm_preds, x = ~ Rate, y = ~ Percentile_standing, line = list(color = "#333333", width = 0.8), 
      name = "Linear", opacity = 0.6, showlegend = FALSE, hoverinfo = "none", inherit = FALSE
    ) %>% 
    add_lines(
      data = loess_preds, x = ~ Rate, y = ~ Percentile_standing, line = list(color = "#F26A21", width = 1), 
      name = "Loess", opacity = 0.7, showlegend = FALSE, hoverinfo = "none", inherit = FALSE
    ) %>% 
    layout(
      title = input$selected_section,
      xaxis = list(title = "Rating", showline = FALSE),
      yaxis = list(title = "Percentile of standing", autorange = "reversed"),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.3,
        xanchor = "center",
        font = list(size = 10),    # Reduce font size
        itemwidth = 30             # Adjust item width to save space
      )
      )
  
  return(p)
  })
  
  # Render Standing
  output$name_table <- renderDT({
    datatable(data = filtered_data()[,.(`No.`, Name, Pts, Rate, St)], 
              options = list(pageLength = 10), 
              rownames = FALSE)
  })
}

# Run the App
shinyApp(ui = ui, server = server)