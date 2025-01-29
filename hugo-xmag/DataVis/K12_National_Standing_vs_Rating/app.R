

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


combined_data <- fread(here::here("input/2024 Final standing.csv"))

# Shiny UI
# Subset unique grades for user selection
unique_grades <- unique(combined_data$Grade_full)

ui <- fluidPage(
  titlePanel("Interactive plot and table"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_grade", "Select Grade:", choices = unique_grades, selected = unique_grades[3])
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
  # Reactive data: Filtered data based on selected grade
  filtered_data <- reactive({
    combined_data[combined_data$Grade_full == input$selected_grade, ]
  })
  
  # Render Scatterplot
  # option 1 
  output$scatterplot <- plotly::renderPlotly({
      # g <- ggplot(filtered_data()[, .(`No.`, Pts, Rate, Percentile, RateCategory, Name)], 
      #             aes(x = Rate, y = Percentile, 
      #                 color = RateCategory, 
      #                 text = paste0(Name,
      #                               "<br>Points: ", Pts,
      #                               "<br>Standing (percentile): ", `No.`, " (", round(Percentile), "%)",
      #                               "<br>Rating: ", Rate
      #                              ))) +
      #   geom_point(size = 2, alpha = 0.5) +
      #   scale_y_reverse() +
      #   scale_color_manual(values = custom_colors) +
      #   labs(title = paste(input$selected_grade),
      #        x = "Rating", y = "Percentile of standing", color = "Rating") +
      #   mytheme1 +
      #   theme(legend.position = "bottom")
      # return(plotly::ggplotly(g, tooltip = "text"))
      
  # option 2 using plot_ly
  fd <- filtered_data()[, .(`No.`, Pts, Rate, Percentile, RateCategory, Name)]
  fd <- fd[!is.na(Rate) & !is.na(Percentile)]
  
  # Fit linear model
  lm_fit <- lm(Percentile ~ Rate, data = fd)
  lm_preds <- data.frame(Rate = seq(min(fd$Rate), max(fd$Rate), length.out = 100))
  lm_preds$Percentile <- predict(lm_fit, newdata = lm_preds)
  lm_preds <- lm_preds[lm_preds$Percentile >= 0 & lm_preds$Percentile <= 100,]
  
  # Fit loess model
  loess_fit <- loess(Percentile ~ Rate, data = fd, span = 0.75)
  loess_preds <- data.frame(Rate = seq(min(fd$Rate), max(fd$Rate), length.out = 100))
  loess_preds$Percentile <- predict(loess_fit, newdata = loess_preds)
  loess_preds <- loess_preds[loess_preds$Percentile >= 0 & loess_preds$Percentile <= 100,]
  
  # Build the plot
  p <- plot_ly(
    data = fd,
    hoverinfo = "text",
    x = ~Rate,
    y = ~Percentile,
    color = ~RateCategory,
    colors = custom_colors, 
    text = ~paste0(
      Name,
      "<br>Points: ", Pts,
      "<br>Standing (percentile): ", `No.`, " (", round(Percentile), "%)",
      "<br>Rating: ", Rate
    ),
    type = "scatter",
    mode = "markers",
    marker = list(size = 6, opacity = 0.7)
  ) %>% 
    add_lines(
      data = lm_preds, x = ~ Rate, y = ~ Percentile, line = list(color = "#333333", width = 0.8), 
      name = "Linear", opacity = 0.6, showlegend = TRUE,inherit = FALSE
    ) %>% 
    add_lines(
      data = loess_preds, x = ~ Rate, y = ~ Percentile, line = list(color = "#F26A21", width = 1), 
      name = "Loess", opacity = 0.8, showlegend = TRUE,inherit = FALSE
    ) %>% 
    layout(
      title = input$selected_grade,
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