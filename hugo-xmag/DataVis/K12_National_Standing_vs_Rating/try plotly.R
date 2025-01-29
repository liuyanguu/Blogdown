fd <- combined_data[combined_data$Grade_full == "Grade2", ]
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
  text = ~paste0(
    Name,
    "<br>Points: ", Pts,
    "<br>Standing (percentile): ", `No.`, " (", round(Percentile), "%)",
    "<br>Rating: ", Rate
  ),
  type = "scatter",
  mode = "markers",
  marker = list(size = 4, opacity = 0.5)
) %>% 
  layout(
    title = "Grade2",
    xaxis = list(title = "Rating", showline = FALSE),
    yaxis = list(title = "Percentile of standing", autorange = "reversed"),
    legend = list(orientation = "h", x = 0.1, y = -0.2)
  ) %>% add_lines(
    data = lm_preds, x = ~ Rate, y = ~ Percentile, line = list(color = "#333333", width = 0.8), 
    name = "Linear", opacity = 0.6, inherit = FALSE
  ) %>% add_lines(
    data = loess_preds, x = ~ Rate, y = ~ Percentile, line = list(color = "#F26A21", width = 1), 
    name = "Loess", opacity = 0.8, inherit = FALSE
  )

p
