df <- reshape2::melt(outer(1:4, 1:4), varnames = c("X1", "X2"))

p1 <- ggplot(df, aes(X1, X2)) + geom_tile(aes(fill = value))
p2 <- p1 + geom_point(aes(size = value))

# Basic form
p1 +
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90",
                       guide = guide_colourbar(barwidth = 20, barheight = 0.5))+
    # scale_color_gradient(guide = guide_colourbar(),
    #                    low="#FFCC33", high="#6600CC", 
    #                    breaks=c(0,1), labels=c("Low","High")) +

  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
        legend.position = "bottom")

data_long = shap_long
x_bound <- max(abs(data_long$value))
require('ggplot2')
require('ggforce') # for `geom_sina`
ggplot(data = data_long)+
  coord_flip() + 
  # sina plot: 
  geom_sina(aes(x = variable, y = value, color = stdfvalue),
            method = "counts", maxwidth = 0.7) +
  # print the mean absolute value on the left: 
  geom_text(data = unique(data_long[, c("variable", "mean_value"), with = F]),
            aes(x = variable, y=-Inf, label = sprintf("%.3f", mean_value)),
            size = 3, alpha = 0.7,
            hjust = -0.2, 
            fontface = "bold", inherit.aes = F) + # bold
  # # if want to add a "SHAP" bar notation
  # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
  #          label = expression(group("|", bar(SHAP), "|"))) + 
  scale_color_gradient(low="#FFCC33", high="#6600CC",
                       guide = guide_colourbar(barwidth = 0.4, barheight = 15))+
  # scale_color_gradient(guide = guide_colourbar(),
  #                    low="#FFCC33", high="#6600CC", 
  #                    breaks=c(0,1), labels=c("Low","High")) +
  scale_y_continuous(limits = c(-x_bound, x_bound)) +
  # reverse the order of features
  scale_x_discrete(limits = rev(levels(data_long$variable))) + 
  geom_hline(yintercept = 0) + # the vertical line
  labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value") +
  theme_bw() + 
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())


