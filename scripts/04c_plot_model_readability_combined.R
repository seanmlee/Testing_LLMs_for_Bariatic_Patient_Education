

# libraries --------------------------------------------------------------------
library(cowplot)


# plot -------------------------------------------------------------------------
combined_plot <- plot_grid(
  plot_readability_type,
  plot_readability,
  labels = c("a)", "b)"),
  ncol   = 1,
  align  = "v",
  axis   = "lr"
)

print(combined_plot)


# write ------------------------------------------------------------------------
ggsave(
  "out/plot_model_readability.tiff", 
  dpi = 300, 
  width = 9, 
  height = 6
)
