

# libraries --------------------------------------------------------------------
library(cowplot)


# plot -------------------------------------------------------------------------
combined_plot <- plot_grid(
  plot_model_correctness_type,
  plot_model_correctness,
  labels = c("a)", "b)"),
  ncol   = 1,
  align  = "v",
  axis   = "lr",
  rel_heights = c(0.5, 1)
)

print(combined_plot)


# write ------------------------------------------------------------------------
ggsave(
  "out/plot_model_correctness.tiff", 
  dpi = 300, 
  width = 7, 
  height = 11
)
