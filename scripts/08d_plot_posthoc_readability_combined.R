

# libraries --------------------------------------------------------------------
library(cowplot)


# plot -------------------------------------------------------------------------
# common x scale for all plots
common_x <- scale_x_continuous(
  breaks = c(0.01, 0.1, 1, 10, 100),
  labels = c(".01", ".1", "1", "10", "100"),
  name   = "Odds Ratio",
  expand = c(0, 0),
  limits = c(0.01, 100),
  oob    = scales::oob_squish  # <- clamp outside values to edges
)

plot_posthoc_readability_type    <- plot_posthoc_readability_type + common_x
plot_posthoc_readability_dougall <- plot_posthoc_readability_dougall + common_x
plot_posthoc_readability_docsgpt <- plot_posthoc_readability_docsgpt + common_x

combined_plot <- plot_grid(
  plot_readability_type,
  plot_readability_dougall,
  plot_readability_docsgpt,
  labels = c("a)", "b)", "c)"),
  ncol   = 1,
  align  = "v",
  axis   = "lr",
  rel_heights = c(0.6, 1, 1)
)

print(combined_plot)


# write ------------------------------------------------------------------------
ggsave(
  "out/plot_posthoc_readability.tiff", 
  dpi = 300, 
  width = 9, 
  height = 7
)
