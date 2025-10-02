

# libraries --------------------------------------------------------------------
library(tidyverse)


# read posthoc -----------------------------------------------------------------
all_results <- read.csv("posthoc_docsgpt_vs_others.csv", header = TRUE)


# format labels ----------------------------------------------------------------
all_results_plot <- all_results %>%
  
  mutate(
    prompt = str_extract(condition_1, "_[a-d]$") |> str_remove("_"),
    model  = str_remove(condition_2, "_[a-d]$"),
    label  = paste0(model),
    log_OR = log(odds_ratio),
    log_CI_lower = log(CI_lower),
    log_CI_upper = log(CI_upper)
  ) %>%
  
  filter(
    !grepl("DougallGPT", condition_2, ignore.case = TRUE)) %>%
  
  mutate(
    
    label = paste(
      "Reference:", 
      label, 
      "\nComparison: DocsGPT"
    ),
    
    prompt = factor(
      prompt, 
      levels = c("a", "b", "c", "d")
    )
    
  )


# plot -------------------------------------------------------------------------
plot_posthoc_correctness_docsgpt <- 
  
  ggplot(
    
    all_results_plot, 
    
    aes(
      x = odds_ratio, 
      y = label
    )
    
  ) +
  
  geom_errorbarh(
    
    aes(
      xmin = CI_lower, 
      xmax = CI_upper
    ), 
    
    height = 0, 
    size = 0.5,
    color = "black"
    
  ) +
  
  geom_point(
    
    size = 5,
    pch = 21,
    color = "black",
    fill = "white"
    
  ) +
  
  geom_vline(
    
    xintercept = 1, 
    linetype = "dashed", 
    color = "red",
    alpha = 0.5,
    linewidth = 0.5
    
  ) +
  
  scale_x_log10(
    
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c(".01", ".1", "1", "10", "100"),
    name = "Odds Ratio"
    
  ) +
  
  labs(
    y = NULL
  ) +
  
  theme_bw(
    base_size = 13
  ) +
  
  theme(
    
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "white"),
    panel.spacing = unit(1.5, "lines"),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(hjust = 0.65, margin = margin(r = 5, t = 2, b = 2)),
    axis.title.x = element_text(margin = margin(b = 0))
    
    ) +
  
  facet_grid(
    
    ~ prompt, 
    
    labeller = labeller(
      
      prompt = c(
        "a" = "Prompt A", 
        "b" = "Prompt B", 
        "c" = "Prompt C", 
        "d" = "Prompt D"
      )
      
    )
    
  )

print(plot_posthoc_correctness_docsgpt)
