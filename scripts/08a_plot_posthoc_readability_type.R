

# libraries --------------------------------------------------------------------
library(tidyverse)
library(cowplot)


# df ---------------------------------------------------------------------------
df <- tribble(
  ~prompt_type, ~Reference, ~Comparison, ~estimate, ~CI_lower, ~CI_upper, ~pval_chr,
  "a", "Gemini", "DougallGPT", -5.71, -13.91, 2.49, "0.25",
  "a", "Claude Opus", "DougallGPT", -8.13, -16.34, 0.07, "0.054",
  "a", "DocsGPT", "DougallGPT", -5.01, -13.21, 3.19, "0.255",
  "a", "GPT 4o", "DougallGPT", -3.37, -11.57, 4.83, "0.472",
  "b", "Gemini", "DougallGPT", -7.89, -16.09, 0.31, "0.017",
  "b", "Claude Opus", "DougallGPT", -7.96, -16.16, 0.24, "0.017",
  "b", "DocsGPT", "DougallGPT", -9.89, -18.09, -1.69, "0.004",
  "b", "GPT 4o", "DougallGPT", -10.95, -19.15, -2.75, "0.002",
  "c", "Gemini", "DougallGPT", -17.11, -25.31, -8.91, "<0.001",
  "c", "Claude Opus", "DougallGPT", -22.59, -30.79, -14.39, "<0.001",
  "c", "DocsGPT", "DougallGPT", -18.9, -27.11, -10.7, "<0.001",
  "c", "GPT 4o", "DougallGPT", -9.83, -18.04, -1.63, "0.002",
  "d", "Gemini", "DougallGPT", -4.45, -12.65, 3.76, "0.181",
  "d", "Claude Opus", "DougallGPT", -14.43, -22.64, -6.23, "<0.001",
  "d", "DocsGPT", "DougallGPT", -13.21, -21.41, -5.01, "<0.001",
  "d", "GPT 4o", "DougallGPT", -10.73, -18.94, -2.53, "<0.001",
  
  "a", "Gemini", "DocsGPT", -0.7, -8.91, 7.5, "0.809",
  "a", "Claude Opus", "DocsGPT", -3.12, -11.33, 5.08, "0.472",
  "a", "GPT 4o", "DocsGPT", -1.64, -9.84, 6.56, "0.637",
  "a", "DougallGPT", "DocsGPT", -5.01, -13.21, 3.19, "0.255",
  "b", "Gemini", "DocsGPT", 2, -6.2, 10.2, "0.634",
  "b", "Claude Opus", "DocsGPT", 1.93, -6.27, 10.13, "0.634",
  "b", "GPT 4o", "DocsGPT", 1.06, -7.14, 9.26, "0.796",
  "b", "DougallGPT", "DocsGPT", -9.89, -18.09, -1.69, "0.004",
  "c", "Gemini", "DocsGPT", 1.8, -6.41, 10, "0.537",
  "c", "Claude Opus", "DocsGPT", -3.68, -11.89, 4.52, "0.229",
  "c", "GPT 4o", "DocsGPT", -9.07, -17.27, -0.87, "0.003",
  "c", "DougallGPT", "DocsGPT", -18.9, -27.11, -10.7, "<0.001",
  "d", "Gemini", "DocsGPT", 8.77, 0.56, 16.97, "0.005",
  "d", "Claude Opus", "DocsGPT", -1.22, -9.42, 6.98, "0.675",
  "d", "GPT 4o", "DocsGPT", -2.48, -10.68, 5.72, "0.438",
  "d", "DougallGPT", "DocsGPT", -13.21, -21.41, -5.01, "<0.001",
  
  "a", "General", "Medicine", -3.23, -7.3, 0.83, "0.118",
  "b", "General", "Medicine", -3.99, -8.05, 0.07, "0.054",
  "c", "General", "Medicine", -7.06, -11.12, -2.99, "<0.001",
  "d", "General", "Medicine", -3.26, -7.33, 0.8, "0.115"
)


# format labels ----------------------------------------------------------------
df_type <- df %>%
  
  filter(
    Reference == "General"
    ) %>%
  
  mutate(
    
    prompt_type = factor(
      prompt_type, 
      levels = c("a", "b", "c", "d")
    ),
    
    y_label = paste0(
      "Reference: ", 
      Reference, 
      "\nComparison: ", 
      Comparison
    )
    
  )

# plot -------------------------------------------------------------------------
plot_posthoc_readability_type <- 
  
  ggplot(
    
    df_type, 
    
    aes(
      y = y_label, 
      x = estimate
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
    
    xintercept = 0, 
    linetype = "dashed", 
    color = "red",
    alpha = 0.5,
    linewidth = 0.5
    
  ) +
  
  scale_x_continuous(
    limits = c(-12, 12),
    breaks = c(-10, 0, 10)
  ) +
  
  labs(
    
    x = "β", 
    
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
  
  facet_wrap(
    ~prompt_type, 
    nrow = 1,
    
    labeller = labeller(
      
      prompt_type = c(
        "a" = "Prompt A", 
        "b" = "Prompt B", 
        "c" = "Prompt C", 
        "d" = "Prompt D"
      )
      
    )
    
  ) 

print(plot_posthoc_readability_type)
