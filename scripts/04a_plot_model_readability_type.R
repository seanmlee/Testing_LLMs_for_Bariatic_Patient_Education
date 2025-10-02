

# libraries --------------------------------------------------------------------
library(tidyverse)


# preds ----------------------------------------------------------------------
critval <- 1.96

newdata <- expand.grid(
  
  model_type = c("general", "medicine"),
  prompt_type = c("a", "b", "c", "d")
  
)


preds <- predict(
  
  mod_readability_type, 
  newdata = newdata, 
  type = "response", 
  se.fit = FALSE
  
)


fit <- as.data.frame(
  
  cbind(
    newdata,
    preds
  )
  
)


# plot -------------------------------------------------------------------------
plot_model_readability_type <- 
  
  fit %>%
  
  mutate(
    
    model_type = factor(
      model_type, 
      levels = c(
        "general",
        "medicine"
      )
    ) 
    
  ) %>%
  
  ggplot(
    
    aes(
      x = model_type, 
      y = preds,
      color = model_type,
      fill = model_type
    )
    
  ) +
  
  geom_col(
    width = 0.5, 
    fill = "white"
    ) +  
  
  scale_y_continuous(
    limits = c(0, 60),
    breaks = c(0, 30, 60)
  ) +
  
  scale_x_discrete(
    labels = c(
      "general" = "General",
      "medicine" = "Medicine"
    )
  ) +
  
  scale_color_manual(
    values = c("general" = "black", "medicine" = "#ff61c3")
  ) +
  
  scale_fill_manual(
    values = c("general" = "black", "medicine" = "#ff61c3")
  ) +
  
  labs(
    title = "",
    x = "",
    y = "Readability Score"
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    
    legend.position = "none",
    panel.grid = element_blank(),
    panel.spacing.y = unit(1.5, "lines"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(
      angle = 25,
      hjust = 1,
      vjust = 1 
    ),
    axis.text = element_text(),
    axis.title = element_text(),
    axis.title.x = element_text(margin = margin(t = 0)),
    axis.title.y = element_text(margin = margin(r = 5)),
    legend.title = element_text(),
    legend.text = element_text()
    
  ) +
  
  facet_grid(
    
    ~prompt_type,
    
    labeller = labeller(
      prompt_type = c(
        "a" = "Prompt A", 
        "b" = "Prompt B", 
        "c" = "Prompt C", 
        "d" = "Prompt D"
      )
    )
    
  )

plot_model_readability_type
