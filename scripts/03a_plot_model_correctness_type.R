

# libraries --------------------------------------------------------------------
library(tidyverse)


# preds ----------------------------------------------------------------------
critval <- 1.96

newdata <- expand.grid(
  
  model_type = c("general", "medicine"),
  prompt_type = c("a", "b", "c", "d")
  
)


preds <- predict(
  
  mod_correctness_type, 
  newdata = newdata, 
  type = "probs", 
  se.fit = TRUE
  
)


fit <- as.data.frame(
  
  cbind(
    newdata,
    preds
  )
  
)


fit_long <- fit %>%
  
  pivot_longer(
    cols = `0`:`2`, 
    names_to = "outcome", 
    values_to = "probability") %>%
  
  mutate(
    outcome = as.integer(outcome)
  )

print(fit_long)


# plot -------------------------------------------------------------------------
plot_model_correctness_type <- 
  
  fit_long %>%
  
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
      x = as.factor(outcome), 
      y = probability,
      color = model_type,
      fill = model_type,
      group = model_type
    )
    
  ) +
  
  geom_line(
    position = position_dodge(width = 0.75)
  ) +
  
  geom_point(
    pch = 21,
    color = "black",
    size = 2.5,
    position = position_dodge(width = 0.75)
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.5),
    labels = scales::percent_format(accuracy = 1)
  ) +

  scale_x_discrete(
    labels = c(
      "0" = "Wrong",
      "1" = "Partial",
      "2" = "Correct"
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
    y = "Probability of Outcome"
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
    
    rows = vars(model_type), 
    
    cols = vars(prompt_type),
    
    labeller = labeller(
      
      prompt_type = c(
        "a" = "Prompt A", 
        "b" = "Prompt B", 
        "c" = "Prompt C", 
        "d" = "Prompt D"
      ),
      
      model_type = c(
        "general" = "General",
        "medicine" = "Medicine"
      )
      
    )
    
  )

plot_model_correctness_type
