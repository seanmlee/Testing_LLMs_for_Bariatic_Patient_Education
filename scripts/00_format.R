

# library ----------------------------------------------------------------------
library(tidyverse)


# format -----------------------------------------------------------------------
ai <- read.csv("data/ai.csv", header = TRUE) %>%
  
  mutate(
    
    outcome_binary = case_when(
      outcome == 1 ~ 0,
      outcome == 2 ~ 1,
      TRUE ~ outcome
    ),
    
    prompt_type = factor(
      prompt_type, 
      levels = c(
        "a",
        "b",
        "c",
        "d"
      )
    ),
 
    model = ifelse(
      model == "Cluade Opus",
      "Claude Opus",
      model
    )
    
  )

