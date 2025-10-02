

# library ----------------------------------------------------------------------
library(brglm2)


# mod_correctness --------------------------------------------------------------
mod_correctness <- brmultinom(
  outcome ~
    model * prompt_type, 
  data = ai
)
sjPlot::tab_model(mod_correctness)

mod_correctness_type <- brmultinom(
  outcome ~
    model_type * prompt_type, 
  data = ai
)
sjPlot::tab_model(mod_correctness_type)


# mod_readability --------------------------------------------------------------
mod_readability <- lm(
  readability_score ~ 
    model * prompt_type,
  data = ai
)
sjPlot::tab_model(mod_readability)

mod_readability_type <- lm(
  readability_score ~ 
    model_type * prompt_type,
  data = ai
)
sjPlot::tab_model(mod_readability_type)
