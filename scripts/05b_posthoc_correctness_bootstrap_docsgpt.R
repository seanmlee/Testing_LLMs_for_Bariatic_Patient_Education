

# library --------------------------------------------------------------------
library(tidyverse)
library(brglm2)
library(boot)


preferred_refs <- c("DocsGPT")

models <- unique(ai$model)
prompt_types <- unique(ai$prompt_type)

newdata <- expand.grid(
  model = models,
  prompt_type = prompt_types
)

get_cond_name <- function(model, prompt_type) {
  paste(model, prompt_type, sep = "_")
}

run_bootstrap_comparison <- function(data, cond_docsgpt, cond_other, level = "2", newdata, R = 200) {
  set.seed(123)
  
  boot_result <- boot(
    data = data,
    statistic = function(d, i) {
      d_boot <- d[i, ]
      mod_boot <- brmultinom(outcome ~ model * prompt_type, data = d_boot)
      preds <- predict(mod_boot, newdata = newdata, type = "probs")
      preds_df <- as.data.frame(preds)
      preds_df$id <- paste(newdata$model, newdata$prompt_type, sep = "_")
      
      p_docsgpt <- preds_df[[level]][preds_df$id == cond_docsgpt]
      p_other   <- preds_df[[level]][preds_df$id == cond_other]
      
      odds_docsgpt <- p_docsgpt / (1 - p_docsgpt)
      odds_other   <- p_other / (1 - p_other)
      
      odds_docsgpt / odds_other
    },
    R = R
  )
  
  mod_orig <- brmultinom(outcome ~ model * prompt_type, data = data)
  preds_orig <- predict(mod_orig, newdata = newdata, type = "probs")
  preds_df <- as.data.frame(preds_orig)
  preds_df$id <- paste(newdata$model, newdata$prompt_type, sep = "_")
  
  p_docsgpt <- preds_df[[level]][preds_df$id == cond_docsgpt]
  p_other   <- preds_df[[level]][preds_df$id == cond_other]
  
  odds_docsgpt <- p_docsgpt / (1 - p_docsgpt)
  odds_other   <- p_other / (1 - p_other)
  
  point_estimate <- odds_docsgpt / odds_other
  
  ci <- boot.ci(boot_result, type = "perc")$percent[4:5]
  p_val_raw <- 2 * min(mean(boot_result$t >= 1), mean(boot_result$t <= 1))
  p_val_fmt <- ifelse(p_val_raw < 0.001, "<0.001", sprintf("%.3f", p_val_raw))
  
  data.frame(
    condition_1 = cond_docsgpt,
    condition_2 = cond_other,
    odds_ratio = point_estimate,
    CI_lower = ci[1],
    CI_upper = ci[2],
    p_value = p_val_fmt
  )
}

pairs_to_compare <- purrr::map(prompt_types, function(pt) {
  other_models <- setdiff(models, preferred_refs)
  purrr::map(other_models, function(m) {
    list(
      cond_docsgpt = get_cond_name("DocsGPT", pt),
      cond_other = get_cond_name(m, pt)
    )
  })
}) |> unlist(recursive = FALSE)

all_results <- purrr::map_df(pairs_to_compare, function(pair) {
  run_bootstrap_comparison(
    data = ai,
    cond_docsgpt = pair$cond_docsgpt,
    cond_other = pair$cond_other,
    level = "2",
    newdata = newdata,
    R = 200
  )
})

all_results <- all_results %>%
  mutate(
    p_value_numeric = as.numeric(str_replace(p_value, "<0.001", "0.001")),
    p_value_BH = p.adjust(p_value_numeric, method = "fdr"),
    p_value_BH_fmt = ifelse(p_value_BH < 0.001, "<0.001", sprintf("%.3f", p_value_BH))
  )

write.csv(all_results, "out/posthoc_docsgpt_vs_others.csv", row.names = FALSE)
