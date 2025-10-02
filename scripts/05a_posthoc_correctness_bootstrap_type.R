

# library --------------------------------------------------------------------
library(tidyverse)
library(brglm2)
library(boot)


ai <- ai %>%
  mutate(model_type = relevel(factor(model_type), ref = "general"))

preferred_ref <- "general"

model_types <- unique(ai$model_type)

prompt_types <- unique(ai$prompt_type)

newdata <- expand.grid(
  model_type = model_types,
  prompt_type = prompt_types
)

get_cond_name <- function(model_type, prompt_type) {
  paste(model_type, prompt_type, sep = "_")
}

run_bootstrap_comparison <- function(data, cond_medicine, cond_general, level = "2", newdata, R = 200) {
  set.seed(123)
  
  boot_result <- boot(
    data = data,
    statistic = function(d, i) {
      d_boot <- d[i, ]
      mod_boot <- brmultinom(outcome ~ model_type * prompt_type, data = d_boot)
      preds <- predict(mod_boot, newdata = newdata, type = "probs")
      preds_df <- as.data.frame(preds)
      preds_df$id <- paste(newdata$model_type, newdata$prompt_type, sep = "_")
      
      p_medicine <- preds_df[[level]][preds_df$id == cond_medicine]
      p_general <- preds_df[[level]][preds_df$id == cond_general]
      
      odds_medicine <- p_medicine / (1 - p_medicine)
      odds_general <- p_general / (1 - p_general)
      
      odds_medicine / odds_general
    },
    R = R
  )
  
  mod_orig <- brmultinom(outcome ~ model_type * prompt_type, data = data)
  preds_orig <- predict(mod_orig, newdata = newdata, type = "probs")
  preds_df <- as.data.frame(preds_orig)
  preds_df$id <- paste(newdata$model_type, newdata$prompt_type, sep = "_")
  
  p_medicine <- preds_df[[level]][preds_df$id == cond_medicine]
  p_general <- preds_df[[level]][preds_df$id == cond_general]
  
  odds_medicine <- p_medicine / (1 - p_medicine)
  odds_general <- p_general / (1 - p_general)
  
  point_estimate <- odds_medicine / odds_general
  
  ci <- boot.ci(boot_result, type = "perc")$percent[4:5]
  p_val_raw <- 2 * min(mean(boot_result$t >= 1), mean(boot_result$t <= 1))
  p_val_fmt <- ifelse(p_val_raw < 0.001, "<0.001", sprintf("%.3f", p_val_raw))
  
  data.frame(
    condition_1 = cond_medicine,
    condition_2 = cond_general,
    odds_ratio = point_estimate,
    CI_lower = ci[1],
    CI_upper = ci[2],
    p_value = p_val_fmt
  )
}

pairs_to_compare <- purrr::map(prompt_types, function(pt) {
  other_types <- setdiff(model_types, preferred_ref)
  purrr::map(other_types, function(mtype) {
    list(
      cond_medicine = get_cond_name(mtype, pt),
      cond_general = get_cond_name(preferred_ref, pt)
    )
  })
}) |> unlist(recursive = FALSE)

all_results <- purrr::map_df(pairs_to_compare, function(pair) {
  run_bootstrap_comparison(
    data = ai,
    cond_medicine = pair$cond_medicine,
    cond_general = pair$cond_general,
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

write.csv(all_results, "out/posthoc_medicine_vs_general.csv", row.names = FALSE)
