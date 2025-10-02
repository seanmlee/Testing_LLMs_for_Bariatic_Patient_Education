

# library --------------------------------------------------------------------
library(tidyverse)
library(emmeans)


emm1 <- emmeans(mod_readability, ~ model | prompt_type)

pairs1 <- pairs(emm1, reverse = TRUE, adjust = "fdr") %>%
  summary(infer = TRUE) %>%
  as.data.frame()

dougall_vs_others <- pairs1 %>%
  filter(str_detect(contrast, "DougallGPT")) %>%
  separate(contrast, into = c("Reference", "Comparison"), sep = " - ", fill = "right") %>%
  mutate(
    Reference = ifelse(Reference == "DougallGPT", Comparison, Reference),
    Comparison = "DougallGPT"
  )

docs_vs_others <- pairs1 %>%
  filter(str_detect(contrast, "DocsGPT")) %>%
  separate(contrast, into = c("Reference", "Comparison"), sep = " - ", fill = "right") %>%
  mutate(
    Reference = ifelse(Reference == "DocsGPT", Comparison, Reference),
    Comparison = "DocsGPT"
  )

model1_results <- bind_rows(dougall_vs_others, docs_vs_others) %>%
  mutate(
    Reference = as.character(Reference),
    Comparison = as.character(Comparison),
    CI = paste0(round(lower.CL, 2), ", ", round(upper.CL, 2)),
    estimate = round(estimate, 2),
    `Corrected p-value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  mutate(`Corrected p-value` = as.character(`Corrected p-value`)) %>%
  select(prompt_type, Reference, Comparison, estimate, CI, `Corrected p-value`)

emm2 <- emmeans(mod_readability2, ~ model_type | prompt_type)

pairs2 <- pairs(emm2, reverse = TRUE, adjust = "fdr") %>%
  summary(infer = TRUE) %>%
  as.data.frame()

medicine_vs_general <- pairs2 %>%
  filter(str_detect(contrast, "general") & str_detect(contrast, "medicine")) %>%
  separate(contrast, into = c("Reference", "Comparison"), sep = " - ", fill = "right") %>%
  mutate(
    Reference = ifelse(Reference == "medicine", Comparison, Reference),
    Comparison = "medicine"
  ) %>%
  mutate(
    Reference = as.character(Reference),
    Comparison = as.character(Comparison),
    CI = paste0(round(lower.CL, 2), ", ", round(upper.CL, 2)),
    estimate = round(estimate, 2),
    `Corrected p-value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)),
    `Corrected p-value` = as.character(`Corrected p-value`)
  ) %>%
  select(prompt_type, Reference, Comparison, estimate, CI, `Corrected p-value`)

final_results <- bind_rows(model1_results, medicine_vs_general) %>%
  mutate(
    Comparison = factor(Comparison, levels = c("DougallGPT", "DocsGPT", "medicine"))
  ) %>%
  arrange(Comparison, prompt_type)

print(final_results)

write.csv(final_results, "out/posthoc_readability.csv", row.names = FALSE)
