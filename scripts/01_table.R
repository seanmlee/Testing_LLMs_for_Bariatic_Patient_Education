

# library ----------------------------------------------------------------------
library(table1)


# stratify by prompt_type ------------------------------------------------------
prompt_type_a <- ai %>% filter(prompt_type == "a") 
prompt_type_b <- ai %>% filter(prompt_type == "b")
prompt_type_c <- ai %>% filter(prompt_type == "c")
prompt_type_d <- ai %>% filter(prompt_type == "d")


# overall ----------------------------------------------------------------------
tbl_overall <-
table1(
  ~ as.factor(outcome) +
    readability_score | model_type + model,
  overall = FALSE,
  data = ai
)

tbl_a <-
table1(
  ~ as.factor(outcome) +
    readability_score | model_type + model,
  overall = FALSE,
  data = prompt_type_a
)

tbl_b <-
table1(
  ~ as.factor(outcome) +
    readability_score | model_type + model,
  overall = FALSE,
  data = prompt_type_b
)

tbl_c <-
  table1(
    ~ as.factor(outcome) +
      readability_score | model_type + model,
    overall = FALSE,
    data = prompt_type_c
  )

tbl_d <-
  table1(
    ~ as.factor(outcome) +
      readability_score | model_type + model,
    overall = FALSE,
    data = prompt_type_d
  )
