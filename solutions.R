
## setup

library(tidyverse)
library(gt)
library(lubridate)

## import

cvd_messy <- read_rds('data/cvd_messy.rds')

# Exercise 1 --------------------------------------------------------------

cvd_tidy <- cvd_messy %>% 
  mutate(
    age_number = str_remove_all(question_age, pattern = '\\D'),
    age_number = as.numeric(age_number),
    age_number = if_else(age_number > 500, 
      true = year(today())-age_number, 
      false = age_number
    ),
    subst_duo = factor(
      x = question_substance, 
      levels = c(
        "I smoke and drink",
        "I drink but don't smoke",
        "I smoke but don't drink",
        "I don't smoke or drink"
      ),
      labels = c("Yes_Yes", "Yes_No", "No_Yes", "No_No")
    )
  ) %>% 
  separate(subst_duo, into = c("drink", "smoke")) %>% 
  separate(question_bp, into = c("bp_vals", "bp_meds"), sep = ', ') %>% 
  mutate(bp_vals = str_remove(bp_vals, ' mm Hg')) %>% 
  separate(bp_vals, into = c('sbp', 'dbp'), sep = '/', convert = TRUE) %>% 
  mutate(
    bp_meds = factor(
      x = bp_meds, 
      levels = c("participant not using medication to lower BP",
        "participant uses medication to lower BP"),
      labels = c("No", "Yes")
    )
  ) %>% 
  separate_rows(labs, sep = '; ') %>% 
  separate(labs, into = c('lab_name', 'lab_value'), sep = ': ') %>% 
  mutate(
    lab_value = if_else(lab_value == 'NA', 
      true = NA_character_, 
      false = lab_value
    )
  ) %>% 
  pivot_wider(names_from = lab_name, values_from = lab_value,
    values_fn = list(lab_value = as.numeric)) %>% 
  mutate(
    cvd_status = str_detect(cvd_fup, pattern = 'No CVD', negate = TRUE),
    cvd_status = as.numeric(cvd_status),
    cvd_time = str_extract(cvd_fup, '\\d*\\.?\\d+'),
    cvd_time = as.numeric(cvd_time)
  ) %>% 
  select(ID, cvd_status, cvd_time, everything(), 
    -starts_with('question'), -cvd_fup) %>% 
  mutate_if(is.character, as.factor)

write_rds(cvd_tidy, 'solutions/01_solution.rds')

# Exercise 2 --------------------------------------------------------------

cvd_modified <- cvd_tidy %>% 
  mutate(
    diabetes = if_else(hba1c > 6.5, 'Yes', 'No'),
    albuminuria = if_else(albumin / creatinine >= 30, 'Yes', 'No'),
    sbp_midrange = sbp >= 130 & sbp < 140,
    dbp_midrange = dbp >= 80 & dbp < 90,
    bp_midrange = sbp_midrange | dbp_midrange,
    rec_bpmeds_acc_aha = case_when(
      sbp >= 140 | dbp >= 90 ~ "Yes",
      bp_midrange & 
        (diabetes == "Yes" | albuminuria == "Yes" | age_number > 65) ~ "Yes",
      bp_midrange &
        (diabetes == "No" & albuminuria == "No" & age_number <= 65) ~ "No",
      sbp < 130 & dbp < 80 ~ "No",
      TRUE ~ NA_character_
    ),
    rec_bpmeds_jnc7 = if_else(sbp > 140 | dbp > 90, "Yes", "No"),
    bp_midrange = factor(bp_midrange, labels = c("No", "Yes"))
  ) %>% 
  select(-sbp_midrange, -dbp_midrange, -creatinine, -albumin) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.factor, fct_explicit_na, na_level = 'Unknown')



data_you %>% filter(rec_bpmeds_acc_aha == 'Unknown')

data_bcj %>% filter(rec_bpmeds_acc_aha == 'Unknown')

setdiff(data_bcj$ID, data_you$ID)

count(cvd_modified, rec_bpmeds_acc_aha)

table(cvd_modified$age_number > 65)

table(cvd_modified$rec_bpmeds_acc_aha)

write_rds(cvd_modified, 'solutions/02_solution.rds')

# Exercise 3 --------------------------------------------------------------

library(glue)

tbl_diab <- cvd_modified %>% 
  group_by(rec_bpmeds_acc_aha) %>% 
  count(diabetes) %>% 
  mutate(perc = round(100 * n / sum(n))) %>% 
  ungroup() %>% 
  transmute(
    rec_bpmeds_acc_aha, 
    diabetes, 
    tbl_value = glue("{n} ({perc}%)")
  ) %>% 
  pivot_wider(names_from = rec_bpmeds_acc_aha, values_from = tbl_value)

write_rds(tbl_diab, 'solutions/03_solution.rds')

# Exercise 4 --------------------------------------------------------------

library(gtsummary)

tab_span_label <- paste(
  'Recommended to _initiate_ or _intensify_ medications',
  'to lower BP by the 2017 ACC/AHA guidelines',
  sep = '<br/>')

tbl_cvd <- cvd_modified %>% 
  select(
    rec_bpmeds_acc_aha,
    rec_bpmeds_jnc7,
    age_number,
    sbp,
    dbp,
    bp_midrange,
    bp_meds,
    drink,
    smoke,
    hba1c,
    diabetes,
    albuminuria
  ) %>% 
  tbl_summary(
    by = rec_bpmeds_acc_aha, 
    missing = 'ifany',
    label = list(
      rec_bpmeds_jnc7 ~ "Recommended initiation / intensification by JNC7",
      age_number ~ "Age, years",
      sbp ~ "Systolic blood pressure, mm Hg",
      dbp ~ "Diastolic blood pressure, mm Hg",
      bp_midrange ~ "Systolic/diastolic BP 130-140/80-90 mm Hg",
      bp_meds ~ "Currently using BP lowering medication",
      drink ~ "Alcohol",
      smoke ~ "Smoking",
      hba1c ~ "Hemoglobin A1c",
      diabetes ~ "Diabetes",
      albuminuria ~ "Albuminuria"
    )
  ) %>% 
  as_gt() %>% 
  tab_spanner(
    columns = vars(stat_1, stat_2, stat_3), 
    label = md(tab_span_label)
  )

write_rds(tbl_cvd, 'solutions/04_solution.rds')

# Exercise 5 --------------------------------------------------------------

library(survival)

cvd_model <- cvd_modified %>% 
  filter(
    rec_bpmeds_acc_aha != 'Unknown',
    rec_bpmeds_jnc7 != 'Unknown'
  ) %>% 
  unite(rec_bpmeds_acc_aha, rec_bpmeds_jnc7, col = 'rec') %>% 
  mutate(
    rec = factor(
      x = rec,
      levels = c("No_No", "Yes_No", "Yes_Yes"),
      labels = c(
        "Not recommended BP medication by either guideline",
        "Recommended BP medication by ACC/AHA only",
        "Recommended BP medication by both guidelines"
      )
    )
  )

mdl <- coxph(Surv(cvd_time, cvd_status) ~ rec, data= cvd_model)

tbl_mdl <- tbl_regression(mdl, exponentiate = TRUE, 
  label = rec ~ "Recommendation for BP lowering medications")

write_rds(tbl_mdl, 'solutions/05_solution.rds')


# extras i didn't use
# abpm_demo <- abpm_wide %>% 
#   select(id, age, sex, race) %>% 
#   mutate(asr = glue("{age}_{sex}..{race}"))
# 
# abpm_demo %>% 
#   separate(col = asr, into = c('age', 'sr'), sep = '_') %>%
#   separate(col = sr, into = c('sex', 'race'), sep = '..')
# 
# abpm_demo <- abpm_demo %>% 
#   separate(col = asr, into = c('age', 'sr'), sep = '_') %>%
#   separate(col = sr, into = c('sex', 'race'), sep = '\\.\\.')
# 
# abpm_smry <- abpm_long %>% 
#   separate(type, into = c('measure', 'index'), sep = '_', convert = T) %>% 
#   filter(index < 9) %>% 
#   group_by(id, measure) %>% 
#   summarize(
#     mean = mean(value, na.rm = TRUE), 
#     sd = sd(value, na.rm = TRUE)
#   ) %>% 
#   pivot_wider(values_from = c(mean, sd), names_from = measure) %>% 
#   drop_na()
# 
# abpm_first10 <- abpm_demo %>% 
#   right_join(abpm_smry) %>% 
#   mutate_if(is.character, as.factor) %>% 
#   mutate(
#     hyp = factor(
#       x = mean_sbp > 140 | mean_dbp > 90, 
#       labels = c("No", "Yes")
#     )
#   )
# 
# abpm_model <- glm(hyp ~ sex + race, 
#   data = abpm_first10, family = 'binomial')
# 
# 
# 
# 
# desc_sex <- abpm_first10 %>% 
#   group_by(sex) %>% 
#   summarize(p = mean(hyp == 'Yes'), n = n()) %>% 
#   mutate_if(is.factor, as.character) %>% 
#   rename(level = sex)
# 
# desc_race <- abpm_first10 %>% 
#   group_by(race) %>% 
#   summarize(p = mean(hyp == 'Yes'), n = n()) %>% 
#   mutate_if(is.factor, as.character) %>% 
#   rename(level = race)
# 
# abpm_tbl <- tidy(abpm_model, conf.int = TRUE) %>% 
#   add_ref_rows(data = abpm_first10) %>% 
#   mutate_if(is.factor, as.character) %>% 
#   transmute(
#     variable, level,
#     pg = pointGap(estimate, conf.low, conf.high, ref_value = 0),
#     pg = exp(pg)
#   ) %>% 
#   filter(variable != '(Intercept)')
# 
# bind_rows(sex = desc_sex, race = desc_race, .id = 'variable') %>% 
#   right_join(abpm_tbl) %>% 
#   mutate(variable = Hmisc::capitalize(variable)) %>% 
#   group_by(variable) %>% 
#   mutate(
#     percent = tbv_round(100 * n / sum(n)),
#     prev = tbv_round(100 * p),
#     n = format(n, big.mark = ',')
#   ) %>% 
#   unite(n, percent, col = 'nperc', sep = ', ') %>% 
#   select(variable, level, nperc, prev, pg) %>% 
#   gt(groupname_col = 'variable', rowname_col = 'level') %>% 
#   cols_label(nperc = 'N, %', pg = 'OR, 95% CI') %>% 
#   cols_align('center')
# 
# semi_join(abpm_demo, abpm_smry)
# 
# abpm_smry