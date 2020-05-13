
library(tidyverse)
library(glue)


# for lecture

abpm_wide <- read_csv('data/abpm_wide_synthetic.csv') 

abpm_wide <- abpm_wide %>% 
  mutate(id = row_number())

abpm_long <- abpm_wide %>%
  mutate(id = row_number()) %>% 
  pivot_longer(cols = sbp_0:hr_23, names_to = 'type') %>% 
  select(id, type, value)

abpm_demo <- abpm_wide %>% 
  select(id, age, sex, race) %>% 
  unite(col = 'asr', age, sex, race)

write_rds(abpm_long, 'data/abpm_long.rds')
write_rds(abpm_demo, 'data/abpm_demographics.rds')

# for exercises

cvd <- read_csv('../data/synthdata_cvd.csv') %>% 
  bind_cols(ID = 1:nrow(.), .)

library(lubridate)

cvd_messy <- cvd %>% 
  transmute(
    ID,
    question_age = if_else(age > 50, 
      glue("I was born in {year(today()) -age}"),
      glue("I am {age} years old")
    ),
    question_substance = case_when(
      alc == 1 & currentsmoker == 1 ~ "I smoke and drink",
      alc == 1 & currentsmoker == 0 ~ "I drink but don't smoke",
      alc == 0 & currentsmoker == 1 ~ "I smoke but don't drink",
      alc == 0 & currentsmoker == 0 ~ "I don't smoke or drink"
    ),
    question_bp = glue("{round(sbp)}/{round(dbp)} mm Hg"),
    question_bp = if_else(bpmeds == 'Yes', 
      glue("{question_bp}, participant uses medication to lower BP"),
      glue("{question_bp}, participant not using medication to lower BP")),
    rando_vec = rbinom(n = n(), size = 3, prob = 1/2),
    labs = case_when(
      rando_vec == 0 ~ glue(
        "albumin: {round(albumin,3)}; \\
        hba1c: {round(hba1c, 3)}; \\
        creatinine: {round(creatinine, 3)}"
      ),
      rando_vec == 1 ~ glue(
        "creatinine: {round(creatinine, 3)}; \\
        albumin: {round(albumin,3)}; \\
        hba1c: {round(hba1c, 3)}"
        
      ),
      rando_vec == 2 ~ glue(
        "creatinine: {round(creatinine, 3)}; \\
        albumin: {round(albumin,3)}; \\
        hba1c: {round(hba1c, 3)}"
      ),
      rando_vec == 3 ~ glue(
        "hba1c: {round(hba1c, 3)}; \\
        creatinine: {round(creatinine, 3)}; \\
        albumin: {round(albumin,3)}"
      )
    ),
    cvd_fup = if_else(chd_strk == 'Yes', 
      glue("experienced CVD event after {round(time_chd_strk, 3)} years"),
      glue("No CVD event observed during {round(time_chd_strk, 3)} years")
    )
  ) %>% 
  select(-rando_vec) %>% 
  mutate_at(.vars = vars(-ID), as.character)

write_rds(cvd_messy, 'data/cvd_messy.rds')




#



