pacman::p_load(
  tidyverse,
  rio,
  here,
  jmv,
  janitor,
  gtsummary
)

##importing_data
data_raw <- import(here("posts3", "stroke_data.csv"))


##cleaning_data 

data_clean <- data_raw %>%                    
  clean_names() %>%                              ## cleaning column names
  mutate(
    sex = case_when(                             ## recode into same variable 
      sex == 1 ~ "male",
      sex == 0  ~ "female"
    )
  ) %>% 
  mutate(sex = as.factor(sex)) %>%               ## Clean Data Types
  mutate(
    work_type = case_when(
      work_type == 0 ~ "never_worked",
      work_type == 1 ~ "child",
      work_type == 2 ~ "governement_job",
      work_type == 3 ~ "self_employed",
      work_type == 4 ~ "private_sector"
    )
  ) %>% 
  mutate(
    residence_type = case_when(
      residence_type == 1 ~ "Urban",
      residence_type == 0 ~ "Rural"
    )
  ) %>% 
  mutate(across(c(hypertension, heart_disease, ever_married, smoking_status, stroke), ~ .x %>%
                  as.factor() %>%
                  fct_recode(
                    "Yes" = "1",
                    "No" = "0"
                  ))
  )





## Descriptive Analytics  =======

#using gtsummary
data_clean %>% 
tbl_summary()

#using jamovi
descriptives(data_clean, vars = vars(sex, age, hypertension, heart_disease,
                                     ever_married, work_type, residence_type, avg_glucose_level, 
                                     bmi, smoking_status, stroke), freq = TRUE)

## manually 
mean(data_clean$age, na.rm = TRUE)

## Hypothesis Testing ======================

# Using gtsumary 
data_clean %>% 
  tbl_summary(by=stroke) %>% 
  add_p()


jmv::contTables(
  data = data_clean,
  rows = hypertension,
  cols = stroke,
  counts = TRUE,
  risk = TRUE,       # This calculates both Odds Ratio and Relative Risk
  chiSq = TRUE       # This is a good practice to include
)



## using jmv

