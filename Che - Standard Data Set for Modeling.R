# Load packages ----
library(tidyverse)

# Load data ----
app_train <- read_csv("application_train.csv")

bureau_df <- read_csv("bureau.csv")

desired_columns <- read_csv("desired_columns.csv")

# Identify numerically encoded categorical variables ----
num2fac <- app_train %>%
  select(where(is.numeric)) %>%
  mutate(across(everything(), as.character)) %>%
  select(where(~ all(!grepl("\\.|-", .)))) %>%
  #select(-c(own_car_age, hour_appr_process_start, matches("^(obs|def|amt|cnt)"))) %>%
  select(-c(OWN_CAR_AGE, HOUR_APPR_PROCESS_START, matches("^(OBS|DEF|AMY|CNT)"))) %>%
  colnames() 

# Create data frame ----
app_train1 <- app_train %>%
  # Select columns from 'desired_columns.csv'
  select(desired_columns$ColumnName) %>% 
  # Handling numerically encoded categorical variables
  mutate(across(c(where(is.character), all_of(num2fac)), factor), 
         # Fixing invalid NA's
         across(c(CODE_GENDER, ORGANIZATION_TYPE), 
                ~case_when(. != "XNA" ~ .)),
         # Replacing NA's in social circle columns with 0
         across(contains("SOCIAL_CIRCLE"), ~replace_na(., 0)),
         # Replacing NA's with 0
         across(c(AMT_REQ_CREDIT_BUREAU_HOUR, AMT_REQ_CREDIT_BUREAU_DAY,
                  AMT_REQ_CREDIT_BUREAU_WEEK, AMT_REQ_CREDIT_BUREAU_MON,
                  AMT_REQ_CREDIT_BUREAU_QRT, AMT_REQ_CREDIT_BUREAU_YEAR),
                ~replace_na(., factor("0"))),
         # Fixing unusual `DAYS_EMPLOYED` values
         DAYS_EMPLOYED = case_when(DAYS_EMPLOYED <= 0 ~ DAYS_EMPLOYED),
         # Creating ordinal version of `OWN_CAR_AGE`
         OWN_CAR_AGE2 = cut(OWN_CAR_AGE,
                            c(0, 5, 10, 15, Inf),
                            right = FALSE),
         OWN_CAR_AGE2 = case_when(FLAG_OWN_CAR == "N" ~ factor("No Car Owned"),
                                  .default = OWN_CAR_AGE2)) %>%
  # Creating aggregate variable from the `EXT_SOURCE_*` variables
  rowwise() %>%
  mutate(AVG_EXT_SCORE = mean(c_across(contains("EXT_"))),
         .after = EXT_SOURCE_3) %>%
  ungroup() %>%
  # Removing rows with NA's in below columns
  filter(if_all(c(FLAG_OWN_CAR, AMT_GOODS_PRICE, AMT_ANNUITY, 
                  CNT_FAM_MEMBERS, DAYS_LAST_PHONE_CHANGE),
                ~!is.na(.)))