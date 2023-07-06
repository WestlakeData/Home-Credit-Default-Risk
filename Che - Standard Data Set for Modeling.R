<<<<<<< HEAD
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

# Aggregate bureau data ----
# __ Wide version data frame
bureau_agg_a <- bureau_df %>%
  # Seemed to be the most relevant credit types
  filter(CREDIT_TYPE %in% c("Consumer credit","Credit card","Car loan","Mortgage",
                            "Microloan","Loan for business development","Another type of loan"),
         # Ensure credit was actually given
         AMT_CREDIT_SUM > 0,
         # Thought this made most sense
         CREDIT_ACTIVE %in% c("Active", "Closed"),
         # It's unclear if they convert nominal curreny values so kept only most common
         CREDIT_CURRENCY == "currency 1") %>%
  # Clean credit type values to use as column names
  mutate(CREDIT_TYPE = gsub(" +", "_", tolower(CREDIT_TYPE))) %>%
  group_by(SK_ID_CURR, CREDIT_TYPE) %>%
  summarise(avg_credit = mean(AMT_CREDIT_SUM),
            # Decided that average of ratios made more sense than ratio of averages
            avg_overage = mean(AMT_CREDIT_MAX_OVERDUE/AMT_CREDIT_SUM, na.rm = TRUE)) %>%
  pivot_wider(names_from = CREDIT_TYPE,
              values_from = c(avg_credit, avg_overage)) %>%
  ungroup()

# __ Version in EDA ----
bureau_agg_b <- bureau_df %>%
  group_by(SK_ID_CURR) %>%
  summarise(avg_days_credit = mean(DAYS_CREDIT, na.rm = TRUE),
            avg_credit_day_overdue = mean(CREDIT_DAY_OVERDUE, na.rm = TRUE),
            avg_days_credit_enddate = mean(DAYS_CREDIT_ENDDATE, na.rm = TRUE),
            avg_amt_credit_max_overdue = mean(AMT_CREDIT_MAX_OVERDUE, na.rm = TRUE),
            avg_cnt_credit_prolong = mean(CNT_CREDIT_PROLONG, na.rm = TRUE),
            avg_amt_credit_sum = mean(AMT_CREDIT_SUM, na.rm = TRUE),
            avg_amt_credit_sum_debt = mean(AMT_CREDIT_SUM_DEBT, na.rm = TRUE),
            avg_amt_credit_sum_limit = mean(AMT_CREDIT_SUM_LIMIT, na.rm = TRUE),
            avg_amt_credit_sum_overdue = mean(AMT_CREDIT_SUM_OVERDUE, na.rm = TRUE))
=======
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
>>>>>>> eea01b62677994c0ef8b17eaac552826f30d1006
