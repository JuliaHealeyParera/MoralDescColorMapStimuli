library(tidyverse)
library(tidymodels)
library(afex)
library(here)
library(knitr)
library(webshot)
library(GGally)

with_diffs <- read_csv(here('data', 'with_diffs.csv'))
orig <- read_csv(here('data', 'visual_data_041325.csv'))

#WARNING: Super simplified way of doing this
#Should really be using Spearman's rho to calculate correlation between ordinal var
#For the sake of rough analysis, simply converted ordinal var to continuous integers
#Invalid approach because requires assumption that effect on predictor when increasing one unit is uniform across all levels
#Very invalid assumption for all encoded variables

# Create a named vector: names = original text, values = numeric codes
lookup <- c(
  "Extremely Liberal" = "-3",
  "Somewhat Liberal" = "-2",
  "Slightly Liberal" = "-1",
  
  "Not sure/prefer not to answer" = "0",
  "No schooling completed" = "0",
  "Not at all religious" = "0",
  "Moderate" = "0",
  "Not at all good" = "0",
  "Never" = "0",
  
  "$0 - $20,000" = "1",
  "Nursery school to 8th grade" = "1",
  "Slightly religious" = "1",
  "Slightly Conservative" = "1",
  "Slightly good" = "1",
  "Rarely" = "1",
  
  "$20,001 - $40,000" = "2",
  "Some high school, no diploma" = "2",
  "Somewhat religious" = "2",
  "Somewhat Conservative" = "2",
  "Somewhat good" = "2",
  "Sometimes" = "2",
  
  "$40,001 - $60,000" = "3",
  "High school graduate (high school diploma, or a legally-accepted equivalent)" = "3",
  "Very religious" = "3",
  "Extremely Conservative" = "3",
  "Very good" = "3",
  "Very often" = "3",
  
  "$60,001 - $80,000" = "4",
  "Some college, but no college degree" = "4",
  "Extremely religious" = "4",
  "Extremely good" = "4",
  "Always" = "4",
  
  "80,001 - $100,000" = "5",
  "Associate degree" = "5",
  
  "$100,001 - $150,000" = "6",
  "Bachelor's degree" = "6",
  
  "$150,001 - $250,000" = "7",
  "Master's degree" = "7",
  
  "$250,001+" = "8",
  "Professional degree (examples: MD, DDS, DVM, LLB, JD)" = "8",
  
  "Doctorate degree (examples: PhD, EdD)" = "9"
)

cleaning <- function(x) {
  out <- lookup[x]  
  out[is.na(out)] <- x
  return(out)
}

dem_clean <- orig |>
  filter(!is.na(PROLIFIC_PID) & !is.na(SubjNumTest_1) & row_number() > 3) |>
  select(-c(StartDate:Q237)) |>
  select(-c(PageCount:condition)) |>
  select(-c(starts_with('MoralDes'))) 


names(dem_clean) <- c("subjnumtest_1", "subjnumtest_2", "subjnumtest_3",
                      "gender", "gender_other", "hand", "ethnicity", "age",
                      "social_orien", "fiscal_orien", 
                      "relig_type", "relig_other", "relig_strength",
                      "educ_deg", "emp_status", "income", "PROLIFIC_PID")
numer <- c('subjnumtest_1', 
           'subjnumtest_2', 
           'subjnumtest_3',
           'age',
           'social_orien',
           'fiscal_orien',
           'relig_strength', 
           'educ_deg', 
           'income')

dem_corr <- dem_clean |>
  mutate(across(everything(), cleaning), 
         across(numer, as.numeric)) |>
  select(all_of(numer))

ggpairs(
  dem_corr,
  upper = list(continuous = "cor"),    # Pearson correlation in upper triangle
  lower = list(continuous = "points"), # Scatterplots in lower triangle
  diag = list(continuous = "barDiag")  # Histograms on the diagonal
)

cor(dem_inv$subjnumtest_1, dem_inv$subjnumtest_2) # 0.63, high enough to average together
income_order <- c("Not sure/prefer not to answer",
                  "$0 - $20,000", 
                  "$20,001 - $40,000",
                  "$40,001 - $60,000", 
                  "$60,001 - $80,000", 
                  "$80,001 - $100,000",
                  "$100,001 - $150,000",
                  "$150,001 - $250,000",
                  "$250,001+")
orien_order <- c("Moderate",
                 "Extremely Liberal",
                 "Somewhat Liberal",
                 "Slightly Liberal",
                 "Slightly Conservative",
                 "Somewhat Conservative",
                 "Extremely Conservative")
deg_order <- c(
                 "Some high school, no diploma",
                 "High school graduate (high school diploma, or a legally-accepted equivalent)",
                 "Some college, but no college degree",
                 "Associate degree",
                 "Bachelor's degree",
                 "Master's degree",
                 "Professional degree (examples: MD, DDS, DVM, LLB, JD)",
                 "Doctorate degree (examples: PhD, EdD)")

dem_inv <- dem_clean |> 
  filter(!(age == "test")) |>
  mutate(across(starts_with('subjnumtest'), cleaning),
         across(starts_with('subjnumtest'), as.numeric),
         numtestavg = (subjnumtest_1 + subjnumtest_2)/2, 
         income = fct_relevel(income, income_order),
         social_orien = fct_relevel(social_orien, orien_order),
         fiscal_orien = fct_relevel(fiscal_orien, orien_order),
         educ_deg = fct_relevel(educ_deg, deg_order),
         age = as.numeric(age)) |>
  right_join(with_diffs, by = join_by(PROLIFIC_PID == PROLIFIC_PID))
 
#Modeling with interaction term, conceptual stepwise
#Conceptually, would expect: 
  #Number Test
  #Fiscal orientation
  #Social orientation
  #Educational degree
  #Income
  #Religious strength?

mor_base <- lm(diff ~ intuition*color*full_type, data = dem_inv)
summary(mor_base)$adj.r.squared # 0.1876946

mor_num_test <- lm(diff ~ intuition*color*full_type + numtestavg, data = dem_inv)
summary(mor_num_test)$adj.r.squared # 0.1872317 -- decrease, move onto next step

mor_fiscal <- lm(diff ~ intuition*color*full_type + fiscal_orien, data = dem_inv)
summary(mor_fiscal)$adj.r.squared # 0.1916018

mor_f_social <- lm(diff ~ intuition*color*full_type + fiscal_orien + social_orien, data = dem_inv)
summary(mor_f_social)$adj.r.squared # 0.1923415

mor_fs_educ <- lm(diff ~ intuition*color*full_type + fiscal_orien + social_orien + educ_deg, data = dem_inv)
summary(mor_fs_educ)$adj.r.squared # 0.199067

mor_fse_income <- lm(diff ~ intuition*color*full_type + fiscal_orien + social_orien + educ_deg + income, data = dem_inv)
summary(mor_fse_income)$adj.r.squared # 0.2006867

mor_fsei_relstr <- lm(diff ~ intuition*color*full_type + fiscal_orien + social_orien + educ_deg + income + relig_strength, data = dem_inv)
summary(mor_fsei_relstr)$adj.r.squared # 0.2014023

mor_fsei_empl <- lm(diff ~ intuition*color*full_type + fiscal_orien + social_orien + educ_deg + income + relig_strength + emp_status, data = dem_inv)
summary(mor_fsei_empl)$adj.r.squared # 0.2149156

mor_fseie_age <- lm(diff ~ intuition*color*full_type + fiscal_orien + social_orien + educ_deg + income + relig_strength + emp_status + age, data = dem_inv)
summary(mor_fseie_age)$adj.r.squared # 0.2218145

anova(mor_fseie_age)
# significant: fiscal_orien, educ_deg, emp_status, age 

##Additive ver
mor_base_ad <- lm(diff ~ intuition+color+full_type, data = dem_inv)
summary(mor_base_ad)$adj.r.squared # 0.09549053

mor_num_test_ad <- lm(diff ~ intuition+color+full_type + numtestavg, data = dem_inv)
summary(mor_num_test_ad)$adj.r.squared # 0.09461083 -- decrease, move onto next step

mor_fiscal_ad <- lm(diff ~ intuition+color+full_type + fiscal_orien, data = dem_inv)
summary(mor_fiscal_ad)$adj.r.squared # 0.09932094 -- decrease 

mor_social_ad <- lm(diff ~ intuition+color+full_type + social_orien, data = dem_inv)
summary(mor_f_social_ad)$adj.r.squared # 0.09682194 -- decrease

mor_educ_ad <- lm(diff ~ intuition+color+full_type + educ_deg, data = dem_inv)
summary(mor_fs_educ_ad)$adj.r.squared # 0.103224 

mor_e_income_ad <- lm(diff ~ intuition+color+full_type + educ_deg + income, data = dem_inv)
summary(mor_fse_income_ad)$adj.r.squared # 0.1037659

mor_ei_relstr_ad <- lm(diff ~ intuition+color+full_type + educ_deg + income + relig_strength, data = dem_inv)
summary(mor_fsei_relstr_ad)$adj.r.squared # 0.1044584

mor_ei_empl_ad <- lm(diff ~ intuition+color+full_type + educ_deg + income + relig_strength + emp_status, data = dem_inv)
summary(mor_fsei_empl_ad)$adj.r.squared # 0.1164208

mor_eie_age_ad <- lm(diff ~ intuition+color+full_type + educ_deg + income + relig_strength + emp_status + age, data = dem_inv)
summary(mor_fseie_age_ad)$adj.r.squared # 0.1215736

anova(mor_eie_age_ad)
# significant: educ_deg, emp_status, age
