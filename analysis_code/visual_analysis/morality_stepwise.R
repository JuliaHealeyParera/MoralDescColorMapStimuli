library(tidyverse)
library(tidymodels)
library(afex)
library(here)
library(knitr)
library(webshot)

with_diffs <- read_csv('data/with_diffs.csv')
orig <- read_csv('data/visual_data_041325.csv')

cleaning <- function(x) {
  ret <- case_when(x == "Not at all"  ~ 0,
                   x == "Slightly" ~ 1, 
                   x == "Somewhat" ~ 2,
                   x == "Very much" ~ 3, 
                   x == "Completely" ~ 4)
  return(ret)
}

morality_clean <- orig |>
  filter(!is.na(PROLIFIC_PID) & !is.na(SubjNumTest_1) & row_number() > 3) |>
  select(c(PROLIFIC_PID, starts_with('MoralDes'))) |>
  mutate(across(starts_with("MoralDes"), cleaning)) 

pivoted_moral_cln <- morality_clean |>
  pivot_longer(cols = -PROLIFIC_PID, 
               names_to = "q_name", 
               values_to = "vals") 

grouped <- pivoted_moral_cln |>
  separate_wider_delim(q_name, delim = "_", names = c("moral", "type", "num")) |>
  group_by(PROLIFIC_PID, type) |>
  summarize(avg = mean(vals)) |>
  pivot_wider(names_from = type, 
              values_from = avg)

#Distributions visualized
ggplot(pivoted_moral_cln |> select(-PROLIFIC_PID), aes(x = vals)) +
  geom_density() + 
  facet_wrap(~q_name)

morality_diffs <- with_diffs |> 
  left_join(grouped, by = join_by(PROLIFIC_PID == PROLIFIC_PID)) 

###MODEL CREATION###
#First, try with inclusion of interaction terms between intuition, color, full type
#Since we only have 3 additional predictors, try every possible combination
mor_base <- lm(diff ~ intuition*color*full_type, data = morality_diffs)
summary(mor_base)$adj.r.squared # 0.1876946 -- MAX of this section

mor_delib <- lm(diff ~ intuition*color*full_type + Delib, data = morality_diffs)
summary(mor_delib)$adj.r.squared # 0.1874436 -- second highest

mor_intuition <- lm(diff ~ intuition*color*full_type + Intuition, data = morality_diffs)
summary(mor_intuition)$adj.r.squared # 0.1874253

mor_rules <- lm(diff ~ intuition*color*full_type + Rules, data = morality_diffs)
summary(mor_rules)$adj.r.squared # 0.1873113

mor_delib_intuition <- lm(diff ~ intuition*color*full_type + Delib + Intuition, data = morality_diffs)
summary(mor_delib_intuition)$adj.r.squared # 0.1872395

mor_delib_rules <- lm(diff ~ intuition*color*full_type + Delib + Rules, data = morality_diffs)
summary(mor_delib_rules)$adj.r.squared # 0.1869823

mor_intuition_rules <- lm(diff ~ intuition*color*full_type + Intuition + Rules, data = morality_diffs)
summary(mor_intuition_rules)$adj.r.squared # 0.187128

mor_all <- lm(diff ~ intuition*color*full_type + Intuition + Rules + Delib, data = morality_diffs)
summary(mor_all)$adj.r.squared # 0.1868014


#Now, repeat without interaction terms (fully additive)
mor_base_add <- lm(diff ~ intuition + color + full_type, data = morality_diffs)
summary(mor_base_add)$adj.r.squared # 0.09549053

mor_delib_add <- lm(diff ~ intuition + color + full_type + Delib, data = morality_diffs)
summary(mor_delib_add)$adj.r.squared # 0.09554782 -- MAX of this section

mor_intuition_add <- lm(diff ~ intuition + color + full_type + Intuition, data = morality_diffs)
summary(mor_intuition_add)$adj.r.squared # 0.0951271

mor_rules_add <- lm(diff ~ intuition + color + full_type + Rules, data = morality_diffs)
summary(mor_rules_add)$adj.r.squared # 0.095136

mor_delib_intuition_add <- lm(diff ~ intuition + color + full_type + Delib + Intuition, data = morality_diffs)
summary(mor_delib_intuition_add)$adj.r.squared # 0.09528199

mor_delib_rules_add <- lm(diff ~ intuition + color + full_type + Delib + Rules, data = morality_diffs)
summary(mor_delib_rules_add)$adj.r.squared # 0.09503916

mor_intuition_rules_add <- lm(diff ~ intuition + color + full_type + Intuition + Rules, data = morality_diffs)
summary(mor_intuition_rules_add)$adj.r.squared # 0.0948746

mor_all_add <- lm(diff ~ intuition + color + full_type + Intuition + Rules + Delib, data = morality_diffs)
summary(mor_all_add)$adj.r.squared # 0.09478342
