library(tidyverse)
library(tidymodels)
library(afex)
library(ggbeeswarm)
library(here)
library(knitr)
library(kableExtra)
library(webshot)

data <- read_csv('visual_data_041325.csv')
pilot <- read_csv('pilot_data_040625.csv')

returns <- c(
  '663a6b5037a74507ad1dd2d9',
  '5ed1d52eea444913b2d4576d',
  '5d029dce2e09e800191a12ec',
  '5d029dce2e09e800191a12ec',
  '66d492edfefae8688e1d8e77',
  '63ac642d0319ac77a16ce1bc',
  '67af612069cec649704dcfae',
  '6679d30fd961448c416289c1',
  '66cfdf92dee873a6ca9158b7',
  '602d3534af703b6fe8b4033d',
  '67ba53c0e6771922e5f11dee',
  '5e56457f6370ba0184e041b4',
  '646792a49a85f35e7a7f5169',
  '67a7e8d44efbc2324e7078f8',
  '654e3fca4ff8d33df725feae',
  '6677328704eab5e142f81a4c',
  '668578d0988bd4b0ac5fc373',
  '67d34b9dd1bec3ec30caf1b5',
  '5ea060b71ce81301340a6b64',
  '5e8b89d0afcb1606e761ef7f',
  '66db806ba5f0250aa53ac293',
  '67ed199c2c4aa8a3b6666b94',
  '67d34b9dd1bec3ec30caf1b5',
  '6679d30fd961448c416289c1',
  '663a6b5037a74507ad1dd2d9',
  '5ea060b71ce81301340a6b64',
  '654e3fca4ff8d33df725feae',
  '66db806ba5f0250aa53ac293',
  '66cfdf92dee873a6ca9158b7',
  '5e8b89d0afcb1606e761ef7f')

pilot <- pilot |>
  filter(!(PROLIFIC_PID %in% returns) & row_number() > 31)

q_num = c('Q260','Q227','Q9...160',
          'Q257','Q226','Q9...139',
           'Q255','Q225','Q9...125')

pilot_question_names <- data.frame(
  q_num = q_num,
  intuition = c('table','table','table',
                'table','table','table',
                'table','table','table'),
  damage_lvl = c('8642','8642','8642',
                 '6211','6211','6211',
                 '4321','4321','4321'),
  question_type = c('allocation', 'penalty', 'praiseworthy',
                    'allocation', 'penalty', 'praiseworthy',
                    'allocation', 'penalty', 'praiseworthy'))
  
pilot <- pilot |>
  select(c(PROLIFIC_PID,all_of(q_num))) |>
  pivot_longer(cols = -PROLIFIC_PID, 
               names_to = "q_num", 
               values_to = "ans") |> 
  left_join(pilot_question_names, by = join_by(q_num == q_num))

data <- data |>
  filter(!is.na(PROLIFIC_PID) & !is.na(SubjNumTest_1) & row_number() > 3)

q_num = c('Q241','Q233','Q213',
          'Q310','Q311','Q312',
          'Q295','Q296','Q297',
          'Q377_1','Q377_4','Q377_5','Q377_6',
          'Q378_1','Q378_4','Q378_5','Q378_6',
          'Q379_1','Q379_4','Q379_5','Q379_6',
          'Q244','Q160','Q9...63',
          'Q315','Q316','Q317',
          'Q290','Q291','Q292',
          'Q374_1','Q374_4','Q374_5','Q374_6',
          'Q375_1','Q375_4','Q375_5','Q375_6',
          'Q376_1','Q376_4','Q376_5','Q376_6',
          'Q320','Q321','Q322',
          'Q245','Q206','Q9...103',
          'Q300','Q301','Q302',
          'Q371_1','Q371_4','Q371_5','Q371_6',
          'Q372_1','Q372_4','Q372_5','Q372_6', 
          'Q373_1','Q373_4','Q373_5','Q373_6',
          'Q246','Q215','Q9...129',
          'Q305','Q306','Q307',
          'Q325','Q326','Q327',
          'Q370_1','Q370_4','Q370_5','Q370_6',
          'Q380_1','Q380_4','Q380_5','Q380_6',
          'Q381_1','Q381_4','Q381_5','Q381_6')

question_names <- data.frame(
  qnum = q_num,
  intuition = c('unint','unint','unint',
                'unint','unint','unint',
                'unint','unint','unint',
                'unint','unint','unint','unint',
                'unint','unint','unint','unint',
                'unint','unint','unint','unint',
                'int','int','int',
                'int','int','int',
                'int','int','int',
                'int','int','int','int',
                'int','int','int','int',
                'int','int','int','int',
                'unint','unint','unint',
                'unint','unint','unint',
                'unint','unint','unint',
                'unint','unint','unint','unint',
                'unint','unint','unint','unint',
                'unint','unint','unint','unint',
                'int','int','int',
                'int','int','int',
                'int','int','int',
                'int','int','int','int',
                'int','int','int','int',
                'int','int','int','int'),
  color = c('rocket','rocket','rocket',
            'rocket','rocket','rocket',
            'rocket','rocket','rocket',
            'rocket','rocket','rocket','rocket',
            'rocket','rocket','rocket','rocket',
            'rocket','rocket','rocket','rocket',
            'rocket','rocket','rocket',
            'rocket','rocket','rocket',
            'rocket','rocket','rocket',
            'rocket','rocket','rocket','rocket',
            'rocket','rocket','rocket','rocket',
            'rocket','rocket','rocket','rocket',
            'blues','blues','blues',
            'blues','blues','blues',
            'blues','blues','blues',
            'blues','blues','blues','blues',
            'blues','blues','blues','blues',
            'blues','blues','blues','blues',
            'blues','blues','blues',
            'blues','blues','blues',
            'blues','blues','blues',
            'blues','blues','blues','blues',
            'blues','blues','blues','blues',
            'blues','blues','blues','blues'),
  damage_lvl = c('8642','8642','8642',
                 '6211','6211','6211',
                 '4321','4321','4321',
                 '8642','8642','8642','8642',
                 '6211','6211','6211','6211',
                 '4321','4321','4321','4321',
                 '8642','8642','8642',
                 '6211','6211','6211',
                 '4321','4321','4321',
                 '8642','8642','8642','8642',
                 '6211','6211','6211','6211',
                 '4321','4321','4321','4321',
                 '8642','8642','8642',
                 '6211','6211','6211',
                 '4321','4321','4321',
                 '8642','8642','8642','8642',
                 '6211','6211','6211','6211',
                 '4321','4321','4321','4321',
                 '8642','8642','8642',
                 '6211','6211','6211',
                 '4321','4321','4321',
                 '8642','8642','8642','8642',
                 '6211','6211','6211','6211',
                 '4321','4321','4321','4321'),
  question_type = c('allocation', 'penalty', 'praiseworthy',
                    'allocation', 'penalty', 'praiseworthy',
                    'allocation', 'penalty', 'praiseworthy',
                    'acc_prova', 'acc_provb', 'acc_provc', 'acc_provd',
                    'acc_prova', 'acc_provb', 'acc_provc', 'acc_provd',
                    'acc_prova', 'acc_provb', 'acc_provc', 'acc_provd',
                    'allocation', 'penalty', 'praiseworthy',
                    'allocation', 'penalty', 'praiseworthy',
                    'allocation', 'penalty', 'praiseworthy',
                    'acc_prova', 'acc_provb', 'acc_provc', 'acc_provd',
                    'acc_prova', 'acc_provb', 'acc_provc', 'acc_provd',
                    'acc_prova', 'acc_provb', 'acc_provc', 'acc_provd',
                    'allocation', 'penalty', 'praiseworthy',
                    'allocation', 'penalty', 'praiseworthy',
                    'allocation', 'penalty', 'praiseworthy',
                    'acc_prova', 'acc_provb', 'acc_provc', 'acc_provd',
                    'acc_prova', 'acc_provb', 'acc_provc', 'acc_provd',
                    'acc_prova', 'acc_provb', 'acc_provc', 'acc_provd',
                    'allocation', 'penalty', 'praiseworthy',
                    'allocation', 'penalty', 'praiseworthy',
                    'allocation', 'penalty', 'praiseworthy',
                    'acc_prova', 'acc_provb', 'acc_provc', 'acc_provd',
                    'acc_prova', 'acc_provb', 'acc_provc', 'acc_provd',
                    'acc_prova', 'acc_provb', 'acc_provc', 'acc_provd'))

demographic_morality <- data |>
  select(Q234:PROLIFIC_PID)

pivoted <- data |> 
  filter(PROLIFIC_PID != "5e9409cd1dabe7246810c3b6") |>
  select(c(all_of(q_num), PROLIFIC_PID)) |>
  pivot_longer(cols = -PROLIFIC_PID, 
               names_to = "question_num", 
               values_to = "ans") |>
  filter(!is.na(ans)) |>
  left_join(question_names, by = join_by(question_num == qnum))

### ACCURACY ANALYSIS

accuracies <- pivoted |>
  select(-PROLIFIC_PID) |>
  filter(grepl("acc_.*", question_type, perl = TRUE)) |>
  mutate(
    province = str_extract(question_type, "(?<=acc_prov)[a-d]"), 
    base_lvl = case_when(province == 'a' ~ substr(damage_lvl, 1, 1),
                         province == 'b' ~ substr(damage_lvl, 2, 2),
                         province == 'c' ~ substr(damage_lvl, 3, 3),
                         province == 'd' ~ substr(damage_lvl, 4, 4)), 
    ans = str_replace(ans, ',', '.'),
    diff = as.numeric(ans) - as.numeric(base_lvl),
    int_col = paste0(intuition, "_", color))

damage_grps_accuracy <- accuracies |>
  filter(province == "d") |>
  group_by(intuition, color, damage_lvl) |>
  summarize(mean = mean(diff), 
            median = median(diff), 
            var = var(diff))

accuracies_summary <- accuracies |>
  group_by(intuition, color) |>
  summarize(mean = mean(diff), 
            median = median(diff), 
            var = var(diff))

accuracies_plot <- accuracies |>
  mutate(int_col = case_when(int_col == "int_blues" ~ "IntBlue",
                             int_col == "int_rocket" ~ "IntRoc",
                             int_col == "unint_blues" ~ "UnintBlue",
                             int_col == "unint_rocket" ~ "UnintRoc",
                             TRUE ~ int_col),
         damage_lvl = paste0("Damage Level: ", damage_lvl),
         int_col = fct_relevel(int_col, c('IntBlue', 'UnintBlue', 'IntRoc', 'UnintRoc')))

int_blues <- accuracies_plot |> filter(int_col == "IntBlue")

accuracy_boxplots <- ggplot(accuracies_plot, aes(x = int_col, y = diff)) +
  geom_boxplot(fill = "gray75") +
  geom_boxplot(data = int_blues, fill = "white", color = "blue2") +
  facet_wrap(~damage_lvl) + 
  labs(title = "Intuitive Brewer Blues color mapping consistently results in the \nleast accurate interpretations.",
       x = "Intuition Level and Color Map",
       y = "Difference Between Observed and Actual Value") +
  coord_cartesian(ylim = c(-7.5, 7.5)) + 
  theme_bw()

largest_val <- accuracies_plot |> filter(province == "a")
int_blues_large <- largest_val |> filter(int_col == "IntBlue")

largest_val_boxplot <- ggplot(largest_val, aes(x = int_col, y = diff)) +
  geom_boxplot(fill = "gray75") +
  facet_wrap(~damage_lvl) +
  geom_boxplot(data = int_blues_large, fill = "white", color = "blue2") +
  geom_hline(yintercept = 0, color = "red4", linewidth = 0.45) +
  labs(title = "Participants interpreted larger values less accurately.",
       subtitle = "Once again, Intuitive Blue was the least accurate mapping.",
       x = "Intuition Level and Color Map",
       y = "Difference Between Observed and Actual Value") + 
  coord_cartesian(ylim = c(-7.5, 7.5)) + 
  theme_bw()

smallest_val <- accuracies_plot |> filter(province == "d")
unint_blue_8642 <- smallest_val |> filter(int_col == "UnintBlue" & damage_lvl == "Damage Level: 8642")

smallest_val_boxplot <- ggplot(smallest_val, aes(x = int_col, y = diff)) +
  geom_boxplot(fill = "gray75") +
  facet_wrap(~damage_lvl) + 
  geom_boxplot(data = unint_blue_8642, fill = "white", color = "blue2") +
  geom_hline(yintercept = 0, color = "red4", linewidth = 0.45) +
  labs(title = "Participants interpreted lower values more accurately.",
       subtitle = "In this case, Unintuitive Blue in the 8642 scenario was the least accurate mapping.",
       x = "Intuition Level and Color Map",
       y = "Difference Between Observed and Actual Value") + 
  coord_cartesian(ylim = c(-7.5, 7.5)) + 
  theme_bw()

label_path <- here("result_visualizations", "accuracy_boxplots.png") 
ggsave(label_path, plot = accuracy_boxplots, width = 6.5, height = 4, dpi = 300)

label_path <- here("result_visualizations", "accuracy_boxplots_largeval.png") 
ggsave(label_path, plot = largest_val_boxplot, width = 6.5, height = 4, dpi = 300)

label_path <- here("result_visualizations", "accuracy_boxplots_smallval.png") 
ggsave(label_path, plot = smallest_val_boxplot, width = 6.5, height = 4, dpi = 300)

### ANOVA EXPLORATION

with_diffs <- pivoted |>
  filter(!grepl("acc_.*", question_type, perl = TRUE)) |>
  mutate(
    base_line = case_when(
      question_type %in% c('penalty', 'praiseworthy') ~ 0, 
      damage_lvl == "8642" ~ 40, 
      damage_lvl == "6211" ~ 60, 
      damage_lvl == "4321" ~ 40
    ), 
    ans = case_when( 
      ans == "Very Blameworthy" ~ '-3',
      ans == "Moderately Blameworthy" ~ '-2',
      ans == "A Little Blameworthy" ~ '-1',
      ans %in% c("Neither", "no penalty") ~ '0',
      ans %in% c("A Little Praiseworthy", "1% penalty") ~ '1',
      ans %in% c("Moderately Praiseworthy", "2% penalty") ~ '2',
      ans %in% c("Very Praiseworthy", "3% penalty") ~ '3',
      ans == "4% penalty" ~ '4', 
      ans == "5% penalty" ~ '5',
      TRUE ~ ans
      ),
    ans = as.numeric(str_replace(ans, ',', '.')),
    diff = as.numeric(ans) - as.numeric(base_line), 
    full_type = paste0(question_type, "_", damage_lvl))


### BETWEEN SUBJECTS REPEATED MEASURES ANOVA -- no table data
prepped <- with_diffs |>
  select(PROLIFIC_PID, ans, full_type, intuition, color, question_type) |> 
  mutate(intcol = paste0(intuition, "_", color))

alloc <- prepped |> filter(question_type == "allocation") 
ggplot(alloc, aes(x = ans)) +
  geom_density(fill = "red4", alpha = .5) +
  facet_grid(full_type~intcol, scales = "free")
penal <- prepped |> filter(question_type == "penalty")
ggplot(penal, aes(y = ans)) +
  geom_boxplot() +
  facet_grid(full_type~intcol, scales = "free")
praise <- prepped |> filter(question_type == "praiseworthy") 
ggplot(praise, aes(y = ans)) +
  geom_boxplot() +
  facet_grid(full_type~intcol, scales = "free")

#allocation
alloc <- prepped |>
  filter(grepl('allocation.*', full_type, perl = TRUE))
alloc_repmeas <- aov_ez(
  id = "PROLIFIC_PID",
  dv = "ans",
  data = alloc,
  within = "full_type",
  between = c("intuition", "color")
)
summary(alloc_repmeas)

#penalty
penalty <- prepped |>
  filter(grepl('penalty.*', full_type, perl = TRUE))
penalty_repmeas <- aov_ez(
  id = "PROLIFIC_PID",
  dv = "ans",
  data = penalty,
  within = "full_type",
  between = c("intuition", "color")
)
summary(penalty_repmeas)

#praiseworthy
praise <- prepped |>
  filter(grepl('praiseworthy*', full_type, perl = TRUE))
praise_repmeas <- aov_ez(
  id = "PROLIFIC_PID",
  dv = "ans",
  data = praise,
  within = "full_type",
  between = c("intuition", "color")
)
summary(praise_repmeas)


##FINISH LATER IF NECESSARY? For comparison with table data
sig_lvls <- data.frame(
  question_type = c("Allocation", "Penalty", "Praiseworthiness") 
)

sig_lvls$pvals <- list(
  alloc_repmeas$anova_table[["Pr(>F)"]],
  penalty_repmeas$anova_table[["Pr(>F)"]],
  praise_repmeas$anova_table[["Pr(>F)"]])

sig_lvls <- sig_lvls |> 
  unnest_wider(pvals, names_sep = "_") |>
  mutate(across(starts_with("pvals_"), ~ {
    raw_num <- as.numeric(.x)
    formatted <- ifelse(
      raw_num < 0.0001,
      formatC(raw_num, format = "e", digits = 2),  # scientific notation
      formatC(raw_num, format = "f", digits = 4)   # fixed-point
    )
    paste0(formatted, "*")
  })) |>
  rename("Intuition and Color Map" = pvals_1, 
         "Question type and Damage Level" = pvals_2,
         "Interaction Term" = pvals_3, 
         "Question Type" = question_type) 

kbl <- sig_lvls |>
  kable(
    format = "html",  
    caption = "Significance Levels (P-Values) for Main Effects and Interaction Terms",
    col.names = c("Question Type", 
                  "Intuition and Color Map", 
                  "Question Type and Damage Level", 
                  "Interaction Term"),
    align = "lccc",
    escape = FALSE  
  ) |>
  kable_styling(
    full_width = FALSE,
    position = "center",
    font_size = 13,
    bootstrap_options = c("condensed"),
    stripe_color = "#f5f5f5",  # very light gray
    html_font = "Helvetica, Arial, sans-serif"
  ) |>
  row_spec(0, bold = TRUE, color = "black", background = "white") |>  # header
  column_spec(1:4, border_left = TRUE, border_right = TRUE)


### BETWEEN SUBJECTS REPEATED MEASURES ANOVA -- table data included
pilot <- pilot |>
  mutate(full_type = paste0(question_type, "_", damage_lvl),
         ans = as.numeric(case_when( 
           ans == "Very Blameworthy" ~ '-3',
           ans == "Moderately Blameworthy" ~ '-2',
           ans == "A Little Blameworthy" ~ '-1',
           ans %in% c("Neither", "no penalty") ~ '0',
           ans %in% c("A Little Praiseworthy", "1% penalty") ~ '1',
           ans %in% c("Moderately Praiseworthy", "2% penalty") ~ '2',
           ans %in% c("Very Praiseworthy", "3% penalty") ~ '3',
           ans == "4% penalty" ~ '4', 
           ans == "5% penalty" ~ '5',
           TRUE ~ ans
         ))) |>
  rename("intcol" = intuition) |>
  select(-c(q_num, damage_lvl)) |>
  filter(!is.na(PROLIFIC_PID) & PROLIFIC_PID != "64012ee6e3f2e59aaa36f718")

temp_prep <- prepped |>
  select(-c(intuition, color))

maps_tables <- rbind(temp_prep, pilot)

#allocation
alloc_tab <- maps_tables |>
  filter(grepl('allocation.*', full_type, perl = TRUE))
alloc_tab_repmeas <- aov_ez(
  id = "PROLIFIC_PID",
  dv = "ans",
  data = alloc_tab,
  within = "full_type",
  between = "intcol"
)
summary(alloc_tab_repmeas)

#penalty
penalty_tab <- maps_tables |>
  filter(grepl('penalty.*', full_type, perl = TRUE))
penalty_tab_repmeas <- aov_ez(
  id = "PROLIFIC_PID",
  dv = "ans",
  data = penalty_tab,
  within = "full_type",
  between = "intcol"
)
summary(penalty_tab_repmeas)

#praiseworthy
praise_tab <- maps_tables |>
  filter(grepl('praiseworthy*', full_type, perl = TRUE))
praise_tab_repmeas <- aov_ez(
  id = "PROLIFIC_PID",
  dv = "ans",
  data = praise_tab,
  within = "full_type",
  between = "intcol"
)
summary(praise_tab_repmeas)

tab_sig_lvls <- data.frame(
  question_type = c("Allocation", "Penalty", "Praiseworthiness") 
)

tab_sig_lvls$pvals <- list(
  alloc_tab_repmeas$anova_table[["Pr(>F)"]],
  penalty_tab_repmeas$anova_table[["Pr(>F)"]],
  praise_tab_repmeas$anova_table[["Pr(>F)"]])

tab_sig_lvls <- tab_sig_lvls |> 
  unnest_wider(pvals, names_sep = "_") |>
  mutate(across(starts_with("pvals_"), ~ {
    raw_num <- as.numeric(.x)
    formatted <- ifelse(
      raw_num < 0.0001,
      formatC(raw_num, format = "e", digits = 2),  # scientific notation
      formatC(raw_num, format = "f", digits = 4)   # fixed-point
    )
    paste0(formatted, "*")
  })) |>
  rename("Intuition and Color Map" = pvals_1, 
         "Question type and Damage Level" = pvals_2,
         "Interaction Term" = pvals_3, 
         "Question Type" = question_type) 

kbl <- tab_sig_lvls |>
  kable(
    format = "html",  
    caption = "Significance Levels (P-Values) for Main Effects and Interaction Terms",
    col.names = c("Question Type", 
                  "Intuition and Color Map", 
                  "Question Type and Damage Level", 
                  "Interaction Term"),
    align = "lccc",
    escape = FALSE  
  ) |>
  kable_styling(
    full_width = FALSE,
    position = "center",
    font_size = 13,
    bootstrap_options = c("condensed"),
    stripe_color = "#f5f5f5",  # very light gray
    html_font = "Helvetica, Arial, sans-serif"
  ) |>
  row_spec(0, bold = TRUE, color = "black", background = "white") |>  # header
  column_spec(1:4, border_left = TRUE, border_right = TRUE)

### BETWEEN SUBJECTS ANOVAS, NO REPEATED MEASURES

#8642 allocation
alloc_8642 <- prepped |>
  filter(full_type == "allocation_8642")
alloc_8642_lm <- lm(
  ans ~ intuition * color,
  data = alloc_8642
)
alloc_8642_anova <- anova(alloc_8642_lm)

#6211 allocation
alloc_6211 <- prepped |>
  filter(full_type == "allocation_6211")
alloc_6211_lm <- lm(
  ans ~ intuition * color,
  data = alloc_6211
)
alloc_6211_anova <- anova(alloc_6211_lm)

#4321 allocation 
alloc_4321 <- prepped |>
  filter(full_type == "allocation_4321")
alloc_4321_lm <- lm(
  ans ~ intuition * color,
  data = alloc_4321
)
alloc_4321_anova <- anova(alloc_4321_lm)

#8642 penalty 
penalty_8642 <- prepped |>
  filter(full_type == "penalty_8642")
penalty_8642_lm <- lm(
  ans ~ intuition * color,
  data = penalty_8642
)
penalty_8642_anova <- anova(penalty_8642_lm)

#6211 penalty
penalty_6211 <- prepped |>
  filter(full_type == "penalty_6211")
penalty_6211_lm <- lm(
  ans ~ intuition * color,
  data = penalty_6211
)
penalty_6211_anova <- anova(penalty_6211_lm)

#4321 penalty 
penalty_4321 <- prepped |>
  filter(full_type == "penalty_4321")
penalty_4321_lm <- lm(
  ans ~ intuition * color,
  data = penalty_4321
)
penalty_4321_anova <- anova(penalty_4321_lm)

#8642 praiseworthy 
praise_8642 <- prepped |>
  filter(full_type == "praiseworthy_8642")
praise_8642_lm <- lm(
  ans ~ intuition * color,
  data = praise_8642
)
praise_8642_anova <- anova(praise_8642_lm)

#6211 praiseworthy
praise_6211 <- prepped |>
  filter(full_type == "praiseworthy_6211")
praise_6211_lm <- lm(
  ans ~ intuition * color,
  data = praise_6211
)
praise_6211_anova <- anova(praise_6211_lm)

#4321 praiseworthy 
praise_4321 <- prepped |>
  filter(full_type == "praiseworthy_4321")
praise_4321_lm <- lm(
  ans ~ intuition * color,
  data = praise_4321
)
praise_4321_anova <- anova(praise_4321_lm)

between_subj_pvals <- data.frame(model = c("alloc_8642_anova",
                                             "alloc_6211_anova",
                                             "alloc_4321_anova",
                                             "penalty_8642_anova",
                                             "penalty_6211_anova",
                                             "penalty_4321_anova",
                                             "praise_8642_anova",
                                             "praise_6211_anova",
                                             "praise_4321_anova"))
between_subj_pvals$pvals = list(alloc_8642_anova[["Pr(>F)"]][1:3],
                                alloc_6211_anova[["Pr(>F)"]][1:3],
                                alloc_4321_anova[["Pr(>F)"]][1:3],
                                penalty_8642_anova[["Pr(>F)"]][1:3],
                                penalty_6211_anova[["Pr(>F)"]][1:3],
                                penalty_4321_anova[["Pr(>F)"]][1:3],
                                praise_8642_anova[["Pr(>F)"]][1:3],
                                praise_6211_anova[["Pr(>F)"]][1:3],
                                praise_4321_anova[["Pr(>F)"]][1:3])

between_subj_pvals <- between_subj_pvals |>
  unnest_wider(pvals, 
               names_sep = "_") |>
  rename("intuition" = pvals_1,
         "color" = pvals_2,
         "intuition_color" = pvals_3)

### DEMOGRAPHICS AND MORAL DECISION MAKING 

cleaning <- function(x) {
  ret <- case_when(x == "Not at all"  ~ 0,
                   x == "Slightly" ~ 1, 
                   x == "Somewhat" ~ 2,
                   x == "Very much" ~ 3, 
                   x == "Completely" ~ 4)
  return(ret)
}

morality_clean <- demographic_morality |>
  select(c(PROLIFIC_PID, starts_with('MoralDes'))) |>
  mutate(across(starts_with("MoralDes"), cleaning)) 

pivoted_moral_cln <- morality_clean |>
  pivot_longer(cols = -PROLIFIC_PID, 
               names_to = "q_name", 
               values_to = "vals")

#Distributions visualized
ggplot(pivoted_moral_cln |> select(-PROLIFIC_PID), aes(x = vals)) +
  geom_density() + 
  facet_wrap(~q_name)

morality_diffs <- with_diffs |> 
  left_join(demographic_morality, by = join_by(PROLIFIC_PID == PROLIFIC_PID)) 

morality_model <- lm(diff ~ intuition*color*full_type + MoralDes_Intuition_2 + MoralDes_Rules_2 + MoralDes_Delib_2, data = morality_diffs)
summary(morality_model)

#Not helpful: MoralDes_Intuition_3, MoralDes_Delib_3, MoralDes_Rules_3, MoralDes_Intuition_1, MoralDes_Rules_1, MoralDes_Delib_1

dem_clean <- demographic_morality |>
  select(Q19:Q35)








#Preparatory stepwise code for demographic info
stepwise_mod1 <- linear_reg() |> fit(diff ~ intuition, with_diffs)
summary(stepwise_mod1$fit)$adj.r.squared

stepwise_mod2 <- linear_reg() |> fit(diff ~ intuition + color, with_diffs)
summary(stepwise_mod2$fit)$adj.r.squared

stepwise_mod3 <- linear_reg() |> fit(diff ~ intuition + color + damage_lvl, with_diffs)
summary(stepwise_mod3$fit)$adj.r.squared




  
  




Q246 - blues intuitive 4321 
Q215 
Q9...129

Q305 - blues intuitive 6211 
Q306
Q307

Q325 - blues intuitive 8642
Q326
Q327

Q370_1, Q370_4, Q370_5, Q370_6 - blues intuitive 8642 accuracy
Q380_1, Q380_4, Q380_5, Q380_6 - blues intuitive 6211 accuracy
Q381_1, Q381_4, Q381_5, Q381_6 - blues intuitive 4321 accuracy




Q241 - rocket unintutive 4321 
Q233 
Q213

Q310 - rocket unintuitive 6211 
Q311
Q312

Q295 - rocket unintuitive 8642
Q296
Q297

Q377_1, Q377_4, Q377_5, Q377_6 - rocket unintuitive 8642 accuracy
Q378_1, Q378_4, Q378_5, Q378_6 - rocket unintuitive 6211 accuracy
Q379_1, Q379_4, Q379_5, Q379_6 - rocket unintuitive 4321 accuracy

Q244 - rocket intuitive 4321 
Q160 
Q9...63

Q315 - rocket intuitive 6211 
Q316
Q317

Q290 - rocket intuitive 8642
Q291
Q292

Q374_1, Q374_4, Q374_5, Q374_6 - rocket intuitive 8642 accuracy
Q375_1, Q375_4, Q375_5, Q375_6 - rocket intuitive 6211 accuracy
Q376_1, Q376_4, Q376_5, Q376_6 - rocket intuitive 4321 accuracy

Q320 - blues unintutive 4321 
Q321 
Q322

Q245 - blues unintuitive 6211 
Q206
Q9...103

Q300 - blues unintuitive 8642
Q301
Q302

Q371_1, Q371_4, Q371_5, Q371_6 - blues unintuitive 8642 accuracy
Q372_1, Q372_4, Q372_5, Q372_6 - blues unintuitive 6211 accuracy
Q373_1, Q373_4, Q373_5, Q373_6 - blues unintuitive 4321 accuracy
