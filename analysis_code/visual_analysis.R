library(tidyverse)
library(tidymodels)
library(afex)
library(here)
library(knitr)
library(webshot)

here()
orig <- read_csv('data/visual_data_041325.csv')
pivoted <- read_csv('data/cleaned_visual.csv')
with_diffs <- read_csv('data/with_diffs.csv')

###ANOVA EXPLORATION###

### BETWEEN SUBJECTS REPEATED MEASURES ANOVA -- no table data
prepped <- with_diffs |>
  select(PROLIFIC_PID, ans, diff, damage_lvl, full_type, intuition, color, question_type) |> 
  mutate(intcol = paste0(intuition, "_", color))

#dist plots
alloc <- prepped |> filter(question_type == "allocation") 
ggplot(alloc, aes(x = ans)) +
  geom_density(fill = "red4", alpha = .5) +
  facet_grid(full_type~intcol, scales = "free")
penalty <- prepped |> filter(question_type == "penalty")
ggplot(penalty, aes(y = ans)) +
  geom_boxplot() +
  facet_grid(full_type~intcol, scales = "free")
praise <- prepped |> filter(question_type == "praiseworthy") 
ggplot(praise, aes(y = ans)) +
  geom_boxplot() +
  facet_grid(full_type~intcol, scales = "free")

#allocation
alloc_repmeas <- aov_ez(
  id = "PROLIFIC_PID",
  dv = "diff",
  data = alloc,
  within = "damage_lvl",
  between = c("intuition", "color")
)
summary(alloc_repmeas)

#penalty
penalty_repmeas <- aov_ez(
  id = "PROLIFIC_PID",
  dv = "diff",
  data = penalty,
  within = "damage_lvl",
  between = c("intuition", "color")
)
summary(penalty_repmeas)

#praiseworthy
praise_repmeas <- aov_ez(
  id = "PROLIFIC_PID",
  dv = "diff",
  data = praise,
  within = "damage_lvl",
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

maps_tables <- maps_tables |> 
  separate(full_type, into = c("type_part", "damage_lvls"), sep = "_", remove = FALSE) |>
  separate(intcol, into = c("intuition", "color"), sep = "_", remove = FALSE) |>
  mutate(intuition = case_when(intcol == "table" ~ "Table", 
                               grepl('^unint.*', intcol, perl = TRUE) ~ "Unintuitive",
                               grepl('^int.*', intcol, perl = TRUE) ~ "Intuitive"),
         color = ifelse(is.na(color), "Table", color),
         base_line = case_when(
             question_type %in% c('penalty', 'praiseworthy') ~ 0, 
             grepl('.*_8642', full_type, perl = TRUE) ~ 40, 
             grepl('.*_6211', full_type, perl = TRUE) ~ 60, 
             grepl('.*_4321', full_type, perl = TRUE) ~ 40
           ),
         diff = ans - base_line,
         damage_lvls = paste0('Damage Levels: ', damage_lvls), 
         intuition = fct_relevel(intuition, c("Table", "Intuitive","Unintuitive")),
         intcol = fct_relevel(intcol, c("table", "int_blues", "int_rocket", "unint_blues", "unint_rocket")))

#allocation
alloc_tab <- maps_tables |>
  filter(grepl('allocation.*', full_type, perl = TRUE))  |>
  filter(ans < 300) |> 
  mutate(color_group = case_when(
    intcol == "table" ~ "white",
    intcol %in% c("int_blues", "unint_blues") ~ "brewer",
    intcol %in% c("int_rocket", "unint_rocket") ~ "rocket"
  ))

# Plot
allocation_spread <- ggplot(alloc_tab, aes(x = intcol, y = diff, fill = color_group)) +
  geom_boxplot(color = "black", alpha = .4) +
  scale_fill_manual(values = c("white" = "white", "brewer" = "blue", "rocket" = "red")) +  # Map color_group to colors
  facet_wrap(~damage_lvls) +
  geom_hline(yintercept = 0, color = "red4", linewidth = 0.45) +
  scale_x_discrete(
    labels = c(
      "table" = "Table",
      "int_blues" = "",
      "int_rocket" = "Intuitive",
      "unint_blues" = "",
      "unint_rocket" = "Unintuitive"
    )
  ) +
  theme_bw() + 
  labs(x = "Intuition Level",
       y = "Difference Between Observed and Proportional Value \nof Resources Allocated to Province A",
       title = "Unintuitive blues differ from the expected proportional value \nmost sporadically.",
       fill = "Color Map")
  theme(
    panel.spacing = unit(.4, "cm"), # Increase spacing between facets
    strip.text = element_text(size = 12),  # Optional: Change facet label size
    strip.background = element_rect(fill = "lightgray")  # Optional: Add background color to facet labels
  )
  
label_path <- here("result_visualizations", "allocation_spread.png") 
ggsave(label_path, plot = allocation_spread, width = 8, height = 5, dpi = 300)

#model
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
  filter(grepl('penalty.*', full_type, perl = TRUE)) |>
  filter(ans < 300) |> 
  mutate(color_group = case_when(
    intcol == "table" ~ "white",
    intcol %in% c("int_blues", "unint_blues") ~ "brewer",
    intcol %in% c("int_rocket", "unint_rocket") ~ "rocket"
  ))

#plot
penalty_spread <- ggplot(penalty_tab, aes(x = intcol, y = diff, fill = color_group)) +
  geom_boxplot(color = "black", alpha = .4) +
  scale_fill_manual(values = c("white" = "white", "brewer" = "blue", "rocket" = "red")) +  # Map color_group to colors
  facet_wrap(~damage_lvls) +
  scale_x_discrete(
    labels = c(
      "table" = "Table",
      "int_blues" = "",
      "int_rocket" = "Intuitive",
      "unint_blues" = "",
      "unint_rocket" = "Unintuitive"
    )
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +  # Format y-axis with % sign
  theme_bw() + 
  labs(x = "Intuition Level",
       y = "Punishment Allocated to Province A",
       title = "In the 4321 damage scenario, intuitive and unintuitive blues \nresulted in opposite punishment allocations.",
       fill = "Color Map") +
  theme(
    panel.spacing = unit(.4, "cm"), # Increase spacing between facets
    strip.text = element_text(size = 12),  # Optional: Change facet label size
    strip.background = element_rect(fill = "lightgray")  # Optional: Add background color to facet labels
  )

label_path <- here("result_visualizations", "penalty_spread.png") 
ggsave(label_path, plot = penalty_spread, width = 8, height = 5, dpi = 300)

#model
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
  filter(grepl('praiseworthy*', full_type, perl = TRUE)) |>
  filter(ans < 300) |> 
  mutate(color_group = case_when(
    intcol == "table" ~ "white",
    intcol %in% c("int_blues", "unint_blues") ~ "brewer",
    intcol %in% c("int_rocket", "unint_rocket") ~ "rocket"
  ))

# Plot
praise_spread <- ggplot(praise_tab, aes(x = intcol, y = diff, fill = color_group)) +
  geom_boxplot(color = "black", alpha = .4) +
  scale_fill_manual(values = c("white" = "white", "brewer" = "blue", "rocket" = "red")) +  # Map color_group to colors
  facet_wrap(~damage_lvls) +
  geom_hline(yintercept = 0, color = "red4", linewidth = 0.45) +
  scale_x_discrete(
    labels = c(
      "table" = "Table",
      "int_blues" = "",
      "int_rocket" = "Intuitive",
      "unint_blues" = "",
      "unint_rocket" = "Unintuitive"
    )
  ) +
  theme_bw() + 
  labs(x = "Intuition Level",
       y = "Praiseworthiness and Blameworthiness Ratings /nnfor Province A",
       title = "Rocket intuitive and unintuitive mappings result in \nsimilar praiseworthiness ratings.",
       fill = "Color Map") + 
  theme(
    panel.spacing = unit(.4, "cm"), # Increase spacing between facets
    strip.text = element_text(size = 12),  # Optional: Change facet label size
    strip.background = element_rect(fill = "lightgray")  # Optional: Add background color to facet labels
)

label_path <- here("result_visualizations", "praise_spread.png") 
ggsave(label_path, plot = praise_spread, width = 8, height = 5, dpi = 300)

#model
praise_tab_repmeas <- aov_ez(
  id = "PROLIFIC_PID",
  dv = "ans",
  data = praise_tab,
  within = "full_type",
  between = "intcol"
)
summary(praise_tab_repmeas)

#tabular format
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

#correlation pairwise, demographics
#avg both numeric parameters, check correlation, if high can avg 
#relationship between numeric questions and accuracy -- not now
  #assign each participant a score proportional to their relative inaccuracy in accuracy questions
  #do the same for each allocation/penalty/praiseworthy question type to quantify distance from baseline
  #calculate correlation/predictive capability of inaccuracy based on allocation decisions? is this valid?

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
