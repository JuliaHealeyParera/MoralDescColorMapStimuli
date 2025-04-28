library(tidyverse)
library(tidymodels)
library(afex)
library(here)
library(knitr)
library(webshot)

here()
orig <- read_csv('data/visual_data_041325.csv')
pivoted <- read_csv('data/cleaned_visual.csv')

###ACCURACY ANALYSIS###
accuracies <- pivoted |>
  #Only accuracy type questions
  filter(grepl("acc_.*", question_type, perl = TRUE)) |>
  mutate(
    #Extract letter for province in question
    province = str_extract(question_type, "(?<=acc_prov)[a-d]"), 
    #Choose first number in damage level listing for a (ex. 8 of 8642), second for b, etc.
    base_lvl = case_when(province == 'a' ~ substr(damage_lvl, 1, 1), 
                         province == 'b' ~ substr(damage_lvl, 2, 2),
                         province == 'c' ~ substr(damage_lvl, 3, 3),
                         province == 'd' ~ substr(damage_lvl, 4, 4)), 
    #For visualization purposes, combine intuition and color
    int_col = paste0(intuition, "_", color),
    #Observed - expected (residual)
    diff = as.numeric(ans) - as.numeric(base_lvl),
    #Clean
    ans = str_replace(ans, ',', '.'),
    base_lvl = as.numeric(base_lvl))

#Lowest value in each damage level grouping, coincides with Province D
lowest_val_acc <- accuracies |>
  filter(province == "d") |>
  group_by(intuition, color, damage_lvl) |>
  summarize(mean = mean(diff), 
            median = median(diff), 
            var = var(diff))

#Highest value in each damage level grouping, coincides with Province A
highest_val_acc <- accuracies |>
  filter(province == "a") |>
  group_by(intuition, color, damage_lvl) |>
  summarize(mean = mean(diff), 
            median = median(diff), 
            var = var(diff))

#2s only in each grouping
#(evaluation of each accuracy for only provinces with damage level = 2)
two_val_acc <- accuracies |>
  filter(base_lvl == 2) |>
  group_by(intuition, color, damage_lvl) |>
  summarize(mean = mean(diff), 
            median = median(diff), 
            var = var(diff))

#All values
accuracies_summary <- accuracies |>
  group_by(intuition, color) |>
  summarize(mean = mean(diff), 
            median = median(diff), 
            var = var(diff))

###CREATING PLOTS###
#Editing for visual appeal and plotting
accuracies_plot <- accuracies |>
  mutate(int_col = case_when(int_col == "int_blues" ~ "IntBlue",
                             int_col == "int_rocket" ~ "IntRoc",
                             int_col == "unint_blues" ~ "UnintBlue",
                             int_col == "unint_rocket" ~ "UnintRoc",
                             TRUE ~ int_col),
         damage_lvl = paste0("Damage Level: ", damage_lvl),
         int_col = fct_relevel(int_col, c('IntBlue', 'UnintBlue', 'IntRoc', 'UnintRoc'))) 

#General
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

#Largest value
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

#Smallest value
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

#2s
twos <- accuracies_plot |> filter(base_lvl == 2)
int_blue_6211 <- twos |> filter(int_col == "IntBlue" & damage_lvl == "Damage Level: 6211")

twos_val_boxplot <- ggplot(twos, aes(x = int_col, y = diff)) +
  geom_boxplot(fill = "gray75") +
  facet_wrap(~damage_lvl) + 
  geom_boxplot(data = int_blue_6211, fill = "white", color = "blue2") +
  geom_hline(yintercept = 0, color = "red4", linewidth = 0.45) +
  labs(subtitle = "The number 2 was least accurately interpreted for Intuitive Blue 6211.",
       title = "Accuracy of interpretation of the number 2 in each scenario.",
       x = "Intuition Level and Color Map",
       y = "Difference Between Observed and Actual Value") + 
  coord_cartesian(ylim = c(-7.5, 7.5)) + 
  theme_bw()

#Save
label_path <- here("result_visualizations", "accuracy_boxplots_2s.png") 
ggsave(label_path, plot = twos_val_boxplot, width = 6.5, height = 4, dpi = 300)

label_path <- here("result_visualizations", "accuracy_boxplots.png") 
ggsave(label_path, plot = accuracy_boxplots, width = 6.5, height = 4, dpi = 300)

label_path <- here("result_visualizations", "accuracy_boxplots_largeval.png") 
ggsave(label_path, plot = largest_val_boxplot, width = 6.5, height = 4, dpi = 300)

label_path <- here("result_visualizations", "accuracy_boxplots_smallval.png") 
ggsave(label_path, plot = smallest_val_boxplot, width = 6.5, height = 4, dpi = 300)
