library(tidyverse)

#Most recent pilot data
data <- read_csv('pilot_data_040625.csv')

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

#Removing returned submissions, eliminating observations from testing
data <- data |>
  filter(!(PROLIFIC_PID %in% returns) & row_number() > 31) 
#Table detailing what each question is--could have also directly renamed df col
tracking_qcodes <- data.frame(ALLOC = c(13, 14, 15, 16, 17, 18, 19, 20), 
                              allocq = c('Q255', 'Q256', 'Q257', 'Q258','Q259', 'Q260','Q261','Q262'),
                              penaltyq = c('Q225', 'Q223', 'Q226','Q228','Q230','Q227','Q229','Q232'), 
                              praiseworthq = c('Q9...125','Q9...132','Q9...139','Q9...146','Q9...153','Q9...160','Q9...167','Q9...174'))

all_col <- c(tracking_qcodes$allocq, tracking_qcodes$penaltyq, tracking_qcodes$praiseworthq)

#Assign numerical encodings
levels <- function(x) {
  num <- case_when(x == "Very Blameworthy" ~ '-3',
                   x == "Moderately Blameworthy" ~ '-2',
                   x == "A Little Blameworthy" ~ '-1',
                   x %in% c("Neither", "no penalty") ~ '0',
                   x %in% c("A Little Praiseworthy", "1% penalty") ~ '1',
                   x %in% c("Moderately Praiseworthy", "2% penalty") ~ '2',
                   x %in% c("Very Praiseworthy", "3% penalty") ~ '3',
                   x == "4% penalty" ~ '4', 
                   x == "5% penalty" ~ '5'
                   )
  return(num)
}

values = c(40, 50, 60, 18, 47, 40, 40, 19, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
baseline_values <- data.frame(question_num = all_col, values = values)

to_analyze <- data |> 
  select(all_col) |>
  mutate_at(c(tracking_qcodes$praiseworthq, tracking_qcodes$penaltyq), levels) |>
  na.omit() |>
  pivot_longer(cols = everything(), 
               names_to = "question_num", 
               values_to = "answer") |>
  mutate(answer = as.numeric(answer)) |>
  group_by(question_num) |>
  summarize(mean = mean(answer),
            median = median(answer)) |>
  left_join(baseline_values, by = join_by(question_num == question_num)) |>
  mutate(
    mean_metric = ifelse(
      question_num %in% tracking_qcodes$allocq, 
      abs(mean - values), 
      mean),
    median_metric = ifelse(
      question_num %in% tracking_qcodes$allocq, 
      abs(median - values), 
      median))

top_qs <- to_analyze |>
  filter(question_num %in% c("Q255", "Q225", "Q9...125", "Q257", 'Q226', 'Q9...139'))
