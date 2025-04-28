library(tidyverse)
library(tidymodels)
library(afex)
library(here)
library(knitr)
library(webshot)

here()
data <- read_csv('data/visual_data_041325.csv')
pilot <- read_csv('data/pilot_data_040625.csv')

###CLEANING PILOT### - For eventual joining 
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

write_csv(pilot, 'data/cleaned_pilot.csv')

###CLEANING VISUAL DATA### 
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

#Matching Qualtrics responses/data to question attributes
pivoted <- data |> 
  filter(PROLIFIC_PID != "5e9409cd1dabe7246810c3b6") |>
  select(c(all_of(q_num), PROLIFIC_PID)) |>
  pivot_longer(cols = -PROLIFIC_PID, 
               names_to = "question_num", 
               values_to = "ans") |>
  filter(!is.na(ans)) |>
  left_join(question_names, by = join_by(question_num == qnum))

write_csv(pivoted, 'data/cleaned_visual.csv')

#Df with distance from baseline
with_diffs <- pivoted |>
  #Remove accuracy questions
  filter(!grepl("acc_.*", question_type, perl = TRUE)) |>
  mutate(
    base_line = case_when(
      question_type %in% c('penalty', 'praiseworthy') ~ 0, 
      damage_lvl == "8642" ~ 40, #(8 / (8+6+4+2))
      damage_lvl == "6211" ~ 60, #(6 / (6+2+1+1))
      damage_lvl == "4321" ~ 40, #(4 / (4+3+2+1))
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
    #Account for edge cases where participants interpreted allocation as being out of 10 
    #Assume that any value less than 10 (exclusive) was submitted out of 10, should be out of 100
    ans = ifelse((question_type == "allocation") & (ans < 10), 
                 ans * 10, 
                 ans),
    diff = as.numeric(ans) - as.numeric(base_line), 
    full_type = paste0(question_type, "_", damage_lvl))

write_csv(with_diffs, 'data/with_diffs.csv')
