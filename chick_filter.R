# load required 
library(readr)
library(data.table)
 
# load dat - export of study level data in wide format with 1 row of headers 
Annotation_data_2025_01_30_Wide_format <- read_csv("data/Annotation_data_-_2025_01_30_-_Wide_format_-_29bc6a0b-5ece-46b1-bcaf-776ff4275ecd_-_Investigators_Unblinded.csv")

# Identify studyIDs with discrepancies
discrepant_studyIDs <- Annotation_data_2025_01_30_Wide_format %>%
  group_by(StudyId) %>%
  summarize(unique_values = n_distinct(`Exclusion of article?_fabf514a-b197-40d4-ae77-b9f24f3981f0_Answer`)) %>%
  filter(unique_values > 1) %>%
  pull(StudyId)

# Optional: Filter and display the rows with discrepancies
discrepant_rows <- Annotation_data_2025_01_30_Wide_format %>% filter(StudyId %in% discrepant_studyIDs)
# print(discrepant_rows)

# number of unique IDs with discrepant inclusion decisions
length(unique(discrepant_rows$StudyId))
#176

# Determine the majority response when there are three or more occurrences
df_majority <- Annotation_data_2025_01_30_Wide_format %>%
  group_by(StudyId, `Exclusion of article?_fabf514a-b197-40d4-ae77-b9f24f3981f0_Answer`) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(StudyId) %>%
  filter(count == max(count)) %>%
  ungroup()

summary(df_majority$`Exclusion of article?_fabf514a-b197-40d4-ae77-b9f24f3981f0_Answer`)

df_majority_incl <- df_majority %>% filter(`Exclusion of article?_fabf514a-b197-40d4-ae77-b9f24f3981f0_Answer` == "no")

length(unique(df_majority_incl$StudyId))
#1021 included studies - by majority vote

# Filter the entire dataframe to retain majority responses and previous information
df_incl_filtered <- Annotation_data_2025_01_30_Wide_format %>%
  left_join(df_majority_incl, by = c("StudyId", "Exclusion of article?_fabf514a-b197-40d4-ae77-b9f24f3981f0_Answer")) %>%
  filter(!is.na(count) | StudyId %in% discrepant_studyIDs) %>%
  select(-count)  # Remove count column if not needed



#import experimental data
Annotation_data_Experimentlevel <- read_csv("data/Annotation_data_-_2025_01_30_-_Long_format_-_29bc6a0b-5ece-46b1-bcaf-776ff4275ecd_-_Investigators_Unblinded_-_Experimentlevel.csv
data/Annotation_data_-_2025_01_30_-_Long_format_-_29bc6a0b-5ece-46b1-bcaf-776ff4275ecd_-_Investigators_Unblinded_-_Experimentlevel.csv")


names(Annotation_data_Experimentlevel)


# Identify discrepancies in answers for each StudyId and Question
discrepant_questions <- Annotation_data_Experimentlevel %>%
  group_by(StudyId, Question, QuestionId) %>%
  summarize(unique_answers = n_distinct(Answer), .groups = "drop") %>%
  filter(unique_answers > 1)

# Count discrepancies for each question
discrepant_question_counts <- discrepant_questions %>%
  count(Question, sort = TRUE)

print(discrepant_question_counts)
