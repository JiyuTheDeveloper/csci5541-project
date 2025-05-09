#combine all the datasets and FLAG with keywords for certain columns
{
  library(tidyverse)
  library(dplyr)
}

#read in all data
{
#aug2019
  anipjaug19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Aug_2019/anipjaug19.csv/anipjaug19.csv", 
                         col_types = cols(timestamp = col_datetime()))
  depjaug19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Aug_2019/depjaug19.csv/depjaug19.csv", 
                        col_types = cols(timestamp = col_datetime()))
  lonaug19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Aug_2019/lonaug19.csv/lonaug19.csv", 
                       col_types = cols(timestamp = col_datetime()))
  mhaug19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Aug_2019/mhaug19.csv/mhaug19.csv", 
                      col_types = cols(timestamp = col_datetime()))
  swaug19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Aug_2019/swaug19.csv/swaug19.csv", 
                      col_types = cols(timestamp = col_datetime()))
  
  #sep2019
  anisep19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Sep_2019/anisep19.csv/anisep19.csv", 
                         col_types = cols(timestamp = col_datetime()))
  depasep19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Sep_2019/depasep19.csv/depasep19.csv", 
                        col_types = cols(timestamp = col_datetime()))
  lonasep19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Sep_2019/lonasep19.csv/lonasep19.csv", 
                       col_types = cols(timestamp = col_datetime()))
  mhsep19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Sep_2019/mhsep19.csv/mhsep19.csv", 
                      col_types = cols(timestamp = col_datetime()))
  swsep19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Sep_2019/swsep19.csv/swsep19.csv", 
                      col_types = cols(timestamp = col_datetime()))
  
  #Oct2019
  anioct19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Oct_2019/anioct19.csv/anioct19.csv", 
                       col_types = cols(timestamp = col_datetime()))
  depoct19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Oct_2019/depoct19.csv/depoct19.csv", 
                       col_types = cols(timestamp = col_datetime()))
  lonoct19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Oct_2019/lonoct19.csv/lonoct19.csv", 
                       col_types = cols(timestamp = col_datetime()))
  mhoct19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Oct_2019/mhoct19.csv/mhoct19.csv", 
                      col_types = cols(timestamp = col_datetime()))
  swsoct19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Oct_2019/swsoct19.csv/swsoct19.csv", 
                       col_types = cols(timestamp = col_datetime()))

  #Nov2019
  aninov19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Nov_2019/aninov19.csv/aninov19.csv", 
                       col_types = cols(timestamp = col_datetime()))
  sdepnov19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Nov_2019/sdepnov19.csv/sdepnov19.csv", 
                       col_types = cols(timestamp = col_datetime()))
  lonnov19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Nov_2019/lonnov19.csv/lonnov19.csv", 
                       col_types = cols(timestamp = col_datetime()))
  mhnov19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Nov_2019/mhnov19.csv/mhnov19.csv", 
                      col_types = cols(timestamp = col_datetime()))
  swnov19 <- read_csv("Spring 2025/CSCI 5541_Datasets/2019/Nov_2019/swnov19.csv/swnov19.csv", 
                       col_types = cols(timestamp = col_datetime()))
}

{
combined_dataset <- bind_rows(anipjaug19, depjaug19, lonaug19,  mhaug19, swaug19, 
                              anisep19,   depasep19, lonasep19, mhsep19, swsep19, 
                              anioct19,   depoct19,  lonoct19,  mhoct19, swsoct19, 
                              aninov19,   sdepnov19, lonnov19,  mhnov19, swnov19)
  
rm(anipjaug19, depjaug19, lonaug19,  mhaug19, swaug19, 
   anisep19,   depasep19, lonasep19, mhsep19, swsep19, 
   anioct19,   depoct19,  lonoct19,  mhoct19, swsoct19, 
   aninov19,   sdepnov19, lonnov19,  mhnov19, swnov19)

combined_dataset <- combined_dataset[,-1]

new_cols <- c("label_mental_health", "disorder", "diagnoised", "SeekingHelp_copingMechanisms", "details", 
              "label_Gender_Identity", "matched_gender_word", "Gender_Identity", "Details", 
              "label_racial_identity", "matched_racial_word", "race_identity", "details", 
              "label_queer_identity", "matched_queer_word", "details", "extra_comments")
for (col in new_cols) {
  combined_dataset[[col]] <- NA
}
yes_no_cols <- c("label_mental_health", "diagnoised", "SeekingHelp_copingMechanisms", 
                 "label_Gender_Identity" , 
                 "label_racial_identity", 
                 "label_queer_identity")
for (col in yes_no_cols) {
  combined_dataset[[col]] <- -1
}
rm(new_cols, yes_no_cols, col)
}

{
  #want to flag now for certain subgroups, make easier to find
  gender_identity <- c(" male", "female", " man", "woman", "nonbinary", "NUM[0-9]+[FM]\\)", "\\([fFmM]\\)")
  gender_pattern <- paste(gender_identity, collapse = "|")
  
  racial_identity <- c(" white", " black", "asian", "african", "european", "south american", "hispanic", "latino")
  racial_pattern <- paste(racial_identity, collapse = "|")
  
  queer_identity <- c(" gay", "lesbian", "asexual", " ace", "queer", "LGBTQIA", " fag", "bisexual", " bi ", " trans ", "transgender")
  queer_pattern <- paste(queer_identity, collapse = "|")
  
  library(data.table)
  setDT(combined_dataset)

  extract_matched_word <- function(text, pattern_list) {
    matched_words <- unlist(lapply(pattern_list, function(p) {
      if (grepl(p, tolower(text), ignore.case = TRUE)) {
        return(p)
      }
      return(NULL)
    }))
    if (length(matched_words) > 0) {
      return(paste(matched_words, collapse = ", "))
    }
    return(NA)
  }
  
  #now run through all selftext. identifies and assigns 3 for TRUE and -1 for FALSE, then finds matched word
  combined_dataset[, label_Gender_Identity := fifelse(grepl(gender_pattern, tolower(selftext), ignore.case = TRUE), 3, -1)]
  combined_dataset[, matched_gender_word := sapply(selftext, extract_matched_word, pattern_list = gender_identity)]
  print("gender done")
  combined_dataset[, label_racial_identity := fifelse(grepl(racial_pattern, tolower(selftext), ignore.case = TRUE), 3, -1)]
  combined_dataset[, matched_racial_word := sapply(selftext, extract_matched_word, pattern_list = racial_identity)]
  print("race done")
  combined_dataset[, label_queer_identity := fifelse(grepl(queer_pattern, tolower(selftext), ignore.case = TRUE), 3, -1)]
  combined_dataset[, matched_queer_word := sapply(selftext, extract_matched_word, pattern_list = queer_identity)]
  print("queer done")
  
  rm(gender_identity, gender_pattern, queer_identity, queer_pattern, racial_identity, racial_pattern)
}

{
  print("All")
  combined_dataset %>% filter(label_Gender_Identity==3 | label_racial_identity==3 | label_queer_identity==3) %>% 
    summarise(
      gender_identity_sum = sum(label_Gender_Identity==3, na.rm = TRUE),
      racial_identity_sum = sum(label_racial_identity==3, na.rm = TRUE),
      queer_identity_sum = sum(label_queer_identity==3, na.rm = TRUE))
  
  # gender_identity_sum racial_identity_sum queer_identity_sum
  #                23567                2568               1656

  # Summarize the counts and proportions for the identity labels
  print("People that have all 3 flagged")
  combined_dataset %>% filter(label_Gender_Identity==3 & label_racial_identity==3 & label_queer_identity==3) %>% 
    summarise(
      gender_identity_sum = sum(label_Gender_Identity==3, na.rm = TRUE),
      racial_identity_sum = sum(label_racial_identity==3, na.rm = TRUE),
      queer_identity_sum = sum(label_queer_identity==3, na.rm = TRUE))
  #78
  
  
  # print("depression")
  # combined_dataset %>% filter(subreddit=="depression") %>% 
  #   summarise(
  #     gender_identity_sum = sum(label_Gender_Identity, na.rm = TRUE)/3,
  #     racial_identity_sum = sum(label_racial_identity, na.rm = TRUE)/3,
  #     queer_identity_sum = sum(label_queer_identity, na.rm = TRUE)/3)

}

# 
# depression_data <-  combined_dataset %>% filter(subreddit=="depression") 
# depression_data %>% 
#   summarise(
#     gender_identity_sum = sum(label_Gender_Identity, na.rm = TRUE)/3,
#     racial_identity_sum = sum(label_racial_identity, na.rm = TRUE)/3,
#     queer_identity_sum = sum(label_queer_identity, na.rm = TRUE)/3)

library(writexl)
library(openxlsx)

long_column <- "selftext"
combined_dataset_filtered <- combined_dataset[nchar(combined_dataset[[long_column]]) <= 32767, ]

write_xlsx(combined_dataset_filtered, "C:/Users/JostV/OneDrive/Documents/Spring 2025/CSCI 5541_Datasets/combined_dataset_Final.xlsx")


