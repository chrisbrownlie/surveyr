# setup --------------------------------------------------------------------------------------------------------

# Load dplyr to assist
library(dplyr)

# Load surveyr package
devtools::load_all(export_all = FALSE)

# Import raw data
data <- readxl::read_xlsx("../demo_data.xlsx")

# Show questions
names(data)

# Transform into clean format
new_data <- data %>%
  rename("user" = 1,
         "q1_position_in_trust" = 2,
         "q2_rsc_office_met" = 3,
         "q12_main_challenges" = 4,
         "q15_not_expectations" = 5,
         "q22_changes" = 6,
         "q24_reflections" = 7)

# Focussing on Q24: Do you have any other reflections about your meetings with your RSC office?
# Take a look at some of the data for comparisons later
sample <- 27:37
new_data[sample,"q24_reflections"]

# anonymise() ---------------------------------------------------------------------------------------

# See names 'Joe Bloggs' and 'John Doe' - column needs to be anonymised
# Default behaviour
anon_data <- new_data %>%
  anonymise(q24_reflections)

anon_data[sample,"q24_reflections"]

# If we want to distinguish between the different people (i.e. for analysis, see if one person mentioned alot etc.)
anon_data <- new_data %>%
  anonymise(q24_reflections,
            identify = TRUE)

anon_data[sample,"q24_reflections"] # Random numbers assigned

# If we want to make the column gender neutral as well
anon_data <- new_data %>%
  anonymise(q24_reflections,
            identify = TRUE,
            gender = TRUE)

anon_data[sample,"q24_reflections"]

# If we want to anonymise certain names (i.e. we have a list of certain names)
names_to_anonymise <- c("Joe Bloggs", "the RSC")
anon_data <- new_data %>%
  anonymise(q24_reflections,
            auto = FALSE, # So that the function doesn't automatically overwrite every name it finds
            add_names = names_to_anonymise, # Manually defined names to anonymise
            identify = TRUE,
            gender = TRUE)

anon_data[sample,"q24_reflections"]

# Default behaviour for rest of the demo
anon_data <- new_data %>%
  anonymise(q24_reflections)

anon_data[sample,"q24_reflections"]

# clean_column() -----------------------------------------------------------------------------------------
# After anonymising, column needs to be standardised (lower case, punctuation etc.) before it can be analysed
clean_data <- anon_data %>%
  clean_column(q24_reflections)

clean_data[sample,"q24_reflections"]

# If we want to keep 'null responses'
clean_data <- anon_data %>%
  clean_column(q24_reflections,
               null_response = FALSE)

clean_data[sample,"q24_reflections"]


# common_words() --------------------------------------------------------------------------------------------
# Simple frequency analysis, filters out words that appear less than 5 times so
# filters out groups with only a couple of respondents
clean_data %>%
  common_words(q24_reflections)

# More words
clean_data %>%
  common_words(q24_reflections,
               n = 10)

# Remove certain words
clean_data %>%
  common_words(q24_reflections,
               remove = c("meeting", "rsc", "trust"),
               n = 10)

# Breakdown by demographic
clean_data %>%
  common_words(q24_reflections,
               q2_rsc_office_met,
               remove = c("meeting", "rsc", "trust"),
               n = 1)

# To include all groups
clean_data %>%
  common_words(q24_reflections,
               q2_rsc_office_met,
               remove = c("meeting", "rsc", "trust"),
               n = 5) # Top 5 words but some are being excluded because they occur less than 5 times

clean_data %>%
  common_words(q24_reflections,
               q2_rsc_office_met,
               remove = c("meeting", "rsc", "trust"),
               n = 5,
               min = 0) # include top five words regardless of how often they occur

# Stopwords are currently being removed, to keep them in
clean_data %>%
  common_words(q24_reflections,
               q2_rsc_office_met,
               stopwords = FALSE)

# Can also calculate the proportion of responses in each group
# that mentioned the word
clean_data %>%
  common_words(q24_reflections,
               q2_rsc_office_met,
               remove = c("meeting", "rsc", "trust"),
               n = 1,
               proportion = TRUE)

# Finally, if exporting to external document, can make it more visually appealing with prettify()
clean_data %>%
  common_words(q24_reflections,
               q2_rsc_office_met,
               proportion = TRUE,
               remove = c("meeting", "rsc", "trust"),
               n = 2) %>%
  prettify(alias = c("Which RSC did the respondent meet?" = "q2_rsc_office_met"),
           count_bar = TRUE,
           colour_groups = TRUE)

# n_grams() --------------------------------------------------------------------------------------------------
# Method of tabulating most common n-grams, removing n_grams that are mostly stopwords
clean_data %>%
  n_grams(q24_reflections) # defaults to bigrams

# Can decrease the threshold to be stricter on stopwords
clean_data %>%
  n_grams(q24_reflections,
          stop_thresh = 0) # 0 means exclude n-grams that contain one or more stopwords, 1 means include all n-grams

# Other options
clean_data %>%
  n_grams(q24_reflections,
          n = 3, # trigrams
          word = "meeting", # filter to only include n-grams containing the word 'meeting'
          stop_thresh = 0.4) # allow only one stopword per trigram

# Prettify
clean_data %>%
  n_grams(q24_reflections,
          n = 3, # trigrams
          word = "meeting", # filter to only include n-grams containing the word 'better'
          stop_thresh = 0.4) %>% # allow only one stopword per ngram
  prettify(alias = c("Phrase" = "ngram"),
           count_bar = TRUE)

# sentiment_score() -----------------------------------------
sentiment_data <- clean_data %>%
  sentiment_score(q24_reflections)
# themes ----------------------------------------------------------------------
new_data %>%
  summarise_topics(q12_main_challenges,
                   exclude = "rsc",
                   num_topics = 2)

topics_data <- new_data %>%
  clean_column(q12_main_challenges) %>%
  classify_topics(q12_main_challenges,
                  topic_aliases = c("1" = "finances",
                                    "2" = "growth"),
                  output = "main_challenges_topic",
                  confidence = TRUE)

# Example workflow ---------------------------------------------------------------------------------------------
# (After running setup)
new_data %>%
  anonymise(q24_reflections,
            identify = TRUE) %>%
  clean_column(q24_reflections) %>%
  common_words(q24_reflections,
               q2_rsc_office_met,
               remove = c("meeting", "rsc", "trust"),
               n = 1) %>%
  prettify(alias = c("Which RSC did the respondent meet?" = "q2_rsc_office_met"),
           count_bar = TRUE)
