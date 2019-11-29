# surveyr

### Note: surveyr is currently in development. Feel free to use the package as-is and give feedback to help improvements.

## Overview
surveyr is an R package which can aid in the analysis of survey response datasets. It combines the best parts of several data wrangling and text analysis packages in order to abstract from the extensive repetitive process that such analysis often requires.
The package provides a set of simple, easy to understand functions to aid in the pre-processing of dataframes and in particular the analysis of text data. Some key functions include:
- `clean()` to perform basic text pre-processing including lowercasing, removing punctuation and other standardisation
- `anonymise()` to remove references to names where sensitive data requires it
- `common_words()` to quickly tabulate the most commonly used words in response to a question
- `sentiment_score()` to easily measure the sentiment of responses to a question
- `summarise_topics()` provides a single, simple function to concisely define the topics present among responses to a question, using a Latent Dirichlet Allocation model.

The surveyr package was designed with [tidyverse](www.tidyverse.org) syntax and workflow in mind, utilising many aspects of tidy evaluation. As such, this package is best utilised in tandem with these packages.

## Installation
```
# surveyr is not currently available on CRAN, to download use 
devtools::install_github("chrisbrownlie/surveyr")
```

## Usage
```
library(surveyr)

clean_responses <- responses %>%
  anonymise_all() %>%
  clean_all()
  
clean_responses %>%
  common_words(`q3: Why is this your favourite restaurant?`)
  
clean_responses %>%
  n_grams(`q1: What are your favourite kinds of food?`)
  
clean_responses %>%
  sentiment_score(`q4: What are your thoughts about this restaurant in particular?`)
  
clean_responses %>%
  summarise_topics(`q5: How would you improve this restaurant?`)
```
  
## Feedback
If you find any bugs, inconsistencies or simply have feedback on how to improve the package, please raise an [issue here](github.com/chrisbrownlie/surveyr).
