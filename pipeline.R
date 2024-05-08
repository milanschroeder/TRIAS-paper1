# Full Analysis Pipeline

library(tidyverse)

# data prep ####
  source("00_connectDB.R") # establish database connection
    # returns: MariaDBconnection con

  source("01_read_corpus.R") # read corpus and metadata from DB
    # returns: all_meta, all_texts

  # source("02_clean_corpus.R") # clean paragraphs for analysis
    # returns: all_texts.rds
    all_texts <- readr::read_rds("data/all_texts.rds") # to skip lengthy cleaning

# digitality ####

  source("10_digitality_scaling.R") # Semantic scaling of paragraph "Digitality"
    # returns: paras_scaled
  
  source("11_digitality_scaling_descriptives.Rmd") # descriptives on semantic scaling classifier
  
  # alternative approach: 1x_digitality_zeroshot.ipynb
    # returns: zeroshot_digital.csv
    
  # source("12_create_validation_sample.R") # validation sample creation
    # returns: validation_sample.csv
  
  # validation done in https://manualcoding.pythonanywhere.com/
    # returns: output_manual_coding.csv
    
  source("13_scaling_validation_results.Rmd") # validation sample results & cutoff selection
  
# country mentions ####

  source("20_country_mentions.R") # identification of country mentions along newsmap dictionary
  
  source("21_country_mentions_descriptives.Rmd") # descriptives on country identification
  
# analysis ####
  
  # ...
