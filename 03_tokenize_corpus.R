# Tokenize EC corpus (for later use with tokenized word embedding models)
# Author: ChRauh

# Input: data/all_texts.rds
# Output: large_data/tokens.rds


# Packages ####
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v2.0.0 
library(tidytext) # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools, CRAN v0.4.1
library(quanteda) # Quantitative Analysis of Textual Data, CRAN v3.3.1


# Text data ####
texts <- read_rds("data/all_texts.rds")


# Tokenize ####
# Using tidytext's vectorized approach here

start <- Sys.time()
tok <- 
  texts %>% 
  # head(10) %>% # For testing
  select(id, text_para) %>% 
  unnest_tokens(input = text_para,
                output = token,
                token = "words",
                to_lower = T)
Sys.time()-start # 1,2 mins


# Pre-processing | feature selection ####
# To be documented, and to be noted downstream 

stopwords <- quanteda::stopwords("english")

# tok$length <- nchar(tok$token)
# short <-
#   tok  %>% 
#   filter(length == 1) %>% 
#   select(token) %>% 
#   unique() %>% 
#   arrange(token)

tok <- 
  tok %>% 
  filter(!(token %in% stopwords)) %>% # Exclude en stopwords
  filter(str_detect(token, "[a-zäöüß]")) %>% # Only tokens with letters in them
  filter(nchar(token) > 1)


# Export ####
write_rds(tok, "./large_data/tokens.rds", compress = "gz")
