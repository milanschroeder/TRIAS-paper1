# Code / scale EC communication along word weights from pre-trained word embedding model beforehand
# this time using the Concept Movers Distance rather than cosine similarity to average vectors
# Author: CR (03.06.2024)


# Intuition similar to semantic similarity, but hopefully less noisy
# CMD algorithm first searches for closest word pairs between concept and coding docs
# To then calculate the lowest 'transportation costs' from one into the other

# Paper: https://link.springer.com/article/10.1007/s42001-019-00048-6
# Code: https://culturalcartography.gitlab.io/text2map/articles/CMDist-concept-movers-distance.html


# Packages ###
library(tidyverse)
library(text2map)
library(text2vec)
library(textclean)
library(stringi)
library(quanteda)
library(readxl)
library(magrittr)


# Word embeddings ####

# Using the ready-made GLOVE word embeddings trained on Wikipedia and Gigaword
# Version with 400k vocabulary and 300 dimensions

# Get it here: https://nlp.stanford.edu/projects/glove/ # ! LARGE FILE !

# I have parsed this for other purposes already, if you start from the raw file see:
# https://gist.github.com/tjvananne/8b0e7df7dcad414e8e6d5bf3947439a9

# glove.300 <- read_rds("C:/Users/chris/Downloads/glove.6B.300d.rds") # HP path
# glove.300 <- read_rds("C:/Users/rauh/Downloads/glove.6B.300d.rds") # ThinkPad path
glove.300 <- read_rds("C:/Users/rauh/Downloads/glove.6B.300d.rds") # WZB path
glove.300 <- read_rds("./large_data/glove.6B.300d.rds") # MS path

# Clean up the vocabulary a bit (lots of rare trash in there, exclude stopwords)
vocab <- names(glove.300) %>% 
  as.data.frame() %>% 
  rename(token = 1) %>% 
  filter(str_detect(token, "[a-z]")) %>% 
  filter(nchar(token) > 1) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  filter(!str_detect(token, "\\.")) %>% 
  filter(!str_detect(token, "[0-9]"))
glove.300 <- glove.300 %>% select(vocab$token)

# This has the tokens in columns and the dimensions in rows
# cmd and text2vec generally expect vocab in rows and vectors values in columns, so I transpose this accordingly here
glove.300 <- glove.300 %>%
  as.matrix() %>%
  t() 

rm(vocab)
gc()



# Texts to be coded ####

# Paragraph-level
# CMDist expects a DTM which I construct using quanteda tools here
# Pre-processing similar to 03_tokenize_corpus.R - so as to ensure comparability to the semantic similarity measures

texts <- read_rds("data/all_texts.rds")

# Pre-processing and DTM construction

start <- Sys.time()
dfm_para <- 
  texts %>% 
  arrange(id) %>% # Just to ensure matching down the line
  rename(text = text_para) %>% 
  mutate(text = tolower(text)) %>% 
  # head(20) %>% # Sample for testing
  corpus() %>% 
  tokens(remove_punct = T,
         remove_symbols = T,
         remove_numbers = T,
         remove_url = T,
         verbose = T) %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_keep(min_nchar = 2) %>% 
  dfm(verbose = T)
Sys.time() - start  



# Seed dictionaries ####
# cf. 10_semantic_similarity_weights.R

# The simple 'digitality' dictionary
seed_dp_simple <- c("digital", "online", "computer", "internet", "algorithm")

# The advanced / intersubjective 'digitality' dictionary
# Five human coders received a list of the top-1500 terms semantically close to the radically simplified dictionary
# Each coder rated each terms as "Clearly and exclusively referring to the digital technology?" [0|1]
files <- paste0("./data/DigitalTerms/", list.files("./data/DigitalTerms/"))
ratings <- read_xlsx(files[1], sheet = 2) %>% rename(token = 1, coder1 = 2) %>% 
  left_join(read_xlsx(files[2], sheet = 2) %>% rename(token = 1, coder2 = 2), by = "token") %>% 
  left_join(read_xlsx(files[3], sheet = 2) %>% rename(token = 1, coder3 = 2), by = "token") %>% 
  left_join(read_xlsx(files[4], sheet = 2) %>% rename(token = 1, coder4 = 2), by = "token") %>%
  left_join(read_xlsx(files[5], sheet = 2) %>% rename(token = 1, coder5 = 2), by = "token") %>% 
  mutate(codesum = rowSums(across(where(is.numeric)))) %>% 
  arrange(desc(codesum))
# Now we take those tokens for which a majority of coders (3/5) suggested that this was a relevant term ...
seed_dp_adv <- ratings %>% 
  filter(codesum >=3) %>% 
  select(token) %>% 
  pull()
rm(files, ratings)

# Economy terms
seed_econ <- c("economy", "economic", "markets", "trade", "business")

# Security terms
seed_security <- c("security", "defense", "military", "espionage", "intelligence")

# Liberal rights terms
seed_librigths <- c("rights", "liberty", "freedom", "justice", "equality")

seed_geo <- c("geopolitical", "geopolitics", "international", "global", "world", "worldwide")

seed_chance <- c("potential", "benefit", "profit", "advantage", "chance")
seed_risk <- c("danger", "threat", "vulnerabilities", "crisis", "risk") # protection?


# Calculate concept movers distances ####

# Note: no scaling applied (can be done ex post)
# Note: paralell = FALSE

start <- Sys.time()
cmds <- cbind(CMDist(dtm = dfm_para, cw = seed_dp_simple, wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cw = seed_dp_adv, wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cw = seed_econ, wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cw = seed_security, wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cw = seed_librigths, wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cw = seed_geo, wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cw = seed_chance, wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cw = seed_risk, wv = glove.300, scale = F)[,2]) %>%
  as.data.frame() %>% 
  rename(cmd_digital_simple = 1,
         cmd_digital_adv =2,
         cmd_economy = 3,
         cmd_security = 4,
         cmd_librights = 5,
         cmd_geo = 6,
         cmd_chance = 7,
         cmd_risk = 8) %>% 
  mutate(id = row_number()) %>% # Quanteda preserves order (I hope ;))
  left_join(texts %>% select(c(id, text_para)), by = "id") %>% 
  relocate(c(id, text_para))
Sys.time() - start  




# risk-chance scaling:
cmds %<>% mutate(risk_chance = cmd_chance - cmd_risk)


write_rds(cmds, "./data/CMDs.rds")
