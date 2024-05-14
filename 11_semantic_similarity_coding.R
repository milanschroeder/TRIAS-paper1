# Code / scale EC communication along word weights extracted from pre-trained word embedding model beforehand
# Author: CR

# Input: tokens.rds (tokenized version of EC documents), token weight lists extracted from pre-trained model
# Outputs:


# Packages ####
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v2.0.0 


# Tokenized texts ####
tokens <- read_rds("./large_data/tokens.rds")


# Semantic similarity weights ####
# Extracted from pre-trained word vector model

digital1 <- read_rds("./large_data/SemSimilWeights-DigitalitySimple.rds")
digital2 <- read_rds("./large_data/SemSimilWeights-DigitalityAdvanced.rds")
digital3 <- read_rds("./large_data/SemSimilWeights-DigitalityAdvancedFreqCorrection.rds")

economy <- read_rds("./large_data/SemSimilWeights-Economy.rds")
security <- read_rds("./large_data/SemSimilWeights-Security.rds")
librights <- read_rds("./large_data/SemSimilWeights-LibRights.rds")

confcoop <- read_rds("./large_data/SemSimilWeights-ConflictCooperation.rds")


# Merge semantic similarity weights to tokens data ####

df <- tokens %>% 
  left_join(digital1 %>% 
              select(token, sim.target) %>% 
              rename(digital_simil_simple = sim.target),
            by = "token") %>% 
  left_join(digital2 %>% 
              select(token, sim.target) %>% 
              rename(digital_simil_advanced = sim.target),
            by = "token") %>% 
  left_join(digital3 %>% 
              select(token, sim.target) %>% 
              rename(digital_simil_advanced_freq = sim.target),
            by = "token") %>% 
  left_join(economy %>% 
              select(token, sim.target) %>% 
              rename(economy_simil = sim.target),
            by = "token") %>%
  left_join(librights %>% 
              select(token, sim.target) %>% 
              rename(librights_simil = sim.target),
            by = "token") %>% 
  left_join(security %>% 
              select(token, sim.target) %>% 
              rename(security_simil = sim.target),
            by = "token") %>%
  left_join(confcoop %>% 
              select(token, conf_coop),
            by = "token")

nrow(df) == nrow(tokens)

rm(list=setdiff(ls(), "df"))
gc()


# Aggregate to document (=paragraph) level ####

out <- df %>% 
  group_by(id) %>% 
  summarise_if(is.numeric, mean, na.rm = T) %>% # Means of semantic simil weights by id, unknown tokens excluded (grouping var should be automatically excluded)
  ungroup()


# Export ####
write_rds(out, "./large_data/SemanticSimilarityCodes_ParagraphLevel.rds", compress = "gz")



# Some qualitative inspection ####

# All texts (that was the starting point for tokenization)
para <- read_rds("./data/all_texts.rds")

# Difference in cases
nrow(para) - nrow(out) # 45979 !

# Which paragraphs are not in the semantic similarity data?
test <- para %>% 
  filter(!(id %in% out$id))
# Reasonable at first sight: Greek, only stopwords or numbers, etc.




