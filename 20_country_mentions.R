# Extract countries mentioned in Commission communication 
# Authors:  @ChRauh 


# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v2.0.0 
library(quanteda) # Quantitative Analysis of Textual Data, CRAN v3.3.1 
library(newsmap) # Semi-Supervised Model for Geographical Document Classification, CRAN v0.8.2


# Text data ####
texts <- read_rds("./data/all_texts.rds") %>% 
  rename(text = text_para) %>% 
  mutate(text = str_replace_all(text, fixed(" U.S. "), " US ")) %>% 
  mutate(text = str_replace_all(text, fixed(" U.S.A. "), " USA ")) 

# Quanteda corpus
corp <- corpus(texts$text,
               docvars = data.frame(texts[, c(1,2,4)])) # id, doc_pos, doc_key

# Quanteda tokens object
# this takes time and memory ...
tok <- tokens(corp, 
              remove_punct = T,
              split_hyphens = T) %>% # Note! Cases such as "US-led", "China-based" ....
  # tokens_remove(pattern = "us", valuetype = "fixed") %>% # US vs us, maybe add capitalized counts later? 
  tokens_remove(pattern = c(stopwords("en")), 
                valuetype = "fixed", 
                padding = TRUE)  



# Look up country mentions in tokenized texts ####

# Dictionary
ctry_dict <- newsmap::data_dictionary_newsmap_en

# Look up
toks_ctry <- tokens_lookup(tok, 
                           dictionary = ctry_dict, 
                           case_insensitive = TRUE,
                           levels = 3) # Countries on level 3 of the dict (1 is continents, 2 is region)

# Matrix/data frame
ctry_hits <- dfm(toks_ctry, tolower = FALSE) %>% 
  convert(to = "data.frame") %>% 
  select(-doc_id) %>% 
  cbind(docvars(toks_ctry)) %>% 
  relocate(id, doc_pos, doc_key)

sum(ctry_hits$CN) # China paragraphs: 21797
sum(ctry_hits$US) # US paragraphs: 95998 


# Correct the "US" vs "us" issue
# Upper case occurrences should count into the variable, lower case occurrences shouldn't
# Exploiting the consistent order of texts and dictionary hits here

ctry_hits$us_plus <- str_count(texts$text, fixed(" US "))
sum(ctry_hits$us_plus) # 20673
ctry_hits$us_minus <- str_count(texts$text, fixed(" us "))
sum(ctry_hits$us_minus) # 41275

sum(ctry_hits$US) # 95998
ctry_hits$US <- ctry_hits$US - ctry_hits$us_minus + ctry_hits$us_plus
sum(ctry_hits$US) # 75396

ctry_hits$us_minus <- ctry_hits$us_plus <- NULL


# Export ####
# gz compressed version
write_rds(ctry_hits, "./large_data/CountryMentions_NM-dict_ParagraphLevel.rds", compress = "gz")


