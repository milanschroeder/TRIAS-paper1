# Visual / descriptive analyses of extracted text data
# Authors:  @ChRauh 

# Packages
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v2.0.0 
library(countrycode)
library(flextable)


# Preparations: text data, ids and meta data ####

all_texts <- read_rds("./data/all_texts.rds") # Paragraph level
all_meta <- read_rds("./data/all_meta.rds") # Document level

# doc_keys of all english-language documents in the database
# We filter along those lines for now

doc_keys_en <- all_meta %>% 
  filter(lang == "en") %>% 
  select(doc_key, doc_type)



# Corpus descriptives ####

# Number of docs for analysis
nrow(doc_keys_en) # 76941 - number of documents for first paper
nrow(doc_keys_en) == nrow(unique(doc_keys_en)) # Should be TRUE

# Number of documents by type
doc_types <- table(doc_keys_en$doc_type, useNA = "ifany") %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  mutate(Var1 = as.character(Var1)) %>% 
  rename(`Document type` = 1,
         N = 2)
doc_types[nrow(doc_types)+1,] <- c("Total", sum(doc_types$N))
doc_types$`Document type`[is.na(doc_types$`Document type`)] <- "Not specified" # What are those? (53)
t <- flextable(doc_types)
save_as_docx("Type of documents in TRIAS Corpus (english-language only for now)" = t, 
             path = "./output/tables/Document_Types.docx")
rm(doc_types, t)


  




