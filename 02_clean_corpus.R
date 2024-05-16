# Clean EC Corpus paragraphs

# Input: tibble all_texts (both from 01_read_corpus.R)
# Output: data/all_texts.rds


library(tidyverse)
library(magrittr) # for assignment pipe %<>% 
library(cld2) # for language identification

# add IDs (starting with 0 for easy python compatibility)
all_texts %<>% 
  rownames_to_column("id") %>% mutate(id = as.numeric(id) -1)


# fix titles ####
str_count(all_texts$text_para, "<br>") %>% sum() # 18717 html breaks in plain text...
all_texts %>% filter(., str_detect(text_para, "<br>")) %>% count(para_type) # ...but only in titles


# paste titles together:

titles <- 
  all_texts %>% 
  filter(para_type == "title") %>%
  group_by(doc_key) %>%
  summarise(text_para = str_c(text_para, collapse = " "),
            id = min(id)) %>% 
  ungroup() %>% 
  left_join(.,
            all_texts %>% 
              filter(para_type == "title") %>% 
              select(-text_para) %>% 
              distinct(id, .keep_all = T), 
            by = join_by(id, doc_key)
  )


all_texts %<>% 
  filter(para_type != "title") %>% 
  bind_rows(.,
            titles)

rm(titles)

# fix paragraphs ####

# ill-formatted paragraphs in corpus:
str_count(all_texts$text_para, "\n") %>% sum() # 128422 newlines...
str_detect(all_texts$text_para, "\n") %>% sum() # ... resulting in 42163 multi-paragraph paragraphs

all_texts %<>% 
  tidyr::separate_rows(text_para, sep = "\n") %>% # split at newline

# remove uninformative/service paragraphs (not fully exhaustive, but those are reliable):
  mutate(
    text_para = str_remove(text_para, "^IP.*? "), # IP num, doesn't capture anything about IP-address etc., as only on paragraph start
    text_para = str_remove(text_para, "^Brussels.*?[0-9]{4} "),
    # remove "check against delivery" (variants) from speeches:
    text_para = str_remove(text_para, 
                           "(?i)[[:print:]pleas]*check ag[ainstdwh delvry[:print:]]*"),
    # some in different languages catched by hand:
    text_para = str_remove(text_para, "\\[check against\\]|\\[check as delivery\\]|[:graph:]*muutokset mahdollisia puhuttaessa|[:graph:]*muutokset puhuttaessa mahdollisia|[:graph:]*Seul le prononcé fait foi[:graph:]*|[:graph:]*Seul le prononcé fait[:graph:]*|puhuttaessa mahdollisia|mahdollisia puhuttaessa"))

  ### shouldnt this be later, if at all??? 
   #  text_para = str_remove_all(text_para, "<[^>]*>")
  all_texts %>% summarize(sum(str_detect(text_para, "<[^>]*>"))) # 18175 <...> 

# cleaning split up bc complex regex challanges memory...
all_texts %<>% 
# mark ill-formatted list elements as list (not fully exhaustive, but those are reliable):
  mutate(para_type = ifelse(str_detect(text_para, "\\[if !supportLists\\]|.") | 
                              str_starts(text_para, "·|-·|o "), 
                            "list", 
                            para_type)) %>% 
  
# remove paragraphs without any meaningful content (styles etc.)
  filter(., str_detect(text_para, "PRV_ENDREF|Font Definitions|Style Definitions|view_mode|\\[if gte mso 9\\]", negate = T)) %>% 

# remove html etc. from paragraphs with meaningful contents: 
  mutate(text_para = str_remove_all(text_para, '<br>|</b>|<b>|<sup>|</sup>|<sub>|</sub>|<p align=\"center\">|\\[if !supportLists\\]|\\[endif\\]|body p \\{ text-align: justify; \\}|<!-- PRV_REF[^>]*-->')) %>% 

# some more basic cleaning:
  mutate(text_para = str_squish(text_para), # remove excessive whitespaces
         n_chars_para = str_length(text_para)) %>% # count characters per paragraph 
    filter(., text_para != "") %>% # remove empty paragraphs
    filter(., str_detect(text_para, "\\w")) %>%  # remove paragraphs without any word characters
    
# detect language after cleaning:
    mutate(lang = cld2::detect_language(text_para)) %>%
    
# correct doc_pos after tidying up:
    group_by(doc_key, para_type) %>% 
    mutate(doc_pos = dplyr::row_number()) %>% 
    ungroup() %>% 
    
    mutate(
      
# correct id after tidying up
      id = dplyr::row_number() - 1, # for easy compatability with python classifications
      ) %>% 
  
  # add doc_type
  left_join(.,
            all_meta %>% select(doc_key, doc_type), 
            join_by(doc_key)
            )
    
# save/read ####
  readr::write_rds(all_texts, "data/all_texts.rds")
    