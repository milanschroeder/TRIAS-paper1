library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v2.0.0 


paras <- read_rds("data/all_texts.rds")
docs <- read_rds("./data/all_meta.rds")

# Document types in corpus

doc_types <- docs %>% 
  mutate(
    year = lubridate::year(date)
    # year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()
  ) %>% 
  filter(year < 2024) %>% 
  group_by(doc_type) %>% 
  summarise(count = n(),
            mean_paras = round(mean(n_paras, na.rm = T), 2),
            min_date = min(date, na.rm = T),
            max_date = max(date, na.rm = T),
            en_share = round(mean(lang == "en", na.rm = T), 4)*100) %>% # percentage
  filter(!is.na(doc_type)) %>% 
  arrange(desc(count))


# Wordocunts

wc <- paras %>% 
  mutate(wordcount = str_count(text_para, "\\W+")+1) %>% 
  group_by(doc_key) %>% 
  summarise(wordcount = sum(wordcount)) %>% 
  ungroup() %>% 
  left_join(docs %>% select(doc_key, doc_type), by = "doc_key") %>% 
  group_by(doc_type) %>% 
  summarise(mean_wc = round(mean(wordcount), 2))

doc_types <- doc_types %>% 
  left_join(wc, by = "doc_type") %>% 
  relocate(doc_type, count, mean_paras, mean_wc)

# Sumnmary row

summ <- data.frame(doc_type = "Totals/Averages",
                  count = sum(doc_types$count),
                  mean_paras = mean(doc_types$mean_paras),
                  mean_wc = mean(doc_types$mean_wc),
                  min_date = min(doc_types$min_date),
                  max_date = max(doc_types$max_date),
                  en_share = mean(doc_types$en_share))

doc_types <- rbind(doc_types, summ)

# Export
write_csv2(doc_types, "./output/tables/CorpusOverview.csv")


# Covered in the paper

covered <- paras %>% 
  select(-doc_type) %>% # uses different codes for some reason
  left_join(docs %>% select(doc_key, doc_type, date), by = "doc_key") %>% 
  mutate(
    year = lubridate::year(date)
    # year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()
  ) %>% 
  filter(year < 2024) %>% 
  filter(year >= 1997) %>% 
  filter(doc_type %in% c("Speech", "Press release", "Statement")) %>% 
  filter(lang == "en")

covered %>% 
  filter(!duplicated(doc_key)) %>% 
  group_by(doc_type) %>% 
  summarise(count = n())
  









