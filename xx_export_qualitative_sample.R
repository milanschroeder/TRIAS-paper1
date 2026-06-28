# export for qualitative analysis:

# Authors:  @milanschroeder 

# Packages
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v2.0.0 


docs <- read_rds("./large_data/DocumentLevelData.rds")
docs <- left_join(docs,
                  read_rds("./data/DocLevelData_zs.rds") %>%
                    select(-any_of(names(docs)[names(docs) %in% names(read_rds("./data/DocLevelData_zs.rds"))]), doc_key),
                  by = "doc_key"
) %>%
  mutate(
    period = case_when(
      date >= as_date("2018-12-01") ~ "Huawei",
      date >= as_date("2018-03-01") ~ "Cambridge Analytica",
      date >= as_date("2015-10-01") ~ "Digital Silk Road",
      date >= as_date("2013-06-01") ~ "Snowden",
      date >= as_date("2012-12-01") ~ "WCIT",
      date >= as_date("2010-07-01") ~ "Stuxnet",
      date >= as_date("2005-11-01") ~ "WSIS 2005",
      date >= as_date("2003-12-01") ~ "WSIS 2003",
      TRUE ~ "before"
    ) %>% as.factor()
  )

paras <- read_rds("./large_data/ParagraphLevelData.rds")
paras <- left_join(paras,
                   read_rds("./data/ParaLevelData_zs.rds") %>%
                     select(-any_of(names(paras)[names(paras) %in% names(read_rds("data/ParaLevelData_zs.rds"))]), id),
                   by = "id"
)


# When does a document matter for digitality?
# Taking initial choices here as the NLI classification is not likely to be finished in time and 
# as the sem_simil classification on the para level creates false positives 

docs$digitalshare_sim <- docs$digitalpara/docs$npara
hist(docs$digitalshare_sim) # sem_simil
# hist(docs$share_digital_para) # max_subtopic
docs$digitalshare <- docs$share_digital_para

sum(docs$digitalshare_sim >= .2) # At least 1/5 of paras has to be relevant for digitality to count the doc in, 7719

docs$digitalsimil <- docs$digitalshare_sim >= .2 # sem_simil
docs$digital_zs <- docs$share_digital_para >= .2 # zs
docs$digital <- docs$digital_zs

# digital docs only: ####
dp_docs <-
  docs %>%
  filter(digital)

# export for qualitative inspection: ####
### GP? ####
GP_mentions <- 
  docs %>% 
  filter(cm_CN_wider > 0 & US > 0) %>% 
  mutate(link = paste0("https://ec.europa.eu/commission/presscorner/", doc_key),
         digitalshare_zs = share_digital_para,
         co_mentions = cm_CN_wider + US,
         relevance = co_mentions*digitalshare_zs*digitalshare_sim) %>% 
  select(doc_key, link, title, date, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider) %>% 
  arrange(-(relevance)) %>% 
  left_join(.,
            paras %>% 
              filter(str_detect(text_para, "(?i)China|Beijing|\\bUSA\\b|United States|Washington")) %>% 
              group_by(doc_key) %>% 
              summarise(text_para = paste(text_para, collapse = "\n[...]\n")) %>% 
              ungroup(),
            join_by(doc_key)
  ) %>% 
  select(link, title, date, text_para, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider)
  
  
  #xlsx::write.xlsx(GP_mentions, "../../../../schroeder/Nextcloud/Shared/TRIAS Brückenprojekt/co_mentions_superpowers_digitality.xlsx")


### Event Mentions: ####
event_mentions <- 
  paras %>% 
  filter(str_detect(text_para, "(?i)WSIS|World Summit on the Information Society|Stuxnet|WCIT|World Conference on International Telecommunications|Snowden|Digital Silk Road|Cambridge Analytica|Huawei"
  )) %>% 
  select(text_para, date, title_long, doc_key) %>% 
  arrange(date) %>% 
  group_by(date, title_long, doc_key) %>% 
  summarise(text_para = paste(text_para, collapse = "\n[...]\n")) %>% 
  ungroup() %>% 
  left_join(., 
            docs %>%
              mutate(link = paste0("https://ec.europa.eu/commission/presscorner/", doc_key),
                     digitalshare_zs = share_digital_para,
                     co_mentions = cm_CN_wider + US,
                     relevance = co_mentions*digitalshare_zs*digitalshare_sim) %>% 
              select(link, doc_key, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider),
            join_by(doc_key)) %>% 
  select(link, title = title_long, date, text_para, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider)
  

# xlsx::write.xlsx(event_mentions, "../../../../schroeder/Nextcloud/Shared/TRIAS Brückenprojekt/event_mentions.xlsx")


### Legislation Mentions: ####
DSA_DSM_mentions <- 
  paras %>% 
  filter(str_detect(text_para, "(?i)\\bDSA\\b|Digital Services Act|Single Market For Digital Services|2020/0361[/COD]|\\bDMA\\b|Digital Markets Act|contestable and fair markets in the digital sector|2020/0374[/COD]") &
           str_detect(text_para, "Dexia", negate=T)
         ) %>% 
  arrange(date) %>% 
  group_by(date, title_long, doc_key) %>% 
  summarise(text_para = paste(text_para, collapse = "\n[...]\n")) %>% 
  ungroup() %>% 
  left_join(., 
            docs %>%
              mutate(link = paste0("https://ec.europa.eu/commission/presscorner/", doc_key),
                     digitalshare_zs = share_digital_para,
                     co_mentions = cm_CN_wider + US,
                     relevance = co_mentions*digitalshare_zs*digitalshare_sim) %>% 
              select(link, doc_key, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider),
            join_by(doc_key)) %>% 
    select(link, title = title_long, date, text_para, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider) 
    

# xlsx::write.xlsx(DSA_DSM_mentions, "../../../../schroeder/Nextcloud/Shared/TRIAS Brückenprojekt/DSA_DSM_mentions.xlsx")



### European Third Way Mentions: ####
EUWay_mentions <- 
  paras %>% 
  filter(str_detect(text_para, "(?i)European Way|Third Way"
  )) %>% 
  select(text_para, date, title_long, doc_key) %>% 
  arrange(date) %>% 
  group_by(date, title_long, doc_key) %>% 
  summarise(text_para = paste(text_para, collapse = "\n[...]\n")) %>% 
  ungroup() %>% 
  left_join(., 
            docs %>%
              mutate(link = paste0("https://ec.europa.eu/commission/presscorner/", doc_key),
                     digitalshare_zs = share_digital_para,
                     co_mentions = cm_CN_wider + US,
                     relevance = co_mentions*digitalshare_zs*digitalshare_sim) %>% 
              select(link, doc_key, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider),
            join_by(doc_key)) %>% 
  select(link, title = title_long, date, text_para, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider) %>% 
  arrange(-digitalshare_zs)


# xlsx::write.xlsx(EUWay_mentions, "../../../../schroeder/Nextcloud/Shared/TRIAS Brückenprojekt/EUWay_mentions.xlsx")

### Global Actor Mentions: ####
globalEU_mentions <- 
  paras %>% 
  filter(str_detect(text_para, "(?i)Global Actor|Global EU"
  )) %>% 
  select(text_para, date, title_long, doc_key) %>% 
  arrange(date) %>% 
  group_by(date, title_long, doc_key) %>% 
  summarise(text_para = paste(text_para, collapse = "\n[...]\n")) %>% 
  ungroup() %>% 
  left_join(., 
            docs %>%
              mutate(link = paste0("https://ec.europa.eu/commission/presscorner/", doc_key),
                     digitalshare_zs = share_digital_para,
                     co_mentions = cm_CN_wider + US,
                     relevance = co_mentions*digitalshare_zs*digitalshare_sim) %>% 
              select(link, doc_key, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider),
            join_by(doc_key)) %>% 
  select(link, title = title_long, date, text_para, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider) %>% 
  arrange(-digitalshare_zs)


# xlsx::write.xlsx(globalEU_mentions, "../../../../schroeder/Nextcloud/Shared/TRIAS Brückenprojekt/globalEU_mentions.xlsx")

# literal "GP" mentions ####

geopolitics_mentions <- 
  paras %>% 
  filter(str_detect(text_para, "(?i)geopolitic"
  )) %>% 
  select(text_para, date, title_long, doc_key) %>% 
  arrange(date) %>% 
  group_by(date, title_long, doc_key) %>% 
  summarise(text_para = paste(text_para, collapse = "\n[...]\n")) %>% 
  ungroup() %>% 
  left_join(., 
            docs %>%
              mutate(link = paste0("https://ec.europa.eu/commission/presscorner/", doc_key),
                     digitalshare_zs = share_digital_para,
                     co_mentions = cm_CN_wider + US,
                     relevance = co_mentions*digitalshare_zs*digitalshare_sim) %>% 
              select(link, doc_key, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider),
            join_by(doc_key)) %>% 
  select(link, title = title_long, date, text_para, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider)

geopolitics_mentions %>%
  filter(date > as.Date("2004-01-01")) %>% 
  mutate(month = zoo::as.yearmon(date)) %>% 
  group_by(month) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = month, y = count)) +
  geom_point() +
  geom_smooth(se = F) +
  theme_light() + 
  geom_vline(linetype = "dashed", xintercept = zoo::as.yearmon("2019-12-01")) +
  xlab("") +
  ylab('"geopolitics" in Commission public communication') +
  coord_cartesian(xlim = c(zoo::as.yearmon(as.Date("2004-10-01")), zoo::as.yearmon(as.Date("2023-03-01"))))
  

### digital sovereignty Mentions: ####
digital_sovereignty_mentions <- 
  paras %>% 
  filter(str_detect(text_para, "(?i)digital sovereign"
  )) %>% 
  select(text_para, date, title_long, doc_key) %>% 
  arrange(date) %>% 
  group_by(date, title_long, doc_key) %>% 
  summarise(text_para = paste(text_para, collapse = "\n[...]\n")) %>% 
  ungroup() %>% 
  left_join(., 
            docs %>%
              mutate(link = paste0("https://ec.europa.eu/commission/presscorner/", doc_key),
                     digitalshare_zs = share_digital_para,
                     co_mentions = cm_CN_wider + US,
                     relevance = co_mentions*digitalshare_zs*digitalshare_sim) %>% 
              select(link, doc_key, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider),
            join_by(doc_key)) %>% 
  select(link, title = title_long, date, text_para, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider) %>% 
  arrange(-digitalshare_zs)


# xlsx::write.xlsx(digital_sovereignty_mentions, "../../../../schroeder/Nextcloud/Shared/TRIAS Brückenprojekt/digital_sovereignty_mentions.xlsx")


### sovereignty Mentions: ####
sovereignty_mentions <- 
  paras %>% 
  filter(str_detect(text_para, "(?i)sovereign"
  )) %>% 
  select(text_para, date, title_long, doc_key) %>% 
  arrange(date) %>% 
  group_by(date, title_long, doc_key) %>% 
  summarise(text_para = paste(text_para, collapse = "\n[...]\n")) %>% 
  ungroup() %>% 
  left_join(., 
            docs %>%
              mutate(link = paste0("https://ec.europa.eu/commission/presscorner/", doc_key),
                     digitalshare_zs = share_digital_para,
                     co_mentions = cm_CN_wider + US,
                     relevance = co_mentions*digitalshare_zs*digitalshare_sim) %>% 
              select(link, doc_key, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider),
            join_by(doc_key)) %>% 
  select(link, title = title_long, date, text_para, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider) %>% 
  arrange(-digitalshare_zs)


# xlsx::write.xlsx(sovereignty_mentions, "../../../../schroeder/Nextcloud/Shared/TRIAS Brückenprojekt/sovereignty_mentions.xlsx")

# plot sovereignty: ####
ggplot() + 
  geom_histogram(aes(x = date), fill = "blue", data = sovereignty_mentions) + 
  geom_histogram(aes(x = date), fill = "red", data = digital_sovereignty_mentions) +
  theme_minimal()

sov_terms <- paras %>% 
  filter(str_detect(text_para, "(?i)sovereign"
  )) %>% 
  mutate(
    sov_term = str_extract(text_para, "(?i)(\\b[\\w'-]+\\b)?\\s*(\\b\\w*sovereign\\w*\\b)\\s*(\\b\\w+\\b)?" 
  ) %>% str_to_lower(.) %>% 
    str_remove_all(paste0("\\b", stopwords::stopwords(), collapse = "\\b|\\b", "\\b|[[:digit:]]+")) %>% 
    str_squish(.),
  sov_context = str_extract(text_para, "(?i)(\\b[\\w'-]+\\b)?\\s*(\\b[\\w'-]+\\b)?\\s*(\\b\\w*sovereign\\w*\\b)\\s*(\\b[\\w'-]+\\b)?\\s*(\\b[\\w'-]+\\b)?" 
  ) %>% str_to_lower(.) %>% 
    str_squish(.)
  ) %>% 
  select(sov_term, sov_context, text_para, date, title_long, doc_key) %>% 
  arrange(date) 

sov_terms_count <- 
  sov_terms %>% 
  group_by(sov_term) %>% 
  summarise(term_count = n()) %>%
  ungroup() %>% 
  arrange(-term_count)

plotly::ggplotly(  
sov_terms %>%
  mutate(sov_term = fct_lump_min(sov_term, min = 10),
         year = year(date)) %>%
  group_by(year, sov_term) %>% 
  summarise(term_count = n()) %>% 
  ggplot() +
  geom_line(aes(x = year, y = term_count, colour = sov_term)) +
  theme_minimal()
)

# %>% 
  summarise(sov_contexts = paste(sov_context, collapse = "\n[...]\n")) %>% 
  ungroup() %>% 
  left_join(., 
            docs %>%
              mutate(link = paste0("https://ec.europa.eu/commission/presscorner/", doc_key),
                     digitalshare_zs = share_digital_para,
                     co_mentions = cm_CN_wider + US,
                     relevance = co_mentions*digitalshare_zs*digitalshare_sim) %>% 
              select(link, doc_key, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider),
            join_by(doc_key)) %>% 
  select(link, title = title_long, date, text_para, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider) %>% 
  arrange(-digitalshare_zs)

