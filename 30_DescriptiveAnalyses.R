# Visual / descriptive analyses of extracted text data
# Authors:  @ChRauh, @milanschroeder 

# Packages
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v2.0.0 
library(countrycode)
library(flextable)
library(magrittr)
library(zoo)
library(ggtext)


# Assemble extracted text data ####
# Paragraph level

paras <- 
  # Start from text data, only this one holds the numeric paragraph id
  read_rds("./data/all_texts.rds") %>% 
  select(doc_key, id, text_para) %>% 
  mutate(wordcount = str_count(text_para, "\\W+")+1) %>% 
  # Add meta data, document-level
  left_join(., 
            read_rds("./data/all_meta.rds") %>% 
              select(doc_key, date, doc_type, lang, title_long),
            by = "doc_key") %>% 
  # Add semantic similarities, paragraph level (only identified by id)
  left_join(read_rds("./large_data/SemanticSimilarityCodes_ParagraphLevel.rds"),
            by = "id") %>% 
  # Mark paragraphs that speak about digital affairs
  # For now using the semantic similarity to the advanced digital affairs dictionary
  # at a cut-off that maximizes F1
  # ADAPT THIS LINE TO THE FINALLY CHOSEN MEASURE
  mutate(digital = digital_simil_advanced >= .18) %>% 
  mutate_at(vars(digital), ~replace_na(., 0)) %>% # Missing bc out-of-dictionary paras
  select(-starts_with("digital_")) %>% 
  # Add country mentions
  left_join(read_rds("./large_data/CountryMentions_NM-dict_ParagraphLevel.rds") %>% 
              select(-doc_key),
            by = "id") %>% 
  # Filter by document types to be used in the paper
  filter(doc_type %in% c("Speech", "Press release", "Statement")) %>% 
  # Filter non-english texts
  filter(lang == "en") %>% 
  select(-lang) %>% 
  # Column order
  relocate(id, doc_pos, doc_key, doc_type, title_long, text_para, date, wordcount, digital, contains("_simil"))
  
gc() # Free memory 


# Pick cases for qualitative examples in RD section of text ####
# Looking through these examples: Many false positives - we need more precision ;)

# Econ simil
paras %>% distinct(text_para, .keep_all = T) %>% 
  filter(digital) %>% 
  filter(wordcount > 10) %>% 
  arrange(desc(economy_simil)) %>% 
  select(c(id, text_para, economy_simil, doc_key)) %>% 
  head(15)
paras$text_para[paras$id == 331845] 

paras %>% distinct(text_para, .keep_all = T) %>%
  filter(!digital) %>% 
  filter(wordcount > 10) %>% 
  arrange(desc(economy_simil)) %>% 
  select(c(id, text_para, economy_simil, doc_key)) %>% 
  head(15) 
paras$text_para[paras$id == 594929] 

# Security simil
paras %>% distinct(text_para, .keep_all = T) %>%
  filter(digital) %>% 
  filter(wordcount > 10) %>% 
  arrange(desc(security_simil)) %>% 
  select(c(id, text_para, economy_simil, doc_key)) %>% 
  head(15)
paras$text_para[paras$id == 1923360] 

paras %>% distinct(text_para, .keep_all = T) %>%
  filter(!digital) %>% 
  filter(wordcount > 10) %>% 
  arrange(desc(security_simil)) %>% 
  select(c(id, text_para, economy_simil, doc_key)) %>% 
  head(15)
paras$text_para[paras$id == 222898] 

# Liberal rights
paras %>% distinct(text_para, .keep_all = T) %>%
  filter(digital) %>% 
  filter(wordcount > 10) %>% 
  arrange(desc(librights_simil)) %>% 
  select(c(id, text_para, economy_simil, doc_key)) %>% 
  head(15)
paras$text_para[paras$id == 173325] 

paras %>% distinct(text_para, .keep_all = T) %>%
  filter(!digital) %>% 
  filter(wordcount > 10) %>% 
  arrange(desc(librights_simil)) %>% 
  select(c(id, text_para, economy_simil, doc_key)) %>% 
  head(15)
paras$text_para[paras$id == 22645] # Nice one!



# Aggregate data to document level ####
 
docs <- 
  paras %>%  
  group_by(doc_key) %>% 
  # Different aggregations across variables
  summarise(doc_type = unique(doc_type), # Static vars (is there a smarter coding approach?)
            title = unique(title_long),
            date = unique(date),
            npara = n(), # Count of paragraphs in doc
            digitalpara = sum(digital), # Count of digital paragraphs
            across(economy_simil:conf_coop, mean, na.rm = T), # Semantic similarities averaged across paras (NA removal slows things down drastically)
            across(BI:WS, sum)) %>% # Count of country mentions per doc
  ungroup() %>% 
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
  ) %>% 
  arrange(date, doc_key) 

### save for later:
 write_rds(docs, "./large_data/DocumentLevelData.rds", compress = "gz")
 write_rds(paras, "./large_data/ParagraphLevelData.rds", compress = "gz")


# join ZS Classifications and aggregations from 31_match_geopolitics.R:

 docs <- read_rds("./large_data/DocumentLevelData.rds")
 docs <- left_join(docs,
                  read_rds("./data/DocLevelData_zs.rds") %>%
                    select(-any_of(names(docs)[names(docs) %in% names(analysis_data_doclevel)]), doc_key),
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

sum(docs$digitalshare_sim >= .2) # At least 1/5 of paras has to be relevant for digitality to count the doc in, 7719

docs$digitalsimil <- docs$digitalshare_sim >= .2 # sem_simil
docs$digital_zs <- docs$share_digital_para >= .2 # zs
docs$digital <- docs$digitalsimil

# digital docs only: ####
dp_docs <-
  docs %>%
  filter(digital)

# export for qualitative inspection:
docs %>% 
  filter(cm_CN_wider > 0 & US > 0) %>% 
  mutate(link = paste0("https://ec.europa.eu/commission/presscorner/", doc_key),
         digitalshare_zs = share_digital_para,
         co_mentions = cm_CN_wider + US,
         relevance = co_mentions*digitalshare_zs*digitalshare_sim) %>% 
  select(link, title, date, relevance, co_mentions, digital_zs, digitalsimil, digitalshare_sim, digitalshare_zs, US, CN, cm_CN_wider) %>% 
  arrange(-(relevance)) %>% 
  xlsx::write.xlsx(., "../../../../schroeder/Nextcloud/Shared/TRIAS Brückenprojekt/co_mentions_superpowers_digitality.xlsx")

gc()


# qualitative examples: ####
docs %>%
  arrange(desc(digitalshare)) %>% 
  select(npara, digitalshare, title) %>% 
  head(15) # Strong bias to the single paragraph documents in the early period ... grr

docs %>% 
  mutate(
    year = lubridate::year(date)
    # year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()
    ) %>% 
  group_by(year) %>% 
  summarise(npara = mean(npara)) %>% 
  ggplot(aes(x=year, y = npara, group = 1))+
  geom_line() # From 1997 onewwards we ha ve a reasonable number of paras per doc only

docs %>% 
  mutate(
    year = lubridate::year(date)
    # year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()
  ) %>% 
  filter(year >= 1997) %>% 
  arrange(desc(digitalshare)) %>% 
  select(npara, digitalshare, title) %>% 
  head(15) # Looks very reasonable, stick with this one for now


# Quick checks ####

# hist(docs$digital) # Long right tail
# test <- docs %>% filter(digital > 50) # Press releases with attached docs 
# table(test$doc_type) # Mainly looong speeches 
# 
# docs %>% 
#   select(contains("simil")) %>% 
#   pivot_longer(cols = everything()) %>% 
#   ggplot(aes(x=value, color = name))+
#   geom_density()


# Emphasis of digital affairs over time ####

# Document level ####
# Share of docs with high prevalence of 'digital' paragraphs (see above)

df <- docs %>% 
#  select(date, digitalshare) %>% 
  group_by(date) %>% 
  mutate(
    # digitalshare_day = sum(digitalsimil, na.rm = T)/ n(),
    digitalshare_day = sum(digital, na.rm = T)/ n(),
    year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% # Not using data prior to 1997 !
  ungroup() %>% 
  filter(year >= 1997) %>% 
  mutate(month = as.character(date) %>% str_remove("-[0-9]{2}$")) %>% 
  relocate(month) %>% 
  group_by(month) %>% 
  # mutate(digitalshare_month = sum(digitalsimil, na.rm = T) / n()) %>% 
  mutate(digitalshare_month = sum(digital, na.rm = T) / n()) %>% 
  ungroup() %>% 
  mutate(
    period = case_when(
      as.Date(paste0(month, '-01')) >= as_date("2018-12-01") ~ "Huawei",
      as.Date(paste0(month, '-01')) >= as_date("2018-03-01") ~ "Cambridge Analytica",
      as.Date(paste0(month, '-01')) >= as_date("2015-10-01") ~ "Digital Silk Road",
      as.Date(paste0(month, '-01')) >= as_date("2013-06-01") ~ "Snowden",
      as.Date(paste0(month, '-01')) >= as_date("2012-12-01") ~ "WCIT",
      as.Date(paste0(month, '-01')) >= as_date("2010-07-01") ~ "Stuxnet",
      as.Date(paste0(month, '-01')) >= as_date("2005-11-01") ~ "WSIS 2005",
      as.Date(paste0(month, '-01')) >= as_date("2003-12-01") ~ "WSIS 2003",
      TRUE ~ "before"
    ) %>% as.factor()
  )


# Plotting parameters
breaks <- 
  df %>% 
  select(month) %>% 
  unique() %>% 
  filter(str_detect(month, "-01")) %>%
  mutate(month = as.Date(paste0(month, "-01"))) %>% 
  pull()
labels <- breaks %>% str_remove_all("-01") 

# Plot
# @ Milan - why are you smooting on daily values here?
# In my reading this is overly excessive in terms of computation and makes the confidence intervalls of the lm smaller than they should be (for the monthyl series)
# But happy to learn, if I'm missing something ...
pl.dp_salience_events <- 
  ggplot(df) +
  geom_point(aes(x = as.Date(paste0(month, "-15")), y= digitalshare_month), colour = "#619933") +
  geom_smooth(aes(x = date, y = digitalshare_day, group = period), 
              method = "lm", se = T, color = "black") +

  scale_x_continuous(breaks = breaks, labels = labels) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Salience of digital affairs in the public communication of the European Commission",
       subtitle = paste0("Monthly share of Commission press releases, speeches, and statements that contain at least 20% of pargraphs classified as \'digital affairs\'"),
       x = "",
       y= "",
       caption = "Monthly time-series with linear smooth;\nBreaks represent major geopolitical events relating to digital affairs.") +
  theme_bw() +
  theme(legend.position = "none",
        # text=element_text(family = "Dahrendorf"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(face = "bold"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", size = 14)) +
  geom_vline(xintercept = as.Date("2003-12-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2005-11-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2010-07-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2012-12-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2013-06-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2015-10-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-03-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-12-01"), linetype = "dotted") +
  annotate("text", 
           x = c(as.Date("2003-12-01"), as.Date("2005-11-01"), as.Date("2010-07-01"), as.Date("2012-12-01"), 
                 as.Date("2013-06-01"), as.Date("2015-10-01"), as.Date("2018-03-01"), as.Date("2018-12-01")) + 111,
           y = .495,
           label = c("WSIS 2003", "WSIS 2005", "Stuxnet", "WCIT", "Snowden", "Digital Silk Road", "Cambridge Analytica", "Huawei"),
           angle = 270, size = 2.3, hjust = 0
                 )

pl.dp_salience_events

ggsave("./output/plots/dp_salience_events_DocLevel.png", width = 26, height = 15, units = "cm")



# Paragraph level 

df <- paras %>% 
  select(date, digital) %>% 
  mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric(), # Not using data prior to 1997 !
         month = as.character(date) %>% str_remove("-[0-9]{2}$")) %>% 
  filter(year >= 1997) %>% 
  relocate(month) %>% 
  group_by(month) %>% 
  summarise(digitalshare_month = sum(digital, na.rm = T) / n()) %>% 
  ungroup() %>% 
  mutate(
    period = case_when(
      as.Date(paste0(month, '-01')) >= as_date("2018-12-01") ~ "Huawei",
      as.Date(paste0(month, '-01')) >= as_date("2018-03-01") ~ "Cambridge Analytica",
      as.Date(paste0(month, '-01')) >= as_date("2015-10-01") ~ "Digital Silk Road",
      as.Date(paste0(month, '-01')) >= as_date("2013-06-01") ~ "Snowden",
      as.Date(paste0(month, '-01')) >= as_date("2012-12-01") ~ "WCIT",
      as.Date(paste0(month, '-01')) >= as_date("2010-07-01") ~ "Stuxnet",
      as.Date(paste0(month, '-01')) >= as_date("2005-11-01") ~ "WSIS 2005",
      as.Date(paste0(month, '-01')) >= as_date("2003-12-01") ~ "WSIS 2003",
      TRUE ~ "before"
    ) %>% as.factor()
  )

# Plot
pl.dp_salience_events <- 
  ggplot(df) +
  geom_point(aes(x = as.Date(paste0(month, "-15")), y= digitalshare_month), colour = "#619933") +
  geom_smooth(aes(x = as.Date(paste0(month, "-15")), y = digitalshare_month, group = period), 
              method = "lm", se = T, color = "black") +
  
  scale_x_continuous(breaks = breaks, labels = labels) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Salience of digital affairs in the public communication of the European Commission",
       subtitle = paste0("Monthly share of paragraphs in Commission press releases, speeches, and statements that are classified as \'digital affairs\'"),
       x = "",
       y= "",
       caption = "Monthly time-series with linear smooth;\nBreaks represent major geopolitical events relating to digital affairs.") +
  theme_bw() +
  theme(legend.position = "none",
        # text=element_text(family = "Dahrendorf"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(face = "bold"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", size = 14)) +
  geom_vline(xintercept = as.Date("2003-12-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2005-11-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2010-07-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2012-12-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2013-06-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2015-10-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-03-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-12-01"), linetype = "dotted") +
  annotate("text", 
           x = c(as.Date("2003-12-01"), as.Date("2005-11-01"), as.Date("2010-07-01"), as.Date("2012-12-01"), 
                 as.Date("2013-06-01"), as.Date("2015-10-01"), as.Date("2018-03-01"), as.Date("2018-12-01")) + 111,
           y = .22,
           label = c("WSIS 2003", "WSIS 2005", "Stuxnet", "WCIT", "Snowden", "Digital Silk Road", "Cambridge Analytica", "Huawei"),
           angle = 270, size = 2.3, hjust = 0
  )

pl.dp_salience_events

ggsave("./output/plots/dp_salience_events_ParaLevel.png", width = 26, height = 15, units = "cm")



# Country mentions // "Geopolitics" ####

# Daily counts 

cm <- docs %>% 
  group_by(date) %>% 
  summarise(across(BI:WS, sum), # sum up per day
            n_docs = length(unique(doc_key))) %>% # docs per day
  rowwise() %>% mutate(cm_total = sum(c_across(BI:WS))) %>% # all country mentions per day
  ungroup() %>% 
  mutate(Year = year(date),
         Month = zoo::as.yearmon(date),
         across(BI:WS, ~ .x / n_docs, .names = "{col}_per_doc"),
         across(BI:WS, ~ if_else(cm_total != 0, .x / cm_total * 100, 0), .names = "{col}_share"), # % of all country mentions
  )%>% 
  select(date, Month, Year, n_docs, cm_total, everything())

# total counts (for writing)

country_freqs <- cm %>% 
  pivot_longer(cols = BI:WS, names_to = "Country", values_to = "Country Mentions") %>% 
  group_by(Country) %>% 
  summarise(`Country Mentions` = sum(`Country Mentions`)) %>% 
  arrange(-`Country Mentions`)


cm_digital <- docs %>% 
  
  # only articles with some digitality:
  filter(digital) %>% 
  
  group_by(date) %>% 
  summarise(across(BI:WS, sum), # sum up per day
            n_docs = length(unique(doc_key))) %>% # docs per day
  rowwise() %>% mutate(cm_total = sum(c_across(BI:WS))) %>% # all country mentions per day
  ungroup() %>% 
  mutate(Year = year(date),
         Month = zoo::as.yearmon(date),
         across(BI:WS, ~ .x / n_docs, .names = "{col}_per_doc"),
         across(BI:WS, ~ if_else(cm_total != 0, .x / cm_total * 100, 0), .names = "{col}_share"), # % of all country mentions
  )%>% 
  select(date, Month, Year, n_docs, cm_total, everything()) %>% 
  right_join(., cm %>% select(date), by = "date") # ensure all dates are present -> same structure as cm 


# total counts (for writing)

sum(cm_digital$n_docs, na.rm = T)

country_freqs_digital <- cm_digital %>% 
  pivot_longer(cols = BI:WS, names_to = "Country", values_to = "Country Mentions") %>% 
  group_by(Country) %>% 
  summarise(`Country Mentions` = sum(`Country Mentions`, na.rm = T)) %>% 
  arrange(-`Country Mentions`)


# differences between digital and non-digital country mentions:
cm_diff <- cbind(
  cm %>% select(date:cm_total),
  cm_digital %>% select(BI:WS) - cm %>% select(BI:WS)
)

# Which countries does the Commission talk about ? - Maps

# Prepare map template
library(maps)
library(countrycode)
world_map <- map_data(map = "world")
world_map$region <- maps::iso.alpha(world_map$region) # convert country name to ISO code


# Map all country mentions
pl.map <- 
  cm %>% 
  mutate(BE = NA) %>% # Set Belgium to missing, to avoid the "Brussels" bias
  mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% # No data before 1997, don't trust it
  filter(year >= 1997) %>% 
  pivot_longer(cols = BI:WS, names_to = "Country", values_to = "Country Mentions") %>% 
  group_by(Country) %>% 
  summarise(`Country Mentions` = sum(`Country Mentions`)) %>% 
  arrange(-`Country Mentions`) %>% 
  ggplot(., aes(map_id = Country)) +
  geom_map(aes(fill = `Country Mentions`), map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_continuous(name = "", high = "#0380b5", low = "grey95", na.value="white") +
  labs(title = "Which countries does the European Commission mention most often in its public communication?",
       subtitle = paste0("Count of literal country mentions in ",   nrow(docs %>% filter(date >= as.Date("1997-01-01"))), " english-language press releases, speeches, and statements published by the Commission 1997-2023"),
       caption = "Country mentions extracted along the Newsmap dictionaries (Watanabe 2018);\nBelgium deliberately set to missing.")+
  coord_fixed()+
  theme_void() +
  theme(legend.position = c(0.1, 0.3),
        plot.margin = unit(c(.3,.3,.3,.3), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", size = 14))
pl.map
ggsave("./output/plots/CountryMentions/WorldMapTotal.png", pl.map, width = 26, height = 15, units = "cm")





# Map digital country mentions
pl.map_digital <- 
  cm_digital %>% 
  mutate(BE = NA) %>% # Set Belgium to missing, to avoid the "Brussels" bias
  mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% # No data before 1997, don't trust it
  filter(year >= 1997) %>% 
  pivot_longer(cols = BI:WS, names_to = "Country", values_to = "Country Mentions") %>% 
  group_by(Country) %>% 
  summarise(`Country Mentions` = sum(`Country Mentions`, na.rm = T)) %>% 
  arrange(-`Country Mentions`) %>% 
  ggplot(., aes(map_id = Country)) +
  geom_map(aes(fill = `Country Mentions`), map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_continuous(name = "", high = "#619933", low = "grey95", na.value="white") +
  labs(title = "Which countries does the European Commission mention in its public communication on digital affairs?",
       subtitle = paste0("Count of literal country mentions in ",   nrow(docs %>% filter(date >= as.Date("1997-01-01") & digitalsimil)), " english-language press releases, speeches, and statements on digital affairs by the Commission 1997-2023"),
       caption = "Country mentions extracted along the Newsmap dictionaries (Watanabe 2018);\nBelgium deliberately set to missing.")+
  coord_fixed()+
  theme_void() +
  theme(legend.position = c(0.1, 0.3),
        plot.margin = unit(c(.3,.3,.3,.3), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", size = 14))
pl.map_digital
ggsave("./output/plots/CountryMentions/WorldMapDigital.png", width = 26, height = 15, units = "cm")



# Map digital country mentions - general country mentions
pl.map_diff <- 
  cm_diff %>% 
  mutate(BE = NA) %>% # Set Belgium to missing, to avoid the "Brussels" bias
  mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% # No data before 1997, don't trust it
  filter(year >= 1997) %>% 
  pivot_longer(cols = BI:WS, names_to = "Country", values_to = "Country Mentions") %>% 
  group_by(Country) %>% 
  summarise(`Country Mentions` = sum(`Country Mentions`, na.rm = T)) %>% 
  arrange(-`Country Mentions`) %>% 
  ggplot(., aes(map_id = Country)) +
  geom_map(aes(fill = `Country Mentions`), map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_continuous(name = "", high = "red", low = "blue", na.value="white") +
  labs(title = "Which countries does the European Commission mention more often in its digital-related public communication?",
       subtitle = "Difference of literal country mentions in english-language press releases, speeches, and statements published by the Commission 1997-2023 that relate to digital topics and all country mentions",
       caption = "Country mentions extracted along the Newsmap dictionaries (Watanabe 2018);\nBelgium deliberately set to missing.")+
  coord_fixed()+
  theme_void() +
  theme(legend.position = c(0.1, 0.3),
        plot.margin = unit(c(.3,.3,.3,.3), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", size = 14))
pl.map_diff
ggsave("./output/plots/CountryMentions/WorldMapDigitalDiff.png", pl.map, width = 26, height = 15, units = "cm")




# Top mentioned countries, for writing

cm %>% 
  mutate(BE = NA) %>% # Set Belgium to missing, to avoid the "Brussels" bias
  mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% # No data before 1997, don't trust it
  filter(year >= 1997) %>% 
  pivot_longer(cols = BI:WS, names_to = "Country", values_to = "Country Mentions") %>% 
  group_by(Country) %>% 
  summarise(`Country Mentions` = sum(`Country Mentions`)) %>% 
  arrange(desc(`Country Mentions`)) %>% 
  mutate(rank = row_number()) %>% 
  head(50) %>% 
  write_csv2("./output/plots/CountryMentions/Top50-MostMentionedCountries_1997-2023.csv")


# Share of digital docs mentioning country by month:

df_dp <- 
rbind(
 
   dp_docs %>% 
    select(date, US, CN) %>% 
    mutate(across(c(US, CN), as.logical)) %>% # Mere presence instead of count of country mentions
    mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% # Not using data prior to 1997 !
    filter(year >= 1997) %>% 
    mutate(month = as.character(date) %>% str_remove("-[0-9]{2}$")) %>% 
    relocate(month) %>%
  
  group_by(date) %>% 
  summarize(across(c(US, CN), ~ sum(.x, na.rm = T) / n(), .names = "{col}")) %>% 
  pivot_longer(cols = c(US, CN), names_to = "iso2", values_to = "share") %>% 
  mutate(type = "daily") %>% 
  ungroup(), 
  
  
  dp_docs %>% 
    select(date, US, CN) %>% 
    mutate(across(c(US, CN), as.logical)) %>% # Mere presence instead of count of country mentions
    mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% # Not using data prior to 1997 !
    filter(year >= 1997) %>% 
    mutate(month = as.character(date) %>% str_remove("-[0-9]{2}$")) %>% 
    relocate(month) %>%
    
  group_by(month) %>% 
  summarize(across(c(US, CN), ~ sum(.x, na.rm = T) / n(), .names = "{col}")) %>%
  pivot_longer(cols = c(US, CN), names_to = "iso2", values_to = "share") %>% 
  mutate(type = "monthly",
         date = as.Date(paste0(month, "-15"))) %>% 
  select(-month) %>% 
  ungroup(),
  
  dp_docs %>% 
    select(date, US, CN) %>% 
    mutate(across(c(US, CN), as.logical)) %>% # Mere presence instead of count of country mentions
    mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% # Not using data prior to 1997 !
    filter(year >= 1997) %>% 
    mutate(month = as.character(date) %>% str_remove("-[0-9]{2}$")) %>% 
    relocate(month) %>%
    
  group_by(year) %>% 
  summarize(across(c(US, CN), ~ sum(.x, na.rm = T) / n(), .names = "{col}")) %>% 
  pivot_longer(cols = c(US, CN), names_to = "iso2", values_to = "share") %>% 
  mutate(type = "yearly",
         date = as_date(paste0(year, "-07-01"))) %>% 
  select(-year) %>% 
  ungroup()
  ) %>% 
  mutate(country = countrycode(iso2, origin = "iso2c", "country.name")) %>% 
  mutate(country = factor(country, levels = c("United States", "China"))) %>% 
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
    ) %>% as.factor(),
    month = as.character(date) %>% str_remove("-[0-9]{2}$") 
  )

df_dp %>%
  filter(type == "yearly") %>% 
  ggplot() +
  geom_point(aes(x = date, y = share, colour = country))+
  geom_smooth(aes(x =date, y = share, group = period), colour = "black", method = "lm", se = T, 
              data = df_dp %>% filter(type == "yearly"))+
  facet_wrap(. ~country, nrow = 2) 
  
# breaks <- 
#   df_dp %>% 
#   select(month) %>% 
#   unique() %>% 
#   filter(str_detect(month, "-01")) %>%
#   pull()
# labels <- breaks %>% str_remove_all("-01") 

breaks <- 
  df %>% 
  select(month) %>% 
  unique() %>% 
  filter(str_detect(month, "-01")) %>%
#  mutate(month = as.Date(paste0(month, "-01"))) %>% 
  pull()
labels <- breaks %>% str_remove_all("-01") 


ggplot() +
  # geom_point(aes(x = month, y = share, colour = country),
  #           df_dp %>% filter(type == "yearly")) +
 # geom_smooth(aes(x = month, y = share, group = period), colour = "black", method = "lm", se = T, 
 #              data = df_dp %>% filter(type == "monthly")) +
  geom_smooth(aes(x = month, y = share, group = period, colour = country), method = "lm", se = T,
              data = df_dp %>% filter(type == "monthly")) +
  # geom_smooth(aes(x = month, y = share, group = period, colour = country), method = "loess", se = F, 
  #             data = df_dp %>% filter(type == "monthly")) +
  scale_color_manual(values = c("United States" = "#0380b5", "China" = "red")) +
  facet_wrap(. ~country, nrow = 2, scales = "free_y") +
#  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
   scale_x_discrete(breaks = breaks, labels = labels) +
  scale_y_continuous(labels =scales::percent)+
  labs(title = "Share of digital policy documents that mention major world power in the public communication of the European Commission",
       subtitle = paste0("Monthly share of digital related Commission press releases, speeches, and statements that mention the respective country at least once"),
       x = "",
       y= "",
       caption = "Monthly time-series with linear smoothing")+
  theme_bw()+
  theme(legend.position = "none",
        #text=element_text(family = "Dahrendorf"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(face = "bold"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", size = 10),
        plot.subtitle = element_text(size = 9)) +
  # coord_cartesian(ylim = c(0, 1)) +
  geom_vline(xintercept = "2003-12", linetype = "dotted") +
  geom_vline(xintercept = "2005-11", linetype = "dotted") +
  geom_vline(xintercept = "2010-07", linetype = "dotted") +
  geom_vline(xintercept = "2012-12", linetype = "dotted") +
  geom_vline(xintercept = "2013-06", linetype = "dotted") +
  geom_vline(xintercept = "2015-10", linetype = "dotted") +
  geom_vline(xintercept = "2018-03", linetype = "dotted") +
  geom_vline(xintercept = "2018-12", linetype = "dotted") 
  # annotate("text",
  #          x = c("2003-12", "2005-11", "2010-07", "2012-12", "2013-06", "2015-10", "2018-03", "2018-12"),
  #          y = .95,
  #          country = rep(c("United States", "China"), 4),
  #          label = rep("WSIS 2003", "WSIS 2005", "Stuxnet", "WCIT", "Snowden", "Digital Silk Road", "Cambridge Analytica", "Huawei"),
  #          angle = 270, size = 2.3, hjust = 0)

  ggsave("./output/plots/CountryMentions/US&CN-prevalence_OverTime-digital.png", width = 24, height = 20, units = "cm")
  

# Trends over time for selected countries ####
# US + China plus the rest of BRICS

# Share of docs mentioning country by month
df <- docs %>% 
  select(date, US, CN, BR, RU, IN, ZA, cm_BRICS) %>% 
  mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% # Not using data prior to 1997 !
  filter(year >= 1997) %>% 
  mutate(month = as.character(date) %>% str_remove("-[0-9]{2}$")) %>% 
  relocate(month) %>% 
  mutate_at(c(2:8), as.logical) %>% # Mere presence instead of count of country mentions
  group_by(month) %>% 
  summarise_at(vars(US:cm_BRICS), mean, na.rm = T) %>% # Share of docs that mention the country
  mutate_at(vars(US:cm_BRICS), round, 2) %>% 
  ungroup() %>% 
  pivot_longer(cols = 2:8, names_to = "iso2", values_to = "share") %>% 
  mutate(country = countrycode(iso2, origin = "iso2c", "country.name", nomatch = "BRICS")) %>% 
  mutate(country = factor(country, levels = c("United States", "China", "Brazil", "Russia", "India", "South Africa", "BRICS"))) %>% 
    mutate(
      period = case_when(
        as.Date(paste0(month, '-01')) >= as_date("2018-12-01") ~ "Huawei",
        as.Date(paste0(month, '-01')) >= as_date("2018-03-01") ~ "Cambridge Analytica",
        as.Date(paste0(month, '-01')) >= as_date("2015-10-01") ~ "Digital Silk Road",
        as.Date(paste0(month, '-01')) >= as_date("2013-06-01") ~ "Snowden",
        as.Date(paste0(month, '-01')) >= as_date("2012-12-01") ~ "WCIT",
        as.Date(paste0(month, '-01')) >= as_date("2010-07-01") ~ "Stuxnet",
        as.Date(paste0(month, '-01')) >= as_date("2005-11-01") ~ "WSIS 2005",
        as.Date(paste0(month, '-01')) >= as_date("2003-12-01") ~ "WSIS 2003",
        TRUE ~ "before"
      ) %>% as.factor()
    )
  
# combined mentions of both US & CN:
  ggplot() +
    geom_line(data = docs %>% 
                mutate(year = year(date)) %>% 
                group_by(year) %>% 
                summarize(share = mean(superpowers_mentioned)),
              mapping = aes(x = year, y = share), colour = "#0380b5"
    ) +
    geom_line(data = dp_docs %>% 
                mutate(year = year(date)) %>% 
                group_by(year) %>% 
                summarize(share = mean(superpowers_mentioned)),
              mapping = aes(x = year, y = share), colour = "#619933"
    ) +
    scale_y_continuous(labels =scales::percent)+
    geom_vline(xintercept = 2003+11/12, linetype = "dotted") +
    geom_vline(xintercept = 2005+10/12, linetype = "dotted") +
    geom_vline(xintercept = 2010+6/12, linetype = "dotted") +
    geom_vline(xintercept = 2012+11/12, linetype = "dotted") +
    geom_vline(xintercept = 2013+5/12, linetype = "dotted") +
    geom_vline(xintercept = 2015+9/12, linetype = "dotted") +
    geom_vline(xintercept = 2018+2/12, linetype = "dotted") +
    geom_vline(xintercept = 2018+11/12, linetype = "dotted") +
    labs(title = "Share of documents that mention both major world powers in the public communication of the European Commission",
         subtitle = "Yearly share of Commission press releases, speeches, and statements that mention both the US and China at least once",
         caption = "Green: Documents classified as digital\nBlue: All Documents") +
    theme(legend.position = c(0.1, 0.3),
          plot.margin = unit(c(.3,.3,.3,.3), "cm"),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold", size = 14),
          plot.caption = element_text(face = "italic", size = 8)) +
    coord_cartesian(xlim = c(1997, 2023)) +
    annotate("text",
             x = c(2003+11/12, 2005+10/12, 2010+6/12, 2012+11/12, 2013+5/12, 2015+9/12, 2018+2/12, 2018+11/12) + .3,
             y = .01,
             label = c("WSIS 2003", "WSIS 2005", "Stuxnet", "WCIT", "Snowden", "Digital Silk Road", "Cambridge Analytica", "Huawei"),
             angle = 270, size = 2.2, hjust = 0) +
    theme_bw()
  ggsave("./output/plots/CountryMentions/comentions_superpowers_dp.png", width = 26, height = 15, units = "cm")
  
  
  
# Plotting parameters
breaks <- 
  df %>% 
  select(month) %>% 
  unique() %>% 
  filter(str_detect(month, "-01")) %>% 
  pull()
labels <- breaks %>% str_remove_all("-01") # LOTS OF YEARS MISSING !!! WHHAT IS GOIND ON HERE?

# Plot
pl.powers <- 
  ggplot(df, aes(x = month, y= share, group = period))+
  # geom_line()+
  geom_smooth(method = "loess", se = F, color = "#0380b5")+
  scale_x_discrete(breaks = breaks, labels = labels)+
  facet_wrap(.~country, ncol = 2)+
  scale_y_continuous(labels =scales::percent)+
  labs(title = "Major world power mentions in the public communication of the European Commission",
       subtitle = paste0("Monthly share of Commission press releases, speeches, and statements that mention the respective country at least once"),
       x = "",
       y= "",
       caption = "Monthly time-series smoothed with LOESS (span = .05)")+
  theme_bw()+
  theme(legend.position = "none",
        # text=element_text(family = "Dahrendorf"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(face = "bold"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", size = 14))
  


pl.powers 

ggsave("./output/plots/CountryMentions/US&BRICS-prevalence_OverTime.png", pl.powers, width = 24, height = 24, units = "cm")


# Add digitality to the picture

df2 <- docs %>% 

   # filter(digitalpara > 1) %>% # ! ' USING SEM_SIMIL
  filter(digitalsimil) %>% # using sem_simil
  
  select(date, US, CN, BR, RU, IN, ZA) %>% 
  mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% # Not using data prior to 1997 !
  filter(year >= 1997) %>% 
  mutate(month = as.character(date) %>% str_remove("-[0-9]{2}$")) %>% 
  select(-date) %>% 
  relocate(month) %>% 
  mutate_at(c(2:7), as.logical) %>% # Mere presence instead of count of country mentions
  group_by(month) %>% 
  summarise_at(vars(US:ZA), mean, na.rm = T) %>% # Share of docs that mention the country
  mutate_at(vars(US:ZA), round, 2) %>% 
  ungroup() %>% 
  pivot_longer(cols = 2:7, names_to = "iso2", values_to = "share") %>% 
  mutate(country = countrycode(iso2, origin = "iso2c", "country.name")) %>% 
  mutate(country = factor(country, levels = c("United States", "China", "Brazil", "Russia", "India", "South Africa"))) %>% 
  mutate(
    period = case_when(
      as.Date(paste0(month, '-01')) >= as_date("2018-12-01") ~ "Huawei",
      as.Date(paste0(month, '-01')) >= as_date("2018-03-01") ~ "Cambridge Analytica",
      as.Date(paste0(month, '-01')) >= as_date("2015-10-01") ~ "Digital Silk Road",
      as.Date(paste0(month, '-01')) >= as_date("2013-06-01") ~ "Snowden",
      as.Date(paste0(month, '-01')) >= as_date("2012-12-01") ~ "WCIT",
      as.Date(paste0(month, '-01')) >= as_date("2010-07-01") ~ "Stuxnet",
      as.Date(paste0(month, '-01')) >= as_date("2005-11-01") ~ "WSIS 2005",
      as.Date(paste0(month, '-01')) >= as_date("2003-12-01") ~ "WSIS 2003",
      TRUE ~ "before"
    ) %>% as.factor()
  )
  

df <- rbind(df %>%  mutate(type = "All documents"),
            df2 %>% mutate(type = "Documents emphasizing digital affairs")) %>% 
  filter(iso2 %in% c("CN", "US", "BRICS"))

span <- .1
pl.powers <- 
  ggplot(df, aes(x = month, y= share, color = type, group = type, size = type))+
  # geom_line()+
  geom_smooth(method = "loess", span = span, se = F)+
  scale_x_discrete(breaks = breaks, labels = labels)+
  scale_color_manual(values = c("#0380b5", "#619933"))+
  scale_linetype_manual(values = c("solid", "dotted"), guide = 'none')+
  scale_size_manual(values = c(1, .5), guide = "none")+
  facet_wrap(.~country, nrow = 2)+
  scale_y_continuous(labels =scales::percent)+
  labs(title = "Major world power mentions in the public communication of the European Commission",
       subtitle = paste0("Monthly share of Commission press releases, speeches, and statements that mention the respective country at least once"),
       x = "",
       y= "",
       caption = paste("Monthly time-series smoothed with LOESS (span =", span, ")"),
       color = "Comparision set:")+
  theme_bw()+
  theme(legend.position = "top",
        legend.justification='left',
        legend.direction='horizontal',
        legend.box.margin = margin(-5,0,-5,-5),
        legend.text=element_text(size=11),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(face = "bold"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", size = 14)) 
  
pl.powers 

ggsave("./output/plots/CountryMentions/US&CN-prevalence_OverTime_Digitality.png", pl.powers, width = 24, height = 24, units = "cm")


# to add BRICS:
# +  geom_smooth(df %>% filter(country == "BRICS"), aes(x = month, y= share, color = type, group = type, size = type), 
#                method = "loess", span = span, se = F) +



# with zs:
ggsave("./output/plots/CountryMentions/US&BRICS-prevalence_OverTime_DigitalityZS.png", pl.powers, width = 24, height = 24, units = "cm")


# with sem_simil:
ggsave("./output/plots/CountryMentions/US&BRICS-prevalence_OverTime_Digitality.png", pl.powers, width = 24, height = 24, units = "cm")



# Balance of member and foreign states in EC communication ####
# How inward or outward looking is the EC communication?


# Time sensitive count of meber state mentions
# How to dela with Belgium here (or maybe not: Brussels bias is probably stable over time)
EU_at_time <- 
  tibble::tibble(
    memberstate = c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU", 
                    "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE", "GB"),
    eu_accession = as.Date(c("1995-01-01", "1958-01-01", "2007-01-01", "2013-07-01", 
                             "2004-05-01", "2004-05-01", "1973-01-01", "2004-05-01", 
                             "1995-01-01", "1958-01-01", "1958-01-01", "1981-01-01", 
                             "2004-05-01", "1973-01-01", "1958-01-01", "2004-05-01", 
                             "2004-05-01", "1958-01-01", "2004-05-01", "1958-01-01", 
                             "2004-05-01", "1986-01-01", "2007-01-01", "2004-05-01", 
                             "2004-05-01", "1986-01-01", "1995-01-01", "1973-01-01"))
  )

foreu <- 
  docs %>% 
  mutate(cm_total = rowSums(select(., c(BI:WS)))) %>% # Total count of country mentions in doc
  pivot_longer(all_of(
    countrycode::codelist %>% filter(eu28 == "EU") %>% pull(iso2c)), # Iso2 codes of EU 28 countries
    names_to = "memberstate", 
    values_to = "cms"
  ) %>% 
  right_join(., EU_at_time, join_by(memberstate)) %>% 
  # was in EU at date?
  mutate(eu = eu_accession <= date) %>% 
  mutate(eu = ifelse(memberstate == "GB" & date >= as.Date("2020-02-01"), F, eu)) %>% # Brexit
  select(-c(BI:WS)) %>% 
  # Keep only contemporary EU countries (we're still on liong format)
  filter(eu) %>% 
  # Aggregate to doc level 
  group_by(doc_key) %>% 
  summarise(eu_countries = sum(cms),
            all_countries = unique(cm_total)) %>% 
  ungroup() %>% 
  # Calculate relative presence
  mutate(foreign_countries = all_countries - eu_countries,
         foreu_balance = foreign_countries-eu_countries) %>% # Overweight of foreign over EU countries in document (absolute diff)
  # Add doc_level classification of digitality and date
  left_join(docs %>% select(doc_key, date, digital), by = "doc_key")


# Comparative dataset

df <- rbind(foreu %>% mutate(type = "All documents"),
            foreu %>% filter(digital) %>% mutate(type = "Documents emphasizing digital affairs")) %>% 
  mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% # Not using data prior to 1997 !
  filter(year >= 1997) %>% 
  mutate(month = as.character(date) %>% str_remove("-[0-9]{2}$")) %>% 
  select(-date) %>% 
  relocate(month) %>% 
  group_by(month, type) %>% 
  summarise(foreu_balance = mean(foreu_balance))
  


# Plot 

pl.balance <- 
  ggplot(df, aes(x = month, y= foreu_balance, color = type, group = type, size = type))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_smooth(method = "loess", span = .1, se = F)+
  scale_x_discrete(breaks = breaks, labels = labels)+
  scale_color_manual(values = c("#0380b5", "#619933"))+
  scale_linetype_manual(values = c("solid", "dotted"), guide = 'none')+
  scale_size_manual(values = c(1, .5), guide = "none")+
  labs(title = "Balance of foreign and EU countries mentioned in the public communication of the European Commission",
       subtitle = paste0("Count of foreign countries minus count of contemporaneous EU member states, monthly averages"),
       x = "",
       y= "",
       caption = "Monthly time-series smoothed with LOESS (span = .1)",
       color = "Comparision set:")+
  geom_vline(xintercept = as.Date("2003-12-01") %>% as.character() %>% str_remove("-[0-9]{2}$"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2005-11-01") %>% as.character() %>% str_remove("-[0-9]{2}$"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2010-07-01") %>% as.character() %>% str_remove("-[0-9]{2}$"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2012-12-01") %>% as.character() %>% str_remove("-[0-9]{2}$"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2013-06-01") %>% as.character() %>% str_remove("-[0-9]{2}$"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2015-10-01") %>% as.character() %>% str_remove("-[0-9]{2}$"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-03-01") %>% as.character() %>% str_remove("-[0-9]{2}$"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-12-01") %>% as.character() %>% str_remove("-[0-9]{2}$"), linetype = "dotted") +
  annotate("text", 
           x = c("2003-11", "2005-10", "2010-06", "2012-11", 
                 "2013-05", "2015-09", "2018-02", "2018-11"),
           y = 3,
           label = c("WSIS 2003", "WSIS 2005", "Stuxnet", "WCIT", "Snowden", "Digital Silk Road", "Cambridge Analytica", "Huawei"),
           angle = 270, size = 2.3, hjust = 0
  )+
  theme_bw()+
  theme(legend.position = "top",
        legend.justification='left',
        legend.direction='horizontal',
        legend.box.margin = margin(-5,0,-5,-5),
        legend.text=element_text(size=11),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(face = "bold"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", size = 14))

ggsave("./output/plots/CountryMentions/Foreign-EU-Balance.png", pl.balance, width = 28, height = 16, units = "cm")




# Emphasis of economic, liberal rights, or security concerns ###

concerns <- docs %>% 
  select(date, digital, contains("_simil")) %>% 
  mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% # Not using data prior to 1997 !
  filter(year >= 1997) %>% 
  mutate(month = as.character(date) %>% str_remove("-[0-9]{2}$")) %>% 
  select(-c(date, year)) %>% 
  relocate(month) %>% 
  pivot_longer(contains("_simil"), names_to = "concern", values_to = "sem_simil")
# %>% 
#   group_by(month, digital, concern) %>%
#   summarise(sem_smil = mean(sem_smil))

df <-rbind(concerns %>% group_by(month, concern) %>% summarise(sem_simil = mean(sem_simil)) %>% ungroup() %>% mutate(type = "All documents"),
           concerns %>% filter(digital) %>% group_by(month, concern) %>% summarise(sem_simil = mean(sem_simil)) %>% ungroup() %>% mutate(type = "Documents emphasizing digital affairs"))

df$concern[str_detect(df$concern, "economy")] <- "Economy language"
df$concern[str_detect(df$concern, "librights")] <- "Liberal rights language"
df$concern[str_detect(df$concern, "security")] <- "Security language"
table(df$concern)
df$concern <- factor(df$concern, levels = c("Economy language", "Liberal rights language", "Security language"))

pl.concerns <- 
  ggplot(df, aes(x = month, y= sem_simil, color = type, group = type, size = type))+
  geom_smooth(method = "loess", span = .1, se = F)+
  scale_x_discrete(breaks = breaks, labels = labels)+
  scale_color_manual(values = c("#0380b5", "#619933"))+
  scale_linetype_manual(values = c("solid", "dotted"), guide = 'none')+
  scale_size_manual(values = c(1, .5), guide = "none")+
  facet_wrap(.~concern, ncol = 1, scales = "free_y") +
  labs(title = "Relative emphasis of diffferent concerns in the public communication of the European Commission",
       subtitle = paste0("Based on the semantic similarity to seed word dictionaries on the respective concern, monthly averages"),
       x = "",
       y= "",
       caption = "Monthly time-series smoothed with LOESS (span = .1)",
       color = "Comparision set:")+
  theme_bw()+
  theme(legend.position = "top",
        legend.justification='left',
        legend.direction='horizontal',
        legend.box.margin = margin(-5,0,-5,-5),
        legend.text=element_text(size=11),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(face = "bold"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", size = 14))

ggsave("./output/plots/RelativeEmphasis_Conerns.png", pl.concerns, width = 28, height = 24, units = "cm")

  















# # Preparations: text data, ids and meta data ####
# 
# all_texts <- read_rds("./data/all_texts.rds") # Paragraph level
# all_meta <- read_rds("./data/all_meta.rds") # Document level
# 
# # doc_keys of all english-language documents in the database
# # We filter along those lines for now
# 
# doc_keys_en <- all_meta %>% 
#   filter(lang == "en") %>% 
#   select(doc_key, doc_type)
# 
# 
# 
# # Corpus descriptives ####
# 
# # Number of docs for analysis
# nrow(doc_keys_en) # 76941 - number of documents for first paper
# nrow(doc_keys_en) == nrow(unique(doc_keys_en)) # Should be TRUE
# 
# # Number of documents by type
# doc_types <- table(doc_keys_en$doc_type, useNA = "ifany") %>% 
#   as.data.frame() %>% 
#   arrange(desc(Freq)) %>% 
#   mutate(Var1 = as.character(Var1)) %>% 
#   rename(`Document type` = 1,
#          N = 2)
# doc_types[nrow(doc_types)+1,] <- c("Total", sum(doc_types$N))
# doc_types$`Document type`[is.na(doc_types$`Document type`)] <- "Not specified" # What are those? (53)
# t <- flextable(doc_types)
# save_as_docx("Type of documents in TRIAS Corpus (english-language only for now)" = t, 
#              path = "./output/tables/Document_Types.docx")
# rm(doc_types, t)


  




