# Visual / descriptive analyses of extracted text data
# Authors:  @ChRauh, @milanschroeder 

# Packages
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v2.0.0 
library(countrycode)
library(flextable)
library(magrittr)
library(zoo)


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
paras %>% 
  filter(digital) %>% 
  filter(wordcount > 10) %>% 
  arrange(desc(economy_simil)) %>% 
  select(c(id, text_para, economy_simil, doc_key)) %>% 
  head(15)
paras$text_para[paras$id == 331845] 

paras %>% 
  filter(!digital) %>% 
  filter(wordcount > 10) %>% 
  arrange(desc(economy_simil)) %>% 
  select(c(id, text_para, economy_simil, doc_key)) %>% 
  head(15) 
paras$text_para[paras$id == 594929] 

# Security simil
paras %>% 
  filter(digital) %>% 
  filter(wordcount > 10) %>% 
  arrange(desc(security_simil)) %>% 
  select(c(id, text_para, economy_simil, doc_key)) %>% 
  head(15)
paras$text_para[paras$id == 1923360] 

paras %>% 
  filter(!digital) %>% 
  filter(wordcount > 10) %>% 
  arrange(desc(security_simil)) %>% 
  select(c(id, text_para, economy_simil, doc_key)) %>% 
  head(15)
paras$text_para[paras$id == 222898] 

# Liberal rights
paras %>% 
  filter(digital) %>% 
  filter(wordcount > 10) %>% 
  arrange(desc(librights_simil)) %>% 
  select(c(id, text_para, economy_simil, doc_key)) %>% 
  head(15)
paras$text_para[paras$id == 173325] 

paras %>% 
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
  arrange(date, doc_key)

write_rds(docs, "./large_data/DocumentLevelData.rds", compress = "gz")
# docs <- read_rds("./large_data/DocumentLevelData.rds")

gc()


# When does a document matter for digitality?
# Taking initial choices here as the NLI classification is not likely to be finished in time and 
# as the sem_simil classification on the para level creates false positives 

docs$digitalshare <- docs$digitalpara/docs$npara
hist(docs$digitalshare)
sum(docs$digitalshare >= .2) # At least 1/5 of paras has to be relevant for digitality to count the doc in, 7719

docs$digital <- docs$digitalshare >= .2

docs %>% 
  arrange(desc(digitalshare)) %>% 
  select(npara, digitalshare, title) %>% 
  head(15) # Strong bias to the single paragraph documents in the early period ... grr

docs %>% 
  mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% 
  group_by(year) %>% 
  summarise(npara = mean(npara)) %>% 
  ggplot(aes(x=year, y = npara, group = 1))+
  geom_line() # From 1997 onewwards we ha ve a reasonable number of paras per doc only

docs %>% 
  mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% 
  filter(year >= 1997) %>% 
  arrange(desc(digitalshare)) %>% 
  select(npara, digitalshare, title) %>% 
  head(15) # Looks very reasonable, stick with this one for now




# Quick checks ####

# hist(docs$digital) # Long right tail
# test <- docs %>% filter(digital > 50) # Press relases with attached docs 
# table(test$doc_type) # Mainly looong speeches 
# 
# docs %>% 
#   select(contains("simil")) %>% 
#   pivot_longer(cols = everything()) %>% 
#   ggplot(aes(x=value, color = name))+
#   geom_density()
 


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
       subtitle = paste0("Count of literal country mentions in ",   nrow(docs %>% mutate(year = as.character(date) %>% str_extract("^[0-9]{4}") %>% as.numeric()) %>% filter(year >= 1997)), " english-language press releases, speeches, and statements published by the Commission 1997-2023"),
       caption = "Country mentions extracted along the Newsmap dictionaries (Watanabe 2018);\nBelgium deliberately set to missing.")+
  coord_fixed()+
  theme_void() +
  theme(legend.position = c(0.1, 0.3),
        plot.margin = unit(c(.3,.3,.3,.3), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", size = 14))
# pl.map
ggsave("./output/plots/CountryMentions/WorldMapTotal.png", pl.map, width = 26, height = 15, units = "cm")


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



# Trends over time for selected countries ####
# US + China plus the rest of BRICS

# Share of docs mentioning country by month
df <- docs %>% 
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
  mutate(country = factor(country, levels = c("United States", "China", "Brazil", "Russia", "India", "South Africa")))
  

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
  ggplot(df, aes(x = month, y= share, group = country))+
  # geom_line()+
  geom_smooth(method = "loess", span = .05, se = F, color = "#0380b5")+
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
  filter(digital) %>% # !
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
  mutate(country = factor(country, levels = c("United States", "China", "Brazil", "Russia", "India", "South Africa")))

df <- rbind(df %>%  mutate(type = "All documents"),
            df2 %>% mutate(type = "Documents emphasizing digital affairs"))

pl.powers <- 
  ggplot(df, aes(x = month, y= share, color = type, group = type, size = type))+
  # geom_line()+
  geom_smooth(method = "loess", span = .1, se = F)+
  scale_x_discrete(breaks = breaks, labels = labels)+
  scale_color_manual(values = c("#0380b5", "#619933"))+
  scale_linetype_manual(values = c("solid", "dotted"), guide = 'none')+
  scale_size_manual(values = c(1, .5), guide = "none")+
  facet_wrap(.~country, ncol = 2)+
  scale_y_continuous(labels =scales::percent)+
  labs(title = "Major world power mentions in the public communication of the European Commission",
       subtitle = paste0("Monthly share of Commission press releases, speeches, and statements that mention the respective country at least once"),
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

ggsave("./output/plots/CountryMentions/US&BRICS-prevalence_OverTime_Digitality.png", pl.powers, width = 24, height = 24, units = "cm")













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


  




