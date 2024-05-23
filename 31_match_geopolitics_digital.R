## 31_match_geopolitics_digital

# Input: all_texts.rds, zs_subtopics.csv, CountryMentions_NM-dict_ParagraphLevel.rds, all_meta.rds

# load packages:
library(tidyverse)
library(magrittr)

# adaptable Classification Cutoff:
zs_max_cutoff = 0.7

# lookup EU membership status:
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

analysis_data <- 
  
  # Classification is the same for duplicate texts:
  read_rds("data/all_texts.rds") %>% 
  left_join(.,
            read_csv("data/zs_subtopics.csv") %>% 
              select(text_para = text, target_max:digital_policy),
            join_by(text_para)
            ) %>% 
  filter(!is.na(digital_policy)) %>% 
  
  # calculate combined classification: 
  mutate(zs_max = pmax(digital_communications, 
                       internet_technologies, 
                       digital_services, 
                       digital_algorithms, 
                       digitized_data, 
                       digital_policy, 
                       na.rm = T),
         zs_digital = ifelse(zs_max > zs_max_cutoff, 1, 0)
         ) %>% 
    
  # add Country Mentions:
  left_join(., 
            read_rds("large_data-paper1/CountryMentions_NM-dict_ParagraphLevel.rds") %>% 
              select(-c(doc_pos, doc_key)) , 
            join_by(id)
            ) %>% 
  
  # add Date:
  left_join(., 
            read_rds("data/all_meta.rds") %>% 
              select(doc_key, date), 
            join_by(doc_key)
            ) 

# aggregate Country Mentions for EU (time-sensitive):
EU_mentions <- analysis_data %>% 
  pivot_longer(all_of(
    countrycode::codelist %>% filter(eu28 == "EU") %>% pull(iso2c)), 
    names_to = "memberstate", 
    values_to = "cms"
  ) %>% 
  right_join(., 
             EU_at_time, 
             join_by(memberstate)
  ) %>% 
  # was in EU at date?
  mutate(eu = eu_accession <= date) %>% 
  mutate(eu = ifelse(memberstate == "GB" & date >= as.Date("2020-02-01"), F, eu)) %>%
  select(-c(BI:WS)) %>% 
  group_by(eu, id) %>% 
  # count for EU members at date:
  summarise(cm_EU = sum(cms)) %>% 
  ungroup() %>% 
  filter(eu) %>% 
  select(-eu)

# add EU mentions:
analysis_data %<>% 
  left_join(.,
            EU_mentions,
            join_by(id)
  ) %>% 
  
  # calculate country group mentions:
  mutate(
    cm_total = rowSums(analysis_data %>% select(BI:WS), na.rm = T),
    cm_external = cm_total - cm_EU,
    cm_CN_wider = rowSums(analysis_data %>% select(CN, TW, HK, MO), na.rm = T),
    cm_BRICS = rowSums(analysis_data %>% select(BR, RU, IN, CN, ZA), na.rm = T),
    cm_BRICSplus = rowSums(analysis_data %>% select(IR, AE, ET, EG), na.rm = T) + cm_BRICS
  ) %>% 
  
  # unselect other country mentions: 
  select(-c(BI:WS), US, CN, RU, ZA, IN, BR)


# aggregate on doclevel:

analysis_data_doclevel <- 
  analysis_data %>% 
  group_by(doc_key) %>% 
  summarise(
    
    n_paras_classified = n(),
    
    # aggregate classifications:
    any_digital_para = max(zs_digital, na.rm = T),
    n_digital_para = sum(zs_digital, na.rm = T),
    share_digital_para = n_digital_para / n_paras_classified,
    
    # aggregate classification scores:
    max_digital_score = max(zs_max, na.rm = T),
    mean_digital_score = mean(zs_max, na.rm = T),
    
    # aggregate subscores:
    across(digital_communications:digital_policy, max, .names = "{col}_max"),
    across(digital_communications:digital_policy, mean, .names = "{col}_mean"),
    
    # aggregate manual codings:
    handcoding_max = max(target_max, na.rm = T) %>% ifelse(is.infinite(.), 0, .),

    # aggregate Country mentions:
    across(cm_EU:BR, sum), # Count CMs
    across(cm_EU:BR, ~ ifelse(.x > 0, 1, 0), .names = "{col}_any"), # any CMs 
    across(cm_EU:BR, ~ ifelse(cm_total > 0, .x / cm_total, 0), .names = "{col}_share") # share of all CMs in doc
      
    ) %>% 
  
  # add metadata:
  left_join(., 
            read_rds("data/all_meta.rds"),
            join_by(doc_key)) %>% 
  
  ungroup() %>% 
  select(-cm_total_share)

  