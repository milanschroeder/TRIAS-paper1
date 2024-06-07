## 31_match_geopolitics_digital

# Input: all_texts.rds, zs_subtopics.csv, CountryMentions_NM-dict_ParagraphLevel.rds, all_meta.rds

# load packages:
library(tidyverse)
library(magrittr)

# data merging: ####

# adaptable Classification Cutoff:
zs_max_cutoff = 0.7

# update data:
outfile <- "zs_subtopics.csv"
# googledrive::drive_download(paste0("TRIAS/", outfile), paste0("data/", outfile), overwrite = T)
file.copy(from = paste0("M:/user/schroeder/", outfile), to = paste0("./data/", outfile), overwrite = T)

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
          read_csv(paste0("data/", outfile)) %>% 
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
            read_rds("large_data/CountryMentions_NM-dict_ParagraphLevel.rds") %>% 
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


# aggregate on doclevel: ####

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


# save data: ####
write_rds(analysis_data, "./data/ParaLevelData_zs.rds")
write_rds(analysis_data_doclevel, "./data/DocLevelData_zs.rds")

### Validation (Doclevel) ####
  

cor(analysis_data_doclevel$share_digital_para, analysis_data_doclevel$cm_CN_wider_share)

# define cutoff function:
calc_cutoff_candidates <- function(cutoffvar, targetvar, seqstart = 0, seqend = 1, seqstep = .01, facetingvar = "cutoff") {
  ### targetvar: boolean!
  ### facetingvar needs to be present in  long format data!
  
  library(magrittr)
  
  inputdata <- tibble(
    targetvar = targetvar,
    cutoffvar = cutoffvar,
    facetingvar = facetingvar
  )
  df <- inputdata
  
  cutoff_candidates <- tibble()
  facets <- unique(facetingvar)
  
  for (facet in facets) {
    
    df <- filter(inputdata, facetingvar == facet)
    
    for (cutoff in seq(seqstart, seqend, seqstep)) {
      
      df %<>% mutate(pred = cutoffvar > cutoff) 
      
      TP <- sum(df$pred & df$targetvar)
      TN <- sum(!df$pred & !df$targetvar)
      FP <- sum(df$pred & !df$targetvar)
      FN <- sum(!df$pred & df$targetvar)
      
      precision <- TP / (TP + FP)
      recall <- TP / (TP + FN)
      f1 <- 2*(precision*recall)/(precision+recall)
      accuracy <- (TP + TN) / (TP + TN + FP + FN)
      baccuracy <- ((TP / (TP + FN)) + (TN / (TN + FP))) / 2 # Balanced accuracy (because imbalanced sample!) - https://neptune.ai/blog/balanced-accuracy
      
      cutoff_candidates %<>% 
        bind_rows(., tibble_row(cutoff, precision, recall, f1, accuracy, baccuracy, TP, FP, TN, FN, facet))
    }
  }
  return(cutoff_candidates)
}
# define plotting function:
plot_metrics <- function(data, scorename, plotname, facetingvar = NULL){
  
  library(magrittr)
  data %<>% mutate(facet = facetingvar)
  
  # # Accuracy
  # pl.accuracy <-
  #   ggplot(cutoffs, aes(x=cutoff, y = accuracy, group = facet), )+
  #   geom_point(color = "darkgreen")+
  #   geom_line(color = "darkgreen")+
  #   labs(title = "Accuracy",
  #        subtitle = "What overall share of texts is correctly classified?",
  #        y = "Accuracy\n",
  #        x = "Chosen semantic similarity cutoff")+
  #   theme_bw()
  
  # Balanced Accuracy
  pl.baccuracy <-
    ggplot(data, aes(x=cutoff, y = baccuracy, group = facet), )+
    geom_point(color = "darkgreen")+
    geom_line(color = "darkgreen")+
    labs(title = "<b>Balanced Accuracy</b>: What overall share of documents in each category is correctly classified?",
         y = "Accuracy\n",
         x = "")+
    theme_bw()+
    theme(plot.title = element_markdown())
  
  
  # Precision and recall
  
  pr <- data %>% 
    select(cutoff, precision, recall, facet) %>% 
    pivot_longer(cols = 2:3, names_to = "metric", values_to = "value")
  
  pl.precrec <-
    ggplot(pr, aes(x=cutoff, y = value, color = metric))+
    geom_point()+
    geom_line()+
    scale_color_manual(values = c("#d95f02", "#7570b3"))+
    labs(title = "<b>Precision & recall</b>",
         subtitle = "<span style = 'color:#d95f02;'><b>Precision</b></span>: What share of retrieved documents is actually relevant?<br><span style = 'color:#7570b3;'><b>Recall<b></span>: What share of actually relevant documents is is retrieved?",
         y = "Value\n",
         x = "",
         color = "")+
    theme_bw()+
    theme(legend.position = "none",
          plot.subtitle = element_markdown(),
          plot.title = element_markdown()) 
  
  # F1 Score
  pl.f1 <-
    ggplot(data, aes(x=cutoff, y = f1, group = facet), )+
    geom_point(color = "black")+
    geom_line(color = "black")+
    labs(title = "<b>F1 Score</b>: Harmonized mean of recall & precision",
         y = "F1 score\n",
         x = paste("\nClassification cutoff applied to", scorename)) +
    theme_bw()+
    theme(plot.title = element_markdown()) 
  
  if (!is.null(facetingvar)) {
    pl.f1 <- pl.f1 + facet_grid(. ~ facet)
    pl.precrec <- pl.precrec + facet_grid(. ~ facet)
    pl.baccuracy <- pl.baccuracy + facet_grid(. ~ facet)
    # pl.accuracy <- pl.accuracy + facet_grid(. ~ facet)
  }
  
  # Combined plot
  pl.comb <- # pl.accuracy /
    pl.baccuracy / pl.precrec / pl.f1
  
  ggsave(paste0("./output/plots/", plotname, ".png"), 
         pl.comb, width = 24, height = 27, units = "cm")
  
  return(pl.comb)
}



doc_subtopics <- analysis_data_doclevel %>% 
  pivot_longer(c(digital_communications_max, internet_technologies_max, digital_services_max, digital_algorithms_max, digitized_data_max, max_digital_score), 
               names_to = "label", 
               values_to = "score")

cutoffs_doc <- calc_cutoff_candidates(cutoffvar = doc_subtopics$score, targetvar = doc_subtopics$handcoding_max, facetingvar = doc_subtopics$label)

plot_metrics(data = cutoffs_doc, 
             scorename = "Zeroshot Score with finegrained labels (document level)",
             plotname = "Digital_classification_cutoff_doc",
             facetingvar = cutoffs_doc$facet)
