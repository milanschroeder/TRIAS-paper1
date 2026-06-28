# Code / scale EC communication along word weights from pre-trained word embedding model beforehand
# this time using the Concept Movers Distance rather than cosine similarity to average vectors
# Author: CR (03.06.2024)


# Intuition similar to semantic similarity, but hopefully less noisy
# CMD algorithm first searches for closest word pairs between concept and coding docs
# To then calculate the lowest 'transportation costs' from one into the other

# Paper: https://link.springer.com/article/10.1007/s42001-019-00048-6
# Code: https://culturalcartography.gitlab.io/text2map/articles/CMDist-concept-movers-distance.html


# Packages ###
library(tidyverse)
library(text2map)
library(text2vec)
library(textclean)
library(stringi)
library(quanteda)
library(readxl)
library(magrittr)
library(ggwordcloud)



# Word embeddings ####

# Using the ready-made GLOVE word embeddings trained on Wikipedia and Gigaword
# Version with 400k vocabulary and 300 dimensions

# Get it here: https://nlp.stanford.edu/projects/glove/ # ! LARGE FILE !

# I have parsed this for other purposes already, if you start from the raw file see:
# https://gist.github.com/tjvananne/8b0e7df7dcad414e8e6d5bf3947439a9

# glove.300 <- read_rds("C:/Users/chris/Downloads/glove.6B.300d.rds") # HP path
# glove.300 <- read_rds("C:/Users/rauh/Downloads/glove.6B.300d.rds") # ThinkPad path
# glove.300 <- read_rds("C:/Users/rauh/Downloads/glove.6B.300d.rds") # WZB path
glove.300 <- read_rds("./large_data/glove.6B.300d.rds") # MS path

# Clean up the vocabulary a bit (lots of rare trash in there, exclude stopwords)
vocab <- names(glove.300) %>% 
  as.data.frame() %>% 
  rename(token = 1) %>% 
  filter(str_detect(token, "[a-z]")) %>% 
  filter(nchar(token) > 1) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  filter(!str_detect(token, "\\.")) %>% 
  filter(!str_detect(token, "[0-9]"))
glove.300 <- glove.300 %>% select(vocab$token)

# This has the tokens in columns and the dimensions in rows
# cmd and text2vec generally expect vocab in rows and vectors values in columns, so I transpose this accordingly here
glove.300 <- glove.300 %>%
  as.matrix() %>%
  t() 

rm(vocab)
gc()



# Texts to be coded ####

# Paragraph-level
# CMDist expects a DTM which I construct using quanteda tools here
# Pre-processing similar to 03_tokenize_corpus.R - so as to ensure comparability to the semantic similarity measures

texts <- read_rds("data/all_texts.rds")

# Pre-processing and DTM construction

start <- Sys.time()
dfm_para <- 
  texts %>% 
  arrange(id) %>% # Just to ensure matching down the line
  rename(text = text_para) %>% 
  mutate(text = tolower(text)) %>% 
  # head(20) %>% # Sample for testing
  corpus() %>% 
  tokens(remove_punct = T,
         remove_symbols = T,
         remove_numbers = T,
         remove_url = T,
         verbose = T) %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_keep(min_nchar = 2) %>% 
  dfm(verbose = T)
Sys.time() - start  



# Seed dictionaries ####
# cf. 10_semantic_similarity_weights.R

# The simple 'digitality' dictionary
seed_dp_simple <- c("digital", "online", "computer", "internet", "algorithm")

# The advanced / intersubjective 'digitality' dictionary
# Five human coders received a list of the top-1500 terms semantically close to the radically simplified dictionary
# Each coder rated each terms as "Clearly and exclusively referring to the digital technology?" [0|1]
files <- paste0("./data/DigitalTerms/", list.files("./data/DigitalTerms/"))
ratings <- read_xlsx(files[1], sheet = 2) %>% rename(token = 1, coder1 = 2) %>% 
  left_join(read_xlsx(files[2], sheet = 2) %>% rename(token = 1, coder2 = 2), by = "token") %>% 
  left_join(read_xlsx(files[3], sheet = 2) %>% rename(token = 1, coder3 = 2), by = "token") %>% 
  left_join(read_xlsx(files[4], sheet = 2) %>% rename(token = 1, coder4 = 2), by = "token") %>%
  left_join(read_xlsx(files[5], sheet = 2) %>% rename(token = 1, coder5 = 2), by = "token") %>% 
  mutate(codesum = rowSums(across(where(is.numeric)))) %>% 
  arrange(desc(codesum))
# Now we take those tokens for which a majority of coders (3/5) suggested that this was a relevant term ...
seed_dp_adv <- ratings %>% 
  filter(codesum >=3) %>% 
  select(token) %>% 
  pull()
rm(files, ratings)

# Economy terms
seed_econ <- c("economy", "economic", "markets", "trade", "business")

# Security terms
seed_security <- c("security", "defense", "military", "espionage", "intelligence")

# Liberal rights terms
seed_librigths <- c("rights", "liberty", "freedom", "justice", "equality")

# geopolitics terms
seed_outward <- c("geopolitical", "international", "global", "world", "worldwide", "transnational", "external")
seed_geo <- c("sovereignty", "geopolitical", "geopolitics", "territory", "territorial", "influence", "alliance", "diplomacy")

# risk/chance terms:
seed_chance <- c("potentials", "benefit", "advantage", "opportunity") # - profit
seed_risk <- c("danger", "threat", "vulnerabilities", "peril", "risk", "hazard") # - "protection"

seed_US <- (newsmap::data_dictionary_newsmap_en$AMERICA$NORTH$US %>% 
  str_remove(., "\\*| ") %>% 
  str_replace(., "us", "usa"))[-c(1,5)] %>% 
  c(., "u.s.", "u.s.a.")

seed_CN <- (c(newsmap::data_dictionary_newsmap_en$ASIA$EAST$CN,
             newsmap::data_dictionary_newsmap_en$ASIA$EAST$HK,
             #newsmap::data_dictionary_newsmap_en$ASIA$EAST$MO, # only geopolitical issues relating to china -> Macao less relevant 
             newsmap::data_dictionary_newsmap_en$ASIA$EAST$TW) %>% 
  str_remove(., " "))[-6]


dot_product_similarity <- function(matrix, vector) {
  apply(matrix, 1, function(row) sum(row * vector))
}

find_similar_wvs <- function(seed_dict, concept,  embeddings = read_rds("./large_data/glove.6B.300d.rds")){
  
  seed_vector <- embeddings %>% 
    select(all_of(seed_dict)) %>% # Probably needs a check whether all seeds exist in word vector
    rowMeans() # Aggregation by mean
  
  # find similar terms: ####
  
  require(text2vec)
  this_wv_mat <- matrix(seed_vector, ncol=length(seed_vector), nrow=1)
  all_wvs_mat <- as.matrix(embeddings)
  
  if(dim(this_wv_mat)[[2]] != dim(all_wvs_mat)[[2]]) {
    print("switching dimensions on the all_wvs_matrix")
    all_wvs_mat <- t(all_wvs_mat)
  }
  
  # cosine smilarity:
  # cos_sim = sim2(x=all_wvs_mat, y=this_wv_mat, method="cosine", norm="l2")
  
  # dot product similarity:
  dot_sim = dot_product_similarity(all_wvs_mat, this_wv_mat)
  sorted_sim <- sort(dot_sim, #cos_sim[,1], 
                     decreasing = T)
   
  
  simils <-
    sorted_sim %>% 
    as.data.frame() %>% 
    rename(concept = 1) %>% 
    rownames_to_column("token") %>% 
    mutate(seed = token %in% seed_dict) %>% 
    filter(!(token %in% quanteda::stopwords("english"))) %>% 
    arrange(desc(concept)) %>% 
    mutate(rank.simil=row_number()) %>% 
    relocate(rank.simil) # rank by similarity to seed vector
  
  return(simils)
}

# chance-vs-risk: ####

wvs_chance_risk <- 
  find_similar_wvs(seed_dict = seed_chance, concept = "chance") %>% 
  select(token, chance = concept) %>% 
  left_join(.,
            find_similar_wvs(seed_dict = seed_risk, concept = "risk") %>% 
              select(token, risk = concept),
            by = "token") %>% 
  mutate(chance_risk = chance - risk) %>% 
  arrange(desc(chance_risk))

# plot weights:
# Stratified sample
df <- wvs_chance_risk %>% 
  mutate(group = cut(chance_risk, 5)) %>% # Cut range of scale into n intervals
  group_by(group) %>% 
  sample_n(23) %>% # n words from each interval
  ungroup()



# Selected terms for highlighting

df.anchors <- wvs_chance_risk %>% 
  filter(token %in% c(seed_chance, seed_risk)) %>% 
  mutate(group = "Anchor terms defined by researcher")

df.learned <- wvs_chance_risk %>% 
  filter(token %in% c(
                      # "aggression", "mistrust", "anger", "tensions", "frustration", "escalation", "backlash", "insistence",
                      # "understanding", "partnership", "solidarity", "negotiate", "compromise", "peaceful", "friendship", "coalition",
                      # "coexistence", "unaccaptable", "stable", "stop", "normal", "responsible"
    )) %>% 
  mutate(group = "Example terms scaled by the algorithm")

df.highlight <- rbind(df.anchors, df.learned)
df.highlight$group <- fct_rev(factor(df.highlight$group, levels = c("Anchor terms defined by researcher", "Example terms scaled by the algorithm")))

df <- df %>% filter(!(token %in% df.highlight$token)) # Avoid duplicates



pl<- 
  ggplot()+
  #geom_text(data = df[sample(nrow(df), 2500, replace = F), ], alpha= .3, color = "grey60", aes(y=sample.int(100, size = 2500, replace =T), x = chance_risk, label = token)) +
  geom_text(data = df, alpha= .4, color = "grey60", aes(y=sample.int(100, size = nrow(df), replace =T), x = chance_risk, label = token)) +
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = df.highlight, aes(y=sample.int(100, size = nrow(df.highlight), replace =T), x = chance_risk, label = token, color = group), fontface = "bold")+
  scale_color_manual(values = c("#0380b5", "#9e3173"), name = "")+
  labs(title = "Scaling chance- vs risk-framed language",
       subtitle = paste0("Based on Glove.6B.300d word vector model and a stratified random sample of ", nrow(df), " words from its vocabulary"),
       y = "",
       x = "Extracted word weights\nbetween chance- vs risk-framed language")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        # text = element_text(family = "Dahrendorf"),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) 
pl
ggsave("./output/plots/ScalingWeights_Chance_Risk.png", width = 24, height = 12, units = "cm")


# sidequest: China vs US: ####
wvs_ch_us <- 
  find_similar_wvs(seed_dict = seed_CN, concept = "China") %>% 
  select(token, China = concept) %>% 
  left_join(.,
            find_similar_wvs(seed_dict = seed_US, concept = "US") %>% 
              select(token, US = concept),
            by = "token") %>% 
  mutate(China_US = China - US) %>% 
  arrange(desc(China_US))

# plot weights:
# Stratified sample
df <- wvs_ch_us %>% 
  mutate(group = cut(China_US, 5)) %>% # Cut range of scale into n intervals
  group_by(group) %>% 
#  sample_n(25) %>% # n words from each interval
  ungroup()

# Selected terms for highlighting

df.anchors <- wvs_ch_us %>% 
  filter(token %in% c(seed_CN, seed_US)) %>% 
  mutate(group = "Anchor terms defined by researcher")

df.learned <- wvs_ch_us %>% 
  filter(token %in% c(
    # "aggression", "mistrust", "anger", "tensions", "frustration", "escalation", "backlash", "insistence",
    # "understanding", "partnership", "solidarity", "negotiate", "compromise", "peaceful", "friendship", "coalition",
    # "coexistence", "unaccaptable", "stable", "stop", "normal", "responsible"
  )) %>% 
  mutate(group = "Example terms scaled by the algorithm")

df.highlight <- rbind(df.anchors, df.learned)
df.highlight$group <- fct_rev(factor(df.highlight$group, levels = c("Anchor terms defined by researcher", "Example terms scaled by the algorithm")))

df <- df %>% filter(!(token %in% df.highlight$token)) # Avoid duplicates



pl<- 
  ggplot()+
  #geom_text(data = df[sample(nrow(df), 2500, replace = F), ], alpha= .3, color = "grey60", aes(y=sample.int(100, size = 2500, replace =T), x = chance_risk, label = token)) +
  geom_text(data = df, alpha= .4, color = "grey60", aes(y=sample.int(100, size = nrow(df), replace =T), x = China_US, label = token)) +
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = df.highlight, aes(y=sample.int(100, size = nrow(df.highlight), replace =T), x = China_US, label = token, color = group), fontface = "bold")+
  scale_color_manual(values = c("#0380b5", "#9e3173"), name = "")+
  labs(title = "Scaling China- and US-related terms",
       subtitle = paste0("Based on Glove.6B.300d word vector model and a stratified random sample of ", nrow(df), " words from its vocabulary"),
       y = "",
       x = "Extracted word weights\nbetween China- and US-related terms")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        # text = element_text(family = "Dahrendorf"),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) 
pl
ggsave("./output/plots/ScalingWeights_China_US.png", width = 24, height = 12, units = "cm")



# plot wordclouds of similar terms: ####
plot_similar_terms <- function(seed_dict, concept, embeddings = read_rds("./large_data/glove.6B.300d.rds")){
  
  seed_vector <- embeddings %>% 
    select(all_of(seed_dict)) %>% # Probably needs a check whether all seeds exist in word vector
    rowMeans() # Aggregation by mean
  
  # find similar terms: ####
  
  require(text2vec)
  this_wv_mat <- matrix(seed_vector, ncol=length(seed_vector), nrow=1)
  all_wvs_mat <- as.matrix(embeddings)
  
  if(dim(this_wv_mat)[[2]] != dim(all_wvs_mat)[[2]]) {
    print("switching dimensions on the all_wvs_matrix")
    all_wvs_mat <- t(all_wvs_mat)
  }
  
  # cosine smilarity:
  # cos_sim = sim2(x=all_wvs_mat, y=this_wv_mat, method="cosine", norm="l2")
  
  # dot product similarity:
  dot_sim = dot_product_similarity(all_wvs_mat, this_wv_mat)
  sorted_sim <- sort(dot_sim, #cos_sim[,1], 
                     decreasing = T)
  
  # Extract cosine similarities to this average vector
  # (word weights that would semantically pull a text towards the seed)
  simils <-
    sorted_sim %>% 
    as.data.frame() %>% 
    rename(concept = 1) %>% 
    rownames_to_column("token") %>% 
    mutate(seed = token %in% seed_dict) %>% 
    filter(!(token %in% quanteda::stopwords("english"))) %>% 
    arrange(desc(concept)) %>% 
    mutate(rank.simil=row_number()) %>% 
    relocate(rank.simil) # rank by similarity to seed vector
  
  pl <-
    ggplot(simils %>% ungroup() %>% arrange(desc(concept)) %>% head(200), 
           aes(label = token, size = concept, color = seed)) +
    geom_text_wordcloud() +
    scale_radius(range = c(1.5, 6), limits = c(0, NA)) +
    scale_colour_manual(values = c("blue", "red"))+
    labs(title = paste0("Semantic similarity to vector of \'", concept, "\' seed terms"),
         subtitle = "Based on Glove.6B.300d model; Top-200 words most similar to average word vector of the seed terms\nWords in red are seed terms, words in blue are \'learned\' from the pre-trained word vector model.")+
    theme_minimal()+
    theme(plot.background = element_rect(color = "white"))

  return(pl)
}


plot_similar_terms(seed_dict = seed_geo, concept = "geopolitics")
ggsave("./output/plots/SemanticallySimilarTerms_GP.png", width = 24, height = 12, units = "cm")

plot_similar_terms(seed_dict = seed_outward, concept = "outward-lookingness")
ggsave("./output/plots/SemanticallySimilarTerms_outward-looking.png", width = 24, height = 12, units = "cm")

plot_similar_terms(seed_dict = seed_risk, concept = "risk")
ggsave("./output/plots/SemanticallySimilarTerms_risk.png", width = 24, height = 12, units = "cm")

plot_similar_terms(seed_dict = seed_chance, concept = "chance")
ggsave("./output/plots/SemanticallySimilarTerms_chance.png", width = 24, height = 12, units = "cm")

# CMs:
plot_similar_terms(seed_dict = seed_US, concept = "USA")
ggsave("./output/plots/SemanticallySimilarTerms_USA.png", width = 24, height = 12, units = "cm")

plot_similar_terms(seed_dict = seed_CN, concept = "China")
ggsave("./output/plots/SemanticallySimilarTerms_CN.png", width = 24, height = 12, units = "cm")



# MAIN: Calculate concept movers distances ####

# Note: no scaling applied (can be done ex post)
# Note: parallel = FALSE

start <- Sys.time()
cmds <- cbind(CMDist(dtm = dfm_para, cv = get_centroid(seed_dp_simple, wv = glove.300), wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cv = get_centroid(seed_dp_adv, wv = glove.300), wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cv = get_centroid(seed_econ, wv = glove.300), wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cv = get_centroid(seed_security, wv = glove.300), wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cv = get_centroid(seed_librigths, wv = glove.300), wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cv = get_centroid(seed_outward, wv = glove.300), wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cv = get_centroid(seed_geo, wv = glove.300), wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cv = get_centroid(seed_chance, wv = glove.300), wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cv = get_centroid(seed_risk, wv = glove.300), wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cv = get_centroid(seed_US[1:3], wv = glove.300), wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cv = get_centroid(seed_CN, wv = glove.300), wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cw = seed_CN, wv = glove.300, scale = F)[,2],
              CMDist(dtm = dfm_para, cw = "china", wv = glove.300, scale = F)[,2]
              ) %>%
  as.data.frame() %>% 
  rename(cmd_digital_simple = 1,
         cmd_digital_adv =2,
         cmd_economy = 3,
         cmd_security = 4,
         cmd_librights = 5,
         cmd_outward =6,
         cmd_geo = 7,
         cmd_chance = 8,
         cmd_risk = 9,
         cmd_us = 10,
         cmd_cn = 11,
         test_vec = 12,
         test_word = 13) %>% 
  mutate(id = row_number()) # Quanteda preserves order (I hope ;)) %>% 
Sys.time() - start  


write_rds(cmds, "./data/CMDs.rds")

# risk-chance scaling:
cmds %<>% 
  mutate(cmd_risk_vs_chance = cmd_chance - cmd_risk) %>% 
  left_join(texts %>% select(c(id, text_para, doc_key, doc_type, para_type)), by = "id") %>% 
  relocate(c(id, text_para))



# plot CMDs over time: ####

analysis_data <- read_rds("./data/ParaLevelData_zs.rds")
cmds <- read_rds("./data/CMDs.rds")

avg_cmds <- 
  cmds %>% 
  pivot_longer(cols = starts_with("cmd_"), names_to = "concept", values_to = "avg_distance") %>% 
  left_join(., 
            analysis_data %>% 
              select(-any_of(names(cmds)), id),
            join_by(id)
  ) %>%  
  mutate(month = zoo::as.yearmon(date)) %>% 
  group_by(month, concept) %>%
  mutate(avg_distance_month = mean(avg_distance, na.rm=T),
         year = year(month)) %>% 
  ungroup()

rm(analysis_data, cmds)
gc()


# plot digitality over time:
  ## around 2011-2013 sth pulls CMDs downwards (both for paras classified as digital and non-digital) -> what is it?!?
  ## generally, there's no recency bias to see here!
avg_cmds %>% 
  mutate(concept = str_remove(concept, "cmd_")) %>% 
  filter(concept %in% c("digital_adv" #, "digital_simple"
                        ) & 
           !is.na(zs_digital)) %>%
  ggplot(aes(y = avg_distance, colour = concept)) +
  geom_smooth(aes(x = date)) +
  facet_wrap(vars(zs_digital), nrow = 1) +
  theme_bw()
ggsave("./output/plots/cmd_digitality.png", width = 24, height = 12, units = "cm")

# plot risk/chance over time:
avg_cmds %>% 
  mutate(concept = str_remove(concept, "cmd_")) %>% 
  filter(concept %in% c("risk", "chance") & 
           !is.na(zs_digital)) %>% 
  ggplot(aes(y = avg_distance, colour = concept)) +
#  geom_line(aes(x = month)) +
  geom_smooth(aes(x = date)) +
  facet_wrap(vars(zs_digital), nrow = 1) +
  theme_bw()

# risk vs chance:
avg_cmds %>% 
  mutate(concept = str_remove(concept, "cmd_")) %>% 
  filter(concept %in% c("risk_vs_chance") & 
           !is.na(zs_digital)) %>% 
  ggplot(aes(y = avg_distance, colour = concept)) +
  #  geom_line(aes(x = month)) +
  geom_smooth(aes(x = date)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(vars(zs_digital), nrow = 1) +
  theme_bw()
ggsave("./output/plots/cmd_risk-vs-chance.png", width = 24, height = 12, units = "cm")


# plot concerns over time
avg_cmds %>% 
  mutate(concept = str_remove(concept, "cmd_")) %>% 
  filter(concept %in% c("economy", "security", "librights") & 
           !is.na(zs_digital)) %>% 
  ggplot(aes(y = avg_distance, colour = concept)) +
#  geom_line(aes(x = month)) +
  geom_smooth(aes(x = month)) +
  facet_wrap(vars(zs_digital), nrow = 1) +
  theme_bw()

ggsave("./output/plots/cmd_concerns.png", width = 24, height = 12, units = "cm")

