# Create Validation Sample:

# Input: rds-file zeroshot_results.rds (from zeroshot_digital.ipynb), tibble paras_scaled (from 10_digitality_scaling.R)
# Output: validation_sample.csv

# Packages ####
library(tidyverse)
library(magrittr)

set.seed(68)
options(scipen = 42)


handcoding <- read_csv2("../data/handcoding_raw.csv") %>% 
  select(id, text_para) %>% 
  left_join(., 
            read_csv2("../data/handcoding_prepared.csv") %>% select(-text_para),
            join_by(id)) %>% 
  left_join(., 
            all_texts, 
            join_by(text_para)) %>% select(id = id.y, text_para, pre_na = ms_na, pre_0 = ms_0, doc_ID = id.x) 
# write_csv2(handcoding, "data/precoding.csv")
  

mc <- read_csv("../TRIAS-Development/Data/output_manual_coding.csv") %>% 
  full_join(., 
          handcoding,
          join_by(doc_ID)) %>% 
  mutate(coder_ID = ifelse(is.na(coder_ID), 666, coder_ID),
         Digitality = if_else(pre_0==1 | pre_na ==1, 0, Digitality, Digitality)
         )

write_csv(mc %>% select(doc_ID = id, coder_ID, coding_date, Digitality, text_para), "data/output_manual_coding.csv")

# join digitality scores ####
zeroshot_digital <- read_csv("data/zeroshot_digital.csv") # only distinct paragraphs

# some of these paragraphs have multiple occurrences, so we actually have classifications for a even larger part of EC communication: 
zeroshot_scores_digital <- 
  zeroshot_digital %>% 
  select(text_para, digital_zeroshot) %>% 
  left_join(., all_texts, by=join_by(text_para)) %>% 
  select(names(all_texts), digital_zeroshot)

# paras_scaled <- readr::read_rds("data/paras-DPcoded.rds")

all_texts %<>%
  left_join(.,
            paras_scaled %>% 
              select(-any_of(names(all_texts)), id),
            by = join_by(id)
            ) %>% 
  left_join(.,
            zeroshot_scores_digital %>% 
              select(-any_of(names(all_texts)), id),
            by = join_by(id)
            )

# exclude recurring paragraphs: ####

## recurring paragraphs are typically service information. Some provide legal context for EC proceedings. Also lots of (percentage) numbers from ill-formatted tables etc.

## for validation sample, we exclude larger share of high frequency paragraphs, 
##  as p(serviceparagraph) >> p(relevantparagraph) and hence p(draw | serviceparagraph) >> p(draw | relevantparagraph)

duplicates <- 
  all_texts %>% 
  filter(duplicated(text_para)) %>% 
  group_by(text_para) %>% 
  summarise(freq = n() + 1) %>% 
  arrange(desc(freq))


# plot duplicate exclusion:
exclusion_threshold <- tibble()

for(i in 2:duplicates$freq[1]){
  
  exclusion_threshold %<>%  
    bind_rows(., 
              tibble_row(
                threshold = i,
                paras_excluded = (duplicates %>%
                  filter(freq >= i) %>% 
                  summarise(freq = sum(freq)) %>% 
                              pull(freq))[1]
              )
    )
  }

### face validity check: >= 5 seems reasonable threshold: 
  exclusion_threshold %>% 
    ggplot(aes(x = threshold, y = paras_excluded)) +
    geom_point() +
    geom_line() +
    coord_cartesian(xlim = c(0, 50))
  
  # doppeltexte %>% filter(freq >= 5) %>% arrange(freq) %>% view() # 113287 paragraphs, actually we could drop even more for handcoding!
frequentparas <- duplicates %>% filter(freq >= 5) %>% pull(text_para)



# create sample ####
samplesize = 2750
n_quantiles = 5 # for stratification
r = 50 # percentage random sample part



sample_from <- 
  all_texts %>%
  filter(., 
         !text_para %in% frequentparas &                  # exclude high frequency paras, that is usually service information
           wordcount >= 5 &                               # no meaningful arguments under 3 words, few under 5, excludes mostly greetings and ill-formatted table cells
           lang == "en" &                                 # english only
           str_starts(text_para, "Automatic title|Midday Express|Daily News", negate = T) & # some more trash & service to remove
           str_detect(text_para, "\\b[[:alpha:]]+\\b") &  # at least on real word (not only DocumentID etc.)
           str_detect(text_para, paste("^[0-9]{1,2}", "(", paste(c(month.name, month.abb), collapse = "|"), ")", "[0-9]{4}$"), negate = T), # date as text para
         str_starts(text_para, "Brussels|Strasbourg|\\*Updated|http", negate=T), # metainfo in text # add "|Website of"
         str_detect(text_para, "press release|speech|readout|statement", negate=T) # descriptives within text
         # , doc_type %in% c("speech", "press", "statement", "qa") # infringements, country reports, etc. hardly matter to us. News might need closer attention.
         # (how to) filter merger decisions?
  ) %>% 
  mutate(year = year(date),
         zs_quantile = ntile(digital_zeroshot, n = n_quantiles),
         sim_quantile = ntile(sem_simil, n = n_quantiles),
         zs_sim_diff = digital_zeroshot - sem_simil,
         zs_sim_quant_diff = zs_quantile - sim_quantile,
         zs_sim_diff_group = case_when(
           zs_sim_quant_diff == -4 ~ 'sim>>zs',
           zs_sim_quant_diff == 4 ~ 'zs>>sim',
           TRUE ~ NA
         ))

# random sample ####
randomsamplesize = r/100 * samplesize

grouped_data <- sample_from %>% 
  filter(., 
         is.na(digital_zeroshot) &    # already high likelyhood to be sampled as edge cases
           year<2024 &         # exclude current year
           para_type != "title") %>%  # titles to descriptive to often ("Speech by ... at ..." etc)
  
  # mutate(value_strata = case_when(...))  # split classification values in classes in case they are used for stratifiction
  mutate(doc_type = ifelse(doc_type %in% c("speech", "press", "statement", "qa"), doc_type, "other"),
         group = fct_lump_lowfreq(str_c(doc_type, year))) %>% # to balance p(sampling), keep n per group as close as possible (N_max < 20 * N_min)
  group_by(group)

n_sample_splits <- 
  grouped_data %>% 
  n_groups()

# documentation of drawing prob
(N_per_group <- 
    grouped_data %>% 
    summarise(N = n()) %>% 
    arrange(N))

rand_sample <- 
  grouped_data %>% 
  sample_n(size = round(randomsamplesize / n_sample_splits)) # round up, not down (default) to make up for meaningless paras
# -> 15 observations per group


# sample from semantic scaling & zeroshot classification:

# 100-r% edge cases (also stratified) ####
nr = 100-r
edgesamplesize = nr/100 * samplesize
edgesamples_n = c(1/3, 1/3, 1/3) * edgesamplesize

### zeroshot classifier:
grouped_zs <- sample_from %>% 
  filter(!is.na(zs_quantile)) %>% # lower number of observations, so don't remove titles
  mutate(group = fct_lump_min(str_c("zs", zs_quantile, "q_", year), min = 3)) %>% 
  group_by(group)

(n_sample_splits <- grouped_zs %>% n_groups())
(N_per_group = grouped_zs %>% summarise(N = n()) %>% arrange(N)) # for documentation of drawing prob


edge_sample_zs <- grouped_zs %>% 
  sample_n(size = round(edgesamples_n[1] / n_sample_splits)) # round up, not down (default) to make up for meaningless paras



### semantic similarity:
grouped_sim <- sample_from %>% 
  filter(!is.na(sim_quantile) & para_type != "title") %>% # enugh observations, so titles can be removed
  mutate(group = fct_lump_min(str_c("sim", sim_quantile, "q_", year), min = 3)) %>% 
  group_by(group)

(n_sample_splits <- grouped_sim %>% n_groups())
(N_per_group = grouped_sim %>% summarise(N = n()) %>% arrange(N)) # for documentation of drawing prob


edge_sample_sim <- grouped_sim %>% 
  sample_n(size = round(edgesamples_n[2] / n_sample_splits)) # round up, not down (default) to make up for meaningless paras



### extreme differences between classifiers:
grouped_diff <- sample_from %>% 
  filter(!is.na(zs_sim_diff_group) & !id %in% c(edge_sample_sim$id, edge_sample_zs$id)) %>% # fewer observations, so titles kept
  # no grouping by year here, we draw >2/3 of all cases anyway
  mutate(group = str_c(zs_sim_diff_group)) %>% 
  group_by(group)
# %>% summarise(N = n()) # -> zs>>sim   255; sim>>zs   366

(n_sample_splits <- grouped_diff %>% n_groups())
(N_per_group = grouped_diff %>% summarise(N = n()) %>% arrange(N)) # for documentation of drawing prob

edge_sample_diff <- grouped_diff  %>% 
  sample_n(size = round(edgesamples_n[3] / n_sample_splits)) # round up, not down (default) to make up for meaningless paras


# bind sample together ####
full_sample <- bind_rows(
  rand_sample,
  edge_sample_zs,
  edge_sample_sim,
  edge_sample_diff
) %>% ungroup() %>% 
  distinct(id, .keep_all = T) %>% 
  arrange(wordcount, n_chars_para)

# save sample ####
write.csv(file = "./Data/handcoding_sample.csv", 
          handcoding_sample, 
          quote = c(2), # Important!
          row.names = FALSE)

