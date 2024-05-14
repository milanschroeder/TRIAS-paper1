# Check sem simil "digtal' against human codes
# Author: @ChRauh

# Packages ####
library(tidyverse)
library(tidytext)
library(ggdist)
library(ggtext)
library(patchwork)

# Manual coding results ####
mc <- read_csv("./data/output_manual_coding.csv") %>% 
  arrange(doc_ID, coder_ID) %>% 
  filter(coder_ID != 666) # Cases fqualitatively filtered out before human coding


# Aggregate to text level
# Duplicates because their was an intentionally created overlap sample for which multiple coders coded the same texts
mc <- mc %>% 
  group_by(doc_ID, text_para) %>% 
  summarise(digitality_sum = sum(Digitality),
            obs = n()) %>% 
  ungroup() %>% 
  rename(id = doc_ID,
         text = text_para) %>% 
  mutate(overlap = obs > 1) %>% 
  mutate(digital = digitality_sum > 0) # At least one coder says this is digital

# Still text duplicates in there
# Everything the same except id
mc$dupl <- duplicated(mc %>% select(-id))
sum(mc$dupl)
mc <- mc %>% filter(!dupl) %>% select(-dupl)


# Semantic scaling - digitality ####

# Term weights
digi_weights_simple <- read_rds("./large_data/SemSimilWeights-DigitalitySimple.rds")
digi_weights_advanced <- read_rds("./large_data/SemSimilWeights-DigitalityAdvanced.rds")
digi_weights_advanced_freq <-read_rds("./large_data/SemSimilWeights-DigitalityAdvancedFreqCorrection.rds")

# tokenize texts and merge with weights
digi_scaled <- 
  mc %>% 
  select(id, text) %>% 
  unnest_tokens(input = text,
                output = token,
                token = "words",
                to_lower = T) %>% 
  left_join(digi_weights_simple %>% select(token, sim.target), 
            by = "token") %>% 
  rename(simil_simple = sim.target) %>% 
  left_join(digi_weights_advanced %>% select(token, sim.target), 
            by = "token") %>% 
  rename(simil_adv = sim.target) %>% 
  left_join(digi_weights_advanced_freq %>% select(token, sim.target), 
            by = "token") %>% 
  rename(simil_adv_freq = sim.target) %>% 
  group_by(id) %>% 
  summarise(simil_simple = mean(simil_simple, na.rm = T),
            simil_adv = mean(simil_adv, na.rm = T),
            simil_adv_freq = mean(simil_adv_freq, na.rm = T))

# Write to full data
mc <- mc %>% 
  left_join(digi_scaled, by = "id") %>% 
  arrange(desc(simil_adv_freq))


# Add zeroshot classification
# zs <- read_csv("./data/zeroshot_digital.csv")
# mc <- mc %>% 
#   left_join(zs %>% select(id, digital_zeroshot), by = "id")
# sum(is.na(mc$digital_zeroshot)) # 2395 - smth wrong here!


# Long version
mc_long <- mc %>% select(id, digital, simil_simple, simil_adv, simil_adv_freq) %>% 
  pivot_longer(cols = 3:5, names_to = "seed_dict", values_to = "simil") %>% 
  mutate(seed_dict = ifelse(seed_dict == "simil_simple", "Simple seed dictionary",
                              ifelse(seed_dict == "simil_adv", "Advanced seed dictionary",
                                     "Advanced seed dict. (freq. correction)")),
         seed_dict = factor(seed_dict, levels = c("Simple seed dictionary", 
                                                  "Advanced seed dictionary", 
                                                  "Advanced seed dict. (freq. correction)")))

# Inspect ####

# How much do the different similarity weights differ?
ggplot(mc, aes(x = simil_simple, y = simil_adv, color = digital))+
  geom_point(size = 3, alpha = .4, shape = 16)+
  # scale_color_manual(values = c("#377eb8", "#e41a1c"))+
  scale_color_manual(values = c("#377eb8", "darkred"))+
  geom_vline(xintercept = mean(mc$simil_simple))+
  geom_hline(yintercept = mean(mc$simil_adv))+
  theme_bw()

ggplot(mc, aes(x = simil_simple, y = simil_adv_freq, color = digital))+
  geom_point(size = 3, alpha = .4, shape = 16)+
  # scale_color_manual(values = c("#377eb8", "#e41a1c"))+
  scale_color_manual(values = c("#377eb8", "darkred"))+
  geom_vline(xintercept = mean(mc$simil_simple))+
  geom_hline(yintercept = mean(mc$simil_adv_freq))+
  theme_bw()


# How are the similarity scores distributed?
# Advanced seed dictionary provides narrower distribution and longer right tail: promises more dicriminatory power
ggplot(mc_long, aes(x=simil, color = seed_dict))+
  geom_density()+
  theme_bw()+theme(legend.position = "bottom")


# Compare distribution of semantic similarity weights by human coder choice and dictionary
# Slightly less overlap for the advanced seed dictionary - hoping for more dicriminatory power
# But note the varying scales!

pl <-
  ggplot(mc_long, aes(x = simil, y = digital, color = digital, fill = digital))+
  scale_color_manual(values = c("darkred", "darkgreen"))+
  scale_fill_manual(values = c("darkred", "darkgreen"))+
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  )+
  facet_grid(.~seed_dict, scales = "free_x")+
  labs(title = "Distribution of semantic similarity scores against human coders", 
       subtitle = paste0("Based on a sample of ", nrow(mc), " paragraphs from EC communication documents"),
       y = "At least one coder considered\nthe text relevant for digital affairs\n",
       x = "Semantic similarity score\n(Average cosine simililarity of tokens in text to seed terms)")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))
ggsave("./output/plots/Digitality_SemanticSimilarityVShumanCoders.png", pl, width = 28, height = 12, units = "cm")  





# Cutoffs ####
# Building on Milan's approach

library(magrittr)

# Simple seed dictionary
cutoff_candidates_simple <- tibble()
for (cutoff in seq(0, .6, .01)) {
  
  mc %<>% mutate(pred = simil_simple > cutoff) 
  
  TP <- sum(mc$pred & mc$digital)
  TN <- sum(!mc$pred & !mc$digital)
  FP <- sum(mc$pred & !mc$digital)
  FN <- sum(!mc$pred & mc$digital)
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1 <- 2*(precision*recall)/(precision+recall)
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  baccuracy <- ((TP / (TP + FN)) + (TN / (TN + FP))) / 2 # Balanced accuracy (because imbalanced sample!) - https://neptune.ai/blog/balanced-accuracy
  
  cutoff_candidates_simple %<>% bind_rows(., tibble_row(cutoff, precision, recall, f1, accuracy, baccuracy, TP, FP, TN, FN))
}

# Advanced seed dictionary
cutoff_candidates_adv <- tibble()
for (cutoff in seq(0, .6, .01)) {
  
  mc %<>% mutate(pred = simil_adv > cutoff) 
  
  TP <- sum(mc$pred & mc$digital)
  TN <- sum(!mc$pred & !mc$digital)
  FP <- sum(mc$pred & !mc$digital)
  FN <- sum(!mc$pred & mc$digital)
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1 <- 2*(precision*recall)/(precision+recall)
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  baccuracy <- ((TP / (TP + FN)) + (TN / (TN + FP))) / 2 # Balanced accuracy (because imbalanced sample!) - https://neptune.ai/blog/balanced-accuracy
  
  cutoff_candidates_adv %<>% bind_rows(., tibble_row(cutoff, precision, recall, f1, accuracy, baccuracy, TP, FP, TN, FN))
}


# Advanced seed dictionary with frequency correction
cutoff_candidates_adv_freq <- tibble()
for (cutoff in seq(0, .6, .01)) {
  
  mc %<>% mutate(pred = simil_adv_freq > cutoff) 
  
  TP <- sum(mc$pred & mc$digital)
  TN <- sum(!mc$pred & !mc$digital)
  FP <- sum(mc$pred & !mc$digital)
  FN <- sum(!mc$pred & mc$digital)
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1 <- 2*(precision*recall)/(precision+recall)
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  baccuracy <- ((TP / (TP + FN)) + (TN / (TN + FP))) / 2 # Balanced accuracy (because imbalanced sample!) - https://neptune.ai/blog/balanced-accuracy
  
  cutoff_candidates_adv_freq %<>% bind_rows(., tibble_row(cutoff, precision, recall, f1, accuracy, baccuracy, TP, FP, TN, FN))
}



# Combine data and clean up
cutoffs <- rbind(cutoff_candidates_simple %>% mutate(seed_dict = "Simple seed dictionary"),
                 cutoff_candidates_adv %>% mutate(seed_dict = "Advanced seed dictionary"),
                 cutoff_candidates_adv_freq %>% mutate(seed_dict = "Advanced seed dict. (freq. correction)")) %>% 
  mutate(seed_dict = factor(seed_dict, levels = c("Simple seed dictionary", 
                                                  "Advanced seed dictionary", 
                                                  "Advanced seed dict. (freq. correction)")))

rm(cutoff_candidates_adv, cutoff_candidates_adv_freq, cutoff_candidates_simple, accuracy, baccuracy, cutoff, f1, FN, FP, precision, recall, TN, TP)


# Plot performance metrics


# # Accuracy
# pl.accuracy <-
#   ggplot(cutoffs, aes(x=cutoff, y = accuracy, group = 1), )+
#   geom_point(color = "darkgreen")+
#   geom_line(color = "darkgreen")+
#   facet_grid(.~seed_dict)+
#   labs(title = "Accuracy",
#        subtitle = "What overall share of texts is correctly classified?",
#        y = "Accuracy\n",
#        x = "Chosen semantic similarity cutoff")+
#   theme_bw()

# Balanced Accuracy
pl.baccuracy <-
  ggplot(cutoffs, aes(x=cutoff, y = baccuracy, group = 1), )+
  geom_point(color = "darkgreen")+
  geom_line(color = "darkgreen")+
  facet_grid(.~seed_dict)+
  labs(title = "<b>Balanced Accuracy</b>: What overall share of documents in each category is correctly classified?",
       y = "Accuracy\n",
       x = "")+
  theme_bw()+
  theme(plot.title = element_markdown())


# Precision and recall

pr <- cutoffs %>% 
  select(cutoff, seed_dict, precision, recall) %>% 
  pivot_longer(cols = 3:4, names_to = "metric", values_to = "value")

pl.precrec <-
  ggplot(pr, aes(x=cutoff, y = value, color = metric))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = c("#d95f02", "#7570b3"))+
  facet_grid(.~seed_dict)+
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
  ggplot(cutoffs, aes(x=cutoff, y = f1, group = 1), )+
  geom_point(color = "black")+
  geom_line(color = "black")+
  facet_grid(.~seed_dict)+
  labs(title = "<b>F1 Score</b>: Harmonized mean of recall & precision",
       y = "F1 score\n",
       x = "\nClassification cutoff applied to semantic similarity score")+
  theme_bw()+
  theme(plot.title = element_markdown())

# Combined plot
pl.comb <- 
  pl.baccuracy / pl.precrec / pl.f1

pl.comb

ggsave("./output/plots/DigitalClassification_Cutoff.png", pl.comb, width = 24, height = 27, units = "cm")


# You may decide to use precision or recall on your imbalanced classification problem.
# Maximizing precision will minimize the number false positives, whereas maximizing the recall will minimize the number of false negatives.
# Precision: Appropriate when minimizing false positives is the focus.
# Recall: Appropriate when minimizing false negatives is the focus.
# https://machinelearningmastery.com/precision-recall-and-f-measure-for-imbalanced-classification/


# With a large number of negative samples - precision is probably better
# If the number of negative samples is very large (a.k.a imbalance data set) the false positive rate increases more slowly. Because the true negatives (in the fpr denominator - (FP+TN)) would probably be very high and make this metric smaller.
# Precision however, is not affected by a large number of negative samples, that's because it measures the number of true positives out of the samples predicted as positives (TP+FP).
# Precision is more focused in the positive class than in the negative class, it actually measures the probability of correct detection of positive values, while FPR and TPR (ROC metrics) measure the ability to distinguish between the classes.



# GPT experiments
write_csv(mc %>% select(c(id, text)), "./data/GPT_sample.csv")
