# check sem simil "digtal' against human codes
# Author: @ChRauh

# Packages ####
library(tidyverse)
library(tidytext)
library(ggdist)
library(patchwork)

# Manual coding results ####
mc <- read_csv("./data/output_manual_coding.csv") %>% 
  arrange(doc_ID, coder_ID)

# What is coder_ID == 666 ?


# Get on text level (overlap sample)
mc <- mc %>% 
  group_by(doc_ID, text_para) %>% 
  summarise(digitality_sum = sum(Digitality),
            obs = n()) %>% 
  ungroup() %>% 
  rename(id = doc_ID,
         text = text_para) %>% 
  mutate(overlap = obs > 1) %>% 
  mutate(digital = digitality_sum > 0) # At least one coder say this is digital


# Semantic scaling - digitality ####

# Term weights
digi_weights <- read_rds("./large_data/SemSimilWeights-Digitality.rds")

# tokenize texts and merge with weights
digi_scaled <- 
  mc %>% 
  select(id, text) %>% 
  unnest_tokens(input = text,
                output = token,
                token = "words",
                to_lower = T) %>% 
  left_join(digi_weights %>% select(token, sim.target), 
            by = "token") %>% 
  rename(simil = sim.target) %>% 
  group_by(id) %>% 
  summarise(simil = mean(simil, na.rm = T))

# Write to full data
mc <- mc %>% 
  left_join(digi_scaled, by = "id") %>% 
  arrange(desc(simil))

# Still text duplicates in there ....



# Inspect ####

ggplot(mc, aes(x=simil)) + 
  geom_density()

ggplot(mc, aes(x=simil, color = digital)) + 
  geom_density()


pl <-
  ggplot(mc, aes(x = simil, y = digital, color = digital, fill = digital))+
  # geom_point()
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
  ) +
  labs(title = "Digital affais mentioned in text?", 
       subtitle = "Human coders vs semantic similarity scaling, based on 3442 paragraphs from EC communication",
       y = "At least one coder considered\nthe text relevant for digital affairs\n",
       x = "Semantic similarity score\n(Average cosine simililarity of tokens in text too seed terms\n\"digital\", \"computer\", \"online\", \"internet\", and \"algorithm\")")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggsave("./output/plots/Digitality_SemanticSimilarityVShumanCoders.png", pl, width = 24, height = 12, units = "cm")



# Cutoffs ####

cutoff<- mc %>% 
  mutate(cutoff11 = simil > .11,
         cutoff12 = simil > .12,
         cutoff13 = simil > .13,
         cutoff14 = simil > .14,
         cutoff15 = simil > .15,
         cutoff16 = simil > .16,
         cutoff17 = simil > .17,
         cutoff18 = simil > .18,
         cutoff19 = simil > .19,
         cutoff20 = simil > .20,
         cutoff21 = simil > .21,
         cutoff22 = simil > .22,
         cutoff23 = simil > .23,
         cutoff24 = simil > .24,
         cutoff25 = simil > .25,
         cutoff26 = simil > .26,
         cutoff27 = simil > .27,
         cutoff28 = simil > .28,
         cutoff29 = simil > .29,
         cutoff30 = simil > .30,
         cutoff31 = simil > .31,
         cutoff32 = simil > .32,
         cutoff33 = simil > .33,
         cutoff34 = simil > .34,
         cutoff35 = simil > .35) %>% 
  select(id, digital, starts_with("cutoff")) %>% 
  pivot_longer(cols = starts_with("cutoff"), names_to = "cutoff", values_to = "class_digital") %>% 
  mutate(cutoff = str_replace(cutoff, "cutoff", ".") %>% as.numeric)

# cutoff$outcome <- NA
# cutoff$outcome[cutoff$digital == cutoff$class_digital] <- "correct"
# cutoff$outcome[cutoff$digital == T & cutoff$class_digital == F] <- "falseneg"
# cutoff$outcome[cutoff$digital == F & cutoff$class_digital == T] <- "falsepos"
# sum(is.na(cutoff$outcome))

cutoff$correct <- cutoff$digital == cutoff$class_digital
cutoff$falseneg <- cutoff$digital == T & cutoff$class_digital == F
cutoff$falsepos <- cutoff$digital == F & cutoff$class_digital == T
cutoff$truepos <- cutoff$digital == T & cutoff$class_digital == T
cutoff$trueneg <- cutoff$digital == F & cutoff$class_digital == F

# Aggregate
cutoff_agg <- cutoff %>% 
  group_by(cutoff) %>% 
  summarise(correct = sum(correct),
            falseneg = sum(falseneg),
            falsepos = sum(falsepos),
            truepos = sum(truepos),
            trueneg = sum(trueneg),
            obs = n()) %>% 
  mutate(accuracy = correct/obs,
         precision = truepos/(truepos+falsepos),
         recall = truepos/(truepos+falseneg),
         falsenegrate = falseneg/obs,
         falseposrate = falsepos/obs,
         f1 = 2*(precision*recall)/(precision+recall),
         tpr = truepos/(truepos+falseneg),
         tnr = trueneg/(trueneg+falsepos),
         baccuracy = (tpr+tnr)/2) # Balance accuracy, cf.: https://www.educative.io/answers/what-is-balanced-accuracy


# Plots ####


# Accuracy
pl.accuracy <-
  ggplot(cutoff_agg, aes(x=cutoff, y = accuracy, group = 1), )+
  geom_point(color = "darkgreen")+
  geom_line(color = "darkgreen")+
  geom_vline(xintercept = cutoff_agg$cutoff[cutoff_agg$accuracy == max(cutoff_agg$accuracy)], color = "darkgreen")+
  labs(title = "Accuracy",
       subtitle = "What overall share of texts is correctly classified?",
       y = "Accuracy\n",
       x = "Chosen semantic similarity cutoff")+
  theme_bw()

# Balanced Accuracy
pl.baccuracy <-
  ggplot(cutoff_agg, aes(x=cutoff, y = baccuracy, group = 1), )+
  geom_point(color = "darkgreen")+
  geom_line(color = "darkgreen")+
  geom_vline(xintercept = cutoff_agg$cutoff[cutoff_agg$baccuracy == max(cutoff_agg$baccuracy)], color = "darkgreen")+
  labs(title = "Balanced Accuracy",
       subtitle = "What overall share of texts is correctly classified\n(if one accounts for the highly imbalanced validation sample)?",
       y = "Accuracy\n",
       x = "Chosen semantic similarity cutoff")+
  theme_bw()


# Precision
pl.precision <-
  ggplot(cutoff_agg, aes(x=cutoff, y = precision, group = 1), )+
  geom_point(color = "orange")+
  geom_line(color = "orange")+
  geom_vline(xintercept = cutoff_agg$cutoff[cutoff_agg$precision == max(cutoff_agg$precision)], color = "orange")+
  labs(title = "Precision",
       subtitle = "What share of retrieved documents is actually relevant?",
       y = "Precision\n",
       x = "Chosen semantic similarity cutoff")+
  theme_bw()

# Recall
pl.recall <-
  ggplot(cutoff_agg, aes(x=cutoff, y = recall, group = 1), )+
  geom_point(color = "red")+
  geom_line(color = "red")+
  geom_vline(xintercept = cutoff_agg$cutoff[cutoff_agg$recall == max(cutoff_agg$recall)], color = "red")+
  labs(title = "Recall (sensitivity)",
       subtitle = "What share of actually relevant documents is is retrieved?",
       y = "Recall\n",
       x = "Chosen semantic similarity cutoff")+
  theme_bw()

# F1 Score
pl.f1 <-
  ggplot(cutoff_agg, aes(x=cutoff, y = f1, group = 1), )+
  geom_point(color = "black")+
  geom_line(color = "black")+
  geom_vline(xintercept = cutoff_agg$cutoff[cutoff_agg$f1 == max(cutoff_agg$f1)], color = "black")+
  labs(title = "F1 score",
       subtitle = "Harmonized mean of recall & precision",
       y = "F1-score\n",
       x = "Chosen semantic similarity cutoff")+
  theme_bw()




# Combined plot

pl.comb <- 
  pl.accuracy / pl.baccuracy / pl.precision / pl.recall / pl.f1

ggsave("./output/plots/DigitalClassification_Cutoff.png", pl.comb, width = 16, height = 36, units = "cm")


# You may decide to use precision or recall on your imbalanced classification problem.
# Maximizing precision will minimize the number false positives, whereas maximizing the recall will minimize the number of false negatives.
# Precision: Appropriate when minimizing false positives is the focus.
# Recall: Appropriate when minimizing false negatives is the focus.
# https://machinelearningmastery.com/precision-recall-and-f-measure-for-imbalanced-classification/

# 
# With a large number of negative samples - precision is probably better
# If the number of negative samples is very large (a.k.a imbalance data set) the false positive rate increases more slowly. Because the true negatives (in the fpr denominator - (FP+TN)) would probably be very high and make this metric smaller.
# Precision however, is not affected by a large number of negative samples, that's because it measures the number of true positives out of the samples predicted as positives (TP+FP).
# Precision is more focused in the positive class than in the negative class, it actually measures the probability of correct detection of positive values, while FPR and TPR (ROC metrics) measure the ability to distinguish between the classes.



# False negative rate
pl.false_neg <- 
  ggplot(cutoff_agg, aes(x=cutoff, y = falsenegrate, group = 1), )+
  geom_point(color = "orange")+
  geom_line(color = "orange")+
  geom_vline(xintercept = cutoff_agg$cutoff[cutoff_agg$falsenegrate == min(cutoff_agg$falsenegrate)], color = "orange")+
  theme_bw()

# False positive rate
pl.false_pos <- 
  ggplot(cutoff_agg, aes(x=cutoff, y = falseposrate, group = 1), )+
  geom_point(color = "orange")+
  geom_line(color = "orange")+
  geom_vline(xintercept = cutoff_agg$cutoff[cutoff_agg$falseposrate == min(cutoff_agg$falseposrate)], color = "orange")+
  theme_bw()


