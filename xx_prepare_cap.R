### prepare cap ####

library(tidyverse)

set.seed(42)

# all_texts <- readr::read_rds("./data/all_texts.rds")
# 
# uniquetexts <- 
#   all_texts %>% 
#   distinct(text_para, .keep_all = T) %>% 
#   select(id_original = id, text = text_para) %>% 
#   slice_sample(., prop = 1) %>% 
#   mutate(id = row_number())

# write_rds(uniquetexts, "./data/cap_id_matching.rds")
# write.csv(uniquetexts %>% select(id, text), "../data/paras_for_cap.csv")


### send for coding ###
#...let babelmachine do the work...

### process cap ####


### ultra weird issue: bubblemachine reads certain strings ("n/a", "null", "None", "NA", "N/A") from csv as NA -> NA classified...  

# restore original id:

paras_coded <- read_csv("../data/paras_for_cap_adWKU4cG0F_with_pred.csv") %>% 
  select(-c(`unnamed: 0`)) %>% 
  left_join(., read_rds("./data/cap_id_matching.rds"), join_by(id)) %>% 
  select(-id, -text.x) %>% 
  rename(id = id_original, text = text.y) %>% 
  mutate(major_topic_pred = ifelse(is.na(major_topic_pred), 999, major_topic_pred),
         major_topic_pred_name = ifelse(is.na(major_topic_pred_name), "No Policy Content", major_topic_pred_name))

# codings are the same for same texts:
matched_paras <- 
  read_rds("./data/ParaLevelData_zs.rds") %>% 
    left_join(., 
              paras_coded %>% select(-id),
              join_by(text_para == text)) %>% 
  left_join(., 
            read_rds("./data/DocLevelData_zs.rds") %>% select(-c(date, doc_type)),
            join_by(doc_key), suffix = c("_para", "_doc")) %>%
  
  # addititional variables:
  mutate(vdl = date >= as.Date("2019-12-01"))



# update DB with CAP: ####
source("../00_connectDB.R") # connect
all_texts <- readr::read_rds("./data/all_texts.rds") %>% 
  left_join(., paras_coded %>% select(-id), join_by("text_para" == "text")) %>% 
  rename(c("CAP_code" = "major_topic_pred", "CAP_name" = "major_topic_pred_name"))

dbWriteTable(con, "all_texts", all_texts)


### basic cap plotting ####

rm(matched_paras, paras_coded)
gc()

plot_data <- 
  matched_paras %>% 
  filter(date>=as.Date("1997-01-01") & !is.na(zs_digital)) %>% 
  mutate(dummy=1) %>% 
  pivot_wider(names_from = major_topic_pred_name, values_from = dummy, values_fill = 0, values_fn = ~ mean(.x, na.rm = TRUE)) %>% 
  mutate(Geopolitics = `Defense` + `International Affairs`,
         Economics = `Domestic Commerce` + `Foreign Trade` + `Labor` + `Macroeconomics`
         ) %>% 
  pivot_longer(cols = c(`International Affairs`:`Public Lands`, Economics, Geopolitics), names_to = "major_topic", values_to = "dummy") %>% 
  group_by(date, major_topic, zs_digital, vdl) %>% 
  summarise(share = mean(dummy)) %>% 
  ungroup()

# ToDo: select GP-relevant caps

# plotly::ggplotly(
  plot_data %>% 
    filter(major_topic %in% c("International Affairs", "Defense", "Domestic Commerce", "Foreign Trade", "Labor", "Macroeconomics", "Technology")) %>% 
  ggplot(aes(x=date, y=share, colour = major_topic))+#, group = vdl)) +
    geom_vline(xintercept = as.Date("2019-12-01"), linetype = "longdash") +
    scale_y_continuous(labels = scales::label_percent()) +
    geom_smooth(aes(group = interaction(major_topic, vdl)), method = "loess") +
    facet_wrap(vars(reorder(zs_digital, zs_digital, decreasing = T)), nrow = 2, labeller = as_labeller(c("0"="non-digital context", "1"="digital context"))) +
    theme_light() +
    scale_color_brewer(palette = "Dark2") +
    labs(colour = NULL, y = "Share of paragraphs", x = NULL)
#  )

  ggsave("output/plots/policy_relations_vdl.png", width = 24, height = 12, units = "cm")
  

  plot_data %>% 
    filter(major_topic %in% c("Economics", "Geopolitics", "Technology")) %>% 
    ggplot(aes(x=date, y=share, colour = major_topic))+
    geom_vline(xintercept = as.Date("2019-12-01"), linetype = "longdash") +
    scale_y_continuous(labels = scales::label_percent()) +
    geom_smooth(aes(group = interaction(major_topic, vdl)), method = "loess") +
    facet_wrap(vars(reorder(zs_digital, zs_digital, decreasing = T)), nrow = 2, labeller = as_labeller(c("0"="non-digital context", "1"="digital context"))) +
    theme_light() +
    scale_color_brewer(palette = "Accent") +
    labs(colour = NULL, y = "Share of paragraphs", x = NULL)
  
  ggsave("output/plots/policy_relations_vdl_clustered.png", width = 24, height = 12, units = "cm")
  