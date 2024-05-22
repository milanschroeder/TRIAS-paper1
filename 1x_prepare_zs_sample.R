# Create ZS sample: ####

zs_todo <- 
  all_texts %>%
  filter(lang == "en" & doc_type %in% c("Read-out", "Speech", "Press release", "Statement")) %>%
  select(everything(), text = text_para) %>% 
  left_join(., 
            read_csv("./data/output_manual_coding.csv") %>% 
              group_by(doc_ID, text_para) %>% 
              summarise(
                # if any coder identified digitality, we treat paragraph as digital
                target_max = max(Digitality),
                # alternative: dominant coding
                target_mode = round(mean(Digitality)),
                # allow to remove pre-coded paragraphs later:
                coder_ID = mean(mean(coder_ID))) %>% 
              select(id = doc_ID, target_max, target_mode, coder_ID) %>% 
              ungroup(),
            join_by(id)) %>% 
  distinct(text, .keep_all = T) %>% 
  arrange(coder_ID)

write_csv(zs_todo, "data/all_unique_texts.csv")
googledrive::drive_upload("data/all_unique_texts.csv", "TRIAS/")
