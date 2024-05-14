# Read EC Corpus Data for Analysis
# Authors:: @milanschroeder; @ChRauh

# Input: MariaDBconnection con (from 00_connectDB.R)
# Output: tibble all_meta, tibble all_texts

# data reading function ####
combine_tables <- function(prefix = c("comread", "comspeech", "compress", "comstate",    # main interest
                                      "comqa", "commeet",                                # background info
                                      "comday", "comnews", "cominfringe",  "comreport"), # not relevant here
                           suffix = "meta", 
                           DB = con){
  
  library(tidyverse)
  library(magrittr) # for assignment pipe %<>%
  
  #tables to combine:
  tablelist <- paste0(prefix, "_", suffix)
  
  combined_table <- 
    lapply(tablelist, function(table) {
      DBI::dbReadTable(DB, table, check.names = F)
    }) %>% bind_rows() 
  
  # add doc_type for all tables that don't have it to allow for quick filtering:
  if (suffix != "meta") {
    combined_table %<>% 
      mutate(doc_type = str_extract(doc_key, "(?<=/)[\\w]+?(?=_)"))
  }
  
  return(combined_table)
}


# read all metadata ####
all_meta <- combine_tables()

# Store local copy
write_rds(all_meta %>% select(-c(date_time_cet, text_full)), "./data/all_meta.rds")



# read all text elements ####
# includes: paragraphs, list elements, quotes, headers, subheaders

all_paras <- combine_tables(suffix = c("paragraph")) %>%
  mutate(para_type = "paragraph") %>%
  select(doc_pos, text_para, doc_key, n_chars_para, doc_type, para_type)
all_titles <- combine_tables(suffix = c("meta")) %>% 
  mutate(doc_pos = 1L, para_type = "title", n_chars_para = str_length(title_long)) %>% 
  select(doc_pos, text_para = title_long, doc_key, n_chars_para, doc_type, para_type)
all_lists <- combine_tables(suffix = c("list")) %>% 
  group_by(doc_key) %>%
  mutate(list_pos = row_number(), para_type = "list", n_chars_para = str_length(list_item)) %>% 
  ungroup(doc_key) %>% 
  select(doc_pos = list_pos, text_para = list_item, doc_key, n_chars_para, doc_type, para_type)
all_quotes <- combine_tables(suffix = c("quote")) %>% 
  mutate(para_type = "quote", n_chars_para = str_length(quote_text)) %>% 
  select(doc_pos = quote_pos, text_para = quote_text, doc_key, n_chars_para, doc_type, para_type)
all_subheaders <- combine_tables(suffix = c("subheader")) %>% 
  mutate(para_type = "subheader", n_chars_para = str_length(header_text)) %>% 
  select(doc_pos = header_pos, text_para = header_text, doc_key, n_chars_para, doc_type, para_type)

# combine all text elements: #### 
all_texts <- bind_rows(
  all_paras, all_titles, all_subheaders, all_lists, all_quotes
)

# clean up: ####
DBI::dbDisconnect(con) # Disconnect DB after data is read
rm(all_paras, all_titles, all_lists, all_quotes, all_subheaders, combine_tables, con) # clear memory
gc()

