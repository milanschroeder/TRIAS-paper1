# Identify Commissioners ####


library(tidyverse)
library(rvest)
library(anytime)
library(quanteda)
library(magrittr)
library(fuzzyjoin)
library(future)
library(furrr)


# helper
str_c_regex <- function(...){
  stringr::str_c(..., sep = "|")
}


# scrape wikitable #########

# link <- "https://en.wikipedia.org/w/index.php?title=List_of_European_Commissioners_by_member_state&oldid=1238069428"
# 
# 
# wikipage <- NULL
# while (is.null(wikipage)) {
#   try(
#     wikipage <- 
#       read_html(link)
#   )
# }
# 
# wikitable <- 
#   (wikipage %>% 
#      html_elements(".sortable")
#    )[[1]] %>% 
#   html_table() %>% 
#   mutate(Start = anydate(Start),
#          End = anydate(End))
# 
# (commissioners <- wikitable %>% 
#   mutate(given_name = str_extract(Name, "^[\\w-\']* (Manuel|Maria|Levi|Olav)*"), 
#           middle_names = str_remove_all(Name, "^[\\w-\']* |[\\w-\']*$|(Manuel|Maria|Levi|Olav)"),
#          last_name = paste(middle_names, str_extract(Name, "[\\w-\']*$")) %>% str_squish(),
#          vice_president = str_detect(Portfolio, "\\((Executive )?Vice President\\)"),
#          across(c(Portfolio, Party, Family), ~ str_remove_all(.x, "\\(acting\\)|\\((Executive )?Vice President\\)|\\(First Vice President\\)|\\[\\d*\\]") %>% str_squish()),
#          president = Portfolio == "President",
#          acting = is.na(End), # in VdL1
#          End = if_else(is.na(End), as.Date("2024-11-30"), as.Date(End), NA)
#          ) %>% 
#   group_by(Name) %>% 
#   mutate(office = row_number(),
#          offices_held_total = max(office),
#     #     term = n_distinct(Commission),
#          terms_total = n_distinct(Commission)
#          ) %>% 
#     ungroup() %>% 
#   select(-middle_names)) %>% 
#   view()
# 
# 
# 
# #commissioner_names <- 
# commissioners %<>% #distinct(Name, .keep_all = T) %>% 
#   mutate(name_title = case_when(
#     president ~ paste("President( of the( European)? Commission)?", last_name),
#     vice_president ~ paste("Vice[- ]President( of the (European)? Commission)?|Commissioner", last_name),
#     TRUE ~ paste("Commissioner", last_name)
#   )) %>% 
# 
# # for (i in seq(nrow(commissioner_names))) {
#   
#    mutate(
#     regex = str_c_regex(Name, name_title),
#     regex = ifelse(Name == "Julian King", str_c_regex(regex, "Sir Julian( King)?"), regex),
#     regex = ifelse(Name == "Leon Brittan", str_c_regex(regex, "Sir Leon( Brittan)?"), regex),
#     regex = ifelse(Name == "Jonathan Hill", str_c_regex(regex, "Lord( Jonathan)? Hill"), regex),
#     regex = ifelse(Name == "Stella Kyriakidou", str_c_regex(regex, "Stella Kyriakides", "Commissioner Kyriakides"), regex),
#     regex = ifelse(Name == "Miguel Arias Cañete", str_c_regex(regex, "Miguel Arias", "Commissioner Arias"), regex),
#     regex = ifelse(Name == "Máire Geoghegan-Quinn", str_c_regex(regex, "Máire Geoghegan", "Commissioner Geoghegan"), regex),
#     regex = ifelse(Name == "Benita Ferrero-Waldner", str_c_regex(regex, "Benita Ferrero", "Commissioner Ferrero"), regex),
#     regex = ifelse(Name == "Corina Cretu", str_c_regex(regex, "Corina Creţu", "Commissioner Creţu"), regex),
#     regex = ifelse(Name == "Stefan Füle", str_c_regex(regex, "Štefan Füle"), regex),
#     regex = ifelse(Name == "Jan Figel", str_c_regex(regex, "Ján Figel"), regex),
#     regex = ifelse(Name == "Mariann Fischer Boel", str_c_regex(regex, "Mariann Fischer", "Commissioner Fischer"), regex),
#     regex = ifelse(Name == "Joaquin Almunia", str_c_regex(regex, "Joaquín Almunia"), regex),
#     regex = ifelse(Name == "Dubravaka Šuica", str_c_regex(regex, "Dubravaka Suica", "Commissioner Suica"), regex),
#     regex = ifelse(Name == "Dacian Ciolos", str_c_regex(regex, "Dacian Cioloş", "Commissioner Cioloş"), regex),
#     regex = ifelse(Name == "Pádraig Flynn", str_c_regex(regex, "Padraig Flynn"), regex),
#     regex = ifelse(Name == "Vera Jourova", str_c_regex(regex, "Vĕra Jourová", "Commissioner Jourová", "Vice[- ]President( of the (European)? Commission)? Jourová"), regex),
#     regex = ifelse(Name == "virginijus Sinkevičius", str_c_regex(regex, "virginijus Sinkevičius", "Commissioner Sinkevičius"), regex),
#     regex = ifelse(Name == "Margot Wallström", str_c_regex(regex, "Margot Wallstrom", "Commissioner Wallstrom"), regex),
#     regex = ifelse(Name == "Raymond McSharry", str_c_regex(regex, "Ray(mond)? Mac ?Sharry", "Commissioner Mac ?Sharry"), regex),
#     regex = ifelse(Name == " McSharry", str_c_regex(regex, "Ray(mond)? Mac ?Sharry", "Commissioner Mac ?Sharry"), regex),
#     regex = ifelse(Name == "Vladimier Spidla", str_c_regex(regex, "Vladimie?r Špidla", "Commissioner Špidla"), regex),
#     regex = ifelse(Name == "Elzbieta Bienkowska", str_c_regex(regex, "Elzbieta Bieńkowska", "Commissioner Bieńkowska"), regex),
#     regex = ifelse(Name == "Monika Wulf-Mathies", str_c_regex(regex, "Monika Wulf", "Commissioner Wulf"), regex),
#     regex = ifelse(Name == "Dalia Grybauskaite", str_c_regex(regex, "Dalia Grybauskaité", "Commissioner Grybauskaité"), regex),
#     regex = ifelse(Name == "Janez Potocnik", str_c_regex(regex, "Janez Potočnik", "Commissioner Potočnik"), regex),
#     regex = ifelse(Name == "Meglena Kunewa", str_c_regex(regex, "Meglena Kuneva", "Commissioner Kuneva"), regex),
#     regex = ifelse(Name == "Algirdas Semeta", str_c_regex(regex, "Algirdas Šemeta", "Commissioner Šemeta"), regex),
#     regex = ifelse(Name == "Janez Lenarčič", str_c_regex(regex, "Janez Lenar[cč]i[cč]", "Commissioner Lenar[cč]i[cč]"), regex),
#     regex = ifelse(Name == "László Andor", str_c_regex(regex, "L[aá]szl[oó] Andor"), regex),
#     regex = ifelse(Name == "Cecilia Malmstroem", str_c_regex(regex, "Cecilia Malmstr[oö]m", "Commissioner Malmstr[oö]m"), regex),
#     regex = ifelse(Name == "Olivér Várhelyi", str_c_regex(regex, "Oliv[ée]r V[aá]rhelyi", "Commissioner Varhelyi"), regex)
#     
#     ) 
# #}

# join texts by date + name match:

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# 
# commissioner_mentions <- 
#   fuzzyjoin::regex_left_join(
#     read_rds(paste0(data_path, "cleaned_data/data_doclevel.rds")), 
#     commissioners,
#     by = c(#between(date, Start, End), 
#            "text_doc" = "regex"),
#     ignore_case = T
#     ) %>% 
#   distinct(.keep_all = T) %>% 
#   filter(date >= Start & date <= End) 
# 
# # write_rds(commissioner_mentions, paste0(data_path, "commissioner_mentions.rds"))
# commissioner_mentions <- read_rds(paste0(data_path, "commissioner_mentions.rds"))

# other levels: e.g. commission ####

# commissions_dict <- commissioners %>%
#   mutate(name_title = paste("Commissioner", last_name)) %>% 
#   group_by(Commission) %>% 
#   summarise(Name = list(Name),
#             name_title = list(name_title))
# 
# commissions_dict$dict <- c()
# for (i in seq(nrow(commissions_dict))) {
#   commissions_dict$dict[i] <- list(c(commissions_dict$Name[i], commissions_dict$name_title[i]))
# }

# dict_commisions <- set_names(commissions_dict$dict,
#                              commissions_dict$Name)

# identify commissioner mentions in paragraphs: ####

# check portfolio at that date ####

# multiple portfolios possible -> separate entries in matching table, join with multiple="all" (-> be aware of in duplicate texts as  a result)
# possible to inroduce hierarchy via yaml-file (see e.g. https://raw.githubusercontent.com/koheiw/newsmap/refs/heads/master/dict/english.yml)
# yaml::write_yaml(commissioner_names %>% select(Name, name_title, Commission) %>% head(100) %>% group_by(Commission),
#                   "test.yml")
# test <- dictionary(file = "test.yml")
# ...


# commissioner dictinary: ####
# commissioner_dict <- 
#   set_names(commissioner_names$dict,
#             commissioner_names$Name
#   )
# 
# dict_commisioners <- dictionary(commissioner_dict)
# 
# 
# # commissioner lookup: ####
# all_texts <- read_rds("../data/all_texts.rds")
# corp <- corpus(all_texts$text_para,
#                docvars = all_texts %>% select(id, doc_key))
# 
# toks <- tokens_ngrams(
#   tokens(corp, remove_punct = T, remove_numbers = T), 
#   n = 2:4) # names up to 4 tokens
# #dfm_corp <- dfm(corp, tolower = T, ngrams = 1:2 )
# 
# commissioner_matches <- 
#   tokens_lookup(toks, dict_commisioners) %>% 
#   dfm(., tolower = F) %>% 
#   convert(., to = "data.frame") %>% 
#   select(-doc_id) %>% 
#   cbind(all_texts %>% select(doc_key, id),
#         .) # quanteda maintains order
# 
# write_rds(commissioner_matches, "../data/commissioner_matches.rds")
# 
# commissioners_doc <- 
#   commissioner_matches %>% 
#   group_by(doc_key) %>% 
#   summarise(across(3:last_col(), sum)) %>% 
#   ungroup()
# 
# write_rds(commissioners_doc, "../data/commissioner_matches_doc.rds")
# 
# # add date:
# commissioners_doc <- left_join(read_rds("../TRIAS-paper1/data/all_meta.rds") %>%  select(doc_key, date), 
#                                read_rds("../data/commissioner_matches_doc.rds"), 
#                                join_by(doc_key)) %>%
# 
# # convert to long
# pivot_longer(3:last_col(), names_to = "Name", values_to = "mentions") %>% 
# 
# # join portfolio by commissioner, date
# left_join(.,
#           commissioners %>% mutate(End = ifelse(is.na(End), as.Date(Sys.time()), End)),
#           join_by(Name, 
#                   between(x$date, as.Date(y$Start), as.Date(y$End)))
#           )

# check against: identify all "Commissioner"s ####

data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
data_path <- "/wzb/samba/user/schroeder/transfer/" # Kalliope

VdL_link <- "https://en.wikipedia.org/w/index.php?title=Von_der_Leyen_Commission_II&oldid=1286350008"
vdl2 <- read_html(VdL_link) %>% 
  html_element(".wikitable") %>% 
  html_table()

vdl2 <- vdl2[, c(1:4, 6:8)] %>% select(Portfolio, Name, Party = `EU Party(N. Party)`, MS = `Member state`, DG = `Directorate General[6]`, Date_in = `Took office`) 

vdl2 %<>% mutate(
  first_name = str_extract(Name, "^[^ ]+"),
  given_name = first_name,
  last_name = str_remove(Name, first_name) %>% str_squish(),
  Start = anytime::anydate(Date_in),
  End = as.Date("2029-11-30"),
  EU_Party = str_extract(Party, "^[a-zA-Z]+"),
  nat_Party = str_extract(Party, "([^()]+)(?=\\)$)") %>% str_remove("\\[.+\\]"),
  Position = case_when(
    str_starts(Portfolio, "President") ~ "President",
    str_starts(Portfolio, "(Executive )?Vice") ~ "Vice President",
    T ~ "Commissioner"
  ),
  Commission = "Von der Leyen II"
) %>%
  separate_rows(DG, sep = ", ")




PEU <- readxl::read_excel(path = paste0(data_path, "PEU-Database_Onlinestellung-22_06_2020.xlsx"), sheet = 4) %>% 
 # fix typo in PEU data:
   mutate(Name = ifelse(Name == "Šuica, Dubravaka", "Šuica, Dubravka", Name)) %>% 
  separate_wider_delim(Name, delim = ", ", names = c("last_name", "given_name")) %>% 
  
  mutate(
    first_name = str_extract(given_name, "^[^ ]+"),
    name_connector = str_extract(given_name, " de( Deus)?$| von( der)?$| van( de[nr])?$"),
    given_name = str_remove(given_name, paste(name_connector)),
    last_name = ifelse(is.na(name_connector), 
                       last_name,
                       str_c(name_connector, " ", last_name) %>% str_squish()),
    
    `Date In (Day)` = ifelse(`Date In (Day)` == "n/a|tbd", 1, `Date In (Day)`),
    `Date Out (Day)` = ifelse(`Date Out (Day)` == "n/a|tbd", 30, `Date Out (Day)`),
    `Date In (Month)` = ifelse(`Date In (Month)` == "n/a|tbd", 1, `Date In (Month)`),
    `Date Out (Month)` = ifelse(`Date Out (Month)` == "n/a|tbd", 12, `Date Out (Month)`),
    Commission = ifelse(Commission == "von der Leyen", "von der Leyen I", Commission),
    Start = as.Date(paste(`Date In (Year)`, `Date In (Month)`, `Date In (Day)`, sep = "-")),
    End = as.Date(paste(`Date Out (Year)`, `Date Out (Month)`, `Date Out (Day)`, sep = "-"))
  ) %>%
  select(-c(starts_with("Date"), `Months in Office`, person_id, name_connector, Spell))
      

commission_dates <- PEU %>% 
  group_by(Commission) %>% 
  summarise(Start_Com = min(Start, na.rm = T), End_Com = max(End, na.rm = T)) %>% 
  mutate(End_Com = if_else(Commission == "von der Leyen I", as.Date("2024-11-30"), End_Com))
    
PEU %<>% 
  left_join(., commission_dates, join_by(Commission)) %>% 
  mutate(Start = if_else(is.na(Start), Start_Com, Start),
         End = if_else(is.na(End), End_Com, End)) %>% 
  select(-c(End_Com, Start_Com))

PEU_full <- 
bind_rows(
  PEU,
  vdl2 %>% 
    select(any_of(names(PEU))) 
)

rm(commission_dates, PEU, vdl2, VdL_link)
gc()


PEU_full %<>% #distinct(Name, .keep_all = T) %>% 
  mutate(
    name_title = case_when(
      Position == "President" ~ paste("President( of the( European)? Commission)?", last_name),
      Position == "Vice President" ~ str_c_regex(paste(c("Vice[- ]President( of the( European)? Commission)?", "Commissioner"), last_name)),
      Position == "Director General" ~ paste("Director([- ]General)?", last_name), 
      Position == "Secretary General" ~ paste("Secretary([- ]General)?", last_name),
      Position == "Commissioner" ~ paste("Commissioner", last_name)
      ), 
    Name = paste(given_name, last_name),
  ) 

PEU_full %<>% 
  
  # for (i in seq(nrow(commissioner_names))) {
  
  mutate(
    regex = ifelse(first_name == given_name, 
                   str_c_regex(Name, name_title),
                   str_c_regex(Name, name_title, paste(first_name, last_name))),
    
    # include name different versions present in corpus:
    regex = ifelse(Name == "Julian King", str_c_regex(regex, "Sir Julian( King)?"), regex),
    regex = ifelse(Name == "Leon Brittan", str_c_regex(regex, "Sir Leon( Brittan)?"), regex),
    regex = ifelse(Name == "Jonathan Hill", str_c_regex(regex, "Lord( Jonathan)? Hill"), regex),
    regex = ifelse(Name == "Stella Kyriakidou", str_c_regex(regex, "Stella Kyriakides", "Commissioner Kyriakides"), regex),
    regex = ifelse(Name == "Miguel Arias Cañete", str_c_regex(regex, "Miguel Arias", "Commissioner Arias"), regex),
    regex = ifelse(Name == "Máire Geoghegan-Quinn", str_c_regex(regex, "Máire Geoghegan", "Commissioner Geoghegan"), regex),
    regex = ifelse(Name == "Benita Ferrero-Waldner", str_c_regex(regex, "Benita Ferrero", "Commissioner Ferrero"), regex),
    regex = ifelse(Name == "Corina Cretu", str_c_regex(regex, "Corina Creţu", "Commissioner Creţu"), regex),
    regex = ifelse(Name == "Stefan Füle", str_c_regex(regex, "Štefan Füle"), regex),
    regex = ifelse(Name == "Jan Figel", str_c_regex(regex, "Ján Figel"), regex),
    regex = ifelse(Name == "Mariann Fischer Boel", str_c_regex(regex, "Mariann Fischer", "Commissioner Fischer"), regex),
    regex = ifelse(Name == "Joaquin Almunia", str_c_regex(regex, "Joaquín Almunia"), regex),
    regex = ifelse(Name == "Dubravaka Šuica", str_c_regex(regex, "Dubravaka Suica", "Commissioner Suica"), regex),
    regex = ifelse(Name == "Dacian Ciolos", str_c_regex(regex, "Dacian Cioloş", "Commissioner Cioloş"), regex),
    regex = ifelse(Name == "Pádraig Flynn", str_c_regex(regex, "Padraig Flynn"), regex),
    regex = ifelse(Name == "Vera Jourova", str_c_regex(regex, "Vĕra Jourová", "Commissioner Jourová", "Vice[- ]President( of the (European)? Commission)? Jourová"), regex),
    regex = ifelse(Name == "virginijus Sinkevičius", str_c_regex(regex, "virginijus Sinkevičius", "Commissioner Sinkevičius"), regex),
    regex = ifelse(Name == "Margot Wallström", str_c_regex(regex, "Margot Wallstrom", "Commissioner Wallstrom"), regex),
    regex = ifelse(Name == "Raymond McSharry", str_c_regex(regex, "Ray(mond)? Mac ?Sharry", "Commissioner Mac ?Sharry"), regex),
    regex = ifelse(Name == " McSharry", str_c_regex(regex, "Ray(mond)? Mac ?Sharry", "Commissioner Mac ?Sharry"), regex),
    regex = ifelse(Name == "Vladimier Spidla", str_c_regex(regex, "Vladimie?r Špidla", "Commissioner Špidla"), regex),
    regex = ifelse(Name == "Elzbieta Bienkowska", str_c_regex(regex, "Elzbieta Bieńkowska", "Commissioner Bieńkowska"), regex),
    regex = ifelse(Name == "Monika Wulf-Mathies", str_c_regex(regex, "Monika Wulf", "Commissioner Wulf"), regex),
    regex = ifelse(Name == "Dalia Grybauskaite", str_c_regex(regex, "Dalia Grybauskaité", "Commissioner Grybauskaité"), regex),
    regex = ifelse(Name == "Janez Potocnik", str_c_regex(regex, "Janez Potočnik", "Commissioner Potočnik"), regex),
    regex = ifelse(Name == "Meglena Kunewa", str_c_regex(regex, "Meglena Kuneva", "Commissioner Kuneva"), regex),
    regex = ifelse(Name == "Algirdas Semeta", str_c_regex(regex, "Algirdas Šemeta", "Commissioner Šemeta"), regex),
    regex = ifelse(Name == "Janez Lenarčič", str_c_regex(regex, "Janez Lenar[cč]i[cč]", "Commissioner Lenar[cč]i[cč]"), regex),
    regex = ifelse(Name == "László Andor", str_c_regex(regex, "L[aá]szl[oó] Andor"), regex),
    regex = ifelse(Name == "Loyola de Palacio", str_c_regex(regex, "Loyola ([Dd]e )?Palacio", "Commissioner ([Dd]e )?Palacio", "Vice[- ]President( of the (European)? Commission)? ([Dd]e )?Palacio"), regex),
    regex = ifelse(Name == "Cecilia Malmstroem", str_c_regex(regex, "Cecilia Malmstr[oö]m", "Commissioner Malmstr[oö]m"), regex),
    regex = ifelse(Name == "Olivér Várhelyi", str_c_regex(regex, "Oliv[ée]r V[aá]rhelyi", "Commissioner Varhelyi"), regex)
    
  )  %>% 
  select(-c(last_name, given_name, first_name, name_title))

# doclevel ####

docs <- read_rds(paste0(data_path, "data_doclevel.rds")) %>% 
  select(doc_id, text = text_doc, date)

# Define the number of workers (cores) to use
plan(multisession, workers = floor(availableCores() * 0.9))

# Define batch size
batch_size <- 1000 # Adjust based on your system's memory capacity

# Calculate number of batches
n_batches <- ceiling(nrow(docs) / batch_size)

# Function to process a single batch
process_batch <- function(batch_index) {
  start_idx <- (batch_index - 1) * batch_size + 1
  end_idx <- min(batch_index * batch_size, nrow(docs))
  
  batch_docs <- docs[start_idx:end_idx, ]
  
  batch_mentions <- fuzzyjoin::regex_left_join(
    batch_docs,
    PEU_full, # commissioner data
    by = c("text" = "regex"),
    ignore_case = TRUE
  ) %>%
    distinct(.keep_all = TRUE) %>%
    mutate(active = date >= Start & date <= End) %>%
    select(-c(text, regex, Start, End)) %>% 
    filter(!is.na(Name))
  
  # Signal progress update
  #progressor(sprintf("Processed batch: %d", batch_index))
  
  return(batch_mentions)
}

# Use future_map to process batches in parallel
commissioner_mentions_doc <- future_map_dfr(seq_len(n_batches), process_batch)

plan(sequential) # Reset plan to sequential (default)

write_rds(commissioner_mentions_doc, paste0(data_path, "commissioners_doclevel.rds"))


# paras / sentences ##########

# filter paragraphs & sentences for docs with at least one commissioner mentioned:
paras <- read_rds(paste0(data_path, "data_paralevel.rds")) %>% 
  select(doc_id, text_id, text = text_para) %>% 
  filter(doc_id %in% 
                    commissioner_mentions_doc %>% 
                    filter(!is.na(Name)) %>% 
                    pull(doc_id) %>% unique()) %>% 
  left_join(., commissioner_mentions_doc %>% select(doc_id, date), join_by()) %>% 
  select(-doc_id)

sentences <- read_rds(paste0(data_path, "data_sentlevel.rds")) %>% 
  select(doc_id, sentence_id, text = text_sent, date) %>% 
  filter(doc_id %in% 
           commissioner_mentions_doc %>% 
           filter(!is.na(Name)) %>% 
           pull(doc_id) %>% unique()) %>% 
  select(-doc_id)

rm(commissioner_mentions_doc)
gc()

# run same loop on those:
# sentence level: #########

# Define the number of workers (cores) to use
plan(multisession, workers = floor(availableCores() * 0.9))

# Define batch size
batch_size <- 1000 # Adjust based on your system's memory capacity

# Calculate number of batches
n_batches <- ceiling(nrow(sentences) / batch_size)

# Function to process a single batch
process_batch <- function(batch_index) {
  start_idx <- (batch_index - 1) * batch_size + 1
  end_idx <- min(batch_index * batch_size, nrow(sentences))
  
  batch_docs <- sentences[start_idx:end_idx, ]
  
  batch_mentions <- fuzzyjoin::regex_left_join(
    batch_docs,
    PEU_full, # commissioner data
    by = c("text" = "regex"),
    ignore_case = TRUE
  ) %>%
    distinct(.keep_all = TRUE) %>%
    mutate(active = date >= Start & date <= End) %>%
    select(-c(text, regex, Start, End)) %>% 
    filter(!is.na(Name))
  
  # Signal progress update
  #progressor(sprintf("Processed batch: %d", batch_index))
  
  return(batch_mentions)
}

# Use future_map to process batches in parallel
commissioner_mentions_sent <- future_map_dfr(seq_len(n_batches), process_batch)

plan(sequential) # Reset plan to sequential (default)

write_rds(commissioner_mentions_sent, paste0(data_path, "commissioners_sentlevel.rds"))
rm(commissioner_mentions_sent)
gc()

# para level ######

# Define the number of workers (cores) to use
plan(multisession, workers = floor(availableCores() * 0.9))

# Define batch size
batch_size <- 1000 # Adjust based on your system's memory capacity

# Calculate number of batches
n_batches <- ceiling(nrow(paras) / batch_size)

# Function to process a single batch
process_batch <- function(batch_index) {
  start_idx <- (batch_index - 1) * batch_size + 1
  end_idx <- min(batch_index * batch_size, nrow(paras))
  
  batch_docs <- paras[start_idx:end_idx, ]
  
  batch_mentions <- fuzzyjoin::regex_left_join(
    batch_docs,
    PEU_full, # commissioner data
    by = c("text" = "regex"),
    ignore_case = TRUE
  ) %>%
    distinct(.keep_all = TRUE) %>%
    mutate(active = date >= Start & date <= End) %>%
    select(-c(text, regex, Start, End)) %>% 
    filter(!is.na(Name))
  
  # Signal progress update
  #progressor(sprintf("Processed batch: %d", batch_index))
  
  return(batch_mentions)
}

# Use future_map to process batches in parallel
commissioner_mentions_para <- future_map_dfr(seq_len(n_batches), process_batch)

plan(sequential) # Reset plan to sequential (default)

write_rds(commissioner_mentions_para, paste0(data_path, "commissioners_paralevel.rds"))
rm(commissioner_mentions_para)
gc()

################# old
# docs %<>% 
#   mutate(commissioners = str_extract_all(text_doc, "(?i)(?<=Commissioner |President )(von der )?(de )?[[:upper:]]{1}[[:alpha:]]+( [[:upper:]]{1}[[:alpha:]]+)?"))
# 
#   mutate(commissioners = str_extract_all(text_doc, paste0("(?i)", str_c_regex(commissioners$regex %>% unique()))))


