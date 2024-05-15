# ChatGPT vs Human Coders
# Author: @ChRauh

# Packages ####
library(tidyverse)
library(httr)
library(jsonlite)

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



# Let chatGPT code the stuf ####

# Credentials
api_key <- readLines("./private/gpt.txt")[1]


# Request function 
askGPT <- function(prompt, model, api_key, temperature) {
  
  # Request to ChatGPT API
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = model,
      temperature = temperature,
      messages = list(list(
        role = "user", 
        content = prompt
      ))
    )
  )
  
}



# Coding


# df <-data.frame(NULL)

start <- Sys.time()

for(i in 1:nrow(mc)){
# for(i in 1:2){
  
  print(i)
  
  # Check whether case has already been coded (in case of abortion)
  if(mc$id[i] %in% df$id) {next}
  
  # Set up prompt
  prompt <- paste("Your are supposed to classify a text snippet (provided in quotation marks at the end of the prompt).\n", 
                  "Classification instructions:\n",
                  "Aim is to identify statements that relate to the digital sphere in the widest sense.",
                  "This may include statements that talk about general or specific themes related to the internet, digital communications and services, algorithms and digitized data, or the technologies that underpin digitalisation, for example.",
                  "You will see a text snippet randomly extracted from public communication documents at different points in time.",
                  "Just along what the text snippet says itself: does this particular text touch upon issues or themes related to the digital sphere or not?",
                  "Provide your answer in a well-formatted JSON string that contains the following three elements\n",
                  "REASONING: {A brief reasoning on why this text may talk about the digital sphere or not and why you may be sure or unsure.}\n",
                  "LIKELIHOOD: {A continuous likelihood estimation in the interval between 0 and 1 that this text talks about the digital sphere.}",
                  "CLASSIFICATION: {Your choice of whther this text talks about the digital sphre as TRUE or FALSE}\n",
                  "Here is the text you should asses: \"", mc$text[i], "\"",
                  sep = " ")
  
  # collect gpt response
  response <- askGPT(prompt = prompt, 
                     model = "gpt-3.5-turbo",
                     temperature = 0,
                     api_key = api_key)
  
  # Parse response
  string <- paste(content(response)$choices[[1]]$message$content)
  # current <- fromJSON(string) %>% as.data.frame()
  
  # Data frame
  current <- data.frame(string = string, # GPT makes errors in JSON formatting, parse this separately later
                        text = mc$text[i],
                        id = mc$id[i])
  # Append
  df <- rbind(df, current)
  
  
} 

Sys.time()-start # Duration: 1,5 hours, Costs: ~ 63 US cents

# Intermediary Export
write_rds(df, "./large_data/GPT-Codes.rds")
df <- read_rds("./large_data/GPT-Codes.rds")


# Extract individual responses ####
# fromJSON(string) would workin principle, but formatting errors by GPT

responses <- data.frame(do.call("rbind", 
                                strsplit(as.character(df$string), 
                                         "\n", 
                                         fixed = TRUE))) %>% 
  select(2:4) %>% 
  rename(gpt_reasoning = 1,
         gpt_likelihood = 2,
         gpt_digital = 3) %>% 
  mutate(gpt_reasoning = gpt_reasoning %>% str_replace("(^.*?: )(.*)", "\\2") %>% str_remove_all("\""),
         gpt_likelihood = gpt_likelihood %>% str_replace("(^.*?: )(.*)", "\\2") %>% str_remove_all("\"|,") %>% as.numeric(),
         gpt_digital = gpt_digital %>% str_extract("TRUE|FALSE") %>% as.logical())


# Write to data
responses <- cbind(df, responses) %>% 
  select(-string) %>% 
  left_join(mc %>% select(id, digitality_sum, digital), by = "id") %>% 
  relocate(id, text, digitality_sum, digital)
  
# Export
write_rds(responses, "./large_data/GPT-Codes_vs_Humans.rds")



# Quick inspection ####

hist(responses$gpt_likelihood)


TP <- sum(responses$gpt_digital & responses$digital, na.rm = T)
TN <- sum(!responses$gpt_digital & !responses$digital, na.rm = T)
FP <- sum(responses$gpt_digital & !responses$digital, na.rm = T)
FN <- sum(!responses$gpt_digital & responses$digital, na.rm = T)

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2*(precision*recall)/(precision+recall)
accuracy <- (TP + TN) / (TP + TN + FP + FN)
baccuracy <- ((TP / (TP + FN)) + (TN / (TN + FP))) / 2 # Balanced accuracy (because imbalanced sample!) - https://neptune.ai/blog/balanced-accuracy



