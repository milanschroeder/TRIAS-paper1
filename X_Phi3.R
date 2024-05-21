# Phi3 (via ollama) vs Human Coders
# Author: @ChRauh

# Packages ####
library(tidyverse)
library(rollama)
library(httr2)

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


# Let phi3 code the stuff ####

# ollama needs to be installed locally
# pull_model("phi3") required

# Define the task (as a system message)
system_prompt <- paste("You are supposed to classify text snippets rpvoded by the user.\n", 
                       "Classification instructions:\n",
                       "Your aim is to identify statements that relate to the digital sphere in the widest sense.",
                       "This may include statements that talk about general or specific themes related to the internet, digital communications and services, algorithms and digitized data, or the technologies that underpin digitalisation, for example.",
                       "You will see a text snippet randomly extracted from public communication documents at different points in time.",
                       "Just along what the text snippet says itself: does this particular text touch upon issues or themes related to the digital sphere or not?",
                       "Your answer should provide nothing but a well-formatted JSON string that contains the following three elements\n",
                       "REASONING: {A brief reasoning on why this text may talk about the digital sphere or not and why you may be sure or unsure.}\n",
                       "LIKELIHOOD: {A continuous likelihood estimation in the interval between 0 and 1 that this text talks about the digital sphere.}",
                       "CLASSIFICATION: {Your choice of whether this text talks about the digital sphere as TRUE or FALSE}\n",
                       sep = " ")

# Set the system message
options(rollama_config = system_prompt)

# Check one instance
# Note: query() invokes a new chat each time, no history
# response <-
#   query(mc$text[1],
#       model = "phi3",
#       model_params = list(
#         seed = 42, # Sets the random number seed to use for generation. Setting this to a specific number will make the model generate the same text for the same prompt. (Default: 0)
#         temperature = 0 # The temperature of the model. Increasing the temperature will make the model answer more creatively. (Default: 0.8)
#       ))
# 
# response$message$content # Extract answer (lots of other meta data in there)


# Loop over texts ####

mc$phi3_string <- NA

start <- Sys.time()
for (i in 1:nrow(mc)) {
  
  print(paste0("Text ",i," of 2486"))
  
  response <-
    query(mc$text[i],
          model = "phi3",
          model_params = list(
            seed = 42, # Sets the random number seed to use for generation. Setting this to a specific number will make the model generate the same text for the same prompt. (Default: 0)
            temperature = 0 # The temperature of the model. Increasing the temperature will make the model answer more creatively. (Default: 0.8)
          ))
  
  mc$phi3_string[i] <- response$message$content 

}
duration <- Sys.time()-start


# Observing the responses, I note quite some interpreation beyond text content
# and some recency bias (such as believing that environmental sutainability has smthg to do with digital tpoics)

# Clean up data
# Export

# Calculate performance metriocs for classification