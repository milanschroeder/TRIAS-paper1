# process glove:
### credits to https://gist.github.com/tjvananne/8b0e7df7dcad414e8e6d5bf3947439a9

# function definition --------------------------------------------------------------------------

# input .txt file, exports list of list of values and character vector of names (words)
proc_pretrained_vec <- function(p_vec) {
  
  
  # initialize space for values and the names of each word in vocab
  vals <- vector(mode = "list", length(p_vec))
  names <- character(length(p_vec))
  
  # loop through to gather values and names of each word
  for(i in 1:length(p_vec)) {
    if(i %% 1000 == 0) {print(i)}
    this_vec <- p_vec[i]
    this_vec_unlisted <- unlist(strsplit(this_vec, " "))
    this_vec_values <- as.numeric(this_vec_unlisted[-1])  # this needs testing, does it become numeric?
    this_vec_name <- this_vec_unlisted[1]
    
    vals[[i]] <- this_vec_values
    names[[i]] <- this_vec_name
  }
  
  # convert lists to data.frame and attach the names
  glove <- data.frame(vals)
  names(glove) <- names
  
  return(glove)
}




# using the function -------------------------------------------------------------------------

# here we are reading in the unzipped, raw, GloVe pre-trained word vector object (.txt)
# all you have to change is the file path to where you GloVe object has been unzipped
g6b_300 <- scan(file = "../LargeData/glove.6B.300d.txt", what="", sep="\n")


# call the function to convert the raw GloVe vector to data.frame (extra lines are for wall-time reporting)
t_temp <- Sys.time()
glove.300 <- proc_pretrained_vec(g6b_300)  # this is the actual function call
(t_elap_temp <- paste0(round(as.numeric(Sys.time() - t_temp, units="mins"), digits = 2), " minutes"))

print(dim(glove.300)) 
# [1]   300  400000
rm(g6b_300)

readr::write_rds(glove.300, "./large_data/glove.6B.300d.rds")




# Clean up the vocabulary a bit (lots of rare trash in there, exclude stopwords)
vocab <- names(glove.300) %>% 
  as.data.frame() %>% 
  rename(token = 1) %>% 
  filter(str_detect(token, "[a-z]")) %>% 
  filter(nchar(token) > 1) %>% 
  filter(!(token %in% stopwords("english"))) %>% 
  filter(!str_detect(token, "\\.")) %>% 
  filter(!str_detect(token, "[0-9]"))
glove.300 <- glove.300 %>% select(vocab$token)




# Function to find nearest neighbors in word vectors model ####

# Taken from: https://gist.github.com/tjvananne/8b0e7df7dcad414e8e6d5bf3947439a9
# N.B.: Expects vectors with tokens in columns and dimensions in rows 
find_sim_wvs <- function(seed_dict, concept,  embeddings = read_rds("./large_data/glove.6B.300d.rds"), sim_measure = "cosine"){
  
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
  
  if(sim_measure == "cosine"){
    # cosine smilarity:
    cos_sim = sim2(x=all_wvs_mat, y=this_wv_mat, method="cosine", norm="l2")
    sim = cos_sim#[,1]
  }
  
  if(sim_measure == "dotproduct"){
    
    dot_product_similarity <- function(matrix, vector) {
      apply(matrix, 1, function(row) sum(row * vector))
    }
    
    # dot product similarity:
    dot_sim = dot_product_similarity(all_wvs_mat, this_wv_mat)
    sim = dot_sim
  }
  
  sorted_sim <- sort(sim, 
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


# Seed tokens
seed_vector<- c("digital", "online", "computer", "internet", "algorithm")

# Extract cosine similarities to this average vector
# (word weights that would semantically pull a text towards the seed)
simils <-
  find_sim_wvs(seed_vector, "digital") 

