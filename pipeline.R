# Full Analysis Pipeline

# data prep ####
source("00_connectDB.R") # establish database connection
  # returns: MariaDBconnection con
source("01_read_corpus.R") # read corpus and metadata from DB
  # returns: tibbles all_meta, all_texts
source("03_clean_corpus.R") # clean paragraphs for analysis

# digitality ####

# country mentions ####

# analysis ####

