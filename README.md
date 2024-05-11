# Balancing acts:

## Geopolitical pressures and justifications of the European Commission's digital policies

### Code Repository for TRIAS project paper #1

### by Christian Rauh & Milan Schröder

#### WZB Berlin Social Science Center

### Scripts:
-   *pipeline.R*: full data analysis pipeline 

##### Text preparation

-   *00_connect_DB.R*: Establish database connection
-   *01_read_corpus.R*: Read EC Corpus data from database
-   *02_clean_corpus.R*: Clean EC Corpus data
-   *03_tokenize_corpus.R*: Individual tokens of EC corpus (mild pre-processing)

##### Extract data

-   *10_semantic_similarity_weights.R*: Extracts different semantic similarity weights on token level from pre-trained GLOVE 6b 300d word embedding model (digitality, economy, security, lib. rights, conflictual/cooperative language)
-   *11_semantic_similarity_coding.R*: Applies semantic similarity weights to tokenized EC communication texts and aggregates corresponding scores to paragraph level
-   *12_12_semantic_similiarity_digital_validation.R*: Compare various cut-offs in semantic similarity coding for "digitality" against human codes
-   XXX *1x_digitality_zeroshot.ipynb*: alternative simple zeroshot slassification for benchmarking
-   XXX *12_create_validation_sample.R*: validation sample creation
-   XXX *13_scaling_validation_results.Rmd*: validation sample results & cutoff selection

-   *20_country_mentions.R*: identification of country mentions in EC communication texts along newsmap dictionary
-   XXX *21_country_mentions_descriptives.Rmd*: descriptives on country identification

##### Analyse data

-   XXX *30_analysis.Rmd*: ...

### Data:
-   *...*