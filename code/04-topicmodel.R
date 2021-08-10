####################################
### TOPIC MODEL FOR HYPOTHESIS 2 ###
####################################

# Remember to run "00-Preliminaries.R" before this
# Remember to run "03-recoding-for-topic-model.R" before this script
# All code from line 13-67 are running the LDA-model
# For replicating analysis-reasons, run instead line 72-152

# Load data
# We use this frame because it only includes ads longer than 114 characters which is
# neccesary in order to perform a topic model
Db <- readRDS("/Users/Madshove/Dropbox/Statskundskab/4V/data/Ads_2021-04-20.rds")

Db <- Db %>%
  filter(ad_creation_time <= "2019-06-04")

# Turn into a corpus object (C)
C <- corpus(Db, text_field = "ad_creative_body")

# Pre-process and tokenize the data
Tokens <- tokens(C, remove_numbers = TRUE, remove_punct = TRUE,
                 remove_separators = TRUE, split_hyphens = FALSE,
                 remove_url = TRUE)

# Lower case
Tokens <- tokens_tolower(Tokens)

# Remove stop words
Tokens <- tokens_remove(Tokens, pattern = stopwords("da"))

# extra stopwords added
stopord <- c("så", "må", "gør", "får", "få", "går", "kan", "sætte", "sætter", "se", "kom", "fordi", "vore", "hvem", "gøre", "give", "ved", "fik", "ting")
Tokens <- tokens_remove(Tokens, stopord)

# Stem the tokens
Tokens <- tokens_wordstem(Tokens)

# Add bigrams
Tokens <- tokens_ngrams(Tokens, n = c(1, 2))

# Create document-feature matrix
DFM <- dfm(Tokens)

# Convert the dfm to a format that is usable
# by the "topicmodels" library in R
DFM_topic <- quanteda::convert(DFM, to = "topicmodels")

# Run model with k = 30
system.time({
  model_lda_30 <- LDA(DFM_topic, k = 30, method = "Gibbs",
                      control = list(verbose = 100, seed = 1,
                                     burnin = 200, iter = 1000))
  
  # Save model
  saveRDS(model_lda_30, "../data/model_lda_30.rds")
})

# Run model with k = 50
system.time({
  model_lda_50 <- LDA(DFM_topic, k = 50, method = "Gibbs",
                      control = list(verbose = 100, seed = 1,
                                     burnin = 200, iter = 1000))
  
  # Save model
  saveRDS(model_lda_50, "../data/model_lda_50.rds")
})

# After inspecting the models we go with the one with 50 topics

# Read saved model
model_lda <- readRDS("../data/model_lda_50.rds")

# Retrieve the estimated beta parameters from the model
beta_raw <- model_lda@beta

# Change column and rownames for later visualization
colnames(beta_raw) <- model_lda@terms
beta_raw <- exp(beta_raw)
beta_raw <- t(as.data.frame(beta_raw))
beta_raw <- data.frame(term = rownames(beta_raw), beta_raw)
names(beta_raw)[2:ncol(beta_raw)] <- str_c("Topic ", 1:(ncol(beta_raw)-1))

# Pivot dataframe to long format
beta <- beta_raw %>%
  pivot_longer(cols = 2:ncol(beta_raw))


# Retrieve the estimated gamma parameters from the model
Gamma <- as.data.frame(model_lda@gamma)
names(Gamma) <- str_c("topic_", 1:ncol(Gamma))

# Merge the data with the original document data
Gamma1 <- data.frame(Db[, c("date", "Navn")], Gamma)

# Make dataframe grouped by politicians
GammaD <- Gamma1[!duplicated(Gamma1$Navn), ]

# The typical way to get a sense of what each topic actually is
# means looking at the token that are most predictive
# Here, we'll look at the top ten words in each topic
top_terms <- terms(model_lda, 10)

G1 <- beta %>%
  group_by(name) %>%
  top_n(10) %>%
  arrange(name, -value) %>%
  ungroup()

# Top N highest values by group
G2 <- Reduce(rbind,                                 
                    by(G1,
                       G1["name"],
                       head,
                       n = 20))

# Graph the words most predictive of each topic
pdf("../data/Top_10_Beta.pdf", 12, 80)
G2 %>%
  ggplot(aes(y = term, x = value)) +
  facet_wrap(~ name, scales = "free", ncol = 2) +
  geom_segment(aes(xend = 0, yend = term)) +
  geom_point() +
  scale_y_reordered() +
  theme_minimal()
dev.off()

#### SELECT ONLY THE TOPICS NEEDED FOR ISSUE OWNERSHIP ####
GammaDb <- GammaD %>%
  select(Navn, topic_14, topic_40, topic_49, topic_30, topic_37)

GammaDb <- GammaDb %>%
  mutate(emneBlaa = topic_14+topic_40+topic_49+topic_30+topic_37)

GammaDr <- GammaD %>%
  select(Navn, topic_10, topic_34, topic_39, topic_28, topic_31)

GammaDr <- GammaDr %>%
  mutate(emneRoed = topic_10+topic_34+topic_39+topic_28+topic_31)

GammaD1 <- merge(x = GammaDb, y = GammaDr, by = "Navn")

#### COMBINE WITH DATAFRAME FROM 01-recoding ####
D2 <- merge(x = D1, y = GammaD1, by = "Navn")

# And then make a combined issue ownership variable
D2$emneBlaa[D2$redblok == TRUE] <- 0
D2$emneRoed[D2$redblok == FALSE] <- 0

D2 <- D2 %>%
  mutate(issueo = emneBlaa + emneRoed)
