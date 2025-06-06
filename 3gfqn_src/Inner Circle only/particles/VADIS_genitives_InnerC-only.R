#
# R code for analysis of GENITIVES for the Inner Circle varieties only
# J Grafmiller
# 10/01/2019
#

#============================================================================
# Packages etc.
#============================================================================

library(tidyverse) # for data wrangling
library(lme4) # for regression models
library(party) # for random forests
library(phangorn) # for neighborNets
library(car) # for re-coding

#devtools::install_github("jasongraf1/VADIS") # needs library devtools
library(VADIS)

#============================================================================
# load & prepare dataset
#============================================================================
load("Genitives.RData")

data <- as.data.frame(genitives)

#data <- subset(data, Corpus == "ice") # restrimct attention to ICE
#summary(data)

# define subsets: L1 only
#data <- subset(data, Variety == "NZ" |
#                          Variety == "GB" |
#                          Variety == "IRE" |
#                          Variety == "CAN" )
#data$Variety <- factor(data$Variety)
#summary(data)

# define subsets: L2 only
#data <- subset(data, Variety == "JA" |
#                          Variety == "SIN" |
#                          Variety == "HK" |
#                          Variety == "PHI" |
#                          Variety == "IND" )
#data$Variety <- factor(data$Variety)
#summary(data)

# define subsets: genre
data <- subset(data, VARIETY %in% c("gb", "ire", "nz", "can")) %>%
  droplevels()
# data <- subset(data, MODE == "spoken")
#data <- subset(data, GENRE_COARSE  == "dialogue")
#data <- subset(data, MODE == "written")

# change column names
names(data)[names(data)=="VARIETY"] <- "Variety"
names(data)[names(data)=="VARIANT"] <- "Response"
names(data)[names(data)=="FILE_ID"] <- "FileID"

# prune lemmas for random effects
data$POR_HEAD_LEMMA_pruned <- JGmisc::filter.infrequent(data$POR_HEAD_LEMMA, 20)
data$PUM_HEAD_LEMMA_pruned <- JGmisc::filter.infrequent(data$PUM_HEAD_LEMMA, 20)

### POR_NP_EXPR_TYPE_3
data$POR_NP_EXPR_TYPE_binary <- car::recode(data$POR_NP_EXPR_TYPE_3, " ## adapt as necessary
                                            'blah'  = 'blahblah';
                                            'nc'  = 'nc';
                                            'other'  = 'other';
                                            'np' = 'other'
                                            ")

### nice variety labels
data$varietylabels <- car::recode(data$Variety, " ## adapt as necessary
                                  'blah'  = 'blahblah';
                                  'gb'  = 'BrE';
                                  'ire' = 'IrE';
                                  'nz'  = 'NZE';
                                  'can' = 'CanE';
                                  'ja'  = 'JamE';
                                  'sin' = 'SgE';
                                  'hk'  = 'HKE';
                                  'phi' = 'PhlE';
                                  'ind' = 'IndE'
                                  ")

summary(data)

#============================================================================
# Splitting the dataset into variety-specific dataframes
#============================================================================

data_list <- split(data, data$varietylabels, drop = TRUE) # drop unused levels
data_list <- lapply(data_list, droplevels)
names(data_list)

#============================================================================
# Step 1: specify formula
#============================================================================

f1 <- Response ~
  POR_ANIMACY_2 +         # simplified binary coding
  POR_LENGTH_WORDS +
  PUM_LENGTH_WORDS +
  POR_NP_EXPR_TYPE_binary +    # simplified binary coding
  POR_FINAL_SIBILANCY +
  #PUM_COMPLEXITY +       # removed because of collinearity concerns
  PREVIOUS_CHOICE +
  #POR_COMPLEXITY +       # removed because of collinearity concerns
  SEM_REL_2 +             # simplified binary coding
  POR_HEAD_FREQ

# Add random effects
f2 <- update(f1, .~. + (1|FileID) + (1|POR_HEAD_LEMMA_pruned) + (1|PUM_HEAD_LEMMA_pruned) + (1|GENRE_COARSE))
# f2 <- update(f1, .~. + (1|POR_HEAD_LEMMA_pruned))

# fit unitary model
#d <- data
#d_std <- stand(d, cols = f2)
#model <- glmer(f2, data = d_std, family = binomial, # set optimizer controls to help convergence
#               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e7)))
#summary(model)

#============================================================================
# Step 2: fit logistic regression models to the data (sub)set
#============================================================================

glmer_list <- vector("list")
for (i in seq_along(data_list)){
  cat(i); cat("...")
  d <- data_list[[i]]
  # standardize the model inputs, excluding the response and random effects
  d_std <- stand(d, cols = f2) # use the fitting function for convenience
  # fit the model
  glmer_list[[i]] <- glmer(f2, data = d_std, family = binomial, # set optimizer controls to help convergence
                           control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e7)))
  rm(d, d_std) # remove datasets
}
names(glmer_list) <- names(data_list)

# Checking fits
summary_stats(glmer_list) %>%
  round(3)

#============================================================================
# Step 3: first line of evidence -- "Are the same constraints significant across varieties?"
#============================================================================

#signif_line <- vadis_line1(glmer_list, path = FALSE)
signif_line <- vadis_line1(glmer_list, path = "InnerC-only/genitives/line1.rds")
readRDS("InnerC-only/genitives/line1.rds", refhook = NULL)

#Individual components can be saved as .csv (or.txt) files like so.
write.csv(signif_line$signif.table,
          file = "InnerC-only/genitives/line1_significance_table.csv")
write.csv(as.matrix(signif_line$distance.matrix),
          file = "InnerC-only/genitives/line1_distance_matrix.csv")
write.csv(signif_line$similarity.scores,
          file = "InnerC-only/genitives/line1_similarity_scores.csv")

## calculate similarity coefficient (mean of signif_line$similarity.scores)
mean(signif_line$similarity.scores[,2])
#[1] 0.7654321

#============================================================================
# Step 4: second line of evidence -- effect strength
#============================================================================

coef_line <- vadis_line2(glmer_list, path = "InnerC-only/genitives/line2.rds")
readRDS("InnerC-only/genitives/line2.rds", refhook = NULL)

#Individual components can be saved as .csv (or.txt) files like so.
write.csv(coef_line$coef.table,
          file = "InnerC-only/genitives/line2_coefficient_table.csv")
write.csv(as.matrix(coef_line$distance.matrix),
          file = "InnerC-only/genitives/line2_distance_matrix.csv")
write.csv(coef_line$similarity.scores,
          file = "InnerC-only/genitives/line2_similarity_scores.csv")

## calculate similarity coefficient (mean of signif_line$similarity.scores)
mean(coef_line$similarity.scores[,2])
# [1] 0.4818867

#============================================================================
# Step 5: calculate per-variety CRF models
#============================================================================

crf_func <- function(d) {
  cforest(f1, d, controls = cforest_unbiased(ntree = 500, mtry = 3))
}

crf_list <- lapply(data_list, FUN = crf_func)

# checking fits
summary_stats(crf_list, data_list, response = "Response") %>%
  round(3)

#============================================================================
# Step 6: third line of evidence -- constraint ranking
#============================================================================

varimp_line <- vadis_line3(crf_list, path = "InnerC-only/genitives/line3.rds", conditional = FALSE)

#Individual components can be saved as .csv (or.txt) files like so.
write.csv(varimp_line$varimp.table,
          file = "InnerC-only/genitives/line3_varimp_table.csv")
write.csv(as.matrix(varimp_line$distance.matrix),
          file = "InnerC-only/genitives/line3_distance_matrix.csv")
write.csv(varimp_line$similarity.scores,
          file = "InnerC-only/genitives/line3_similarity_scores.csv")

## calculate similarity coefficient (mean of signif_line$similarity.scores)
mean(varimp_line$similarity.scores[,2])
#[1] 0.7109788

#============================================================================
# Combining the three lines -- calculating mean similarity coefficients
#============================================================================

mean_sims <- data.frame(
  line1 = signif_line$similarity.scores[,2], # get only the values in the 2nd column
  line2 = coef_line$similarity.scores[,2],
  line3 = varimp_line$similarity.scores[,2]
)
mean_sims$mean <- apply(mean_sims, 1, mean)
rownames(mean_sims) <- names(data_list)
round(mean_sims, 3)

mean(mean_sims$mean) # calculate mean similarity coefficient
# [1] 0.6527659


