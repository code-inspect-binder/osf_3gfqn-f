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
load("C:/Users/bensz/OneDrive/Job/Research/VADIS/data/datives_descriptive.RData")

#data <- subset(data, Corpus == "ice") # restrict attention to ICE
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
data <- subset(data, Mode == "spoken")
#data <- subset(data, GENRE_COARSE  == "dialogue")
#data <- subset(data, MODE == "written")

names(data)[names(data)=="Resp"] <- "Response" # rename response variable

# prune lemmas for random effects
data$Verb_pruned <- JGmisc::filter.infrequent(data$Verb, 20)
data$RecHeadPlain_pruned <- JGmisc::filter.infrequent(data$RecHeadPlain, 20)
data$ThemeHeadPlain_pruned <- JGmisc::filter.infrequent(data$ThemeHeadPlain, 20)

### nice variety labels
data$varietylabels <- car::recode(data$Variety, " ## adapt as necessary
                                  
                                  'blah'  = 'blahblah'; 
                                  'GB'  = 'BrE'; 
                                  'IRE' = 'IrE'; 
                                  'NZ'  = 'NZE';
                                  'CAN' = 'CanE';
                                  'JA'  = 'JamE';
                                  'SIN' = 'SgE';
                                  'HK'  = 'HKE';
                                  'PHI' = 'PhlE';
                                  'IND' = 'IndE'
                                  ")

summary(data)

#============================================================================
# Splitting the dataset into variety-specific dataframes
#============================================================================

data_list <- split(data, data$varietylabels, drop = TRUE) # drop unused levels
names(data_list)

#============================================================================
# Step 1: specify formula
#============================================================================

f1 <- Response ~
  logWeightRatio +
  #RecNPExprType # redundant given RecPron
  RecPron + 
  #ThemeNPExprType # redundant given ThemePron
  ThemeBinComplexity +
  ThemeHeadFreq +
  ThemePron + 
  ThemeDefiniteness +
  RecGivenness +
  #VerbSemantics +
  RecHeadFreq
  #RecBinComplexity

# Add random effects
#f2 <- update(f1, .~. + (1|FileID) + (1|Verb_pruned) + (1|ThemeHeadPlain_pruned) + (1|GenreCoarse)) # only 2 lex effects
f2 <- update(f1, .~. + (1|Verb_pruned) + (1|GenreCoarse))

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
signif_line <- vadis_line1(glmer_list, path = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken-only/datives/line1.rds")
readRDS("C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken-only/datives/line1.rds", refhook = NULL)

#Individual components can be saved as .csv (or.txt) files like so.
write.csv(signif_line$signif.table, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken-only/datives/line1_significance_table.csv")
write.csv(as.matrix(signif_line$distance.matrix), 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken-only/datives/line1_distance_matrix.csv")
write.csv(signif_line$similarity.scores, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken-only/datives/line1_similarity_scores.csv")

## calculate similarity coefficient (mean of signif_line$similarity.scores)
mean(signif_line$similarity.scores[,2])
# [1] 0.7222222

#============================================================================
# Step 4: second line of evidence -- effect strength
#============================================================================

coef_line <- vadis_line2(glmer_list, path = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken-only/datives/line2.rds")
readRDS("C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken-only/datives/line2.rds", refhook = NULL)

#Individual components can be saved as .csv (or.txt) files like so.
write.csv(coef_line$signif.table, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken-only/datives/line2_significance_table.csv")
write.csv(as.matrix(coef_line$distance.matrix), 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken-only/datives/line2_distance_matrix.csv")
write.csv(coef_line$similarity.scores, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken-only/datives/line2_similarity_scores.csv")

## calculate similarity coefficient (mean of signif_line$similarity.scores)
mean(coef_line$similarity.scores[,2])
# [1] 0.4880688

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

varimp_line <- vadis_line3(crf_list, path = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken-only/datives/line3.rds", conditional = FALSE)

#Individual components can be saved as .csv (or.txt) files like so.
write.csv(varimp_line$signif.table, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken-only/datives/line3_significance_table.csv")
write.csv(as.matrix(varimp_line$distance.matrix), 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken-only/datives/line3_distance_matrix.csv")
write.csv(varimp_line$similarity.scores, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken-only/datives/line3_similarity_scores.csv")

## calculate similarity coefficient (mean of signif_line$similarity.scores)
mean(varimp_line$similarity.scores[,2])
# [1] 0.6164021


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
# [1] 0.6088977




