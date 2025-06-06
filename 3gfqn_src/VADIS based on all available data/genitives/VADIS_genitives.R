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
load("C:/Users/bensz/OneDrive/Job/Research/VADIS/data/Genitives.RData")

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
#data <- subset(data, MODE == "spoken")
#data <- subset(data, GENRE_COARSE  == "dialogue")
#data <- subset(data, MODE == "written")

# change column names
names(data)[names(data)=="VARIETY"] <- "Variety" 
names(data)[names(data)=="VARIANT"] <- "Response" 
names(data)[names(data)=="FILE_ID"] <- "FileID" 

# prune lemmas for random effects
data$POR_HEAD_LEMMA_pruned <- JGmisc::filter.infrequent(data$POR_HEAD_LEMMA, 20)
data$PUM_HEAD_LEMMA_pruned <- JGmisc::filter.infrequent(data$PUM_HEAD_LEMMA, 20)

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
#f2 <- update(f1, .~. + (1|FileID) + (1|POR_HEAD_LEMMA_pruned) + (1|PUM_HEAD_LEMMA_pruned) + (1|GENRE_COARSE))
f2 <- update(f1, .~. + (1|POR_HEAD_LEMMA_pruned) + (1|GENRE_COARSE))

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

#N baseline predicted.corr     C      AIC Max.VIF kappa HosLem.p
#BrE  1612    0.729          0.854 0.910 1163.020   1.204 1.958    0.014
#CanE 1360    0.616          0.854 0.920 1018.358   1.228 2.243    0.550
#HKE  1529    0.680          0.853 0.917 1135.965   1.123 2.128    0.767
#IndE 1836    0.752          0.874 0.921 1212.166   1.133 1.923    0.565
#IrE  1313    0.703          0.843 0.903 1001.838   1.149 1.978    0.527
#JamE 1307    0.772          0.876 0.919  847.436   1.212 1.981    0.564
#NZE  1867    0.694          0.849 0.905 1418.148   1.109 1.940    0.031
#PhlE 1749    0.719          0.851 0.899 1323.141   1.092 1.912    0.594
#SgE  1225    0.656          0.849 0.910  991.788   1.225 2.365    0.004

#============================================================================
# Step 3: first line of evidence -- "Are the same constraints significant across varieties?"
#============================================================================

#signif_line <- vadis_line1(glmer_list, path = FALSE)
signif_line <- vadis_line1(glmer_list, path = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/genitives/line1.rds")
readRDS("C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/genitives/line1.rds", refhook = NULL)

#Individual components can be saved as .csv (or.txt) files like so.
write.csv(signif_line$signif.table, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/genitives/line1_significance_table.csv")
write.csv(as.matrix(signif_line$distance.matrix), 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/genitives/line1_distance_matrix.csv")
write.csv(signif_line$similarity.scores, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/genitives/line1_similarity_scores.csv")

## calculate similarity coefficient (mean of signif_line$similarity.scores)
mean(signif_line$similarity.scores[,2])
#[1] 0.9111111

#============================================================================
# Step 4: second line of evidence -- effect strength
#============================================================================

coef_line <- vadis_line2(glmer_list, path = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/genitives/line2.rds")
readRDS("C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/genitives/line2.rds", refhook = NULL)

#Individual components can be saved as .csv (or.txt) files like so.
write.csv(coef_line$signif.table, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/genitives/line2_significance_table.csv")
write.csv(as.matrix(coef_line$distance.matrix), 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/genitives/line2_distance_matrix.csv")
write.csv(coef_line$similarity.scores, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/genitives/line2_similarity_scores.csv")

## calculate similarity coefficient (mean of signif_line$similarity.scores)
mean(coef_line$similarity.scores[,2])
# [1] 0.6747373

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

#N baseline predicted.corr     C
#BrE  1612    0.729          0.870 0.929
#CanE 1360    0.616          0.860 0.931
#HKE  1529    0.680          0.853 0.922
#IndE 1836    0.752          0.880 0.927
#IrE  1313    0.703          0.841 0.920
#JamE 1307    0.772          0.881 0.929
#NZE  1867    0.694          0.859 0.919
#PhlE 1749    0.719          0.851 0.913
#SgE  1225    0.656          0.848 0.917

#============================================================================
# Step 6: third line of evidence -- constraint ranking
#============================================================================

varimp_line <- vadis_line3(crf_list, path = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/genitives/line3.rds", conditional = FALSE)

#Individual components can be saved as .csv (or.txt) files like so.
write.csv(varimp_line$signif.table, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/genitives/line3_significance_table.csv")
write.csv(as.matrix(varimp_line$distance.matrix), 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/genitives/line3_distance_matrix.csv")
write.csv(varimp_line$similarity.scores, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/genitives/line3_similarity_scores.csv")

## calculate similarity coefficient (mean of signif_line$similarity.scores)
mean(varimp_line$similarity.scores[,2])
#[1] 0.8252525

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
# [1] 0.8081261



