#============================================================================
# Packages etc.
#============================================================================

library(tidyverse) # for data wrangling
library(lme4) # for regression models
library(party) # for random forests
library(phangorn) # for neighborNets
library(car) # for re-coding
library(car) # for re-coding

#devtools::install_github("jasongraf1/VADIS") # needs library devtools
library(VADIS)

#============================================================================
# load & prepare dataset
#============================================================================
# acess the particle verbs dataset from the VADIS package
data <- particle_verbs_short

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
#data <- subset(data, MODE == "spoken")
#data <- subset(data, GENRE_COARSE  == "dialogue")
#data <- subset(data, MODE == "written")


# JG:
# Set variety labels.
#vars <- c("GB", "CA", "NZ", "IE", "JA", "SG", "HK", "PH", "IN")

# Set register labels
###regs <- c("spok.informal", "spok.formal", "writ.informal", "writ.formal",
###          "online")

# Set genre category labels and reorder `Genre`.
###cats <- c("PrivateDia", "UnscriptMono", "PublicDia", "ScriptedMono",
###          "Letters", "CreativeWrit", "Blog", "GeneralWeb", "StudentWrit",
###          "Reportage", "PopularWrit", "InstructWrit", "PersuasiveWrit",
###          "AcademicWrit")

# Remove some extreme data points and NAs.
###data <- subset(data, DirObjThematicity < 37 & !is.na(Rhythm)) %>%
###  droplevels

# Convert some columns to character (if loading from .txt), and set the levels for other factors
###data <- data %>%
###  dplyr::mutate(
###    Genre = factor(Genre, levels = cats),
###    Register = factor(Register, levels = regs),
###    PrimeType = relevel(PrimeType, ref = "none"),
###    Variety = factor(Variety, levels = vars))

# prune lemmas for random effects
data$VerbPart_pruned <- JGmisc::filter.infrequent(data$VerbPart, 20)
data$Verb_pruned <- JGmisc::filter.infrequent(data$Verb, 20)


### nice variety labels
data$varietylabels <- car::recode(data$Variety, " ## adapt as necessary
                             'blah'  = 'blahblah'; 
                             'GB'  = 'BrE'; 
                             'IE' = 'IrE'; 
                             'NZ'  = 'NZE';
                             'CA' = 'CanE';
                             'JA'  = 'JamE';
                             'SG' = 'SgE';
                             'HK'  = 'HKE';
                             'PH' = 'PhlE';
                             'IN' = 'IndE'
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

# the following is the formula we used in previous runs
#f1 <- Response ~ 
#  #Surprisal.vp.v + # removed because of VIFs > 4-5
#  #Idiomaticity + # too many levels for glm
#  DirObjWordLength +
#  VerbFreq +
#  #DirObjAnimacy + ## too many levels for glm
#  #PMI + # removed because ofVIFs > 3
#  #DirObjExprType + ## too many level for glm
#  DeltaP.cueV +
#  DirectionalModBinary +
#  Semantics +
#  VerbPartFreq +
#  # DirObjFreq too many NAs +
#  CV.binary +
#  DirObjDefiniteness +
#  PartFreq +
#  #Surprisal.vp.p # not considered because of collinearity
#  PrimeType

# formula suggested by Jason
f1 <- Response ~ 
  DirObjLettLength +
  DirObjDefiniteness +
  DirObjGivenness +
  DirObjConcreteness +
  DirObjThematicity +
  DirectionalPP +
  Semantics +
  Surprisal.P

# Add random effects
#f2 <- update(f1, .~. + (1|FileID) + (1|VerbPart_pruned) + (1|DirObjHeadPlain_pruned) + (1|GenreCoarse))
f2 <- update(f1, .~. + (1|VerbPart_pruned) + (1|Genre))

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
#BrE  1324    0.709          0.832 0.893 1118.141   1.164 1.699    0.406
#CanE 1392    0.693          0.830 0.905 1161.421   1.148 1.821    0.051
#HKE  1208    0.851          0.882 0.898  761.069   1.180 1.767    0.125
#IndE 1083    0.902          0.922 0.877  543.611   1.260 1.928    0.549
#IrE  1347    0.734          0.834 0.891 1125.728   1.168 1.714    0.022
#JamE 1106    0.854          0.892 0.880  709.759   1.253 1.726    0.181
#NZE  1543    0.686          0.854 0.912 1267.649   1.133 1.666    0.196
#PhlE 1004    0.865          0.888 0.885  602.609   1.215 1.686    0.855
#SgE  1333    0.848          0.893 0.908  828.958   1.201 1.689    0.042

#============================================================================
# Step 3: first line of evidence -- "Are the same constraints significant across varieties?"
#============================================================================

#signif_line <- vadis_line1(glmer_list, path = FALSE)
signif_line <- vadis_line1(glmer_list, path = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/particles/line1.rds")
readRDS("C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/particles/line1.rds", refhook = NULL)

#Individual components can be saved as .csv (or.txt) files like so.
write.csv(signif_line$signif.table, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/particles/line1_significance_table.csv")
write.csv(as.matrix(signif_line$distance.matrix), 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/particles/line1_distance_matrix.csv")
write.csv(signif_line$similarity.scores, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/particles/line1_similarity_scores.csv")

## calculate similarity coefficient (mean of signif_line$similarity.scores)
mean(signif_line$similarity.scores[,2])
# [1] 0.7430556

#============================================================================
# Step 4: second line of evidence -- effect strength
#============================================================================

coef_line <- vadis_line2(glmer_list, path = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/particles/line2.rds")
readRDS("C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/particles/line2.rds", refhook = NULL)

#Individual components can be saved as .csv (or.txt) files like so.
write.csv(coef_line$signif.table, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/particles/line2_significance_table.csv")
write.csv(as.matrix(coef_line$distance.matrix), 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/particles/line2_distance_matrix.csv")
write.csv(coef_line$similarity.scores, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/particles/line2_similarity_scores.csv")

## calculate similarity coefficient (mean of signif_line$similarity.scores)
mean(coef_line$similarity.scores[,2])
# [1] 0.7875245

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
#BrE  1324    0.709          0.840 0.913
#CanE 1392    0.693          0.832 0.911
#HKE  1208    0.851          0.890 0.927
#IndE 1083    0.902          0.917 0.933
#IrE  1347    0.734          0.831 0.905
#JamE 1106    0.854          0.889 0.930
#NZE  1543    0.686          0.835 0.917
#PhlE 1004    0.865          0.899 0.938
#SgE  1333    0.848          0.876 0.930

#============================================================================
# Step 6: third line of evidence -- constraint ranking
#============================================================================

varimp_line <- vadis_line3(crf_list, path = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/particles/line3.rds", conditional = FALSE)

#Individual components can be saved as .csv (or.txt) files like so.
write.csv(varimp_line$signif.table, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/particles/line3_significance_table.csv")
write.csv(as.matrix(varimp_line$distance.matrix), 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/particles/line3_distance_matrix.csv")
write.csv(varimp_line$similarity.scores, 
          file = "C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/particles/line3_similarity_scores.csv")

## calculate similarity coefficient (mean of signif_line$similarity.scores)
mean(varimp_line$similarity.scores[,2])
# [1] 0.728836

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
# [1] 0.7485742


