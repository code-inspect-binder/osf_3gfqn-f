#============================================================================
# Packages etc.
#============================================================================

#devtools::install_github("jasongraf1/VADIS") # needs library devtools
library(VADIS)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggrepel); set.seed(2345) # for plotting
library(analogue) # for dusing distance matrices
library(phangorn) # for neighbornets
library(vegan) # for Mantel tests


# ================================================
# read in dms
# ================================================

# in this section we read in the RDS objects created in previous runs of the VADIS package,
# and extract the distance matrices per line of evidence and alternation


################
### ALL DATA ###
################
setwd("C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/all_available_data/") 
### genitives
signif_line <- readRDS("genitives/line1.rds", refhook = NULL); d1_genitives_all <- signif_line$distance.matrix
coef_line   <- readRDS("genitives/line2.rds", refhook = NULL); d2_genitives_all <- coef_line$distance.matrix
varimp_line <- readRDS("genitives/line3.rds", refhook = NULL); d3_genitives_all <- varimp_line$distance.matrix

### datives
signif_line <- readRDS("datives/line1.rds", refhook = NULL);   d1_datives_all   <- signif_line$distance.matrix
coef_line   <- readRDS("datives/line2.rds", refhook = NULL);   d2_datives_all   <- coef_line$distance.matrix
varimp_line <- readRDS("datives/line3.rds", refhook = NULL);   d3_datives_all   <- varimp_line$distance.matrix

### particles
signif_line <- readRDS("particles/line1.rds", refhook = NULL); d1_particles_all <- signif_line$distance.matrix
coef_line   <- readRDS("particles/line2.rds", refhook = NULL); d2_particles_all <- coef_line$distance.matrix
varimp_line <- readRDS("particles/line3.rds", refhook = NULL); d3_particles_all <- varimp_line$distance.matrix


####################
### Outer Circle ###
####################
setwd("C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/Outer Circle only/") 
### genitives
signif_line <- readRDS("genitives/line1.rds", refhook = NULL); d1_genitives_out <- signif_line$distance.matrix
coef_line   <- readRDS("genitives/line2.rds", refhook = NULL); d2_genitives_out <- coef_line$distance.matrix
varimp_line <- readRDS("genitives/line3.rds", refhook = NULL); d3_genitives_out <- varimp_line$distance.matrix

### datives
signif_line <- readRDS("datives/line1.rds", refhook = NULL);   d1_datives_out   <- signif_line$distance.matrix
coef_line   <- readRDS("datives/line2.rds", refhook = NULL);   d2_datives_out   <- coef_line$distance.matrix
varimp_line <- readRDS("datives/line3.rds", refhook = NULL);   d3_datives_out   <- varimp_line$distance.matrix

### particles
signif_line <- readRDS("particles/line1.rds", refhook = NULL); d1_particles_out <- signif_line$distance.matrix
coef_line   <- readRDS("particles/line2.rds", refhook = NULL); d2_particles_out <- coef_line$distance.matrix
varimp_line <- readRDS("particles/line3.rds", refhook = NULL); d3_particles_out <- varimp_line$distance.matrix

####################
### Inner Circle ###
####################
setwd("C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/Inner Circle Only/") 
### genitives
signif_line <- readRDS("genitives/line1.rds", refhook = NULL); d1_genitives_inn <- signif_line$distance.matrix
coef_line   <- readRDS("genitives/line2.rds", refhook = NULL); d2_genitives_inn <- coef_line$distance.matrix
varimp_line <- readRDS("genitives/line3.rds", refhook = NULL); d3_genitives_inn <- varimp_line$distance.matrix

### datives
signif_line <- readRDS("datives/line1.rds", refhook = NULL);   d1_datives_inn   <- signif_line$distance.matrix
coef_line   <- readRDS("datives/line2.rds", refhook = NULL);   d2_datives_inn   <- coef_line$distance.matrix
varimp_line <- readRDS("datives/line3.rds", refhook = NULL);   d3_datives_inn   <- varimp_line$distance.matrix

### particles
signif_line <- readRDS("particles/line1.rds", refhook = NULL); d1_particles_inn <- signif_line$distance.matrix
coef_line   <- readRDS("particles/line2.rds", refhook = NULL); d2_particles_inn <- coef_line$distance.matrix
varimp_line <- readRDS("particles/line3.rds", refhook = NULL); d3_particles_inn <- varimp_line$distance.matrix

####################
### spoken only  ###
####################
setwd("C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/spoken data only/") 
### genitives
signif_line <- readRDS("genitives/line1.rds", refhook = NULL); d1_genitives_spo <- signif_line$distance.matrix
coef_line   <- readRDS("genitives/line2.rds", refhook = NULL); d2_genitives_spo <- coef_line$distance.matrix
varimp_line <- readRDS("genitives/line3.rds", refhook = NULL); d3_genitives_spo <- varimp_line$distance.matrix

### datives
signif_line <- readRDS("datives/line1.rds", refhook = NULL);   d1_datives_spo   <- signif_line$distance.matrix
coef_line   <- readRDS("datives/line2.rds", refhook = NULL);   d2_datives_spo   <- coef_line$distance.matrix
varimp_line <- readRDS("datives/line3.rds", refhook = NULL);   d3_datives_spo   <- varimp_line$distance.matrix

### particles
signif_line <- readRDS("particles/line1.rds", refhook = NULL); d1_particles_spo <- signif_line$distance.matrix
coef_line   <- readRDS("particles/line2.rds", refhook = NULL); d2_particles_spo <- coef_line$distance.matrix
varimp_line <- readRDS("particles/line3.rds", refhook = NULL); d3_particles_spo <- varimp_line$distance.matrix

####################
### written only  ###
####################
setwd("C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/written data only/") 
### genitives
signif_line <- readRDS("genitives/line1.rds", refhook = NULL); d1_genitives_wri <- signif_line$distance.matrix
coef_line   <- readRDS("genitives/line2.rds", refhook = NULL); d2_genitives_wri <- coef_line$distance.matrix
varimp_line <- readRDS("genitives/line3.rds", refhook = NULL); d3_genitives_wri <- varimp_line$distance.matrix

### datives
signif_line <- readRDS("datives/line1.rds", refhook = NULL);   d1_datives_wri   <- signif_line$distance.matrix
coef_line   <- readRDS("datives/line2.rds", refhook = NULL);   d2_datives_wri   <- coef_line$distance.matrix
varimp_line <- readRDS("datives/line3.rds", refhook = NULL);   d3_datives_wri   <- varimp_line$distance.matrix

### particles
signif_line <- readRDS("particles/line1.rds", refhook = NULL); d1_particles_wri <- signif_line$distance.matrix
coef_line   <- readRDS("particles/line2.rds", refhook = NULL); d2_particles_wri <- coef_line$distance.matrix
varimp_line <- readRDS("particles/line3.rds", refhook = NULL); d3_particles_wri <- varimp_line$distance.matrix

####################
### GloWbE only  ###
####################
setwd("C:/Users/bensz/OneDrive/Job/Research/VADIS/VADIS_package_runs/Glowbe only/") 
### genitives
signif_line <- readRDS("genitives/line1.rds", refhook = NULL); d1_genitives_glo <- signif_line$distance.matrix
coef_line   <- readRDS("genitives/line2.rds", refhook = NULL); d2_genitives_glo <- coef_line$distance.matrix
varimp_line <- readRDS("genitives/line3.rds", refhook = NULL); d3_genitives_glo <- varimp_line$distance.matrix

### datives
signif_line <- readRDS("datives/line1.rds", refhook = NULL);   d1_datives_glo   <- signif_line$distance.matrix
coef_line   <- readRDS("datives/line2.rds", refhook = NULL);   d2_datives_glo   <- coef_line$distance.matrix
varimp_line <- readRDS("datives/line3.rds", refhook = NULL);   d3_datives_glo   <- varimp_line$distance.matrix

### particles
signif_line <- readRDS("particles/line1.rds", refhook = NULL); d1_particles_glo <- signif_line$distance.matrix
coef_line   <- readRDS("particles/line2.rds", refhook = NULL); d2_particles_glo <- coef_line$distance.matrix
varimp_line <- readRDS("particles/line3.rds", refhook = NULL); d3_particles_glo <- varimp_line$distance.matrix


# ================================================
# Combining distance matrices
# ================================================

# see https://cran.r-project.org/web/packages/analogue/analogue.pdf
# about fuse()
# Fuses, or combines, dissimilarity objects in a very flexible way to create a single dissimilarity object
# that incorporates the separate dissimilarities. In analogue matching, we may wish to combine information
# from two or more proxies, such as diatoms and cladocera, or from biological and chemical
# or physical data in the case of matching modern samples.
# The function can also be used to fuse dissimilarity objects created from a single data set but using
# different dissimilarity coefficients. In this way one could create a new dissimilarity object combining
# dissimilarity based on abundance data and presence absence data into a single measure.
# fuse uses the method of Melssen et al. (2006) to combine dissimilarities. The dissimilarities in
# each dissimilarity object are scaled so that the maximum dissimilarity in each object is 1. The
# scaled dissimilarity objects are then weighted according to the supplied weights

################
### ALL DATA ###
################
# genitives across lines of evidence
dfused_genitives_all <- analogue::fuse(d1_genitives_all, 
                                       d2_genitives_all, 
                                       d3_genitives_all)

# datives across lines of evidence
dfused_datives_all <- analogue::fuse(d1_datives_all, 
                                     d2_datives_all, 
                                     d3_datives_all)

# particles across lines of evidence
dfused_particles_all <- analogue::fuse(d1_particles_all, 
                                       d2_particles_all, 
                                       d3_particles_all)

# fusing fused distance matrices across alternations
dfused_total_all <- analogue::fuse(dfused_genitives_all, 
                                   dfused_datives_all, 
                                   dfused_particles_all)

# fusing fused distance matrices -- genitives + particles only
dfused_total_genpar <- analogue::fuse(dfused_genitives_all, 
                                   dfused_particles_all)

####################
### Outer Circle ###
####################
# genitives across lines of evidence
dfused_genitives_out <- analogue::fuse(d1_genitives_out, 
                                       d2_genitives_out, 
                                       d3_genitives_out)

# datives across lines of evidence
dfused_datives_out <- analogue::fuse(d1_datives_out, 
                                     d2_datives_out, 
                                     d3_datives_out)

# particles across lines of evidence
dfused_particles_out <- analogue::fuse(d1_particles_out, 
                                       d2_particles_out, 
                                       d3_particles_out)

# fusing fused distance matrices across alternations
dfused_total_out <- analogue::fuse(dfused_genitives_out, 
                                   dfused_datives_out, 
                                   dfused_particles_out)

####################
### Inner Circle ###
####################
# genitives across lines of evidence
dfused_genitives_inn <- analogue::fuse(d1_genitives_inn, 
                                       d2_genitives_inn, 
                                       d3_genitives_inn)

# datives across lines of evidence
dfused_datives_inn <- analogue::fuse(d1_datives_inn, 
                                     d2_datives_inn, 
                                     d3_datives_inn)

# particles across lines of evidence
dfused_particles_inn <- analogue::fuse(d1_particles_inn, 
                                       d2_particles_inn, 
                                       d3_particles_inn)

# fusing fused distance matrices across alternations
dfused_total_inn <- analogue::fuse(dfused_genitives_inn, 
                                   dfused_datives_inn, 
                                   dfused_particles_inn)

####################
### spoken only  ###
####################
# genitives across lines of evidence
dfused_genitives_spo <- analogue::fuse(d1_genitives_spo, 
                                       d2_genitives_spo, 
                                       d3_genitives_spo)

# datives across lines of evidence
dfused_datives_spo <- analogue::fuse(d1_datives_spo, 
                                     d2_datives_spo, 
                                     d3_datives_spo)

# particles across lines of evidence
dfused_particles_spo <- analogue::fuse(d1_particles_spo, 
                                       d2_particles_spo, 
                                       d3_particles_spo)

# fusing fused distance matrices across alternations
dfused_total_spo <- analogue::fuse(dfused_genitives_spo, 
                                   dfused_datives_spo, 
                                   dfused_particles_spo)

####################
### written only  ###
####################
# genitives across lines of evidence
dfused_genitives_wri <- analogue::fuse(d1_genitives_wri, 
                                       d2_genitives_wri, 
                                       d3_genitives_wri)

# datives across lines of evidence
dfused_datives_wri <- analogue::fuse(d1_datives_wri, 
                                     d2_datives_wri, 
                                     d3_datives_wri)

# particles across lines of evidence
dfused_particles_wri <- analogue::fuse(d1_particles_wri, 
                                       d2_particles_wri, 
                                       d3_particles_wri)

# fusing fused distance matrices across alternations
dfused_total_wri <- analogue::fuse(dfused_genitives_wri, 
                                   dfused_datives_wri, 
                                   dfused_particles_wri)

####################
### GloWbE only  ###
####################
# genitives across lines of evidence
dfused_genitives_glo <- analogue::fuse(d1_genitives_glo, 
                                       d2_genitives_glo, 
                                       d3_genitives_glo)

# datives across lines of evidence
dfused_datives_glo <- analogue::fuse(d1_datives_glo, 
                                     d2_datives_glo, 
                                     d3_datives_glo)

# particles across lines of evidence
dfused_particles_glo <- analogue::fuse(d1_particles_glo, 
                                       d2_particles_glo, 
                                       d3_particles_glo)

# fusing fused distance matrices across alternations
dfused_total_glo <- analogue::fuse(dfused_genitives_glo, 
                                   dfused_datives_glo, 
                                   dfused_particles_glo)

# ================================================
# MDS
# ================================================

#the following is the code suggested in the vignette
#mds <- cmdscale(d3_particles, k = 3, eig = T) # specify name of input distance matrix here
#mds[[1]] %>%
#  as.data.frame() %>% 
#  mutate(genres = rownames(.)) %>% 
#  ggplot(aes(V1, V2, label = genres)) +
#  geom_point() +
#  geom_text(nudge_y = .01, size = 4)

# nicer plot
input <- dfused_total_all # specify distance matrix name name
fit <- cmdscale(input,eig=TRUE, k=2) # k is the number of dim
fit.df <- as.data.frame(fit[[1]])
names(fit.df) <- c("x","y")
fit.df$Variety <- rownames(fit.df) %>% as.factor
theme_mr = theme_set(theme_light())
theme_mr = theme_update(axis.text = element_text(size = rel(1.), color="black"),
                        axis.ticks = element_line(colour = "grey90", size = 0.25),
                        axis.title = element_text(size=rel(0.9)),
                        panel.border = element_rect(color = "black"),
                        strip.background=element_rect(fill="grey95", color="black"), 
                        strip.text.x=element_text(color="black"))

dev.new(width=2, height=2)
ggplot(fit.df, aes(x,y)) +
  geom_text_repel(aes(label=Variety), size=7, box.padding = unit(0.5, "lines"), segment.colour = "grey65") + # change text size here
  geom_point(size=5) + # change dot size here
  labs(y= "MDS Dimension 2", x="MDS Dimension 1") +
  theme(axis.title = element_text(size=15))


# ================================================
# Neighbornet diagram
# ================================================

dev.new(width=3, height=3)
input <- dfused_total_all #specify distance matrix name name
nnet <- neighborNet(input)
plot(nnet, "2D")



# ================================================
# Mantel test
# ================================================

###################################
# Overlap between lines of evidence
###################################
# genitive alternation: overlap across lines of evidence, all available data 
mantel(d1_genitives_all,d2_genitives_all)
mantel(d1_genitives_all,d3_genitives_all)
mantel(d2_genitives_all,d3_genitives_all)

# dative alternation: overlap across lines of evidence, all available data 
mantel(d1_datives_all,d2_datives_all)
mantel(d1_datives_all,d3_datives_all)
mantel(d2_datives_all,d3_datives_all)

# particle placement alternation: overlap across lines of evidence, all available data 
mantel(d1_particles_all,d2_particles_all)
mantel(d1_particles_all,d3_particles_all)
mantel(d2_particles_all,d3_particles_all)


###################################
# Overlap between alternations
###################################

# all available data
mantel(dfused_genitives_all,dfused_datives_all)
mantel(dfused_genitives_all,dfused_particles_all)
mantel(dfused_datives_all,dfused_particles_all)

# spoken only
mantel(dfused_genitives_spo,dfused_datives_spo)
mantel(dfused_genitives_spo,dfused_particles_spo)
mantel(dfused_datives_spo,dfused_particles_spo)

# written only
mantel(dfused_genitives_wri,dfused_datives_wri)
mantel(dfused_genitives_wri,dfused_particles_wri)
mantel(dfused_datives_wri,dfused_particles_wri)

# Inner Circle
mantel(dfused_genitives_inn,dfused_datives_inn)
mantel(dfused_genitives_inn,dfused_particles_inn)
mantel(dfused_datives_inn,dfused_particles_inn)

# Outer Circle
mantel(dfused_genitives_out,dfused_datives_out)
mantel(dfused_genitives_out,dfused_particles_out)
mantel(dfused_datives_out,dfused_particles_out)

# GloWbE
mantel(dfused_genitives_glo,dfused_datives_glo)
mantel(dfused_genitives_glo,dfused_particles_glo)
mantel(dfused_datives_glo,dfused_particles_glo)



