# (run DanceParty first)
# or read output of DanceParty rds

everyone <- readRDS("input/everyone.rds")

library(mdatools); library(tidyverse); library(factoextra); library(MASS); library(ggordiplots); 
library(nicheROVER)

# let's only do numeric values for now
variables <- c("danceability", "tempo", "valence", "track.popularity", 
               "instrumentalness", "acousticness", "liveness",  
               "energy", "loudness", "speechiness", "release_year")

everyonePCA <- as.data.frame(everyone) %>% 
  select(c(variables, person)) %>% 
  #mutate(person = factor(person)) %>% 
  drop_na()

# pca

evry.pca <- prcomp(everyonePCA[, -12], scale = TRUE)

# plot pca

mycolors <- c("#54086B", "#FF0BAC", "#00BEC5", "#E34234")
names(mycolors) <- levels(everyone$person)

fviz_pca_biplot(evry.pca, label="var", habillage = everyonePCA$person, 
             addEllipses=TRUE, ellipse.level=0.68)+
  theme_minimal()+
  scale_color_manual(values = mycolors)+
  scale_fill_manual(values = mycolors)+
  NULL

#### pca without release year ####

# let's only do numeric values for now
variables <- c("danceability", "tempo", "valence", "track.popularity", 
               "instrumentalness", "acousticness", "liveness",  
               "energy", "loudness", "speechiness")

everyonePCA <- as.data.frame(everyone) %>% 
  select(c(variables, person)) %>% 
  #mutate(person = factor(person)) %>% 
  drop_na()

# pca

evry.pca <- prcomp(everyonePCA[, -11], scale = TRUE)

# plot pca

mycolors <- c("#54086B", "#FF0BAC", "#00BEC5", "#E34234")
names(mycolors) <- levels(everyone$person)

fviz_pca_biplot(evry.pca, label="var", habillage = everyonePCA$person, 
                addEllipses=TRUE, ellipse.level=0.68)+
  theme_minimal()+
  scale_color_manual(values = mycolors)+
  scale_fill_manual(values = mycolors)+
  NULL

#### linear discriminant analysis ####

# lda data needs to be scaled

lda_data <- everyonePCA

lda_data[,1:10] <- scale(lda_data[,1:10])

lda_model <- MASS::lda(person ~ ., data=lda_data)

lda_model

lda_ordi <- gg_ordiplot(lda_model, groups = everyonePCA$person, pt.size = 2, conf = 0.68)

lda_ordiplot <- lda_ordi$plot

lda_ordiplot+
  theme_minimal()+
  scale_color_manual(values = mycolors)+
  scale_fill_manual(values = mycolors)+
  NULL

#### NicheROVER ####

# let's only do numeric values for now
variables_nicherover <- c("danceability", "valence", "track.popularity", 
               "instrumentalness", "acousticness", "liveness",  
               "energy", "loudness", "speechiness", "release_year")

everyone_niche <- as.data.frame(everyone) %>% 
  dplyr::select(c(variables_nicherover, person)) %>% 
  #mutate(person = factor(person)) %>% 
  drop_na()

# 2-d projections of 10 niche regions
#clrs <- c("black", "red", "blue", "orange") # colors for each species
nsamples <- 10
evry_par <- tapply(1:nrow(everyone_niche), everyone_niche$person,
                   function(ii) niw.post(nsamples = nsamples, X = everyone_niche[ii,1:10]))

# format data for plotting function
evry_data <- tapply(1:nrow(everyone_niche), everyone_niche$person, function(ii) X = everyone_niche[ii,1:10])

niche.plot(niche.par = evry_par, niche.data = evry_data, pfrac = .05,
          # iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = mycolors)

# niche overlap plots for 95% niche region sizes
nsamples <- 1000
evry_par <- tapply(1:nrow(everyone_niche), everyone_niche$person,
                   function(ii) niw.post(nsamples = nsamples, X = everyone_niche[ii,1:10]))

# Overlap calculation.  use nsamples = nprob = 10000 (1e4) for higher accuracy.
# the variable over.stat can be supplied directly to the overlap.plot function

over_stat <- overlap(evry_par, nreps = nsamples, nprob = 1e3, alpha = c(.95, 0.99))

#The mean overlap metrics calculated across iteratations for both niche 
#region sizes (alpha = .95 and alpha = .99) can be calculated and displayed in an array.
over_mean <- apply(over_stat, c(1:2,4), mean)*100
round(over_mean, 2)


############### Chris' code

chrisPCA <- chris %>% select(c(variables)) 
sarahPCA <- sarah %>% select(c(variables))
spencerPCA <- spencer %>% select(c(variables))
dustinPCA <- dustin %>% select(c(variables))

# generate indices for calibration set
idx = seq(1, nrow(everyonePCA), by = 8)

# split the values
Xc = everyonePCA[-idx, 1:14] 
cc = everyonePCA[-idx, 15]

Xt = everyonePCA[idx, 1:14]
ct = everyonePCA[idx, 15]

m.all = plsda(Xc, cc, 2, cv = 1)

summary(m.all)
getConfusionMatrix(m.all$calres)
plotPredictions(m.all)


# PCA ---------------------------------------------------------------------

library(MASS)
linear <- lda(person~., everyonePCA)
linear

training = everyonePCA[-idx]
p <- predict(linear, everyonePCA)
ldahist(data = p$x[,1], g = everyonePCA$person)

devtools::install_github('fawda123/ggord')
library(ggord)
ggord(linear, everyonePCA$person, ylim = c(-10, 10))
