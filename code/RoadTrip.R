# (run DanceParty first)
# or read output of DanceParty rds

everyone <- readRDS("input/everyone.rds")

library(mdatools); library(tidyverse); library(factoextra); library(MASS); library(ggordiplots)

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
