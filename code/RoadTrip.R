# (run DanceParty first)

library(mdatools); library(tidyr)

# let's only do numeric values for now
variables <- c("danceability", "tempo", "valence", "track.explicit", "track.popularity", 
               "instrumentalness", "acousticness", "liveness", "time_signature")

everyonePCA <- as.data.frame(everyone) %>% 
  select(c(variables, person)) %>% 
  mutate(person = factor(person)) %>% 
  drop_na()

chrisPCA <- chris %>% select(c(variables)) 
sarahPCA <- sarah %>% select(c(variables))
spencerPCA <- spencer %>% select(c(variables))
dustinPCA <- dustin %>% select(c(variables))

# generate indices for calibration set
idx = seq(1, nrow(everyonePCA), by = 2)

# split the values
Xc = everyonePCA[idx, 1:9] 
cc = everyonePCA[idx, 10]

Xt = everyonePCA[-idx, 1:9]
ct = everyonePCA[-idx, 10]

m.all = plsda(Xc, cc, 7, cv = 1)
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
