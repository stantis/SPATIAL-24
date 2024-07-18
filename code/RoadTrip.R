# (run DanceParty first)
# or read output of DanceParty rds

everyone <- readRDS("input/everyone.rds")

library(mdatools); library(tidyverse); library(factoextra); library(MASS); library(ggordiplots); 
library(nicheROVER); library(ggh4x); library(sp)

# let's only do numeric values for now
variables <- c("danceability", "tempo", "valence", "track.popularity", 
               "instrumentalness", "acousticness", "liveness",  
               "energy", "loudness", "speechiness", "release_year")

everyonePCA <- as.data.frame(everyone) %>% 
  dplyr::select(c(variables, person)) %>% 
  #mutate(person = factor(person)) %>% 
  drop_na()

# pca

evry.pca <- prcomp(everyonePCA[, -12], scale = TRUE)

# plot pca

mycolors <- c("#54086B", "#FF0BAC", "#00BEC5", "#E34234")
names(mycolors) <- levels(everyone$person)

pca_plot <- fviz_pca_biplot(evry.pca, label="var", habillage = everyonePCA$person, 
             addEllipses=TRUE, ellipse.level=0.95, col.var = "black")+
  theme_minimal()+
  scale_color_manual(values = mycolors)+
  scale_fill_manual(values = mycolors)+
  theme(legend.position = "")+
  NULL

ggsave(pca_plot, filename= "output/PCA_plot.png", dpi = 150, width = 15, height = 15, units = "cm")

#### recreate PCA ellipses using SIBER to filter points within and without

points_on_pca <- as.data.frame(cbind(evry.pca$x, everyonePCA$person)) %>%
  mutate_at(vars(matches("PC")), as.numeric) %>%
  rename(person = V12)

ggplot(points_on_pca, aes(x = PC1, y = PC2, color = person))+
  theme_minimal()+
  geom_point()+
  scale_color_manual(values = mycolors)+
  NULL

# get ellipse means for each person

ellipse_means <- points_on_pca %>%
  dplyr::select(PC1, PC2, person) %>%
  group_by(person) %>%
  summarize_at(c("PC1", "PC2"), mean) %>%
  rename(mean_PC1 = PC1, mean_PC2 = PC2) %>%
  as.data.frame(.)

# get covariance matrix

cov_list <- vector("list", 4)

for (i in 1:4) {
  
  person <- ellipse_means$person[i]
  
 cov_list[[i]] <- points_on_pca %>%
    filter(person == person) %>%
    dplyr::select(PC1, PC2) %>%
    cov(.)
 
 names(cov_list)[i] <- person
}

# draw the ellipse

ell_pca <- ggplot(points_on_pca, aes(x = PC1, y = PC2, color = person, fill = person))+
  theme_minimal()+
  geom_point()+
  scale_color_manual(values = mycolors)+
  scale_fill_manual(values = mycolors)+
  stat_ellipse(geom = "polygon", level = 0.40, alpha = 0.2)+
  xlim(-4,8)+
  ylim(-4,5)+
  theme(legend.position = "")+
  NULL

# ggsave(ell_pca, filename= "output/clean_pca_plot.png", 
#        dpi = 200, width = 20, height = 20, units = "cm")

# test if points are inside ellipse

everyone_clean <- as.data.frame(everyone) %>% 
  #dplyr::select(c(variables, person)) %>% 
  #mutate(person = factor(person)) %>% 
  drop_na(variables)

# Extract components
build <- ggplot_build(ell_pca)$data
points <- build[[1]]
ellipses <- build[[2]]

col_v_peeps <- data.frame(colour = mycolors, person = ellipse_means$person)

points_p <- left_join(points, col_v_peeps, by = "colour")
ellipses_p <- left_join(ellipses, col_v_peeps, by = "colour")

evry_gg_ellipse_out <- everyone_clean

evry_gg_out_pca <- points_on_pca

for (i in 1:4) {
  
  p_name <- ellipse_means$person[i]
  
  ellipse_sub <- ellipses_p %>% 
    filter(person == p_name)
  
  in_ell <- data.frame(
    in_ell = as.logical(point.in.polygon(points$x, points$y, ellipse_sub$x, ellipse_sub$y))
  )
  
  names(in_ell) <- paste("song_in_ellipse", p_name, sep = "_")
  
  evry_gg_ellipse_out <- cbind(evry_gg_ellipse_out, in_ell)
  
  evry_gg_out_pca <- cbind(evry_gg_out_pca, in_ell)
  
}

ggplot()+
  theme_minimal()+
  geom_point(data = evry_gg_out_pca, aes(x = PC1, y = PC2, fill = song_in_ellipse_Sarah), pch = 21)+
  scale_fill_manual(values = c("black", "grey"))+
  scale_color_manual(values = mycolors)+
  stat_ellipse(data = points_on_pca, aes(x = PC1, y = PC2, color = person), level = 0.40)+
  xlim(-4,8)+
  ylim(-4,5)+
  NULL

#### find songs in overlap of all ####

all_overlap_songs <- evry_gg_ellipse_out %>%
  filter(song_in_ellipse_Chris == TRUE &
           song_in_ellipse_Dustin == TRUE &
           song_in_ellipse_Sarah == TRUE &
           song_in_ellipse_Spencer == TRUE)

all_overlap_songs_pca <- evry_gg_out_pca %>%
  filter(song_in_ellipse_Chris == TRUE &
           song_in_ellipse_Dustin == TRUE &
           song_in_ellipse_Sarah == TRUE &
           song_in_ellipse_Spencer == TRUE) %>%
  cbind(., all_overlap_songs$track.name)

playlist_plot <- ggplot()+
  theme_minimal()+
  geom_point(data = evry_gg_out_pca, aes(x = PC1, y = PC2, color = person), alpha = 0.3)+
  scale_fill_manual(values = mycolors)+
  scale_color_manual(values = mycolors)+
  stat_ellipse(data = evry_gg_out_pca, 
               geom = "polygon", aes(x = PC1, y = PC2, fill = person, color = person), 
               level = 0.40, alpha = 0.1)+
  geom_point(data = all_overlap_songs_pca, aes(x = PC1, y = PC2), color = "black")+
  geom_text(data = all_overlap_songs_pca, aes(x = PC1, y = PC2, label = track.name))
  xlim(-4,8)+
  ylim(-4,5)+
  theme(legend.position = "")+
  NULL

ggsave(playlist_plot, filename= "output/overlap_playlist_plot.png", 
       dpi = 200, width = 20, height = 20, units = "cm")

overlap_40_clean <- as.data.frame(all_overlap_songs) %>% 
    dplyr::select(c(variables, person)) %>%
    mutate(person = "all")

overlap_v_original <- overlap_40_clean %>%
  mutate(person = "all") %>%
  rbind(everyonePCA)

artists <- data.frame(artist_name = character(25))

#i <- 2

for (i in 1:25) {
  
  artists$artist_name[i] <- paste(all_overlap_songs$track.artists[[i]]$name, collapse = ", ")
  
}

saveRDS(all_overlap_songs, "output/playlist_of_overlap_songs.rds")

playlist_overlap_40 <- cbind(all_overlap_songs$track.name, artists, all_overlap_songs$person)

#### at 95% look at % songs outside everyone elses ellipse ####

# draw the ellipse

ell_pca <- ggplot(points_on_pca, aes(x = PC1, y = PC2, color = person, fill = person))+
  theme_minimal()+
  geom_point()+
  scale_color_manual(values = mycolors)+
  scale_fill_manual(values = mycolors)+
  stat_ellipse(geom = "polygon", level = 0.95, alpha = 0.2)+
  xlim(-4,8)+
  ylim(-4,5)+
  NULL

# test if points are inside ellipse

everyone_clean <- as.data.frame(everyone) %>% 
  #dplyr::select(c(variables, person)) %>% 
  #mutate(person = factor(person)) %>% 
  drop_na(variables)

# Extract components
build <- ggplot_build(ell_pca)$data
points <- build[[1]]
ellipses <- build[[2]]

col_v_peeps <- data.frame(colour = mycolors, person = ellipse_means$person)

points_p <- left_join(points, col_v_peeps, by = "colour")
ellipses_p <- left_join(ellipses, col_v_peeps, by = "colour")

evry_gg_ellipse_out <- everyone_clean

evry_gg_out_pca <- points_on_pca

for (i in 1:4) {
  
  p_name <- ellipse_means$person[i]
  
  ellipse_sub <- ellipses_p %>% 
    filter(person == p_name)
  
  in_ell <- data.frame(
    in_ell = as.logical(point.in.polygon(points$x, points$y, ellipse_sub$x, ellipse_sub$y))
  )
  
  names(in_ell) <- paste("song_in_ellipse", p_name, sep = "_")
  
  evry_gg_ellipse_out <- cbind(evry_gg_ellipse_out, in_ell)
  
  evry_gg_out_pca <- cbind(evry_gg_out_pca, in_ell)
  
}

#### find songs without overlap ###

songs_unique_Chris <- evry_gg_ellipse_out %>%
  filter(person == "Chris" &
           song_in_ellipse_Dustin == FALSE &
           song_in_ellipse_Sarah == FALSE &
           song_in_ellipse_Spencer == FALSE) %>%
  mutate(unique_to = "Chris")

songs_unique_Dustin <- evry_gg_ellipse_out %>%
  filter(person == "Dustin" &
           song_in_ellipse_Chris == FALSE &
           song_in_ellipse_Sarah == FALSE &
           song_in_ellipse_Spencer == FALSE) %>%
  mutate(unique_to = "Dustin")

songs_unique_Sarah <- evry_gg_ellipse_out %>%
  filter(person == "Sarah" &
           song_in_ellipse_Dustin == FALSE &
           song_in_ellipse_Sarah == FALSE &
           song_in_ellipse_Spencer == FALSE) %>%
  mutate(unique_to = "Sarah")

songs_unique_Spencer <- evry_gg_ellipse_out %>%
  filter(person == "Spencer" &
           song_in_ellipse_Dustin == FALSE &
           song_in_ellipse_Sarah == FALSE &
           song_in_ellipse_Spencer == FALSE) %>%
  mutate(unique_to = "Spencer")

all_uniqueness <- rbind(songs_unique_Chris, 
                        songs_unique_Dustin, 
                        songs_unique_Sarah, 
                        songs_unique_Spencer)

no_songs <- everyonePCA %>%
  group_by(person) %>%
  summarise(n_total = n())

uniqueness_summary <- all_uniqueness %>%
  group_by(unique_to) %>%
  summarise(n_unique = n()) %>%
  rename(person = unique_to) %>%
  left_join(., no_songs, by = "person") %>%
  mutate(perc_unique_songs = 100/n_total*n_unique)

songs_unique_Spencer$track.name

songs_unique_Sarah$track.name

songs_unique_Sarah$track.album.name

unique_songs_plot <- ggplot(uniqueness_summary, aes(person, perc_unique_songs, fill = person))+
  theme_minimal()+
  geom_col()+
  scale_fill_manual(values = mycolors)+
  theme(legend.position = "")+
  labs(x = "Person", y = "% songs outside all other niches")

ggsave(unique_songs_plot, filename= "output/unique_songs_plot.png", 
       dpi = 150, width = 15, height = 15, units = "cm")


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

over_stat <- overlap(evry_par, nreps = nsamples, nprob = 1e3, alpha = c(.95))

#The mean overlap metrics calculated across iteratations for both niche 
#region sizes (alpha = .95 and alpha = .99) can be calculated and displayed in an array.
over_mean <- apply(over_stat, c(1:2,4), mean)*100
round(over_mean, 2)

overlap.plot(over_stat, col = mycolors, mean.cred.col = "black", equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")

## overlap plots in ggplot ##

over_stat_df <- over_stat %>% 
  as_tibble(rownames = "species_a") %>% 
  mutate(
    id = 1:nrow(.), 
    species_a = factor(species_a, 
                       level = c("Chris", "Dustin", "Sarah", "Spencer"))
  ) %>% 
  pivot_longer(cols = -c(id, species_a), 
               names_to = "species_b", 
               values_to = "mc_nr")  %>% 
  separate(species_b, into = c("species_c", "sample_number"), 
           sep = "\\.") %>% 
  dplyr::select(-id) %>% 
  rename(species_b = species_c) %>% 
  mutate(
    species_b =  factor(species_b, 
                        level = c("Chris", "Dustin", "Sarah", "Spencer")
    ), 
    mc_nr_perc = mc_nr * 100
  )

over_sum <- over_stat_df %>% 
  group_by(species_a, species_b) %>% 
  summarise(
    mean_mc_nr = round(mean(mc_nr_perc), digits = 2),
    qual_2.5 = round(quantile(mc_nr_perc, probs = 0.025, na.rm = TRUE), digits = 2), 
    qual_97.5 = round(quantile(mc_nr_perc, probs = 0.975, na.rm = TRUE), digits = 2)
  ) %>% 
  ungroup() %>% 
  pivot_longer(cols = -c(species_a, species_b, mean_mc_nr), 
               names_to = "percentage", 
               values_to = "mc_nr_qual") %>% 
  mutate(
    percentage = as.numeric(str_remove(percentage, "qual_"))
  ) 

ggplot(data = over_stat_df, aes(x = mc_nr_perc)) + 
  geom_density(aes(fill = species_a)) + 
  geom_vline(data = over_sum, aes(xintercept = mean_mc_nr), 
             colour = "black", linewidth = 1) +
  geom_vline(data = over_sum, aes(xintercept = mc_nr_qual), 
             colour = "black", linewidth = 1, linetype = 6) +
  scale_fill_manual(values = mycolors) + 
  ggh4x::facet_grid2(species_a ~ species_b, 
                     independent = "y",
                     scales = "free_y") + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_text(colour = "black"), 
    legend.background = element_blank(),
    strip.background = element_blank()
  ) +
  xlim(0,100)+
  labs(x = paste("Overlap Probability (%)", "\u2013", 
                 "Niche Region Size: 95%"), 
       y = "p(Percent Overlap | X)")


overlap_prop_plot <- ggplot(data = over_stat_df, aes(x = mc_nr_perc)) + 
  geom_density(aes(fill = species_a), alpha = 0.6) + 
  # geom_vline(data = over_sum, aes(xintercept = mean_mc_nr), 
  #            colour = "black", linewidth = 1) +
  # geom_vline(data = over_sum, aes(xintercept = mc_nr_qual), 
  #            colour = "black", linewidth = 1, linetype = 6) +
  scale_fill_manual(values = mycolors) + 
  ggh4x::facet_wrap2( ~ species_b,
                     scales = "free_y") + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_text(colour = "black"), 
    legend.background = element_blank(),
    strip.background = element_blank(), 
    legend.position = "",
  ) +
  xlim(0,100)+
  labs(x = paste("Overlap Probability (%)", "\u2013", 
                 "Niche Region Size: 95%"), 
       y = "p(Percent Overlap | X)")

ggsave(overlap_prop_plot, filename = "output/who_should_make_the_playlist.png", 
       width = 20, height = 15, units = "cm", dpi = 150)

### niche sizes ####

# posterior distribution of niche size by species
niche_size <- sapply(evry_par, function(spec) {
  apply(spec$Sigma, 3, niche.size, alpha = .95)
})

# point estimate and standard error
rbind(est = colMeans(niche_size),
      se = apply(niche_size, 2, sd))

niche_size %>%
  as.data.frame(.) %>%
  pivot_longer(cols = 1:4, names_to = "person", values_to = "niche_size_est") %>%
  ggplot(., )+
  geom_violin(aes(x = person, y = niche_size_est, color = person, fill = person)) +
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
