# Run DanceParty.R first

# Visual Exploration ------------------------------------------------------
mycolors <- c("#54086B", "#FF0BAC", "#00BEC5", "#E34234")
names(mycolors) <- levels(everyone$person)

variables <- c("danceability", "tempo", "valence", "track.explicit", "track.popularity", 
               "instrumentalness", "acousticness")


a <- ggplot(data = everyone) + 
  geom_violin(aes(y = danceability, x = person, fill = person), alpha = 0.5) + 
  geom_jitter(aes(y = danceability, x = person, color = person), alpha = 0.5, width = 0.3) + 
  geom_boxplot(aes(y = danceability, x = person, fill = person), width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) + 
  theme_classic() + 
  theme(legend.position = 'none') + 
  labs(x = "", 
       y = "Danceability")

b <- ggplot(data = everyone) + 
  geom_violin(aes(y = tempo, x = person, fill = person), alpha = 0.5) + 
  geom_jitter(aes(y = tempo, x = person, color = person), alpha = 0.5, width = 0.3) + 
  geom_boxplot(aes(y = tempo, x = person, fill = person), width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) + 
  theme_classic() + 
  theme(legend.position = 'none') + 
  labs(x = "", 
       y = "Tempo")

c <- ggplot(data = everyone) + 
  geom_violin(aes(y = valence, x = person, fill = person), alpha = 0.5) + 
  geom_jitter(aes(y = valence, x = person, color = person), alpha = 0.5, width = 0.3) + 
  geom_boxplot(aes(y = valence, x = person, fill = person), width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) + 
  theme_classic() + 
  theme(legend.position = 'none') + 
  labs(x = "", 
       y = "Valence (happiness)")

d <- everyone %>% 
  group_by(person) %>% 
  summarize(value = mean(track.explicit, na.rm=TRUE)*100) %>% 
  ggplot() + 
  geom_col(aes(y = value, x = person, fill = person)) +
  scale_fill_manual(values = mycolors) + 
  labs(x = "", 
       y = "Percent (%) Explicit") + 
  theme_classic() + 
  theme(legend.position = 'none')


e <- ggplot(data = everyone) + 
  geom_violin(aes(y = track.popularity, x = person, fill = person), alpha = 0.5) + 
  geom_jitter(aes(y = track.popularity, x = person, color = person), alpha = 0.5, width = 0.3) + 
  geom_boxplot(aes(y = track.popularity, x = person, fill = person), width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) + 
  theme_classic() + 
  theme(legend.position = 'none') + 
  labs(x = "", 
       y = "Track Popularity")

f <- ggplot(data = everyone) + 
  geom_violin(aes(y = instrumentalness, x = person, fill = person), alpha = 0.5) + 
  geom_jitter(aes(y = instrumentalness, x = person, color = person), alpha = 0.5, width = 0.3) + 
  geom_boxplot(aes(y = instrumentalness, x = person, fill = person), width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) + 
  theme_classic() + 
  theme(legend.position = 'none') + 
  labs(x = "", 
       y = "Instrumentalness")

g <- ggplot(data = everyone) + 
  geom_violin(aes(y = acousticness, x = person, fill = person), alpha = 0.5) + 
  geom_jitter(aes(y = acousticness, x = person, color = person), alpha = 0.5, width = 0.3) + 
  geom_boxplot(aes(y = acousticness, x = person, fill = person), width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) + 
  theme_classic() + 
  theme(legend.position = 'none') + 
  labs(x = "", 
       y = "Acousticness")

h <- ggplot(data = everyone) + 
  geom_violin(aes(y = loudness, x = person, fill = person), alpha = 0.5) + 
  geom_jitter(aes(y = loudness, x = person, color = person), alpha = 0.5, width = 0.3) + 
  geom_boxplot(aes(y = loudness, x = person, fill = person), width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) + 
  theme_classic() + 
  theme(legend.position = 'none') + 
  labs(x = "", 
       y = "Loudness")

i <- ggplot(data = everyone) + 
  geom_violin(aes(y = track.duration_ms/1000, x = person, fill = person), alpha = 0.5) + 
  geom_jitter(aes(y = track.duration_ms/1000, x = person, color = person), alpha = 0.5, width = 0.3) + 
  geom_boxplot(aes(y = track.duration_ms/1000, x = person, fill = person), width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) + 
  theme_classic() + 
  theme(legend.position = 'none') + 
  labs(x = "", 
       y = "Track Duration (s)")

ggarrange(a, b, c, d, e, f, g, h, i, ncol = 3, nrow = 3)
