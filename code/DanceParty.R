# devtools::install_github('charlie86/spotifyr')
library(spotifyr) #note masking of functions by tidyr and geniusr
library(lubridate); library(stringr); library(ggpubr); library(dplyr); 
library(ggcorrplot); library(viridis); library(factoextra); library(ggplot2)
Sys.setenv(SPOTIFY_CLIENT_ID = '4c68298e87354618890421deb9b9600f')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'aedebb1b3d854acca746dd9e6848f4e9')

access_token <- get_spotify_access_token()
authorization_code <- get_spotify_authorization_code(scope = scopes()[c(7,8,9,10,14,15)])

sarah_playlists <- get_user_playlists('1149074328', authorization = authorization_code)
dustin_playlists <- get_user_playlists('dustinthomasharper', authorization = authorization_code)
spencer_playlists <- get_user_playlists('1245476699', authorization = authorization_code)
chris_playlists <- get_user_playlists('12135699044', authorization = authorization_code, limit = 40)

# Sarah's Playlists Pull --------------------------------------------------
ids <- c(sarah_playlists$id) # only her Wrapped portfolios are publicly available

sarah <- get_playlist_audio_features('1149074328', ids, 
                                        authorization = authorization_code[["credentials"]][["access_token"]])
sarah$release_year <- as.integer(substr(sarah$track.album.release_date, start = 1, stop = 4))
sarah$playlist_year <- as.integer(str_sub(sarah$playlist_name, start= -4))
sarah$person <- "Sarah"
sarah <- sarah %>% 
  distinct(track.id, .keep_all = T)

# Dustin's Playlists Pull -------------------------------------------------

ids <- c(dplyr::filter(dustin_playlists, grepl("Tops",name))$id) #his are named 'Tops20XX'

dustin <- get_playlist_audio_features('dustinthomasharper', ids, 
                                     authorization = authorization_code[["credentials"]][["access_token"]])
dustin$release_year <- as.integer(substr(dustin$track.album.release_date, start = 1, stop = 4))
dustin$playlist_year <- as.integer(str_sub(dustin$playlist_name, start= -4))
dustin$person <- "Dustin"
dustin <- dustin %>% 
  distinct(track.id, .keep_all = T)

# Chris' Playlists Pull ---------------------------------------------------

ids <- c(dplyr::filter(chris_playlists, grepl("Your Top Songs",name))$id) #his are named 'Tops20XX'

chris <- get_playlist_audio_features('12135699044', ids, 
                                      authorization = authorization_code[["credentials"]][["access_token"]])
chris$release_year <- as.integer(substr(chris$track.album.release_date, start = 1, stop = 4))
chris$playlist_year <- as.integer(str_sub(chris$playlist_name, start= -4))
chris$person <- "Chris"
chris <- chris %>% 
  distinct(track.id, .keep_all = T)

# Spencer's Playlists Pull ------------------------------------------------

ids <- c(dplyr::filter(spencer_playlists, grepl("Spencers",name))$id) #his are named 'Spencers Top Songs 20XX'

spencer <- get_playlist_audio_features('1245476699', ids, 
                                     authorization = authorization_code[["credentials"]][["access_token"]])
spencer$release_year <- as.integer(substr(spencer$track.album.release_date, start = 1, stop = 4))
spencer$playlist_year <- as.integer(str_sub(spencer$playlist_name, start= -4))
spencer$person <- "Spencer"
spencer <- spencer %>% 
  distinct(track.id, .keep_all = T)

# Put it all together -----------------------------------------------------

everyone <- rbind(sarah, dustin, chris, spencer)
  
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
