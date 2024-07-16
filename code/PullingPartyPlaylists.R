# devtools::install_github('charlie86/spotifyr')
library(spotifyr) #note masking of functions by tidyr and geniusr
library(lubridate); library(stringr); library(ggpubr); library(dplyr); 
library(ggcorrplot); library(viridis); library(factoextra); library(ggplot2); 
library(tidyr); library(mdatools)

Sys.setenv(SPOTIFY_CLIENT_ID = 'YOURKEY')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOURKEY')

access_token <- get_spotify_access_token()
authorization_code <- get_spotify_authorization_code(scope = scopes()[c(7,8,9,10,14,15)])

# I have a feeling we'll have to pull Spotify playlists to our own user account
# well, we can get 86 spotify-curated playlists
spotify_playlists <- get_user_playlists("Spotify", authorization = authorization_code, limit = 100) #86 playlists total

ids <- c(spotify_playlists$id)

spotify <- get_playlist_audio_features('Spotify', ids, 
                                     authorization = authorization_code[["credentials"]][["access_token"]])

spotify$playlist_name_simp <- str_replace_all(spotify$playlist_name, " ", "")
spotify$playlist_name_simp <- gsub('[[:punct:] ]+','',spotify$playlist_name_simp)
spotify$release_year <- as.integer(substr(spotify$track.album.release_date, start = 1, stop = 4))

# let's only do numeric values for now
variables <- c("danceability", "tempo", "valence", "track.explicit", "track.popularity", 
               "instrumentalness", "acousticness", "liveness", "time_signature",
               "energy", "key", "loudness", "speechiness", "release_year", "playlist_name_simp")

spotifyPCA <- as.data.frame(spotify) %>% 
  dplyr::select(c(variables)) %>% 
  mutate(playlist_name_simp = factor(playlist_name_simp)) %>% 
  drop_na()

idx = seq(1, nrow(spotifyPCA), by = 8)

# split the values
Xc = spotifyPCA[-idx, 1:14] 
cc = spotifyPCA[-idx, 15]

Xt = spotifyPCA[idx, 1:14]
ct = spotifyPCA[idx, 15]

m.all = plsda(Xc, cc, 7, cv = 1)
summary(m.all)
getConfusionMatrix(m.all$calres)
plotPredictions(m.all)