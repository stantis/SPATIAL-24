# devtools::install_github('charlie86/spotifyr')
library(spotifyr) #note masking of functions by tidyr and geniusr
library(lubridate); library(stringr); library(dplyr); 
library(ggcorrplot); library(viridis); library(factoextra); library(ggplot2)
Sys.setenv(SPOTIFY_CLIENT_ID = 'YOURKEY')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOURKEY')

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

ids <- c(dplyr::filter(chris_playlists, grepl("Your Top Songs",name))$id) #mine are named 'Your Top Songs'

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
  
