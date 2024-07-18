
everyone <- readRDS("~/github/SPATIAL-24/input/everyone.rds")

ids <- unique(everyone %>% unnest(track.artists) %>% 
  select(id))

names <- unique(everyone$person)

for (i in names){
  df <- everyone %>% filter(person == i)
  ids <- unique(df %>% unnest(track.artists) %>% 
    select(id))
  dfName <- paste0(i)
  assign(dfName, get_artists(
    ids$id,
    authorization = authorization_code[["credentials"]][["access_token"]]
  ))
  }

artists <- get_artists(
  ids$id,
  authorization = authorization_code[["credentials"]][["access_token"]]
)

test <- everyone %>% 
  group_by(track.id) %>% 
  filter(n()>1) %>% 
  select(track.id, person)

test2 <- test %>%
  pivot_wider(
    names_from = track.id, 
    values_from = person
  )

test3 <- test2 %>% 
  pivot_longer(
    everything(), 
    names_to = 'track', 
    values_to = 'pairs'
  )
test3 <- test3 %>% 
  mutate(pairs = sapply(pairs, toString))
