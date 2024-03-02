# loading required libraries
library(spotifyr)
library(dplyr)
library(progress)

# setting Spotify API credentials
Sys.setenv(SPOTIFY_CLIENT_ID = "123db3a906ca4840b3db16e00aa1bc36")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "1372676faad346da874139f116797bf7")
access_token <- get_spotify_access_token()

# loading data, removing columns, and renaming variables
data <- read.csv("./data/raw-data/Hot_100.csv") %>%
  select(-chart_debut, -chart_url, -song_id)
data <- data %>% rename(artist = performer)

# Add a column for spotify_id if not exists
if (!"spotify_song_id" %in% colnames(data)) {
  data$spotify_song_id <- NA
}


fetch_store_song_ids <- function(df) {
  # Ensure access token is refreshed
  access_token <- get_spotify_access_token()
  
  # Initialize progress bar
  pb <- progress_bar$new(total = min(nrow(df), 5))  # Adjusted to process only first 5 rows
  
  # Iterate through the first five rows of the dataset
  for (i in 1:min(nrow(df), 5)) {  # Adjusted to process only first 5 rows
    # Clean artist and song names by removing leading and trailing whitespace
    artist <- trimws(df$artist[i])
    song <- trimws(df$song[i])
    
    # Construct the query string
    query <- paste(artist, song)
    
    # Debugging: Print out the query being sent to Spotify
    cat("Query:", query, "\n")
    
    # Search for the song on Spotify using artist and song names directly from the dataset
    search_result <- search_spotify(q = query, type = "track")
    
    # Debugging: Print out the search result
    print(search_result)
    
    ssi <- search_result$id[1]
    
    df$spotify_song_id[i] <- ssi 
    
    # Update progress bar
    pb$tick()
  }
  
  return(df)
}


data <- fetch_store_song_ids(data)
