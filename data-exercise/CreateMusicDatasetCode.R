# PART 1 - CREATING THE DATASET


# load required packages
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(here)
library(httr)

# set seed for reproducibility 
set.seed(234)

# Set the number of songs and artists
n_songs <- 300
n_artists <- 20

# Create an empty data frame with placeholders for music-related variables
music_data <- data.frame(
SongID = 1:n_songs,
Title = character(n_songs),
Artist = character(n_songs),
Genre = character(n_songs),
BPM = numeric(n_songs),
Danceability = numeric(n_songs),
LengthInSeconds = numeric(n_songs),
ReleaseYear = integer(n_songs),
Rating = numeric(n_songs)
)

# Define a vector of artist names
# I used a random name generator (https://igenerator.net/random-name-generator/artist-name-generator/) to generate names
# I also used this random band name generator (https://rocklou.com/bandnamegenerator)
artist_names <- c("Ophelia Onyx", "Raina Ruby", "Grayson Knight", "Atlas Stonehart", "Indigo Stone",
                  "Jasper Wolf", "Alex Ace", "Gary Joy", "JTK", "Julian Howard", 
                  "LLAP", "Grey Century", "Coven Atmosphere", "Eternal Symphonies", "The Inner Ethers",
                  "The Whirlwind", "H.E.A.V.Y", "Orange Vinyl", "Days Of Cities", "Enterprise")

# Ensure there are exactly 20 names in the vector
if(length(artist_names) != 20) {
  stop("The number of artist names should be exactly 20.")
}
  
# Generating random song titles (probably overkill but I think its fun)
# I used this random word generator (https://randomwordgenerator.com)
generate_random_title <- function() {

  adjectives <- c("Lame", "Towering", "Ambiguous", "Vagabond", "Limping",
                  "Quarrelsome", "Guiltless", "Vengeful", "Quirky", "Chief",
                  "Worried", "Simple", "Redundant", "Charming", "Jobless",
                  "Impossible", "False", "Significant", "Secretive", "Lackadaisical",
                  "Alive", "Disillusioned", "Thin", "Legal", "Unhappy",
                  "Dangerous", "Vivacious", "Important", "Wistful", "Condemned")
  
       nouns <- c("Penalty", "Camera", "Leader", "Membership", "Oven",
                  "Promotion", "Wealth", "Lake", "Drama", "Map",
                  "Cabinet", "Quality", "Television", "Guest", "Bath",
                  "Bonus", "Soup", "Agency", "Insect", "Person",
                  "Student", "Nation", "Manufacturer", "Player", "Year",
                  "Consequence", "Meal", "Presence", "Environment", "Criticism")
  
  random_adjective <- sample(adjectives, 1)
  random_noun <- sample(nouns, 1)
  title <- paste(random_adjective, random_noun, sep = " ")
  return(title)
}

# Generate synthetic data for music-related variables
music_data$Title <- replicate(n_songs, generate_random_title())
music_data$Artist <- sample(artist_names, n_songs, replace = TRUE)
music_data$Genre <- sample(c("Pop", "Rock", "Hip-Hop", "Electronic", "Jazz"), n_songs, replace = TRUE)
music_data$BPM <- round(rnorm(n_songs, mean = 120, sd = 20))
music_data$Danceability <- round(pmax(pmin(runif(n_songs, min = 1, max = 10), 10), 1))
music_data$LengthInSeconds <- round(rnorm(n_songs, mean = 240, sd = 30))
music_data$ReleaseYear <- sample(1950:2020, n_songs, replace = TRUE)
music_data$Rating <- round(pmax(pmin(rnorm(n_songs, mean = 3.5, sd = 1), 10), 1))


# PART 2 - INTRODUCTING DEPENDENCIES BETWEEN VARIABLES

# Artists produce songs in a primary genre, but occasionally produce songs from other genres
artists <- paste0("Artist", 1:n_artists)
primary_genres <- sample(c("Pop", "Rock", "Hip-Hop", "Electronic", "Jazz"), n_artists, replace = TRUE)
artist_genre_mapping <- data.frame(Artist = artists, PrimaryGenre = primary_genres)
fraction_other_genres <- 0.2
for (i in 1:n_artists) {
  artist_songs <- music_data$Artist == artists[i]
  primary_genre <- artist_genre_mapping$PrimaryGenre[i]
  if (any(artist_songs)) {
    num_songs_total <- sum(artist_songs)
    num_other_genres <- round(num_songs_total * fraction_other_genres)
    other_indices <- sample(which(artist_songs), num_other_genres)
    music_data$Genre[other_indices] <- sample(setdiff(c("Pop", "Rock", "Hip-Hop", "Electronic", "Jazz"), primary_genre), num_other_genres, replace = TRUE)
    music_data$Genre[setdiff(which(artist_songs), other_indices)] <- primary_genre
  }
}

# Songs in Jazz and Pop genres tend to have higher BPMs
fraction_adjust_bpm_jazz_pop <- 0.8 
jazz_pop_indices <- which(music_data$Genre %in% c("Jazz", "Pop") & runif(n_songs) < fraction_adjust_bpm_jazz_pop)
music_data$BPM[jazz_pop_indices] <- music_data$BPM[jazz_pop_indices] + 20

# Songs in the Rock and Hip-Hop genres tend to have lower BPMs
fraction_adjust_bpm_hip_hop <- 0.8
hip_hop_indices <- which(music_data$Genre %in% c("Hip-Hop") & runif(n_songs) < fraction_adjust_bpm_hip_hop)
music_data$BPM[hip_hop_indices] <- music_data$BPM[hip_hop_indices] - 10
fraction_adjust_bpm_rock <- 0.6
rock_indices <- which(music_data$Genre %in% c("Rock") & runif(n_songs) < fraction_adjust_bpm_rock)
music_data$BPM[rock_indices] <- music_data$BPM[rock_indices] - 10

# Songs with higher danceability tend to have higher ratings
music_data$Rating[music_data$Danceability > 5] <- music_data$Rating[music_data$Danceability > 5] + 3

# Songs in the Rock and Jazz genres tend to be older
fraction_replace_jazz <- 0.7  
fraction_replace_rock <- 0.6  
jazz_indices <- which(music_data$Genre %in% c("Jazz") & runif(n_songs) < fraction_replace_jazz)
rock_indices <- which(music_data$Genre %in% c("Rock") & runif(n_songs) < fraction_replace_rock)
music_data$ReleaseYear[jazz_indices] <- sample(c(1950:1970), length(jazz_indices), replace = TRUE)
music_data$ReleaseYear[rock_indices] <- sample(c(1970:2000), length(rock_indices), replace = TRUE)

# Songs in the Pop and Electronic genres tend to be newer
fraction_replace_electronic <- 0.6  
fraction_replace_pop <- 0.5  
electronic_indices <- which(music_data$Genre %in% c("Electronic") & runif(n_songs) < fraction_replace_electronic)
pop_indices <- which(music_data$Genre %in% c("Pop") & runif(n_songs) < fraction_replace_pop)
music_data$ReleaseYear[electronic_indices] <- sample(c(1990:2010), length(electronic_indices), replace = TRUE)
music_data$ReleaseYear[pop_indices] <- sample(c(2000:2020), length(pop_indices), replace = TRUE)

# Songs between 115-125 BPM get a boost in danceability
bpm_boost_indices <- which(music_data$BPM >= 115 & music_data$BPM <= 125)
music_data$Danceability[bpm_boost_indices] <- music_data$Danceability[bpm_boost_indices] + 3

# Display the first few rows of the synthetic music dataset
head(music_data)



# PART 3 - ANALYZING AND DISPLAYING THE DATA

# Display the distribution of genres in the dataset
genre_distribution <- music_data %>%
  group_by(Genre) %>%
  summarise(Count = n())

# Bar plot for Genre vs BPM
ggplot(music_data, aes(x = Genre, y = BPM, fill = Genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Genre vs BPM in Synthetic Music Dataset",
       x = "Genre",
       y = "BPM") +
  theme_minimal()

# Scatter plot for BPM and Danceability
ggplot(music_data, aes(x = BPM, y = Danceability, color = Genre)) +
  geom_point() +
  labs(title = "Scatter Plot of BPM and Danceability",
       x = "BPM",
       y = "Danceability",
       color = "Genre") +
  theme_minimal()

# Boxplot for Ratings across Genres
ggplot(music_data, aes(x = Genre, y = Rating, fill = Genre)) +
  geom_boxplot() +
  labs(title = "Boxplot of Ratings Across Genres",
       x = "Genre",
       y = "Rating",
       fill = "Genre") +
  theme_minimal()

# Boxplot for Genres across Years
ggplot(music_data, aes(x = Genre, y = ReleaseYear, fill = Genre)) +
  geom_boxplot() +
  labs(title = "Boxplot of Genres Across Years",
       x = "Genre",
       y = "Release Year",
       fill = "Genre") +
  theme_minimal()

# Bar plot for Danceability vs Ratings
ggplot(music_data, aes(x = Danceability, y = Rating, fill = factor(Danceability))) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Graph of Danceability vs Ratings",
       x = "Danceability",
       y = "Rating") +
  theme_minimal()

# Selecting only half of the artists for better visibility
selected_artists <- sample(artist_names, n_artists / 2)

# Bar plot for Selected Artists vs Genre
ggplot(music_data[music_data$Artist %in% selected_artists, ], 
       aes(x = Artist, fill = Genre)) +
  geom_bar(stat = "count") +
  labs(title = "Bar Graph of Selected Artists vs Genre of Songs Produced",
       x = "Artist",
       y = "Count",
       fill = "Genre") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Creating a data frame with top-rated songs
top_rated_songs <- music_data[order(music_data$Rating, decreasing = TRUE), ]

# Selecting the top 10 rated songs (you can adjust the number as needed)
top_rated_songs <- head(top_rated_songs, 10)

# Displaying the table with Song Title, Artist, Genre, and Rating
top_rated_songs_table <- data.frame(Song_Title = top_rated_songs$Title,
                                    Artist = top_rated_songs$Artist,
                                    Genre = top_rated_songs$Genre,
                                    Rating = top_rated_songs$Rating)

print(top_rated_songs_table)

# Fit a linear model for Rating with BPM and Danceability as predictors.
linear_model <- lm(Rating ~ BPM + Danceability, data = music_data)
summary(linear_model)

# Fit a linear model for BPM with Genre as a predictor.
linear_model_rating_interaction <- lm(BPM ~ Genre, data = music_data)
summary(linear_model_rating_interaction)

# Fit a linear model for Release Year with Genre as a predictor.
linear_model_genre_year <- lm(ReleaseYear ~ Genre, data = music_data)
summary(linear_model_genre_year)

