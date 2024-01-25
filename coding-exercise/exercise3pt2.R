# load packages
library(dslabs)
library(tidyverse)
library(stringr)

# filter to select movies with only one genre
movielens_single_genre <- subset(movielens, str_count(genres, "\\|") == 0)

# filter to select only movies from the 70s
movielens_single_genre70s <- subset(movielens_single_genre, year %in% 1970:1979)

# creating subsets for rating/genres, year/genres, and year/genres/rating.
rating_genres <- movielens_single_genre70s[, c("rating", "genres")]
year_genres <- movielens_single_genre70s[, c("year", "genres")]
year_genres_rating <- movielens_single_genre70s[, c("year", "genres", "rating")]

# plot rating vs genres
p1 <- ggplot(rating_genres, aes(x = rating, y = genres))+geom_point()+
  ggtitle('Rating vs Genres [70s]')+
  theme_classic()+
  xlab('Rating')+
  ylab('Genres')+
  theme(plot.caption = element_text(hjust=0, face="bold"))
  plot(p1)
  
# plot year vs genres
p2 <- ggplot(year_genres, aes(x = year, y = genres))+geom_point()+
  ggtitle('Year vs Genres [70s]')+
  theme_classic()+
  xlab('Year')+
  ylab('Genres')+
  theme(plot.caption = element_text(hjust=0, face="bold"))
  plot(p2)
  
# make genres a factor (for color coding) 
year_genres_rating$genres <- factor(year_genres_rating$genres)

# plot rating vs year, with data points color coded by genre  
p3 <- ggplot(year_genres_rating, aes(x = rating, y = year, color=genres))+geom_point()+
  ggtitle('Rating vs Years [70s]')+
  theme_classic()+
  xlab('Rating')+
  ylab('Year')+
  labs(caption = "Figure 3: poo pee poo pee")+
  theme(plot.caption = element_text(hjust=0, face="bold"))
  plot(p3)
    
  # simple model fits for rating/genres and year/genres
  fit1 <- lm(rating ~ genres, data = movielens_single_genre70s)
  fit2 <- lm(year ~ genres, data = movielens_single_genre70s)
  
  # stats summary for fits 1 and 2
  summary(fit1)
  summary(fit2)

  # fit 2 [year/genres] had a lower p-value, and thus is the better fit

