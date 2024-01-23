---
  title: "R Coding Exercise"
---
#author: Elizabeth Hall (erh08525@uga.edu)
  
# load dslabs package
  library(dslabs)
# get help file for gapminder data
  help(gapminder)
# get overview of data structue
  str(gapminder)
# get data summary
  summary(gapminder)
# determine gapminder object type
  class(gapminder)
  
# assign African countries to africadata
# subset(gapminder, - creates a subset of gapminder dataframe
# continent == "Africa") - filters data to only include African countries
  africadata <- subset(gapminder, continent == "Africa")

# review data 
# str(africadata) - outputs info about africadata
# summary(africadata) - outputs stats summary for africadata
  str(africadata)
  summary(africadata)
  
# create new objects

  # infmortality_lifeexpec <-... - assigns results of operation to new variable
  # africadata[, c...] - subsets specific columns from data frame africadata
  # ("infant_mortality", "life_expectancy") - specifies columns
      infmortality_lifeexpec <- africadata[, c("infant_mortality", "life_expectancy")]
  
  # population_lifeexpec <-... - assigns results of operation to new variable
  # africadata[, c...] - subsets specific columns from data frame africadata
  # ("population", "life_expectancy") - specifies columns
      population_lifeexpec <- africadata[, c("population", "life_expectancy")]

# review data
# str(infmortality_lifeexpec) - outputs info about infmortality_lifeexpec
# summary(infmortality_lifeexpec) - outputs stats summary for infmortality_lifeexpec
  str(infmortality_lifeexpec)
  summary(infmortality_lifeexpec)

# review data
# str(population_lifeexpec) - outputs info about population_lifeexpec
# summary(population_lifeexpec) - outputs stats summary for population_lifeexpec
  str(population_lifeexpec)
  summary(population_lifeexpec)
  
# plot data
  # plot(...) - creates plot
  # (infmortality_lifeexpec$infant_mortality,...) - takes infant_mortality data from variable infmortality_lifeexpec
  # (...,infmortality_lifeexpec$life_expectancy) - takes life_expectancy data from variable infmortality_lifeexpec
  # main - title of graph
  # xlab - x axis title
  # ylab - y axis title
    plot(infmortality_lifeexpec$infant_mortality, infmortality_lifeexpec$life_expectancy,
       main = "Life Expectancy vs. Infant Morality",
       xlab = "Infant Mortality",
       ylab = "Life Expectancy")


  # plot(...) - creates plot
  # (log10(population_lifeexpec$population),...) - takes population data from variable population_lifeexpec, log scales data
  # (...,infmortality_lifeexpec$life_expectancy) - takes life_expectancy data from variable infmortality_lifeexpec
  # main - title of graph
  # xlab - x axis title
  # ylab - y axis title
    plot(log10(population_lifeexpec$population), infmortality_lifeexpec$life_expectancy,
       main = "Life Expectancy vs. Population",
       xlab = "Population (logscale)",
       ylab = "Life Expectancy")

# there is a negative correlation between infant mortality and life expectancy.
# there is a positive correlation between population size and life expectancy, as the population grows people live longer.
    

# finding missing data
# missing_data <-... - assigns results of operation to new variable
# unique(africadata$year[is.na(africadata$infant_mortality)]) - retreives uniquie data from african data for year only where na is true
# str(missing_data) - outputs info about missing_data
missing_data <- unique(africadata$year[is.na(africadata$infant_mortality)]) 
str(missing_data)

# single year
# creates a subset of africadata where the year = 2000
africadata_y2000 <- subset(africadata, year == 2000)

# str(africadata_y2000) - outputs info about africadata_y2000
# summary(africadata_y20000) - outputs stats summary for africadata_y2000
str(africadata_y2000)
summary(africadata_y2000)

# plotting
# this section follows above steps for variable creation and graphing but with data from africadata_y2000
infmortality_lifeexpec2000 <- africadata_y2000[, c("infant_mortality", "life_expectancy")]
population_lifeexpec2000 <- africadata_y2000[, c("population", "life_expectancy")]

plot(infmortality_lifeexpec2000$infant_mortality, infmortality_lifeexpec2000$life_expectancy,
     main = "Life Expectancy vs. Infant Morality",
     xlab = "Infant Mortality",
     ylab = "Life Expectancy",
     sub = "Year 2000")

plot(log10(population_lifeexpec2000$population), infmortality_lifeexpec2000$life_expectancy,
     main = "Life Expectancy vs. Population",
     xlab = "Population (logscale)",
     ylab = "Life Expectancy",
     sub = "Year 2000")

# simple model fits
# fits model with infant mortality as predictor pulling data from africadata_y2000
fit1 <- lm(life_expectancy ~ infant_mortality, data = africadata_y2000)
# fits model with population size as predictor pulling data from africadata_y2000
fit2 <- lm(life_expectancy ~ population, data = africadata_y2000)

# summary(fit1) - outputs stats summary for fit 1
# summary(fit2) - outputs stats summary for fit 2
summary(fit1)
summary(fit2)

# based on the p-values for each fit, fit 1 is a better model for the data.
