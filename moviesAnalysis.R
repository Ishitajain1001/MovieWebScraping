# Loading tidyverse package to use
library(tidyverse)
library(ggplot2)

# Question: Can we accurately predict a movies' user score given the necessary input?


# Loading csv file that was created for further analysis
movies = read.csv("movies.csv")

# Names have dots which are not easy for R to understand so renamed
colnames(movies)[2] = "User_score"
colnames(movies)[5] = "Runtime"
# Runtime is recorded in minutes

# Previewing a couple rows of data
glimpse(movies)

movies |>
        ggplot(aes(x = Year, y = User_score)) +
        geom_point() +
        labs(title = "User score vs. Year of Movies") + 
        theme_bw() 
cor(movies$User_score, movies$Year)
# Data looks very scattered

movies |>
        ggplot(aes(x = User_score)) +
        geom_histogram()
# User scores are skewed right, thus showing that the median is the reliable center of the data.

movies |>
        ggplot(aes(x = User_score)) +
        geom_boxplot()
# Illustrates similar findings of skewed results and shows no that there are no outliers.

summary(movies$User_score)
# The center of the data is at the median of 83 in the User's scores.

table(movies$Director)

movies |>
        ggplot(aes(x = Director)) +
        geom_bar()
        # ungroup() |>

director_freq = movies |>
        group_by(Director) |>
        mutate(freq = n()) |>
        filter(freq > 3)
# 32 records with Directors appearing more than 3 times
# Max frequency of director name appearances are 5 in this data set

director_freq |>
        ggplot(aes(x = Director)) +
        geom_bar() +
        coord_flip()

director_freq |>
        ggplot(aes(x = Director, y = User_score)) +
        geom_boxplot()+
        coord_flip()
# The Boxplot makes it easier to compare centers since the data is mainly skewed. 
# This shows that Christopher Nolan has the most consistently high scores since 50% of his movies from the sample have scores of 84 or larger.

director_freq |>
        ggplot(aes(x = Runtime, y = User_score)) +
        geom_point(aes(color = Director))

# This illustrates a positive trend in the data in runtime and the user's scores of movies without much of a strong correlation. 
# A lot of the Billy Wilder directed movies seem to have a score of 81 while the rest of the directors have pretty scattered scores.

full = lm(User_score~Runtime + factor(Director) + Year, data = director_freq)
summary(full)
# This demonstrates that Runtime and year is significant as we suspected and the directors: Christopher Nolan, Martin Scorses, and Quentin Tarantio all have some significance in determining the score of the movie.