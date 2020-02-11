library(forecast)
library(leaps)
library(Amelia)
library(fastDummies)
library(ggplot2)
library(reshape)
library(dplyr)
library(stringi)
library(Hmisc)
library(stringr)
library(magrittr)
library(data.table)
library(gains)
library(caret)
library(ROCR)


library(ggplot2)
ggcorr(movie, label = TRUE, label_round = 2, label_size = 4, size = 3, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))


# Setting working directory programatically
setwd("D:/MSBA/BA with R/project/IMDB")

# Reading dataset
movie<-read.csv("movie_metadata.csv")

#Number of missing values with respect to column
sapply(movie,function(x) sum(is.na(x)))


#Omitting missing values
movie<- na.omit(movie)

movie <- movie[!duplicated(movie), ]

#Double check for missing values
sapply(movie,function(x) sum(is.na(x)))

#All the movie titles have a special character (�) at the end and some have whitespaces, 
#they might be generated during the data collection. Let's remove them.
movie$movie_title <- gsub("�", "", as.character(factor(movie$movie_title)))
str_trim(movie$movie_title, side = "right")

#Each record of genres is combined with a few types, which will cause the difficulty of analyzing.
head(movie$genres)

# create a new data frame
genres.df <- as.data.frame(movie[,c("genres", "imdb_score")])
# separate different genres into new columns
genres.df$Action <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Action") 1 else 0)
genres.df$Adventure <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Adventure") 1 else 0)
genres.df$Animation <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Animation") 1 else 0)
genres.df$Biography <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Biography") 1 else 0)
genres.df$Comedy <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Comedy") 1 else 0)
genres.df$Crime <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Crime") 1 else 0)
genres.df$Documentary <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Documentary") 1 else 0)
genres.df$Drama <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Drama") 1 else 0)
genres.df$Family <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Family") 1 else 0)
genres.df$Fantasy <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Fantasy") 1 else 0)
genres.df$`Film-Noir` <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Film-Noir") 1 else 0)
genres.df$History <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "History") 1 else 0)
genres.df$Horror <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Horror") 1 else 0)
genres.df$Musical <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Musical") 1 else 0)
genres.df$Mystery <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Mystery") 1 else 0)
genres.df$News <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "News") 1 else 0)
genres.df$Romance <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Romance") 1 else 0)
genres.df$`Sci-Fi` <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Sci-Fi") 1 else 0)
genres.df$Short <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Short") 1 else 0)
genres.df$Sport <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Sport") 1 else 0)
genres.df$Thriller <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Thriller") 1 else 0)
genres.df$War <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "War") 1 else 0)
genres.df$Western <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Western") 1 else 0)
# get the mean of imdb score for different genres
means <- rep(0,23)
for (i in 1:23) {
  means[i] <- mean(genres.df$imdb_score[genres.df[i+2]==1])
}
# plot the means
barplot(means, main = "Average imdb scores for different genres")

#There isn't much difference in the averages of imdb score related to different genres, 
#almost all the averages are in the same range of 6~8. So we think the predictor "genres" can be removed 
#because it's not really related to the score.
movie <- subset(movie, select = -c(genres))

# Deal with 0s
# replace NA with column average for facenumber_in_poster
movie$facenumber_in_poster[is.na(movie$facenumber_in_poster)] <- round(mean(movie$facenumber_in_poster, na.rm = TRUE))
# convert 0s into NAs for other predictors
movie[,c(5,6,8,13,24,26)][movie[,c(5,6,8,13,24,26)] == 0] <- NA
# impute missing value with column mean
movie$num_critic_for_reviews[is.na(movie$num_critic_for_reviews)] <- round(mean(movie$num_critic_for_reviews, na.rm = TRUE))
movie$duration[is.na(movie$duration)] <- round(mean(movie$duration, na.rm = TRUE))
movie$director_facebook_likes[is.na(movie$director_facebook_likes)] <- round(mean(movie$director_facebook_likes, na.rm = TRUE))
movie$actor_3_facebook_likes[is.na(movie$actor_3_facebook_likes)] <- round(mean(movie$actor_3_facebook_likes, na.rm = TRUE))
movie$actor_1_facebook_likes[is.na(movie$actor_1_facebook_likes)] <- round(mean(movie$actor_1_facebook_likes, na.rm = TRUE))
movie$cast_total_facebook_likes[is.na(movie$cast_total_facebook_likes)] <- round(mean(movie$cast_total_facebook_likes, na.rm = TRUE))
movie$actor_2_facebook_likes[is.na(movie$actor_2_facebook_likes)] <- round(mean(movie$actor_2_facebook_likes, na.rm = TRUE))
movie$movie_facebook_likes[is.na(movie$movie_facebook_likes)] <- round(mean(movie$movie_facebook_likes, na.rm = TRUE))

#Around 79% movies are from USA, 8% from UK, 13% from other countries. 
#So we group other countries together to make this categorical variable with less levels: USA, UK, Others.
levels(movie$country) <- c(levels(movie$country), "Others")
movie$country[(movie$country != 'USA')&(movie$country != 'UK')] <- 'Others' 
movie$country <- factor(movie$country)
table(movie$country)

movie1 <- movie
movie <- movie1

#movie$gross[movie$gross > 50000000] <- 50000000
#movie$gross[movie$gross < 1000000] <- 1000000

# For decision tree
movie$gross <- movie$gross / 1000000
movie$budget <- movie$budget / 1000000



#movie$binned_gross <- cut(movie$gross, breaks = c(0,100,200,300,400,500,600,700,800,900,1000))

# Removing unwanted columns
movie <- subset(movie, select = -c(color, director_name, actor_2_name, actor_1_name,
                                 movie_title, actor_3_name, plot_keywords,
                                 movie_imdb_link, content_rating,aspect_ratio, language
                                 ))

gc()
# model selection process
search <- regsubsets(gross ~ ., data = movie, nbest = 1, nvmax = 12, method = "exhaustive")
sum <- summary(search)
summary(search)

# show models
sum$which

# show metrics
sum$rsq
sum$adjr2

#par(mfrow=c(1,1))
plot(search, scale="r2")

# Best model according to regsubsets
num_critic_for_reviews + actor_3_facebook_likes + actor_1_facebook_likes +
num_voted_users + cast_total_facebook_likes + num_user_for_reviews + country_USA + actor_2_facebook_likes


describe(movie)


# Creating dummies for categorical variables
movie <- dummy_cols(movie)

sample <- sample.int(n = nrow(movie), size = floor(.70*nrow(movie)), replace = FALSE)
movies.train.df <- movie[sample,]
movies.test.df <- movie[-sample,]


#Running multiple linear regression
model1<-lm(gross ~ num_voted_users, data = movies.train.df)

#reviewing coefficients and variables significant on the basis of p values
summary(model1)

# use predict() to make predictions on a new set. 
model1.lm.pred <- predict(model1, movies.test.df)
summary(model1.lm.pred)

#calculate rmse by hand to show how it works.  this will typically be done using accuracy()
residuals <- movies.test.df$gross - model1.lm.pred
squaredResiduals <- residuals*residuals
df <- data.frame("Predicted" = model1.lm.pred, "Actual" = movies.test.df$gross,
                 "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse1 <- sqrt(mean(df$Squared.Residuals))
print(rmse1)

# use accuracy() from forecast package to compute common accuracy measures including rmse.
accuracy(model1.lm.pred, movies.test.df$gross)

#plot residuals to examine
hist(df$Residual, breaks = 25, xlab = "Residuals", main = "")

###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p1 <- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 

p1


#Running multiple linear regression

model2 <- lm(gross ~ num_critic_for_reviews + num_user_for_reviews, data = movies.train.df)

#reviewing coefficients and variables significant on the basis of p values
summary(model2)

# use predict() to make predictions on a new set. 
model2.lm.pred <- predict(model2, movies.test.df)
summary(model2.lm.pred)

#calculate rmse by hand to show how it works.  this will typically be done using accuracy()
residuals <- movies.test.df$gross - model2.lm.pred
squaredResiduals <- residuals*residuals
df <- data.frame("Predicted" = model2.lm.pred, "Actual" = movies.test.df$gross,
                 "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse2 <- sqrt(mean(df$Squared.Residuals))
print(rmse2)

# use accuracy() from forecast package to compute common accuracy measures including rmse.
accuracy(model2.lm.pred, movies.test.df$gross)

#plot residuals to examine
hist(df$Residual, breaks = 25, xlab = "Residuals", main = "")

###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p2 <- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 

p2


#Running multiple linear regression

model3 <- lm(gross ~ actor_3_facebook_likes + actor_1_facebook_likes +  actor_2_facebook_likes, 
             data = movies.train.df)

#reviewing coefficients and variables significant on the basis of p values
summary(model3)

# use predict() to make predictions on a new set. 
model3.lm.pred <- predict(model3, movies.test.df)
summary(model3.lm.pred)

#calculate rmse by hand to show how it works.  this will typically be done using accuracy()
residuals <- movies.test.df$gross - model3.lm.pred
squaredResiduals <- residuals*residuals
df <- data.frame("Predicted" = model3.lm.pred, "Actual" = movies.test.df$gross,
                 "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse3 <- sqrt(mean(df$Squared.Residuals))
print(rmse3)

# use accuracy() from forecast package to compute common accuracy measures including rmse.
accuracy(model3.lm.pred, movies.test.df$gross)

#plot residuals to examine
hist(df$Residual, breaks = 25, xlab = "Residuals", main = "")

###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p3 <- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 

p3

#Running multiple linear regression

model4 <- lm(gross ~ num_critic_for_reviews + actor_3_facebook_likes + actor_1_facebook_likes +
               num_voted_users + cast_total_facebook_likes + num_user_for_reviews + country_USA + 
               actor_2_facebook_likes, 
             data = movies.train.df)

#reviewing coefficients and variables significant on the basis of p values
summary(model4)

# use predict() to make predictions on a new set. 
model4.lm.pred <- predict(model4, movies.test.df)
summary(model4.lm.pred)

#calculate rmse by hand to show how it works.  this will typically be done using accuracy()
residuals <- movies.test.df$gross - model4.lm.pred
squaredResiduals <- residuals*residuals
df <- data.frame("Predicted" = model4.lm.pred, "Actual" = movies.test.df$gross,
                 "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse4 <- sqrt(mean(df$Squared.Residuals))
print(rmse4)

# use accuracy() from forecast package to compute common accuracy measures including rmse.
accuracy(model4.lm.pred, movies.test.df$gross)

#plot residuals to examine
hist(df$Residual, breaks = 25, xlab = "Residuals", main = "")

###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),]

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p4 <- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 

p4

describe(movie$gross)

