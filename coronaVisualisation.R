######## data science in r ########
##### INTRO
# create an object
x <- 15 # number
y <- c(13,12,33) #vector
z <- c("a","b","c")
# create a data frame
df <- data.frame(numbers=x,let =z)
df
# create a list
list1 <- list(data=df,x=x,y=y)
list1

#### Data Manipulation and visualisation #####
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("ggplot")
library(tidyr)   #cheatsheet: https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
library(dplyr)
library(ggplot2)  #cheatsheet: https://rstudio.com/wp-content%2Fuploads%2F2015%2F03%2Fggplot2-cheatsheet.pdf%2F

# load dataset
covidData <- read.csv("data/covid_data.csv")

# view dataset
#View(covidData)
head(covidData)
summary(covidData)

# make Date column Date type
covidData$Date <- as.Date(covidData$Date)

# plot total number of confirmed cases

plot(covidData$Date,covidData$Confirmed,type="l")
ggplot(data=covidData, aes(x=Date, y=Confirmed)) + geom_line(aes(color = "red"), size = 1) + theme_minimal()

# filter data only for austria
covidDataAustria <- covidData %>% filter(Country.Region=="Austria")
covidDataAustria <- covidDataAustria %>% filter(Confirmed>1)

#plot number of confirmed cases
ggplot(data=covidDataAustria, aes(x=Date, y=Confirmed)) + geom_line() + theme_minimal()

#plot number of death cases
ggplot(data=covidDataAustria, aes(x=Date, y=deaths)) + geom_line() + theme_minimal()

#plot number of recovered cases in austria
ggplot(data=covidDataAustria, aes(x=Date, y=recovered)) + geom_line() + theme_minimal()


## Now lets plot confirmed, deaths and recoverd together in one plot! - But How?
# Data manipulation! 
# with tidyr::gather
covidDataAustria <- covidDataAustria %>% gather(case, number, c("Confirmed","deaths","recovered"))

# now plot it - try to log() the number
ggplot(data=covidDataAustria, aes(x=Date, y=number,  color=case)) + geom_line() + theme_minimal()
ggplot(data=covidDataAustria, aes(x=Date, y=log(number), color=case)) + geom_line() + theme_minimal()


###### let's now plot 3 different countries against each other #####
# select your countries
countries <- c("Austria","Germany","Italy")

covidDataCountries <- covidData %>% filter(Country.Region %in% countries)
head(covidDataCountries)

# plot the confirmed cases for these countries in one plot
ggplot(data=covidDataCountries, aes(x=Date, y=Confirmed, color=Country.Region)) + geom_line() + theme_minimal()



########## simple machine learning task #############
#### trying to predict the future

# load library
library(dplyr)

# read data
covidData <- read.csv("data/covid_data.csv")

# data cleaning
covidAustria <- covidData %>% 
    select(Country.Region,Confirmed) %>% # only select needed columns
    filter(Country.Region=="Austria", Confirmed >0) # filter only austria and values bigger than 1

head(covidAustria)

# Make the date column numeric 
covidAustria$Date <- 1:nrow(covidAustria)

# split into train and test
trainInd <- round(nrow(covidAustria)*0.85)
train <-covidAustria[0:trainInd, ]
test <-covidAustria[trainInd+1:nrow(covidAustria), ]

# create model with trainingsdata
model = lm(log(Confirmed)~Date + I(Date^2), data=train)
summary(model)

# predict the missing test data
prediction <- exp(predict(model,test))

# create results data table for visualisation
results <- data.frame(actual=test$Confirmed, prediction=prediction)
results$Date <- as.numeric(rownames(results))
results <- covidAustria %>% left_join(results,by = "Date") %>% gather(type, Confirmed, c("Confirmed","prediction"))

# plot the result
ggplot(results, aes(Date,Confirmed,col=type)) + geom_line() + theme_minimal()



