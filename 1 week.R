# 1 week homework
library(ggplot2)
library(corrplot)
library(reshape2)
library(dplyr)
#open file. Change it to your directory's path
setwd(dir = '/Users/Slava/OneDrive - The University of Chicago/Courses/Winter 20 Regressions/1 week/HW')
treasury = read.csv(file = 'treasury.csv')

#fix date format
treasury$Date <- as.Date(treasury$Date, format='%m/%d/%Y')

#plot The correlation between movements in the 1 month rate and the 3 month rate
#I tried to look if we can see on the graph some trends/correlation
ggplot(treasury) +
  geom_point(aes(Date,X1.Mo)) +
  geom_point(aes(Date,X3.Mo), colour = 'red')

#plot The correlation between movements in the 1 month rate and the 10 year rate
ggplot(treasury) +
  geom_point(aes(Date,X1.Mo)) +
  geom_point(aes(Date,X1.Yr), colour = 'red')

#(ii)Compute the differences in consecutive daily interest rates for each maturity.
treasury_diff <- tail(treasury, -1) - head(treasury, -1)
#fix the missing dates in previous line
treasury_diff$Date <- tail(treasury$Date, -1)

#Compute the pairwise correlation matrix for the changes in the interest rates
corr_table <- round((cor(treasury_diff[,-1], method = "pearson")), 2)
#use the corrplot function in R to make a visual representation of the correlation matrix.
corrplot(corr_table, addCoef.col="grey", method = 'shade')
write.csv(corr_table, 'Correlation table.csv')

#create a scatter plot matrix using the 1-month, 2-month, 3-month, 20-year, and 30-year changes in interest rates
plot(treasury_diff[,c(2,3,4,12,13)])

#----------------------1.3 The AIG Stock Price----------------------------------------
# open file
prices <- read.csv('aig.csv')

#calculate mean of prices
mean(prices$Price)

#Calculate variance
var(prices$Price)

#draw prices
ggplot(prices, aes(x=Time, y = Price))+
  geom_line()

#(ii) add new column with new price and calculate new variance
#E(pricei) = 4 − 0.005 · (Timei − 93500)
prices <- mutate(prices, newprice = (4-0.005*(Time-93500)))
plot(prices$Time,prices$newprice,xlab = 'Time', ylab = 'Price')
var(prices$newprice)


#Find the correlation between price and time (pretending the trading time is a random variable), and use this to fit a regression line to the data.
Lin_regr <- lm(prices$Price ~ prices$Time)
print(summary(Lin_regr))
#Plot the data and your line
ggplot(prices, aes(x=Time,y = Price))+
  geom_point(aes(color = 'red'))+
abline(lm(prices$Price ~ prices$Time))


plot(prices$Time, prices$Price,xlab = 'Time', ylab = 'Price')
abline(Lin_regr)

#(iv) What is the average of the residuals from your fit in (iii)?
mean(Lin_regr[["residuals"]])
sum(Lin_regr[["residuals"]])


#estimate σ2
var(prices$Price)
sum((Lin_regr[["residuals"]])^2)/60

#(vi)
prices <- mutate(prices, boothprice = prices$Price[1])
plot(prices$Time,prices$newprice,xlab = 'Time', ylab = 'Price')
var(prices$boothprice)

diff(prices$newprice)

anova()
