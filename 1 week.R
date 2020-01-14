# 1 week homework
library(ggplot2)
library(corrplot)
library(reshape2)
#open file. Change it to your directory's path
setwd(dir = '/Users/Slava/OneDrive - The University of Chicago/Courses/Winter 20 Regressions/1 week/HW')
treasury = read.csv(file = 'treasury.csv')

#plot The correlation between movements in the 1 month rate and the 3 month rate
#I tried to look if we can see on the graph some trends/correlation
ggplot(treasury) +
  geom_point(aes(Date,X1.Mo)) +
  geom_point(aes(Date,X3.Mo), colour = 'red', size = 3)

#plot The correlation between movements in the 1 month rate and the 10 year rate
ggplot(treasury) +
  geom_point(aes(Date,X1.Mo)) +
  geom_point(aes(Date,X1.Yr), colour = 'red', size = 3)

#Compute the differences in consecutive daily interest rates for each maturity.
treasury_diff <- tail(treasury, -1) - head(treasury, -1)
#fix the missing dates in previous line
treasury_diff$Date <- tail(treasury$Date, -1)

#Compute the pairwise correlation matrix for the changes in the interest rates
corr_table <- round((cor(treasury[,-1], method = "pearson")), 2)
#use the corrplot function in R to make a visual representation of the correlation matrix.
corrplot(corr_table)




#----------------above is working, below is needed to be fixed-------------------------


#create a scatter plot matrix using the 1-month, 2-month, 3-month, 20-year, and 30-year changes in interest rates
ggplot(treasury_diff[c(2,3,4,12,13),], aes(x = Date, y = value) )+
  geom_line()

test <- melt(treasury_diff, id="Date")  # convert to long format

ggplot(data=test, aes(x=Date, y=value, colour=variable), scale_x_date()) +
  geom_line()