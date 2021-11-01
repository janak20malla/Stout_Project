
#The Dataset has 10,000 observations and 55 variables. There are different types
#The problem with dataset is that it has a lot of missing values that can
# greatly affect the results.

library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(hrbrthemes)
library(plotrix)
#Select the file from downloaded directory.
loans_full_schema = read.csv(file = file.choose())

newData = loans_full_schema %>% select(loan_amount, emp_length, interest_rate, term, grade, state, annual_income, homeownership, debt_to_income )

#Removing the rows with na values.
newData = newData %>% filter_all(~ !is.na(.))


#Subsetting the Data with columns I want to use for visuals.


#                         Unique Visualizations
#1. Stacked bar chart (x = states, y = annual income , value= home ownership)

stackedBar <- ggplot(newData, aes(x=state,y = annual_income, fill=homeownership)) + geom_bar(stat = "identity") + ggtitle("Homeownership in different states by annual income")
stackedBar <- stackedBar + theme_bw() + xlab("States") + ylab("Annual Income") + labs(fill = "homeownership") + theme(plot.title = element_text (hjust = 0.5)) + theme(axis.line= element_line(colour = "black"), panel.border = element_blank(), panel.grid.minor = element_blank())         
ggplotly(stackedBar)

#2. Scatterplot
p3 <- ggplot(newData, aes(x=interest_rate, y=annual_income)) + geom_point() + ggtitle("Interest Rate by Anuual Income")
p3 <- p3 + theme_bw() +  xlab("Interest Rate") + ylab("Annual Income") + theme(plot.title = element_text (hjust = 0.5)) + theme(axis.line= element_line(colour = "black"), panel.border = element_blank(), panel.grid.minor = element_blank())         
p3

#3Pie Chart
pie(table(newData$state), labels = names(table(newData$state)) , values = frequency,  main = "States") 
      
#4BoxPlot
boxPlot <- ggplot(newData, aes(x = interest_rate, y= grade)) + geom_boxplot() + theme_bw() + xlab("Interest Rate(%)") + ylab("Grade") + ggtitle("Interest rate by grade") + theme(plot.title = element_text (hjust = 0.5)) + scale_color_discrete(name = "Homeownership")  
boxPlot <- boxPlot + theme(axis.line= element_line(colour = "black"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
boxPlot

#5. Histogram
ggplot(newData, aes(x = loan_amount, fill = homeownership)) + 
  geom_histogram(binwidth = 1000) +
  labs(
    x = "Loan amount ($)",
    y = "Frequency",
    title = "Amounts of Lending Club loans"
  ) +
  facet_wrap(~ homeownership, nrow = 3)


#Predicting Interest Rate
plot(newData$annual_income, newData$interest_rate)
model1 = lm(interest_rate ~ annual_income, data = newData)
abline(model1)

