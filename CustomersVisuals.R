#The dataset contains around 686,000 observations and 4 variables.

library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(hrbrthemes)
library(plotrix)
#Select the file from downloaded directory.
casestudy = read.csv(file = file.choose())
casestudy = casestudy %>% filter_all(~ !is.na(.))

#Calculating when coustomer first started.
casestudy <- casestudy %>%
  group_by(`customer_email`)%>%
  mutate(start_year=min(`year`))%>%
  ungroup()

#Calculating if the customer eneded of is still present
casestudy <- casestudy %>%
  group_by(`customer_email`)%>%
  mutate(presentuntil_year=max(`year`))%>%
  ungroup()
#Filtering Customer for each year

# ''''t = Total, nc = new customer , ec = exisitng customer, Rev = Revenue'''

# In year 2015
year2015 = filter(casestudy, year == "2015")
tRev2015 = sum(year2015$net_revenue)
ncRev = tRev2015
ecGrowth = tRev2015 - ncRev
ecRev = tRev2015 
ecRevPr = 0
newCustNum = length(year2015$X)
totCustNum = length(year2015$X)
lostCustdf2015 = filter(casestudy, presentuntil_year == '2015')
losCustNum = length(lostCustdf2015$X)

#In year 2016
year2016 = filter(casestudy, year == "2016")
tRev2016 = sum(year2016$net_revenue)
newcustomer_2016 = filter(casestudy, start_year == "2016")
ncRev1 = sum(newcustomer_2016$net_revenue)
ecGrowth1 = tRev2016 - ncRev1
ecRev1 = tRev2016 - ncRev1
ecRevPr1 = tRev2015
newCustNum1 = length(newcustomer_2016$X)
totCustNum1 = length(year2016$X + totCustNum)
lostCustdf2016 = filter(casestudy, presentuntil_year == '2016')
losCustNum1 = length(lostCustdf2016$X)

#In year 2017
year2017 = filter(casestudy, year == "2017")
tRev2017 = sum(year2017$net_revenue)
newcustomer_2017 = filter(casestudy, start_year=="2017")
ncRev2 = sum(newcustomer_2017$net_revenue)
ecGrowth2 = tRev2017 - ncRev2
ecRev2 = tRev2017 - ncRev2
ecRevPr2 = tRev2016
newCustNum2 = length(newcustomer_2017$X)
totCustNum2 = length(year2017$X + totCustNum1)
lostCustdf2017 = filter(casestudy, presentuntil_year == '2017')
losCustNum2 = length(lostCustdf2017$X)

#all lost customer over the years 

lostCust = c(losCustNum, losCustNum1, losCustNum2)
newCust = c(newCustNum, newCustNum1, newCustNum2)
totalRev = c(tRev2015,tRev2016, tRev2017)
year = c(2015, 2016, 2017)
df = data.frame(lostCust, newCust, totalRev, year)


#Visualizations. 

#Most numbers of customers were lost in 2017 and least in 2015
lostBar <- ggplot(df, aes(x = year, y= lostCust)) + geom_bar(stat = "identity") + theme_bw() + xlab("Year") + ylab("Numbers") + ggtitle("Customers lost over the years") + theme(plot.title = element_text (hjust = 0.5))
lostBar <- lostBar + theme(axis.line= element_line(colour = "black"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
lostBar

#Most new customers of joined in  2015 and least in 2016
newBar <- ggplot(df, aes(x = year, y= newCust)) + geom_bar(stat = "identity") + theme_bw() + xlab("Year") + ylab("Numbers") + ggtitle("New customers over the years") + theme(plot.title = element_text (hjust = 0.5))
newBar <- newBar + theme(axis.line= element_line(colour = "black"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
newBar

#Most revenue was made in 2017 and least in 2016
revBar <- ggplot(df, aes(x = year, y= totalRev)) + geom_bar(stat = "identity") + theme_bw() + xlab("Year") + ylab("Revenue") + ggtitle("Total Revenue over the years") + theme(plot.title = element_text (hjust = 0.5))
revBar <- revBar + theme(axis.line= element_line(colour = "black"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
revBar



