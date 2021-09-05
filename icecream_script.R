# we will install and load our packages----

install.packages("tidyverse")
install.packages("here")
install.packages("ggplot2")
install.packages("janitor")
install.packages("ggbeeswarm")
install.packages("RColorBrewer")

# loading packages 

library("tidyverse")
library("here")
library("ggplot2")
library("janitor")
library("ggbeeswarm")
library("RColorBrewer")

# let's read-in the csv files----
# I converted the files to a .csv

icecream_flavors<- read_csv(here("data","icecream_flavors.csv"))
icecream_sales<- read_csv(here("data","icecream_sales.csv"))
icecream_temp<- read_csv(here("data","icecream_temp.csv"))




# cleaning the data for the flavors file----

head(icecream_flavors)
str(icecream_flavors)


#let's rename de column units sold to units_sold

icecream_flavors <- icecream_flavors %>%
  rename(units_sold =`units sold`)

clean_names(icecream_flavors)


# we see that the week column is numeric, let's change it to string

icecream_flavors$week <- as.factor(icecream_flavors$week)


# Let's check the spelling of the flavors

icecream_flavors %>% distinct(flavor)


# cleaning the data for the sales file----


head(icecream_sales)


#let's check if the sales column contains reasonable data

View(icecream_sales)


#or by sorting the column 

icecream_sales %>% 
  arrange(desc(sales))

# apparently the sales look fair



# cleaning the data for the sales temperature----

glimpse(icecream_temp)

# let's sort it to see if there are missing data

icecream_temp %>% 
  arrange(temperature)


icecream_temp %>% 
  arrange(sales)

View(icecream_temp)

# apparently no missing data and the columns are in right type 



# We have some business questions to answer----

# Q1- What is the most popular flavor of ice cream?----
  
##we assume the units sold column is the number of units sold for
##each item by week. To answer the Q 1, let's calculate the total number of 
## sold units for each flavors


popularfalvor <- icecream_flavors %>%
  na.omit() %>%
  group_by(flavor)%>%
  summarise(total = sum(`units_sold`))%>%
  arrange(desc(total))


View(popularfalvor)
#ANS Lemon is the popular flavor in the year


# Q2- How does temperature affect sales? ----

# we can plot and see
# we see that the data contains 365 observations so let's assume each rows in 
# a day in the year.

glimpse(icecream_temp)

icecream_temp %>%
  ggplot(aes(x = temperature, y = sales, color = temperature))+
  geom_point()+
  geom_smooth()+
  labs(title = "Sales vs Temparature")+
  theme_classic()+
  scale_color_gradient(low="blue", high ="red")

# ANS Q2 : we see a positive correlation between sale and temperature.

# as the Temperature increases so does the sales. But correlation does not 
#imply causation

# let's save the picture with ggsave
ggsave(here("output","sales_vs_temp.png"))




# Q3 -How do weekends and holidays affect sales?----

# for this question, I left out the holidays and I just took in consideration
# the weekends. 
# I made some modifications in the file in Excel
# 1st, I added a column 'days' to determine the name of the days with the formula
# =text(A2,"dddd")
# then I added another column 'week_end' with the formula bellow to check if it
#is weekend or not =if( OR (C2="Saturday", C2="Sunday"), "Yes", "No")

# now let's read in the file



sales_we <- read_csv(here("data","sales_we.csv"))

glimpse(sales_we)

#let's compare the sales on average for the weekdays and weekends

weekdays_vs_weekends_sales<-sales_we %>%
  na.omit() %>%
  group_by(week_end)%>%
  summarise(average_sales =mean(sales))

View(weekdays_vs_weekends_sales)


#ANS : we can see that the sales are the same whether on weekends or weekdays
  
sales_we %>%
  na.omit() %>%
  group_by(days)%>%
  summarise(average_sales =mean(sales))

# same for the days as well.


# Q4 -How does profitability differ for new versus returning customers?

#For this question we don't have the relevant data. we should let know 
#our Stakeholders of the fact. Had we got the data, we could have proceed with
#the  analysis.



