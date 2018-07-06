
# Property price register project ------------------------------------------


# load the packages -------------------------------------------------------
library(dplyr)
library(tidyverse)
library(lubridate)
library(tibbletime)
library(forecast)
library(tseries)
library(ggplot2)
library(scales)



# example -----------------------------------------------------------------

series <- create_series('2013' ~ '2017', 'daily', class = "Date") %>%
  mutate(var = rnorm(1826))

series

qq <- series %>%
  collapse_by("yearly") %>%
  group_by(date) %>%
  summarise(total = n(),
            meanty = mean(var)) 

qq
ggplot(qq, mapping = aes(x= date, y=meanty)) +
  geom_bar()


View(qq)

# load the data -------------------------------------------------------

ppp <-  read_csv("PPR-ALL.csv") %>%
  as.tibble() %>%
  filter(county=="Dublin") 

# think of some levels ----------------------------------------------------

l_notfull <- c("Yes", "No")
l_sansvat <- c("Yes", "No")
l_descript <- c("New Dwelling house /Apartment", 
                "Second-Hand Dwelling house /Apartment")
l_size <- c("greater than or equal to 38 sq metres and less than 125 sq metres",
            "greater than 125 sq metres")


# parse -------------------------------------------------------------------


ppp$date <- dmy(ppp$date) 
ppp$price<- parse_number(ppp$price, locale = locale(grouping_mark = ","))
ppp$notfull <-  parse_factor(ppp$notfull, levels = l_notfull)
ppp$sansvat <- parse_factor(ppp$sansvat, levels = l_sansvat)
ppp$descript <- parse_factor(ppp$descript, levels = l_descript)
ppp$size <- parse_factor(ppp$size, levels = l_size)

ppp
ppp
ppp$descript
ppp$notfull

View(ppp)


# SOME IDEAS WITH DPLYR ---------------------------------------------------

qq <- series %>%
  collapse_by("yearly") %>%
  group_by(date) %>%
  summarise(total = n(),
            meanty = mean(var)) 


time_ppp <- as_tbl_time(ppp, date)
time_ppp
ppp
q

q <- time_ppp %>%
  select(date, price, descript, notfull) %>%
  filter(notfull=="No") %>%
  collapse_by("yearly") %>%
  group_by(date, descript) %>%
  summarise(n = n(),
            meanty = mean(price)) 

q$meanty<- parse_number(q$meanty)
q



# plot --------------------------------------------------------------------
my_title <- "Average House Prices Have Risen Year-On-Year"
my_subtitle <- "Average sales price in the PPR data (Co. Dublin units sold at full price) \nshows a steady rise in prices year on year."
my_caption <- "Source: Property Price Register @privlko"

ggplot(q) +
  geom_bar(stat = "identity",
           position="dodge",
           aes(x=as.factor(date), y=meanty, fill= descript)) +
  coord_flip() +
  scale_x_discrete("Year", 
                   labels=c("2010", "2011", "2012", "2013", "2014", 
                            "2015", "2016", "2017", "2018")) +
  scale_y_continuous("Average Cost",
                     breaks = seq(100000, 700000, by=200000),
                     labels = dollar_format(suffix = "", prefix = "€")) +
  scale_colour_discrete() +
  labs(title= my_title,
       subtitle= my_subtitle,
       caption= my_caption,
       fill= "Sale Type") +
  theme_minimal() +
  scale_fill_discrete(breaks=c("New Dwelling house /Apartment", 
                               "Second-Hand Dwelling house /Apartment", 
                               "NA"),
                      labels=c("New", "Second-Hand", "NA"))

ggsave("ppr_bar.jpg")

??dolar_format
View(q)




# plot2 -------------------------------------------------------------------

my_title1 <- "Volume of sales has remained steady since 2014"
my_subtitle <- "Most of the growth in volume of sales (Co.Dublin units at full price) \nhas happened among new builds. There seems to be a ceiling in raw numbers of sales."
my_caption <- "Source: Property Price Register @privlko"

ggplot(q) +
  geom_bar(stat= "identity",
           position = "dodge",
           aes(x=as.factor(date), y=n, fill=descript))+
  coord_flip() +
  scale_x_discrete("Year", 
                   labels=c("2010", "2011", "2012", "2013", "2014", 
                            "2015", "2016", "2017", "2018")) +
  scale_y_continuous("Number of Sales",
                     breaks = seq(0, 30000, by=5000)) +
  scale_colour_discrete() +
  labs(title= my_title,
       subtitle= my_subtitle,
       caption= my_caption,
       fill= "Sale Type") +
  theme_minimal() +
  scale_fill_discrete(breaks=c("New Dwelling house /Apartment", 
                               "Second-Hand Dwelling house /Apartment", 
                               "NA"),
                      labels=c("New", "Second-Hand", "NA"))



View(q)

qq <- time_ppp %>%
  collapse_by("yearly") %>%
  group_by(date)
  
qq

county_mean <- summarise(by_county, p1 = mean(price, na.rm=TRUE))


county_mean
View(county_mean)
