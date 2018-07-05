
# Property price register project ------------------------------------------


# load the packages -------------------------------------------------------
library(dplyr)
library(tidyverse)
library(lubridate)
library(tibbletime)
library(forecast)
library(tseries)



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

q <- time_ppp %>%
  select(date, price, descript, notfull) %>%
  collapse_by("yearly") %>%
  group_by(date, descript, notfull) %>%
  summarise(n = n(),
            meanty = mean(price))

q


ggplot(q) +
  geom_bar(stat = "identity",
           position="dodge",
           aes(x=as.factor(date), y=meanty, fill= descript)) +
  coord_flip() +
  facet_wrap(~ notfull)


ggplot(q) +
  geom_bar(stat= "identity",
           position = "dodge",
           aes(x=as.factor(date), y=n, fill=descript))+
  coord_flip() +
  facet_wrap(~ notfull)


View(q)

qq <- time_ppp %>%
  collapse_by("yearly") %>%
  group_by(date)
  
qq

county_mean <- summarise(by_county, p1 = mean(price, na.rm=TRUE))


county_mean
View(county_mean)
