library(tidyverse)
library(here)
library(janitor)
library(ggbeeswarm)
library(ggthemes)
library(lubridate)
library(ggplot2)

r <- read_csv("C:/Users/00015/Desktop/tidytuesday/rugby/i.s.2000.csv", skip = 1) %>%
  select(-X10)

r

col_types = cols(
    For = col_integer(),
    Against = col_integer(),
    Diff = col_integer(),
    Tries = col_integer(),
    Conv = col_integer(),
    Pens = col_integer(),
    Drop = col_integer()
    )


r
  
r$date <- parse_date(r$date, "%d/%m/%Y")



