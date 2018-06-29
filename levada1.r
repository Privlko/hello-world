library(tidyverse)
library(here)
library(janitor)
library(ggbeeswarm)
library(ggthemes)
library(lubridate)
library(ggplot2)

school <- read_csv("C:/Users/00015/Desktop/tidytuesday/levada/school_professions.csv")

school <- as.tibble(school)
school
View(school)


school <- school %>%
  gather('may.2004', 'june.2006' ,
         'june.2007', 'july.2010',
         'may.2014', 'may.2015',
         'may.2016', 'may.2018', key = "mnth", value = "portion")



school$mnth <- parse_date(school$mnth, "%B.%Y")


school
school$portion <- parse_vector(school$portion, col_integer(), na = c("<1"))

school

school$occupation <- parse_factor(school$occupation, levels = rev(unique(school$occupation)))

school$point <- as.factor(school$mnth)

school

a <- school %>%
  group_by(occupation) %>%
  filter(point == "2004-05-01") %>%
  summarise(portion = sum(portion, na.rm = TRUE)) %>%
  arrange(desc(portion)) 

a

ggplot(a, aes(x = occupation, 
              y = portion))+
  geom_bar(stat="identity") +
  coord_flip()

ggplot(a, aes(portion, occupation)) +
  geom_point()

View(school)
###




b <- school %>%
  group_by(occupation, point) %>%
  filter(point == "2004-05-01" | point == "2018-05-01") %>%
  summarise(portion = sum(portion, na.rm = TRUE)) %>%
  arrange(desc(portion)) 

b


?geom_text
ggplot(b, aes(portion, occupation)) +
  geom_line(aes(group = occupation))+
  geom_point(aes(color = point), size = 2.5) +
  geom_text(aes(colour=point, label=portion),  
            size=20)


###
lab_right <- 
  b %>%
  group_by(occupation) %>%
  arrange(desc(portion)) %>%
  top_n(1)

lab_right

lab_left <- b %>%
  group_by(occupation) %>%
  arrange(desc(portion)) %>%
  slice(2)

lab_left


ggplot(b, aes(portion, occupation)) +
  geom_line(aes(group = occupation))+
  geom_point(aes(color = point), size = 2.5) +
  geom_text(data = lab_right, 
            aes(colour=point, label=round(portion,0)),  
            size=6, hjust = -.5) +
  geom_text(data = lab_left, 
            aes(colour=point, label=round(portion,0)),  
            size=6, hjust = 1.5) +
  scale_x_continuous(limits = c(-5, +30))


big_diff <- b %>% 
  spread(point, portion) %>% 
  group_by(occupation) %>% 
  mutate(Max = max(`2004-05-01`, `2018-05-01`),
         Min = min(`2004-05-01`, `2018-05-01`),
         Diff = Max / Min - 1) %>% 
  arrange(desc(Diff)) %>%
  filter(Diff > .2)


lab_right <- filter(lab_right, occupation %in% big_diff$occupation)
lab_left <- filter(lab_left, occupation %in% big_diff$occupation)

highlight <- filter(b, occupation %in% big_diff$occupation)

p <-  ggplot(b, aes(portion, occupation)) +
  geom_line(aes(group = occupation),
            alpha=0.2)+
  geom_point(aes(color = point), size = 2.5) +
  geom_text(data = lab_right, 
            aes(colour=point, label=round(portion,0)),  
            size=3, hjust = -1.5) +
  geom_text(data = lab_left, 
            aes(colour=point, label=round(portion,0)),  
            size=3, hjust = 2) +
  scale_x_continuous(limits = c(-5, +30))


my_title <- "Parents have become less interested in kids becoming lawyers, \nand more interested in kids becoming soldiers"
my_subtitle <- "When asking parents, which profession they would like for their children, law, medicine, \nand business remain the top industries. Support for military careers has also grown."
my_caption <- "Source: Levada Centre \nPlot: @privlko"


windowsFonts()
windowsFonts(h1 = windowsFont("Helvetica"))

p+scale_color_discrete(labels = c("2004", "2018")) +
  scale_y_discrete(expand = c(.02, 0))+
  labs(title= my_title,
       subtitle= my_subtitle,
       caption=my_caption,
       x = "Portion of parents choosing the occupation",
       y = "Occupations and profession") +
  theme_minimal()+
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 17, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0),
        axis.title.y = element_text(size = 8, margin = margin(t = 10), color = "darkslategrey", hjust = 0),
        axis.title.x = element_text(size = 8, margin = margin(t = 10), color = "darkslategrey", hjust = 0),
        axis.text.y = element_text(size = 8, margin = margin(t = 10), color = "darkslategrey", hjust = 1)
        )
  
  
  
  theme_minimal(base_size = 12,
                plot.title=element_text(size = "16")) +
  scale_y_discrete(expand = c(.02, 0)) 

+
   


f
windowsFont(family = "h1")

?windowsFont

ggsave("levada1.jpg")
