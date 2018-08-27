library(tidyverse)


z<- tribble(~rate, ~nationality,
        #--/--,
        15, "Jewish",
        22, "Ukranians",
        27, "Chinese",
        27, "Chechen",
        30, "Central asian",
        33, "Africans",
        43, "Romanis")




ggplot(z,aes(fct_reorder(nationality,rate),rate)) +
  geom_bar(stat="identity")
  


##attitudes towards jews

jews<- tribble(~m, ~aug2010, ~jul2016,
            #--/--,--
            "Accept into family", 2, 6,
            "Accept as a friend", 3, 8,
            "Accept as a neighbour", 8, 13,
            "Accept as a work colleague", 9, 6,
            "Accept as a Russian resident", 27, 32,
            "Allow to live in Russia only temporarily", 17, 11,
            "Would not allow to live in Russia", 17, 15,
            "Don't know", 16, 10)

jews

j_title <- "Attitudes to Jews Improved Somewhat"
j_subtitle <- "Differences are slight but minor improvements \nin attitudes to Jewish can be seen since 2010."
j_caption <- "Source: Levada Centre. Plot:@privlko"

ggplot(jews) +
  geom_point(aes(m,aug2010, colour="2010"))+
  geom_point(aes(m,jul2016, colour="2016"))+
  coord_flip()+
  scale_x_discrete("Measure")+
  scale_y_continuous("Percentage in agreement")+
  labs(title=j_title,
       subtitle=j_subtitle,
       caption=j_caption)



View(jews)

##attitudes towards chinese

chinese<- tribble(~m, ~aug2010, ~jul2016,
               #--/--,--
               "Accept into family", 1, 2,
               "Accept as a friend", 1, 5,
               "Accept as a neighbour", 5, 8,
               "Accept as a work colleague", 5, 4,
               "Accept as a Russian resident", 13, 20,
               "Allow to live in Russia only temporarily", 30, 27,
               "Would not allow to live in Russia", 32, 27,
               "Don't know", 13, 9)

chinese

##plot

c_title <- "Attitudes to Chinese"
c_subtitle <- "Differences are slight, but minor improvements emerge"
c_caption <- "Source: Levada Centre. Plot:@privlko"

ggplot(chinese) +
  geom_point(aes(m,aug2010, colour="2010"))+
  geom_point(aes(m,jul2016, colour="2016"))+
  coord_flip()+
  scale_x_discrete("Measure")+
  scale_y_continuous("Percentage in agreement")+
  labs(title=c_title,
       subtitle=c_subtitle,
       caption=c_caption)


##attitudes towards ukrainians

ukr<- tribble(~m, ~aug2010, ~jul2016,
                  #--/--,--
                  "Accept into family", 5, 6,
                  "Accept as a friend", 4, 6,
                  "Accept as a neighbour", 10, 8,
                  "Accept as a work colleague", 6, 3,
                  "Accept as a Russian resident", 31, 29,
                  "Allow to live in Russia only temporarily", 20, 21,
                  "Would not allow to live in Russia", 13, 22,
                  "Don't know", 12, 7)

ukr

##plot

u_title <- "Attitudes to Ukr"
u_subtitle <- "Differences are slight, but minor improvements emerge"
u_caption <- "Source: Levada Centre. Plot:@privlko"

ggplot(ukr) +
  geom_point(aes(m,aug2010, colour="2010"))+
  geom_point(aes(m,jul2016, colour="2016"))+
  coord_flip()+
  scale_x_discrete("Measure")+
  scale_y_continuous("Percentage in agreement")+
  labs(title=u_title,
       subtitle=u_subtitle,
       caption=u_caption)


####timeline

tl<- tribble(~nationality, ~time, ~m,
              #--/--,--
              "Romani", "01/08/2004", 32,
             "Romani", "01/08/2006", 29,
             "Romani", "01/08/2009", 22,
             "Romani", "01/08/2010", 30,
             "Romani", "01/08/2011", 18,
             "Romani", "01/08/2012", 29,
             "Romani", "01/10/2013", 32,
             "Romani", "01/07/2014", 23,
             "Romani", "01/08/2015", 22,
             "Romani", "01/07/2016", 21,
             "Romani", "01/07/2017", 17,
             "Romani", "01/07/2018", 18,
             "Chinese", "01/08/2004", 39,
             "Chinese", "01/08/2006", 41,
             "Chinese", "01/08/2009", 34,
             "Chinese", "01/08/2010", 36,
             "Chinese", "01/08/2011", 30,
             "Chinese", "01/08/2012", 37,
             "Chinese", "01/08/2013", 45,
             "Chinese", "01/07/2014", 33,
             "Chinese", "01/08/2015", 24,
             "Chinese", "01/07/2016", 24,
             "Chinese", "01/07/2017", 15,
             "Chinese", "01/07/2018", 31,
             "Ukrainian", "01/08/2004", 8,
             "Ukrainian", "01/08/2006", 7,
             "Ukrainian", "01/08/2009", 7,
             "Ukrainian", "01/08/2010", 6,
             "Ukrainian", "01/08/2011", 5,
             "Ukrainian", "01/08/2012", 6,
             "Ukrainian", "01/10/2013", 5,
             "Ukrainian", "01/07/2014", 8,
             "Ukrainian", "01/08/2015", 14,
             "Ukrainian", "01/07/2016", 13,
             "Ukrainian", "01/07/2017", 8,
             "Ukrainian", "01/07/2018", 17,
             "Jewish", "01/08/2004", 15,
             "Jewish", "01/08/2006", 13,
             "Jewish", "01/08/2009", 6,
             "Jewish", "01/08/2010", 11,
             "Jewish", "01/08/2011", 8,
             "Jewish", "01/08/2012", 10,
             "Jewish", "01/10/2013", 8,
             "Jewish", "01/07/2014", 8,
             "Jewish", "01/08/2015", 7,
             "Jewish", "01/07/2016", 6,
             "Jewish", "01/07/2017", 4,
             "Jewish", "01/07/2018", 12,
             "No to migration limits", "01/08/2004", 21,
             "No to migration limits", "01/08/2006", 25,
             "No to migration limits", "01/08/2009", 19,
             "No to migration limits", "01/08/2010", 21,
             "No to migration limits", "01/08/2011", 17,
             "No to migration limits", "01/08/2012", 18,
             "No to migration limits", "01/10/2013", 11,
             "No to migration limits", "01/07/2014", 21,
             "No to migration limits", "01/08/2015", 25,
             "No to migration limits", "01/07/2016", 20,
             "No to migration limits", "01/07/2017", 28,
             "No to migration limits", "01/07/2018", 28)
             
        
tl
tl$time <- parse_date(tl$time, "%d/%m/%Y")

tl %>% 
  count(nationality)

l_nation <- c("Chinese", "Jewish", "Romani", "Ukrainian", "No to migration limits")

tl$nationality <- parse_factor(tl$nationality,levels = l_nation)
tl

tl %>% 
  count(nationality)


ggplot(tl, aes(x=time, y=m, group=nationality))+
  geom_line(aes(colour=nationality))+
  geom_point(aes(colour=nationality))+
  scale_y_continuous("Percent agreeing to limit migration for...")
