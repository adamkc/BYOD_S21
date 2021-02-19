##--------------------------------------
# Childs Meadow Data check and cleanup script
# February 17, 2021
##--------------------------------------

library(tidyverse)

# Get data----------------------------------------------
childs <- read.csv("Original Data/Childs_FewVariables.csv") %>%
  mutate(Date=as.Date(Date)) %>%
  mutate(Year = format(Date, "%Y")) ##Check ?strptime for codes


# EXPLORATIONS ------------------------------------------------------

#LifeStage by Year Table:
childs %>% with(.,table(Year,LifeStage))
#or
childs %>% count(LifeStage,Year) %>% spread(LifeStage,n)
#Count of unique pits in each year:
childs %>%
group_by(SurveyYear) %>%
summarize(uniquePits = n_distinct(PitTagNo_Final))

#Number of unique pits caputured in each survey:
childs %>%
group_by(Date) %>%
summarize(uniquePits = n_distinct(PitTagNo_Final)) %>%
View

## Graphing -------------------------------
#Graph showing size (y) and date (x) of pits captured:
childs %>% filter(PitTagNo_Final != "") %>%
filter(SurveyYear >= 2015) %>%
ggplot(aes(x=Date,y=SVL_mm,group=PitTagNo_Final,col=LifeStage)) +
  geom_point() + geom_line()
#Size (weight_g(x) and svl_mm (y)) by LifeStage
childs %>%
mutate(hasPit = !is.na(PitTagNo_Final)) %>%
ggplot(aes(x=SVL_mm,y=Weight_g,col=LifeStage)) +
geom_point() + facet_grid(hasPit~.)
# Definitely a few life stage labels need to be cleaned up

#LifeStage and Sex by time of year (julian day):
childs %>%
mutate(JDay = format(Date,"%j")%>%
         as.numeric()) %>%
ggplot(aes(x=JDay,y=SVL_mm,col=LifeStage)) + geom_point(alpha=.6)
#NA in both Sex and LifeStage with a bunch of "U" and "M" frogs

#Do frogs change sex?
childs %>%
filter(Sex %in% c("M","F","U")) %>%
filter(!is.na(PitTagNo_Final)) %>%
group_by(PitTagNo_Final) %>%
summarize(tallySex = n_distinct(Sex)) %>%
arrange(desc(tallySex)) %>%
View()
#19 frogs have two sexes listed. We should look individually and pick the most common
#  or the most recent sex. Most will be U->M or U->F conversions. fix the U.

# Data Corrections ------------------------------------------------

## Sex Changes------------------------------------------
#Fix NAs of Sex in a day of Metamorphs:
##Check First:
childs %>% filter(is.na(LifeStage)) %>%
  select(SVL_mm,Weight_g,LifeStage,Sex) %>%
  View
##All but 2 are definite, small frog metamorphs... Change just those:
##Then change:
childs$Sex[is.na(childs$Sex) &
             childs$Weight_g < 2.0] <- "U"


## Life Stage Changes------------------------------------------
#Fix NAs in Life Stage in small frogs:
##Check First: (yes all small frogs...)
childs %>% filter(is.na(LifeStage)) %>%
  select(SVL_mm,Weight_g,LifeStage,Sex) %>%
  View
childs$SVL_mm[is.na(childs$LifeStage)]
##Then change:
childs$LifeStage[is.na(childs$LifeStage) &
                   childs$Weight_g < 2.0] <- "M"


#General life stage rules:
##Males are A after 45mm SVL
##Females are A after 50mm SVL
##No sex to determine for Y and M, all should be "U"
##Metamorphs show up after JDay 175 or later
##Yopys are a narrow band of SVL_mm's that depend on JDay:
##   Early season YOPs are comparible to late season metamorphs.
##   Late season YOPs will be larger than Metamorphs but sometimes not by much.
##I see many Subadults that should probably be converted to YOPs. Those are tricky
##   case by case designations.


### Capture history -------------------------------------------------------


childs %>% filter(!is.na(PitTagNo_Final)) %>%
  filter(Year >=2015) %>%
  group_by(Year,PitTagNo_Final) %>%
  count(PitTagNo_Final) %>%
  ungroup() %>%
  tidyr::complete(Year,PitTagNo_Final,fill=list(n=0)) %>%
  mutate(Year = paste0("X",Year)) %>% 
  spread(Year,n)  %>% 
  tidyr::unite(ch,starts_with("X"),sep="") %>% 
  mutate(ch2 = gsub(pattern="[23456]",replacement = 1,x = ch)) %>% 
  count(ch2) %>% View()


childs  %>% group_by(Year,PitTagNo_Final) %>%
  slice(1) %>% ungroup() %>% 
  add_count(PitTagNo_Final) %>% 
  rename(numberOfYearsCaptured=n) %>% 
  filter(complete.cases(UTMEastingNAD83_Final)) %>% 
  st_as_sf(coords=c(12,13),crs=26910) %>%
  st

childs %>% 
  filter(complete.cases(UTMEastingNAD83_Final)) %>% 
  st_as_sf(coords=c(12,13),crs=26910) %>%
  st_intersects(reach4%>%st_buffer(10)%>%
                  st_transform(crs=26910),sparse=FALSE) %>% plot()

reach4 <- sf::read_sf("C:/Users/adamcummings/Box/Cascades Frog/ChildsMdwFrogsBeaver/GISFiles/Study_Reach4.shp") 

st_write(reach4,"C:/Users/adamcummings/Box/Cascades Frog/ChildsMdwFrogsBeaver/GISFiles/Reach4.kml")

temp %>% filter(Year==2020) %>%
  as_Spatial() %>%
  plotKML::kml(file="test.kml",colour=Year,
               folder.name="2020",balloon=TRUE,overwrite=FALSE,add=TRUE,
               shape="http://maps.google.com/mapfiles/kml/pal2/icon18.png")

