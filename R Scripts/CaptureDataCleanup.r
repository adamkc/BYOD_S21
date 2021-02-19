##--------------------------------------
# Childs Meadow Data check and cleanup script
# February 17, 2021
##--------------------------------------

library(tidyverse)

# Get data----------------------------------------------
childs <- read.csv("Original Data/Childs_FewVariables.csv")

head(childs)
str(childs)

childs <- childs %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  mutate(Year = format(Date, "%Y"))

#testcode
oneDate <- "2020-01-05"
twoDate <- "01-05-2020"
threeDate <- "Jan 5, 2020"

as.Date(oneDate)
as.Date(twoDate,format = "%m-%d-%Y")
as.Date(threeDate, format = "%b %d, %Y")

?strptime

# EXPLORATIONS ------------------------------------------------------

#LifeStage by Year Table:
table(childs$Year, childs$LifeStage)

with(childs,table(Year,LifeStage))
childs %>% with(.,table(Year,LifeStage))
#Or:
childs %>% group_by(Year,LifeStage) %>% 
  count() %>%
  spread(LifeStage,n)

#Count of unique pits in each year:
childs %>% group_by(Year) %>%
  summarize(pitCount = n_distinct(PitTagNo_Final))

#Number of unique pits caputured in each survey:

#Is there a size difference between adult male and adult female frogs?
childs %>% 
  group_by(Sex) %>% 
  summarize(meanWeight = mean(Weight_g, na.rm=TRUE))


## Graphing -------------------------------
### First run through power point slides about GGPLOT

#Graph showing size (y) and date (x) of pits captured:
childs %>%
  filter(!is.na(PitTagNo_Final)) %>%
  ggplot(mapping = aes(x=Date, y= SVL_mm)) +
  geom_point(aes(col=LifeStage,shape=Sex)) +
  geom_line(aes(group=PitTagNo_Final)) +
  theme_minimal()

#Size (weight_g(x) and svl_mm (y)) by LifeStage
childs %>%
  ggplot(aes(x=Weight_g,y=SVL_mm,col=LifeStage)) + geom_point()


# Definitely a few life stage labels need to be cleaned up

#LifeStage and Sex by time of year (julian day):

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

#Here's a doozy: Map the 8 most captured frogs in the dataset.
#Tips:
#  Each graph facet is a frog
#  Color each point by year (factor)
#  UTMEasting is x, UTMNorthing is Y


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







