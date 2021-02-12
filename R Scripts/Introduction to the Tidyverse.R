##Load Library:
library(tidyverse)
##This replaces library(dplyr);library(ggplot2);library(tidyr); and others

##Piping:
### Traditional
me1 <- wake_up(me0, time="8am")
me2 <- open(me1,what="eyes")
me3 <- get_out(me2, of="bed")
me4 <- make(get_out(open(me1,what="eyes"), of="bed"), what="eggs")

### With Pipes
a_new_me <- me %>%
  wake_up(time="8am") %>%
  open(what="eyes") %>%
  get_out(of="bed") %>%
  make(what="eggs")


## Make some data: ----------------------------------

data_wide <- read.table(header=TRUE, text='
 subject sex control cond1 cond2
       1   M     7.9  12.3  10.7
       2   F     6.3  10.6  11.1
       3   F     9.5  13.1  13.8
       4   M    11.5  13.4  12.9
')

##Make this data long:
vignette("pivot")

data_long <- data_wide %>% gather("Treatment","Temp",c(3,4,5));data_long
#data_long <- data_wide %>% pivot_longer(c(3,4,5))

## What is the average control value?
data_wide$control %>% mean()
## whats the average value of each subject, sorted lowest to highest?
data_long %>%
  group_by(subject) %>%
  summarize(meanTemp = mean(Temp)) %>%
  ungroup() %>%
  arrange(meanTemp)

## What is the average Male control value?
data_long %>% 
  group_by(sex) %>%
  summarize(meanTemp = mean(Temp))

data_long %>% filter(sex == "M") %>%
  summarize(meanTemp=mean(Temp))

## What is the difference between the average male control value and 
##   the average female control value?

data_long %>% filter(Treatment == "control") %>%
  group_by(sex) %>%
  summarize(meanTemp = mean(Temp)) %>%
  spread(sex,meanTemp) %>%
  mutate(sexDiff = F - M)
