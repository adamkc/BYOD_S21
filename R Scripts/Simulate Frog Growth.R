library(tidyverse)

n=200 #number of frogs
tmax=1500 #number of days of growth
r=0.04 #daily growth rate
r_sd=0.2 #sd of daily growth rate
start=5 #starting weight

pop = matrix(nrow=n,ncol=tmax,data = NA)

pop[,1] <- start

for(i in 2:tmax){
  pop[,i] <- sapply(pop[,i-1],FUN = function(x) {
    x + (r + rnorm(n = 1,mean = 0,sd = r_sd)) #Normal
  })
}

popLong <- pop %>% data.frame() %>%
  mutate(frogId = as.factor(1:n)) %>%
  gather(Day,Length,1:tmax) %>% 
  mutate(Day = rep(1:tmax,each=n),
         JDay = Day %% 365,
         Year = as.factor(Day %/% 365),
         frogYearId = paste0(frogId,Year,sep="_"))

title <- sprintf("Growth: %s ; Growth_SD: %s",r, r_sd)
ggplot(popLong,aes(x=JDay,y=Length,group=frogYearId,col=Year)) +
  geom_line(alpha=.3) +
  ggtitle(title) +
  theme_bw()


popLong %>% filter(JDay == 152) %>%
  #mutate(JDay = as.factor(JDay)) %>%
  ggplot(aes(x=Length,y=Year,height=..density..)) + 
  #geom_density() +
  ggridges::geom_density_ridges() +
  ggtitle(label = title,subtitle = "Only Sample early season (June 1)") + theme_bw()


popLong %>% filter(JDay > 120 & JDay < 280) %>%
  #mutate(JDay = as.factor(JDay)) %>%
  ggplot(aes(x=Length,y=Year,height=..density..)) + 
  #geom_density() +
  ggridges::geom_density_ridges() +
  ggtitle(label = title,subtitle = "Sample all season (May-Oct)") + theme_bw()



##Playing with other Graphs.

popLong %>% filter(JDay == 120) %>%
  ggplot(aes(x=Length)) + geom_histogram() +
  ggtitle(label = title,subtitle = "JDay 120") + theme_bw()


popLong %>% filter(JDay == 120) %>% 
  ggplot(aes(x=Year,y=Length,fill=Year)) + geom_violin() +
  coord_flip()

popLong %>% filter(JDay == 120) %>%
  lm(Length~as.factor(Year),data=.) %>%
  summary() %>% .$adj.r.squared

popLong %>% filter(JDay == 120) %>%
  lm(Length~as.factor(Year),data=.) %>%
  summary() %>% .$adj.r.squared

popLong %>% filter(JDay == 50 | JDay==150 | JDay==350) %>%
  #mutate(JDay = as.factor(JDay)) %>%
  ggplot(aes(x=Length,y=as.factor(JDay),height=..density..)) + 
  #geom_density() +
    ggridges::geom_density_ridges() +
  ggtitle(label = title,subtitle = "JDay 120") + theme_bw()


