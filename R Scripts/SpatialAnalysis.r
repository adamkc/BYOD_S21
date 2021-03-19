#some data:
data <- data.frame(latitude = c(44.5,44.6),
                   longitude = c(-122.5,-122.0),
                   site=c("A","B"))
plot(data)

#make the data spatial:
data.sp <- data %>% 
  sf::st_as_sf(coords=c("longitude","latitude"),
               crs=4326)

# Plot the spatial data
plot(data.sp)

ggplot(data.sp) +
  geom_sf(aes(col=site),size=2) +
  theme_bw()


ggplot(data.sp) +
  geom_sf_text(aes(label=site),hjust=1.2) +
  geom_sf(aes(col=site),size=2) +
  theme_bw()

#I just googled nad83 utm zone 10 (commonly used in California),
# found the EPSG code is 26910, so I can convert easily:

data.sp

data.sp %>% sf::st_transform(crs = 26910 )

#A real example:
#Plot all fires since 2015:
#Data downloaded from: CalFIRE
#Load data:
fires <- sf::read_sf("FIRES/firep19_1.shp")

#Check data:
head(fires)
str(fires)

#Fix, Filter and plot:
fires %>% 
  mutate(year = as.numeric(YEAR_)) %>% 
  filter(year>2015) %>% 
  ggplot() + geom_sf() + theme_bw()

#Add some color:
fires %>% 
  mutate(year = as.numeric(YEAR_)) %>% 
  filter(year>2017) %>% 
  ggplot() + geom_sf(aes(fill=YEAR_),alpha=0.6) + theme_bw()


library(rnaturalearthdata)

cal <- rnaturalearthdata::states50 %>% sf::st_as_sf() %>% filter(name == "California")


fires %>% 
  mutate(year = as.numeric(YEAR_)) %>% 
  filter(year>2017) %>% 
  ggplot() + 
  geom_sf(data=cal) +
  geom_sf(aes(fill=YEAR_),alpha=0.6) + 
  theme_bw()

fires %>% data.frame() %>%
  group_by(YEAR_) %>% 
  summarize(annualTotal = sum(GIS_ACRES,na.rm=TRUE)) %>%
  ggplot(aes(x=as.numeric(YEAR_),y=annualTotal)) + geom_bar(fill="red3",stat="identity")

         