library(sf)
library(dplyr)
library(ggplot2)
library(tmap)

shp <- readRDS(gzcon(url("https://github.com/mgimond/ES218/blob/gh-pages/Data/maine_tracts.Rds?raw=true")))
dat <- read.csv("http://mgimond.github.io/ES218/Data/maine_commute.csv")

shp2 <- left_join(shp,dat,by="Geo_FIPS")
summary(shp)
summary(dat)
 head(shp)
 head(dat)
 ggplot(shp2) + geom_sf(aes(fill=Commute))
ggplot(shp2)  +geom_sf()


load(url("https://github.com/mgimond/ES218/blob/gh-pages/Data/counties48.RData?raw=true"))

ggplot(cnty) + geom_sf(aes())
head(cnty)
ea <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 
       +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

tm_shape(cnty,projection=ea)+
  tm_polygons() + 
  tm_layout(outer.margins = c(.1,.1,.1,.1))


df <- read.csv("http://mgimond.github.io/ES218/Data/Income_education.csv")
df1 <- df %>% select(subregion = County, region = State, B20004001 )
head(df)
head(df1)
df <- read.csv("http://mgimond.github.io/ES218/Data/Income_education_with_FIPS.csv")

county.fips<-maps::county.fips
head(county.fips)
st <- data.frame(region=tolower(state.name), State = tolower(state.abb)) %>% 
  bind_rows( data.frame(region="district of columbia", State="dc") ) 

library(stringr)
df1 <- df %>% 
  inner_join(st, by="State") %>%
  mutate(ID = paste(region,tolower(County), sep = ","))  %>%
  select(ID, B20004001 )
cnty2<- cnty%>%
  left_join(county.fips,by=c("ID"="polyname"))
head(cnty2)
df1 <- df %>% 
  inner_join(st, by="State") %>%
  mutate(ID = paste(region,tolower(County), sep = ","),
         ID = ifelse(region=="louisiana", 
                     str_replace(ID, " parish", ""), ID))  %>%
  select(ID, B20004001 )
cnty.df1 <- inner_join(cnty, df1, by="ID" )
cnty2.df1 <- inner_join(cnty2, df1, by=c("fips" = "FIPS") )
df <- read.csv("http://mgimond.github.io/ES218/Data/Income_education_with_FIPS.csv")
df1 <- df %>% select(FIPS, B20004001 )

tm_shape(cnty2.df1) + tm_fill(col = "B20004001", palette = "Greens") +
  tm_legend(outside = TRUE) 

head(cnty)
cnty2<- cnty%>%
  left_join(county.fips,by=c("ID"="polyname"))
head(cnty2)


###
df <- read.csv("http://mgimond.github.io/ES218/Data/Income_education_with_FIPS.csv")
df1 <- df %>% select(FIPS, B20004001 )
county.fips <- maps::county.fips
head(county.fips)
cnty2 <- cnty %>%
  left_join(county.fips, by=c("ID" = "polyname"))
cnty2.df1 <- inner_join(cnty2, df1, by=c("fips" = "FIPS") )
head(cnty2.df1)
tm_shape(cnty2.df1) + tm_fill(col = "B20004001", palette = "Greens") +
  tm_legend(outside = TRUE) 
