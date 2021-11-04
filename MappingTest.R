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
 ggplot(shp2)+ geom_sf(aes(fill=Commute))
ggplot(shp2)+geom_sf()


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


###ADD Max Row
fltdem<- cnty2.df4 %>%
  select(diff_2020,total_2020,win2020)%>%
  filter(win2020=="DEM")
plot(fltdem$diff_2020,fltdem$total_2020)

quantile(fltdem$diff_2020, probs=c(1,.75,.5,.25,0))

fltgop<- cnty2.df4 %>%
  select(diff_2020,total_2020,win2020)%>%
  filter(win2020=="GOP")
plot(fltgop$diff_2020,fltgop$total_2020)

quantile(fltgop$diff_2020, probs=c(1,.75,.5,.25,0))
summary(cnty2.dem)

cnty2.max<-c(max(cnty2.df4$dem_2020),0)
cnty2.min<-0
cnty2.dem1<-cnty2.df4$dem_2020
cnty2.gop1<-cnty2.df4$gop_2020
cnty2.dem2<-append(cnty2.dem1,cnty2.max)
cnty2.gop2<-append(cnty2.gop1,cnty2.max)

cnty2.dem3<- normalize(cnty2.dem2,method="range",range=c(0,1))
cnty2.gop3<- normalize(cnty2.gop2,method="range",range=c(0,1))
cnty2.dem<-cnty2.dem3[1:3068]
cnty2.gop<-cnty2.gop3[1:3068]

cnty2.df5<-cnty2.df4%>%
  mutate(gop_2020= cnty2.gop)%>%
  mutate(dem_2020=cnty2.dem)%>%
  mutate(diff_2020=gop_2020-dem_2020)

tst<-cnty2.df5%>%
  filter( diff_2020==0)

ggplot(cnty2.df5)+
  geom_sf(aes(fill=diff_2020_bin))+ scale_fill_gradient2(low="#2832c2", high = "#d21404", mid="#a86cc1", breaks = c(-1,-.66,-.33,0,.33,.66,1))

cnty2.df5<-cnty2.df5%>%
  mutate(diff_2020_bin= case_when(
    diff_2020 >0 &diff_2020<.33~.33,
    diff_2020 >.33 &diff_2020<.66~.66,
    diff_2020 >.66 ~.1,
    diff_2020 <0 &diff_2020>-.33~-.33,
    diff_2020 <.33 &diff_2020>-.66~-.66,
    diff_2020 <.66~-1,
    TRUE ~0
  ))%>%
  mutate(diff_2020_binlab= case_when(
    diff_2020 >0 &diff_2020<.33~"Weak GOP",
    diff_2020 >.33 &diff_2020<.66~"Mid GOP",
    diff_2020 >.66 ~"Strong GOP",
    diff_2020 <0 &diff_2020>-.33~"Weak DEM",
    diff_2020 <.33 &diff_2020>-.66~"Mid DEM",
    diff_2020 <.66~"Strong DEM",
    TRUE ~"None"
  ))

ggplot(cnty2.df5)
  geom_sf(aes(fill=diff_2020_binlab))


cnty2.df5%>%
  filter(win2020=="DEM")%>%
  select(diff_2020)%>%
  quantile(diff_2020,probs=c(0,.25,.5,.75,1))
  quantile(diff_2020)
