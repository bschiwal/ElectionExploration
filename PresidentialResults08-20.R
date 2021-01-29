###Load Libraries####
devtools::install_github("UrbanInstitute/urbnmapr")

library("tidyverse")
library("githubinstall")
library("dplyr")
library(urbnmapr)
library(sf)
library(tmap)


##Load Data####
df20 = read.csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv")
#df16 = read.csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2016_US_County_Level_Presidential_Results.csv")
df8.16 = read.csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/US_County_Level_Presidential_Results_08-16.csv")
ecvotes = read.csv("Electoral_College.csv")

##Add Winner to 2020 and 2016 data
#df20win<- data.frame(df20,win="GOP")
#df20win$win[df20win$diff<0] <-"DEM"


#df16win<- data.frame(df16,win="GOP")
#df16win$win[df16win$votes_dem>df16win$votes_gop] <-"DEM"


##Merge 2020 data to 2018-2016 Dataset via FIPS code

dffips <- merge(df20,df8.16, by.x = "county_fips",by.y="fips_code")

#Rename columns 
dffips<-dffips%>%
  rename(gop_2020=votes_gop,dem_2020 = votes_dem,total_2020=total_votes)

#Reduce Data####
dftotal<- data.frame(dffips[1:6],dffips[12:23])

##Calculate and add Add 2020 other column
df20d <- dffips$dem_2020
df20r <- dffips$gop_2020
df20t<- dffips$total_2020
dfoth <- df20t - df20d - df20r
dftotal$oth_2020 <-dfoth

##Clear environment
rm(df20,dffips,df20d,df20r,df20t,dfoth,df8.16)
## add Diff per year####
# fix error in data, Laclede county  Missouri total votes in 2008 incorrect
dftotal[1506,7] = 5218+10875+230

dftotal$diff_2020<- dftotal$gop_2020-dftotal$dem_2020
dftotal$diff_2016<- dftotal$gop_2016-dftotal$dem_2016
dftotal$diff_2012<- dftotal$gop_2012-dftotal$dem_2012
dftotal$diff_2008<- dftotal$gop_2008-dftotal$dem_2008

###add winner per year####
dftotal$win2020<- if_else(dftotal$diff_2020<0,"DEM","GOP")
dftotal$win2016<- if_else(dftotal$diff_2016<0,"DEM","GOP")
dftotal$win2012<- if_else(dftotal$diff_2012<0,"DEM","GOP")
dftotal$win2008<- if_else(dftotal$diff_2008<0,"DEM","GOP")

###add Percent Win per Year
dftotal$win2020_pct<- dftotal$diff_2020/dftotal$total_2020
dftotal$win2016_pct<- dftotal$diff_2016/dftotal$total_2016
dftotal$win2012_pct<- dftotal$diff_2012/dftotal$total_2012
dftotal$win2008_pct<- dftotal$diff_2008/dftotal$total_2008

##Load County Data
county.fips <- maps::county.fips
load(url("https://github.com/mgimond/ES218/blob/gh-pages/Data/counties48.RData?raw=true"))
cnty2<- cnty%>%
  left_join(county.fips,by=c("ID"="polyname"))


head(dftotalmap)
##Build County Map Data Sets
dftotalmap<-dftotal%>%
  select(county_fips,win2020_pct)
cnty2.df1<- cnty2%>%
  left_join(dftotalmap, by=c("fips"= "county_fips"))

ecvoterange<-ecvotes%>%
  filter(Year>=2008)%>%
  spread(Year,Votes)%>%
  rename(ec_2020="2020",
         ec_2016="2016",
         ec_2012="2012",
         ec_2008="2008")



###County Vote Margin PCt
tm_shape(cnty2.df1) + tm_fill(col = "win2020_pct", palette = "Reds") +
  tm_legend(outside = TRUE) 

###County Margin County Vote Margin
ggplot(cnty2.df1) + geom_sf(aes(fill=diff_2020))+
  scale_fill_stepsn(colors=c("#2b2b87","#551199","#ff3d40"),
  breaks=c(mn1,qnt1,0,stdv,mx1)                    )+labs(title = "County Vote Margin")
mx1<-max(dftotalmap$diff_2020)
mn1<-min(dftotalmap$diff_2020)
mx2<-quantile(dftotal$diff_2020)
stdv<-round(sd(dftotal$diff_2020),0)
qnt1<-0-stdv

ggplot(cnty2.df1) + geom_sf(aes(fill=win2020_pct))+
  scale_fill_stepsn(colors=policolor,
  breaks=c(-.75,-.25,0,.25,.75))+labs(title = "County Vote Margin")

qnt1cnty2.df1
summary(cnty2.df1)

policolor<-c("blue","cadetblue1","#0000FF","#F6BDC0","#DC1C13")
brk<-c(qt.values)

ggplot(cnty2.df1) + geom_sf(aes(fill=diff_2020))+
  scale_fill_stepsn(colors=policolor,
  breaks=qt1)+labs(title = "County Vote Margin")

qt<- as.vector(quantile(cnty2.df1$diff_2020, na.rm=TRUE))
qt1<- c(qt[1],0,qt[2:5])

dftotstate<- dftotal%>%
  group_by(state_name)%>%
  summarise_at(vars(total_2020,total_2016,total_2012,total_2008),list(name=sum))
dftotstate

plot(cnty2.df1$diff_2020,cnty2.df1$win2020_pct)
head(ecvotes)

head(ecvoterange)

head(dftotstate)
dftotstate<-rename(dftotstate,st_total_2020=total_2020_name,
       st_total_2016=total_2016_name,
       st_total_2012=total_2012_name,
       st_total_2008=total_2008_name
       )

cnty2.df2<-cnty2.df1%>%
    left_join(ecvoterange,by=c("state_name"="State"))%>%
    left_join(dftotstate, by="state_name")
head(cnty2.df2)

cnty2.df3<- cnty2.df2%>%
  mutate(ec_factor_2020= (diff_2020/st_total_2020)*ec_2020 )%>%
  filter(state_name!="DC")
head(cnty2.df3)
summary(cnty2.df3$ec_factor_2020)

qt<- as.vector(quantile(cnty2.df3$ec_factor_2020, na.rm=TRUE))
qt1<- c(qt[1],0,qt[2:5])

ggplot(cnty2.df3) + geom_sf(aes(fill=ec_factor_2020))+
  scale_fill_stepsn(colors=policolor,
                    breaks=qt1)+labs(title = "County Vote Margin")

ggplot(cnty2.df3)+
  geom_sf(aes(fill=win2020))+
  scale_color_manual(values =policolor)

cnty2.df3%>%
  plot(x="diff_2020",y="total_2020")
plot(cnty2.df3$total_2020,cnty2.df3$diff_2020,xlab = "Total Votes",ylab = "Total Difference")
