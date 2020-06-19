# Libraries
library(dplyr)
library(googleVis)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(ggsignif)
library(GGally)
library(maps)
library(leaflet)
library(htmltools)
library(forcats)
#library(reshape)


global_covid=read.csv('COVID_19_Global.csv',stringsAsFactors = FALSE)
usa_covid=global_covid %>% filter(.,Country.Region=='US')
usa_covid$Last.Update=as.Date(usa_covid$Last.Update)
usa_covid=usa_covid %>% filter(.,Last.Update >= '2020-03-10') %>% 
                        filter(.,!Province.State %in% c('Guam','American Samoa',
                                                        'Grand Princess','Puerto Rico',
                                                        'Diamond Princess',"Wuhan Evacuee",
                                                        "United States Virgin Islands",
                                                        "Virgin Islands","Virgin Islands, U.S.") )
# # remove dates 4/22,4/24,4/29 from NY
# usa_covid=usa_covid %>% 
state.choice=pull(usa_covid %>% group_by(.,Province.State) %>% group_keys())


 us_covid_nyt=read.csv('us-counties.csv',stringsAsFactors = FALSE)
 us_covid_nyt$date=as.Date(us_covid_nyt$date)
 us_covid_nyt=us_covid_nyt %>% rename(.,Last.Update=date,
                                         Province.State=state,
                                         Confirmed.Cases=cases,
                                         Deaths=deaths)
 us_covid_nyt=us_covid_nyt %>% filter(.,Last.Update >= '2020-03-10')
 #nyc_covid_nyt=us_covid_nyt %>% filter(.,Province.State=='New York')
# 
# usa_covid=usa_covid %>% left_join(ny_covid_nyt,by='Province.State')




# # work on time series per state
# 
# state.daily.cases= usa_covid %>% filter(.,Province.State=='New York') %>%
#   group_by(.,Last.Update) %>%
#   summarise(.,Total.Cases=sum(Confirmed.Cases),
#             Total.Deaths=sum(Deaths)) %>%
#   mutate(.,New.Cases=Total.Cases-lag(Total.Cases),
#            New.Deaths=Total.Deaths-lag(Total.Deaths))
#   # mutate(.,three_day_index=1:nrow(state.daily.cases)%/%3) %>%
#   # group_by(.,Last.Update,three_day_index) %>%
#   # summarise(.,Total.Cases=mean(Total.Cases),Total.Deaths=mean(Total.Deaths))
# 
# 
#   state.daily.cases=state.daily.cases %>% filter(.,!Last.Update %in% as.Date(c(
#     '2020-04-22','2020-04-24','2020-04-29')))


# ### Test state.daily.cases------------------------------------------------
# 
# state.daily.cases= usa_covid %>% filter(.,Province.State=='Alabama') %>%
#   group_by(.,Last.Update) %>%
#   summarise(.,Total.Cases=sum(Confirmed.Cases),
#             Total.Deaths=sum(Deaths)) %>%
#   mutate(.,New.Cases=Total.Cases-lag(Total.Cases)) %>%
#   filter(.,New.Cases>=0)
#   # group_by(.,Last.Update) %>%
#   # summarise(.,Total.Cases=sum(Confirmed.Cases),
#   #           Total.Deaths=sum(Deaths)) %>%
#   # mutate(.,New.Cases=Total.Cases-lag(Total.Cases)) %>%
#   # filter(.,New.Cases>=0) %>%
#   # mutate(.,three_day_index=1:nrow(.)%/%3) %>%
#   # group_by(.,three_day_index) %>%
#   # summarise(.,Last.Update=max(Last.Update),New.Cases=mean(New.Cases))
# #gather(.,Number.Type,Count,Total.Cases:Total.Deaths) %>%
# #gather(.,New.Type,New.Count,New.Cases:New.Deaths)
# 
# ###-----------------------------------------------------------------------
  

# covid new cases in NYC-----------------------------------------------------
nyc_covid=us_covid_nyt %>% filter(.,Province.State=='New York',county=='New York City') %>% 
  group_by(.,Last.Update) %>% 
  summarise(.,Total.Cases=sum(Confirmed.Cases),
            Total.Deaths=sum(Deaths)) %>%
  mutate(.,New.Cases=Total.Cases-lag(Total.Cases)) %>%
  filter(.,New.Cases>=0)
#ast.Update,y=New.Cases))+geom_line()
# the county choices
#ny.county.choice=pull(ny_covid %>% group_by(.,Admin2) %>% group_keys())
#----------------------------------------------------------------------------

# mobility data
gb_mobility=read.csv('Global_Mobility_Report.csv',na.strings=c("","NA"),
                     stringsAsFactors = F)
us_mobility=gb_mobility %>% filter(., country_region_code=='US')
us_mobility$date=as.Date(us_mobility$date)
us_mobility=us_mobility %>% filter(.,date>='2020-03-01')

us_mobility=us_mobility %>%
  rename(.,Retail=retail_and_recreation_percent_change_from_baseline,
         Grocery=grocery_and_pharmacy_percent_change_from_baseline,
         Transit=transit_stations_percent_change_from_baseline,
         Park=parks_percent_change_from_baseline,
         Workplace=workplaces_percent_change_from_baseline,
         Residential=residential_percent_change_from_baseline,
         Province.State=sub_region_1)

us_mobility=us_mobility %>% mutate(.,sub_region_2=
                                     if_else(Province.State=='District of Columbia',
                                        'District of Columbia',sub_region_2)) %>% 
                            filter(.,!is.na(Retail),
                                     !is.na(Transit),
                                     !is.na(Park),
                                     !is.na(Workplace),
                                     !is.na(Residential),
                                     #!is.na(sub_region_2),
                                     !is.na(Province.State)
                                   )

# ### Test state.daily.mob---------------------------------------------------
# 
# state.daily.mob= us_mobility %>% filter(.,Province.State=='Alabama') %>%
#   group_by(.,date) %>%
#   summarise(.,Retail=mean(Retail),
#             Grocery=mean(Grocery),
#             Transit=mean(Transit),
#             Park=mean(Park),
#             Workplace=mean(Workplace),
#             Residential=mean(Residential))
# state.daily.mob$idx=seq(1,nrow(state.daily.mob))
# ##-------------------------------------------------------------------------
# 
# ### Test correlation between daily cases and mobility---------------------
# state.daily.cases$idx=seq(-6,(nrow(state.daily.cases)-7))
# state.corr.data=state.daily.cases %>% inner_join(state.daily.mob,by='idx')
# 
# 
# #state.corr.data=merge(state.daily.cases %>% rename(.,date=Last.Update)
# #                      , state.daily.mob, by="idx", all = T)
# 
# # state.corr.data=state.corr.data %>%
# #                   filter(.,!is.na(New.Cases),
# #                             !is.na(Retail),
# #                             !is.na(Grocery),
# #                             !is.na(Transit),
# #                             !is.na(Park),
# #                             !is.na(Workplace),
# #                             !is.na(Residential))
# 
# cor(state.corr.data$New.Cases,state.corr.data$Park)
# ###-----------------------------------------------------------------------

us_mobility_grp=us_mobility %>% group_by(.,date,Province.State) %>% 
                                  summarise(.,Retail=mean(Retail),
                                              Grocery=mean(Grocery),
                                              Transit=mean(Transit),
                                              Park=mean(Park),
                                              Workplace=mean(Workplace),
                                              Residential=mean(Residential))
mob.choice=colnames(us_mobility_grp)[-2][-1]


### Correleation between mobility types all over the states over all the period of time
corr.mob.type=us_mobility %>% select(.,Retail,Grocery,Transit,Park,Workplace,Residential)
#ggcorr(corr.mob.type,method=c("pairwise", "pearson"))


### get correlations between new cases and mobility type


### Plot top and bottom 5 states---------------------------------------
# 
# date.mob= us_mobility_grp %>% filter(.,date=='2020-04-01')
# bottom5mobs=date.mob %>% top_n(.,-5,Park)
# bottom5states=bottom5mobs$Province.State
# bottom5mobdata=us_mobility %>% filter(.,Province.State %in% bottom5states,
#                                          date=='2020-04-01')
# ggplot(bottom5mobdata,aes(x=fct_reorder(Province.State,Park,FUN=median,,na.rm=TRUE,.desc=TRUE),y=Park))+geom_boxplot()
#-----------------------------------------------------------------------


#### Trying to draw a map using leaflet--------------------------------
#colstates=map("state",fill=TRUE,plot=FALSE)
# 
# leaflet_mob=leaflet(date.mob) %>% 
#   addTiles() %>% 
#   addCircleMarkers(,radius = sqrt((date.mob$Retail)**2)) %>% 
#   addPolygons(data=colstates, 
#               #fillColor = heat.colors(6, alpha = 1), 
#               stroke = FALSE) %>% 
#   addProviderTiles("Esri.WorldStreetMap")
# leaflet_mob
#----------------------------------------------------------------------

### Test draw leaflet map of NYC---------------------------------------

#nycounties <- readOGR("json/nycounties.geojson")

# counties=map("state", fill = TRUE,
#              plot = FALSE,
#              region = 'new york')

ny_counties=read.csv('Geocodes_USA_with_Counties.csv',stringsAsFactors = FALSE)
ny_counties=ny_counties %>% filter(.,state=='NY') %>%
                            select(.,county,latitude,longitude) %>% 
                            group_by(.,county) %>% 
                            summarise(.,lat=last(latitude),long=last(longitude)) %>% 
                            mutate(.,county=paste(county,'County'))
ny_mobility=us_mobility %>% filter(.,Province.State=='New York') %>% 
                            rename(.,county=sub_region_2) %>% 
                             filter(.,county=='New York County' | county=='Kings County' |
                                             county=='Bronx County' |
                                             county=='Richmond County' |
                                             county=='Queens County')
                                    
                                    

                  
ny_mobility=merge(ny_mobility,ny_counties,all=T) %>% filter(.,!is.na(county),
                                                              !is.na(Retail),
                                                              !is.na(Grocery),
                                                              !is.na(Transit),
                                                              !is.na(Park),
                                                              !is.na(Workplace),
                                                              !is.na(Residential))



# ny_mob_date=ny_mobility %>% filter(.,date=='2020-04-01')
# ny_mob_date=ny_mob_date %>% mutate(.,mob.input=Retail)
# pal <- colorFactor(
#   palette = colorRampPalette(rainbow(10))(length(ny_mob_date$Retail)),
#   domain = ny_mob_date$Retail)

#  leaflet_ny_mob_date <- leaflet(ny_mob_date) %>%
#    addTiles() %>%
#    # addPolygons(data=counties,
#    #             fillColor = heat.colors(6, alpha = 1),
#    #             stroke = FALSE) %>%
    # addCircleMarkers(radius = sqrt((0.5*ny_mob_date$mob.input)**2),
    #                  stroke = FALSE, fillOpacity = 0.7,
    #                  #clusterOptions = markerClusterOptions(),
    #                  popup = paste("<b>",ny_mob_date$county,"</b>","<br>",
    #                                "Percentage: ", paste0(ny_mob_date$mob.input,'%'), "<br>")) %>%
#    #addProviderTiles("Esri.WorldStreetMap") %>%
#    addProviderTiles("OpenStreetMap.Mapnik") %>%
#    setView(lng = -73.99, lat = 40.65, zoom = 9)
# #   #addLegend(position = "bottomleft",pal=pal,values = ny_mob_date$Retail)
# #   #color='blue' ,values = ny_mob_date$Retail,
# #   #           title = "Value",
# #   #           opacity = 1)
#   static.map <- leaflet() %>% 
#     addTiles() %>% 
#     addProviderTiles("OpenStreetMap.Mapnik") %>%
#     setView(lng = -73.99, lat = 40.65, zoom = 9)
#   
# static.map


#---- NYC daily mobility trends----------------------------------
nyc_daily_mob=ny_mobility %>% group_by(.,date) %>%
  summarise(.,Retail=median(Retail),
            Grocery=median(Grocery),
            Transit=median(Transit),
            Park=median(Park),
            Workplace=median(Workplace),
            Residential=median(Residential))
nyc_daily_mob_trend=nyc_daily_mob %>% 
  gather(.,Mobility.Type,Percentage, Retail:Residential)
#----------------------------------------------------------------

#---- NYC correlations-------------------------------------------
indx.mob.nyc=nyc_daily_mob %>% filter(.,date>='2020-03-02')
indx.mob.nyc$idx=seq(1,nrow(indx.mob.nyc))
indx.cases.nyc=nyc_covid
indx.cases.nyc$idx=seq(2,(nrow(indx.cases.nyc)+1))
nyc.corr=indx.cases.nyc %>% inner_join(indx.mob.nyc,by='idx')
# corr=cor(nyc.corr$New.Cases,nyc.corr$Retail)
# corr=round(corr,2)
# corr
#---------------------------------------------------------------









