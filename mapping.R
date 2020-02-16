# #install.packages("leaflet")
# install.packages("echarts4r")
# install.packages("remotes")
# install.packages("geojsonio")
install.packages("spdplyr")
# remotes::install_github('JohnCoene/echarts4r.maps')
# remotes::install_github("JohnCoene/echarts4r.assets")

library(sp)
library(spdplyr)
library(lubridate)
library(janitor)
library(anytime)
library(nCov2019)
library(RCurl)
library(tidyr)
library(leaflet)
library(RColorBrewer)
library(echarts4r)
library(echarts4r.maps)
library(echarts4r.assets)
library(rgdal)
library(geojsonio)
library(tidyverse)







pal <- colorNumeric("viridis", NULL)

em_bank_list <- as.vector(echarts4r.maps::em_bank())


report_df <- read_csv(getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/02-13-2020.csv")) %>% clean_names("snake")


cleaned_df <- report_df %>% 
              filter(country_region != "Others") %>% 
              mutate(country_region = if_else(country_region == "Mainland China", "China",country_region)) %>%
              mutate(country_region = if_else(country_region == "Hong Kong", "China",country_region)) %>% #i know not cool but needed for mapping package
              mutate(country_region = if_else(country_region == "Taiwan", "China",country_region)) %>% #i know not cool but needed for mapping package
              mutate(country_region = if_else(country_region == "UK", "United_Kingdom",country_region)) %>% 
              mutate(country_region = if_else(country_region == "US", "USA",country_region)) %>% 
              select(-last_update)
str(cleaned_df)  

maps <- unlist(cleaned_df %>% 
                mutate(cleaned_names = str_replace_all(country_region, " ", "_")) %>% 
                distinct(cleaned_names), use.names = F)
#-----------------------------------------------------------------------#
map_list <- intersect(as.vector(echarts4r.maps::em_bank()),maps)
#-----------------------------------------------------------------------#
province_summary_df <- cleaned_df %>% 
                group_by(country_region, province_state) %>%
                summarise(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered)) %>%
                mutate(cleaned_names = str_replace_all(country_region, " ", "_")) %>%
                arrange(desc(deaths))
#-----------------------------------------------------------------------#
summary_df <- cleaned_df %>% 
                group_by(country_region) %>%
                summarise(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered)) %>%
                mutate(death_pc = (deaths/confirmed)*100) %>% 
                mutate(recovered_pc = (recovered/confirmed)*10) %>% 
                mutate(cleaned_names = str_replace_all(country_region, " ", "_")) %>%
                arrange(desc(deaths))
#-----------------------------------------------------------------------#
nrow(summary_df)


e_color_range(summary_df, deaths, colour_range, colors = brewer.pal(9,"Reds")) %>% 
  e_charts(cleaned_names) %>%
  e_geo_3d(recovered_pc, colour_range) 


e_color_range(summary_df, deaths, colour_range, colors = brewer.pal(9,"Reds")) %>%
  e_charts(cleaned_names) %>% 
  e_map_3d(deaths, shading = "lambert") %>% 
  e_visual_map(deaths) # scale to values

summary_df %>% 
  arrange(desc(deaths)) %>% 
  e_color_range(deaths, colour_range, colors = brewer.pal(9,"Reds")) %>%
  e_charts(cleaned_names) %>% 
  e_geo_3d(death_pc, colour_range)

library(echarts4r.maps)

country_name <- "China"
province_state_list province_summary_df %>% 
    filter(country_region == country_name) %>% 
    distinct(province_state) %>% 
    select(province_state)

column_name <- "deaths"
column_name <- enquo(column_name)
          
province_summary_df %>% 
          filter(country_region == country_name) %>% 
          e_color_range( {{ column_name }}, colour_range, colors = brewer.pal(9,"Reds")) %>%
          e_charts(province_state) %>%
          em_map(country_name) %>% 
          e_map({{ column_name }}, map = country_name) %>% 
          e_visual_map({{ column_name }}) %>% 
          e_theme("infographic")

deaths <- "deaths"
province_summary_df %>% 
  filter(country_region == country_name) %>% 
  e_color_range(deaths, colour_range, colors = brewer.pal(9,"Reds")) %>%
  e_charts(province_state) %>%
  em_map(country_name) %>% 
  e_map(deaths, map = country_name) %>% 
  e_visual_map(deaths) %>% 
  e_theme("infographic")









str(province_summary_df)
maps <- em_bank() 
df <- data.frame(
  region = c("Rajasthan", "Odisha", "Gujarat"), 
  value = c(1,2, 3)
)

df %>% 
  e_charts(region) %>%
  em_map("India") %>% 
  e_map(value, map = "India") %>% 
  e_visual_map(value) %>% 
  e_theme("infographic")




#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# TESTING
#-----------------------------------------------------------------------#
choropleth <- data.frame(
  countries = c("France", "Brazil", "China", "Russia", "Canada", "India", "United States",
                "Argentina", "Australia"),
  values = round(runif(9, 10, 25))
)


choropleth %>%
  e_charts(countries) %>%
  e_map(values) %>%
  e_visual_map(min = 10, max = 25)



choropleth %>%
  e_charts(countries) %>%
  e_map_3d(values, shading = "lambert") %>%
  e_visual_map(min = 10, max = 30)



choropleth2 <- data.frame(
  countries = rep(choropleth$countries, 3)) %>%
      mutate(grp = c(
                    rep(2016, nrow(choropleth)),
                    rep(2017, nrow(choropleth)),
                    rep(2018, nrow(choropleth))),
  values = runif(27, 1, 10))




choropleth %>%
  group_by(grp) %>%
  e_charts(countries, timeline = TRUE) %>%
  e_map(values) %>%
  e_visual_map(min = 1, max = 10)



choropleth %>%
  group_by(grp) %>%
  e_charts(countries, timeline = TRUE) %>%
  e_map_3d(values) %>%
  e_visual_map(min = 1, max = 10)



m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% addTiles()

m %>% addProviderTiles(providers$CartoDB.DarkMatter)


Stamen.TonerLite
Esri.WorldGrayCanvas
CartoDB.Positron
#m %>% addProviderTiles(providers$Stamen.TonerLite)
#m %>% addProviderTiles(providers$Esri.WorldGrayCanvas)
m %>% addProviderTiles(providers$CartoDB.Positron)



e_charts() %>% 
  em_maps(map_list, "myMap") %>% 
  e_map(map = "myMap")



maps <- c("China","Japan") 

e_charts() %>% 
  em_maps(em_bank_list[1:4], "myMap") %>% 
  e_map(map = "myMap")


e_charts() %>% 
  em_maps(em_bank_list[1:4], "myMap") %>% 
  e_geo_3d(values, color, type = "AF") 



df <- data.frame(val = 1:10)
e_color_range(df, val, colors)


choropleth <- data.frame(
  countries = c("France", "Brazil", "China", "Russia", "Canada", "India", "United States",
                "Argentina", "Australia"),
  height = runif(9, 1, 5),
  color = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6",
            "#2171B5", "#08519C", "#08306B")
)

choropleth %>%
  e_charts(countries) %>%
  e_geo_3d(height, color) 



# counties <- geojsonio::geojson_read("geo_json/countries.geojson", what = "sp")
# str(counties)
# 
# countries <- rgdal::readOGR("geo_json/LAU_2018.shp")
# str(countries)
# countries[2]$ISO_A3[1]
# 
# 
# 
# counties %>% filter(ADMIN %in% c("New Zealand", "Australia", "Fiji"))
# 
# 
# counties %>% filter(ISO_A3 %in% c("AUS", "NZL", "FJI"))
# 
# leaflet(counties) %>%
#   addTiles() %>% 
#   addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1)
# 
# 
# 
# topoData <- readLines("geo_json/high_res_geo.json") %>% paste(collapse = "\n")
# 
# leaflet() %>% 
#   setView(lng = -98.583, lat = 39.833, zoom = 3) %>%
#   addTiles() %>%
#   addTopoJSON(topoData, weight = 10, color = "#000000", fill = T) %>% 
#   addProviderTiles(providers$CartoDB.Positron)
# 
# head(counties@data, n = 2)




counties@plotOrder
counties@data[2]


lat <- coordinates(gCentroid(counties))[[1]]


main_df <- read_csv(getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/who_covid_19_situation_reports/who_covid_19_sit_rep_time_series/who_covid_19_sit_rep_time_series.csv")) %>% clean_names("snake")
str(main_df)


#country_region



confirmed_df <- read_csv(getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")) %>% clean_names("snake")
str(confirmed_df)

deaths_df <- read_csv(getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")) %>% clean_names("snake")
str(deaths_df)


recoverd_df <- read_csv(getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")) %>% clean_names("snake")
str(recoverd_df)
