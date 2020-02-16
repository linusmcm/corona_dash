# install.packages("mapview")
library(mapview)

nc2 <- read_sf("states_provinces/ne_10m_admin_1_states_provinces.shp", quiet = TRUE, as_tibble = T, stringsAsFactors = F) %>% 
          select(country_region = admin, province_state = woe_name, name, type_en, postal, latitude, longitude, adm0_a3,  gns_name, geometry)
str(nc2)

str(nc2 %>% filter(admin == "Australia"))

mapview(nc2 %>% filter(admin == "Australia"), zcol="confirmed")

aus_df <- nc2 %>% filter(admin == "Australia")
str(aus_df)
st_as_sf(province_summary_df)


province_summary_df <- cleaned_df %>% 
  group_by(country_region, province_state) %>%
  summarise(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered)) %>%
  mutate(cleaned_names = str_replace_all(country_region, " ", "_")) %>%
  arrange(desc(deaths))

st_as_sf(province_summary_df %>% left_join(nc2))

leaflet(options = leafletOptions(zoomControl = FALSE,
                                 minZoom = 3, maxZoom = 3,
                                 dragging = FALSE))


mapview(st_as_sf(province_summary_df %>% left_join(nc2)), zcol="deaths",colors = brewer.pal(9,"Reds"))

