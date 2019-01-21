pacman::p_load(sf, dplyr, ggplot2, randomcoloR)

# Load data from download.kortforsyningen.dk
zips = st_read(dsn = "C:/Users/Andreas Tyge Moller/Google Drive/zip_to_municipality/Shapes/POSTNUMMER.shp",
               layer="POSTNUMMER", stringsAsFactors=FALSE)

municipalities = st_read(dsn = "C:/Users/Andreas Tyge Moller/Google Drive/zip_to_municipality/Shapes/KOMMUNE.shp",
                         layer="KOMMUNE", stringsAsFactors=FALSE)

# Aggregate to one polygon per zip/municipality
complete_zips <- zips %>%
  group_by(POSTNR_TXT) %>% 
  summarise(geometry=st_union(geometry), do_union=F) %>% 
  st_cast("MULTIPOLYGON")

complete_municipalities <- municipalities %>%
  group_by(KOMKODE) %>% 
  summarise(geometry=st_union(geometry), do_union=F) %>% 
  st_cast("MULTIPOLYGON")

# Combine the two geometries
combined_data <- st_intersection(complete_zips, complete_municipalities)

# Calculate area of individual polygons and assign municipality by largest overlap
zip_to_municipality <- combined_data %>%
  mutate(area = st_area(combined_data)) %>% 
  group_by(POSTNR_TXT) %>% 
  filter(area == max(area))
  
ggplot(data=zip_to_municipality) + 
  geom_sf(data=zip_to_municipality, aes(fill=KOMKODE)) +
  theme(legend.position="none") +
  scale_fill_manual(values=distinctColorPalette(length(zip_to_municipality$KOMKODE)))
