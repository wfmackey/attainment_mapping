
# "Mapping" Australian higher education ####

library(tidyverse)
library(maptools)
library(rgdal)     # R wrapper around GDAL/OGR
library(tictoc) #
library(scales)
library(rmapshaper)
library(ggthemes)
library(mapproj)
library(gganimate)


# Define grattan colors
glightyellow <- "#FFE07F"
gyellow <- "#FFC35A"
gorange <- "#F68B33"
gdark <- "#D4582A"
gdarker <- "#BD3603" 
gred <- "#A02226"
gdarkred <- "#621214"

mapcompression = 0.2

# Read ABS shapefiles for SA2
shapefile <- readOGR("data/sa2_2016_aust_shape/", "SA2_2016_AUST") %>% 
  ms_simplify(keep = mapcompression, keep_shapes = T)

shapefile.details <- merge(fortify(shapefile), 
                           as.data.frame(shapefile), 
                           by.x = "id", 
                           by.y = 0)

sa2 <- shapefile.details %>%
  rename(sa2 = "SA2_MAIN16",
         sa2_short = "SA2_5DIG16",
         sa2_name = "SA2_NAME16",
         sa3 = "SA3_CODE16",
         sa3_name = "SA3_NAME16",
         sa4 = "SA4_CODE16",
         sa4_name = "SA4_NAME16",
         gcc = "GCC_CODE16",
         gcc_name = "GCC_NAME16",
         state_code = "STE_CODE16",
         state = "STE_NAME16",
         area = "AREASQKM16"
  ) %>% 
  mutate(city = if_else(grepl('Greater', gcc_name), TRUE, 
                        if_else(state == "Australian Capital Territory", TRUE, 
                                FALSE)))



# Read in higher education attainment data
attainment <- read_csv("data/attainment_by_sa2.csv",
                       skip = 11) %>% 
  rename( sa2_name = "SA2 (UR)",
          postgrad = "Postgraduate Degree Level",
          graddip = "Graduate Diploma and Graduate Certificate Level",
          bach = "Bachelor Degree Level",
          dip = "Advanced Diploma and Diploma Level",
          certIII_IV = "Certificate III & IV Level",
          y10plus = "Secondary Education - Years 10 and above",
          certI_II = "Certificate I & II Level",
          y9below = "Secondary Education - Years 9 and below",
          not_stated = "Not stated",
          total = "Total") %>% 
  # Generate percentage variable
  mutate(pop = total - not_stated,
         bach_count = postgrad + 
           graddip +
           bach,
         bach = 100 * bach_count / pop) %>% 
  select(sa2_name, pop, bach_count, bach) %>% 
  slice(2:n()) %>% 
  mutate_if(is.factor, as.character) %>% 
  filter(!is.na(bach))

# Merge to shapefile in sa2
sa2data <- left_join(sa2, attainment, by = "sa2_name")

sa2nona <- filter(sa2data, !is.na(bach))





# Create useless map for Australia
australia.map <-
  sa2data %>%
  ggplot() + 
  geom_polygon(aes(x = long, 
                   y = lat, 
                   group = group, 
                   fill = bach), 
               color = NA) + 
  scale_fill_gradientn(name = "",
                       colours = c(glightyellow, gdark, gdarkred),
                       values = rescale(c(0, 20, 50, 70)),
                       limits = c(0,70),
                       na.value = "grey90") +
  theme_void() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +
  NULL 

ggsave("australia_map.pdf", australia.map, device = "pdf")




# Generating fake data to test animation ------------------------------------
sa2data1 <- sa2data %>% mutate(year = "2016")
sa2data2 <- sa2data %>% mutate(year = "2011",
                               bach = rnorm(nrow(sa2data), 30, sd = 10))

sa2dataComb <- bind_rows(sa2data1,
                         sa2data2)

i = "Victoria"
animate = TRUE
# ---------------------------------------------------------------------------


# Create map for each capital city
states <- as.character(unique(sa2data$state))

makeCityMap <- function(i, 
                        plot = FALSE, 
                        animate = FALSE,
                        projection = "mercator") {
  
  namedata <- sa2data %>% filter(city == TRUE, state == i) %>% select(gcc_name)
  name <- as.character(namedata[1,1])
  
  if (animate) {
    sa2data <- sa2dataComb %>% 
                group_by(year) %>% 
                mutate(one = 1,
                       count = cumsum(one),
                       year_lab = if_else(count == 1, year, NA_character_))
  } 
  
  p <- 
    sa2data %>% 
    filter(city == TRUE,
           state == i,
           pop > 10)
  
  if (animate) {
  lat_max <- p %>% filter(year == "2016") %>% pull(lat) %>% max()
  lat_min <- p %>% filter(year == "2016") %>% pull(lat) %>% min()
  
  long_max <- p %>% filter(year == "2016") %>% pull(long) %>% max()
  long_min <- p %>% filter(year == "2016") %>% pull(long) %>% min()
  
  center_lat  <- mean(lat_max, lat_min)
  center_long  <- mean(long_max, long_min)
  }
  
  p <- p %>% 
    ggplot() + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = bach), color = NA) + 
    scale_fill_gradientn(name = "",
                         colours = c(glightyellow, gdark, gdarkred),
                         values = rescale(c(0, 20, 50, 70)),
                         limits = c(0,70),
                         na.value = "grey90") +
    coord_map(projection = projection) +
    theme_void() +
    theme(aspect.ratio = 1, 
          legend.position = "right",
          plot.title = element_text(hjust = 0.5), title = element_text(size = 30)) +
    labs(title = paste0("Vocational participation in ", name, " in {closest_state}")) +
    NULL 
  
  if (!animate) {
    ggsave(paste0(name,"_map.pdf"), 
           p,
           height = 10, width = 10)
  }
  
  if (animate) {
    p <- p + 
      geom_text(aes( 
                x = center_long, 
                y = center_lat, 
                label = year_lab),
                size = 20,
                colour = "white") + # the geom text isn't working
      transition_states(
        year,
        transition_length = 2.5,
        state_length = 1.5)
    
    animate(p, width = 1000, height = 1000, nframes = 100)
    anim_save("melb.gif")
  } 
  
  if (plot && !animate) p
  
  print("Done")
  
}



makeCityMap("Tasmania", animate = FALSE, projection = "gilbert")


makeCityMap("Victoria", animate = TRUE)

# purrr::map(states[1:2], makeCityMap, animate = TRUE)

