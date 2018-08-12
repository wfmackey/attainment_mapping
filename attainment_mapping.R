
# "Mapping" Australian higher education ####

library(tidyverse)
library(maptools)
library(rgdal)     # R wrapper around GDAL/OGR
library(tictoc) #
library(scales)

# Define grattan colors
glightyellow <- "#FFE07F"
gyellow <- "#FFC35A"
gorange <- "#F68B33"
gdark <- "#D4582A"
gdarker <- "#BD3603" 
gred <- "#A02226"
gdarkred <- "#621214"

# Read ABS shapefiles for SA2
    shapefile <- readOGR("data/sa2_2016_aust_shape/", "SA2_2016_AUST")
    shapefile.details <- merge(fortify(shapefile), 
                               as.data.frame(shapefile), 
                               by.x="id", 
                               by.y=0)
    
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
                       skip = 10)
  # Make better names
  names(attainment)<- make.names(names(attainment),unique = TRUE)
  
  # Generate percentage variable
  attainment <- attainment %>% 
              rename(sa2_name = HEAP...1.Digit.Level) %>% 
              mutate(pop = Total - Not.stated,
                     bach.count = Postgraduate.Degree.Level + 
                                  Graduate.Diploma.and.Graduate.Certificate.Level +
                                  Bachelor.Degree.Level,
                     bach = 100 * bach.count / pop) %>% 
              select(sa2_name, pop, bach.count, bach) %>% 
              slice(2:n()) %>% 
              mutate_if(is.factor, as.character)

 # Merge to shapefile in sa2
sa2data <- left_join(sa2, attainment, by = "sa2_name")
  
sa2nona <- filter(sa2data, !is.na(bach))





# Create useless map for Australia
australia.map <-
       sa2data %>%
         ggplot() + 
         geom_polygon(aes(x = long, y = lat, group = group, fill = bach), color = NA) + 
         scale_fill_gradientn(name = "",
                              colours = c(glightyellow, gdark, gdarkred),
                              values = rescale(c(0, 20, 50, 70)),
                              limits = c(0,70),
                              na.value = "grey90") +
         theme_void() +
         theme(legend.position = "top",
               plot.title = element_text(hjust = 0.5)) +
         NULL 

ggsave("australia_map.pdf", australia.map ,device = "pdf")



# Create map for each capital city
states <- as.character(unique(sa2data$state))

for (i in states) {
  
namedata <- sa2data %>% filter(city == TRUE, state == i) %>% select(gcc_name)
name <- as.character(namedata[1,1])

assign(paste0(make.names(name),".map"),
       sa2data %>% filter(city == TRUE,
                     state == i,
                     pop > 10) %>%
    ggplot() + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = bach), color = NA) + 
    scale_fill_gradientn(name = "",
                         colours = c(glightyellow, gdark, gdarkred),
                         values = rescale(c(0, 20, 50, 70)),
                         limits = c(0,70),
                         na.value = "grey90") +
    theme_void() +
    theme(aspect.ratio = 1, 
          legend.position = "off",
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(name) + # for the main title
    NULL 
)
get(paste0(make.names(name),".map"))
ggsave(paste0(name,"_map.pdf"), device = "pdf")
toc()
}







## BIN ####
run.bin <- FALSE
if (run.bin) {
    ## Combining of the charts is done in ppt :/
    library(tools)
    library(animation)
    
    compactPDF("mapping_higher_education2.pdf",
               gs_quality = "screen")
    im.convert("mapping_higher_education2.pdf", output = "mapping.png", extra.opts="-density 150")
    
    
    
    ausdata <- australia.map$data %>% group_by(sa2_name, state, gcc_name) %>% summarise(bach = mean(bach))
    findodd <- ausdata %>% filter(state == "South Australia")
    
    western <- sa2data %>% filter(sa2_name == "Western") %>% slice(1)
    
    # sa2data %>% filter(state == "South Australia", gcc_name == "Rest of SA") %>% 
    # ggplot(aes(x = long, y = lat, group = group, fill = bach, label = sa2_name)) + 
    #   geom_polygon(color = NA) + 
    #   geom_text() +
    #   scale_fill_gradientn(name = "",
    #                        colours = c(glightyellow, gdark, gdarkred),
    #                        values = rescale(c(0, 20, 50, 70)),
    #                        limits = c(0,70),
    #                        na.value = "grey90") +
    #   theme_void() +
    #   theme(aspect.ratio = 1, 
    #         legend.position = "off",
    #         plot.title = element_text(hjust = 0.5)) +
    #   NULL 


} ##end run.bin