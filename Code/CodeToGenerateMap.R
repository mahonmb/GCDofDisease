mdat1 <- read.csv("C:/Users/mmahon/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Research/GlobalChangeDriver_original/Data/MapLocationContinentInformation.csv")

mdat1 <- mdat1 %>%
  mutate(Lat = as.numeric(gsub(",.*$", "", Coordinates)),
         Long = as.numeric(gsub(".*,", "", Coordinates)))

mdat1$Global.Change.Driver = as.factor(mdat1$Global.Change.Driver)
mdat1$Global.Change.Driver = factor(mdat1$Global.Change.Driver,
                                    levels = levels(mdat1$Global.Change.Driver)[c(1,3,2,4,5)])

mdat12 <- mdat1 %>%
  filter(Global.Change.Driver == "HLC")
world_map = map_data("world") %>% 
  filter(! long > 180)

countries = world_map %>% 
  distinct(region) %>% 
  rowid_to_column()





map1 <- countries %>% 
  ggplot()+
  geom_map(map = world_map, aes(map_id = region),
           color = "black", fill = "white", size = 0.1)+
  expand_limits(x = world_map$long, y = world_map$lat) +
  geom_point(data = mdat1,
             aes(x = Long,
                 y = Lat,
                 fill = Income),
             size = 1.5, shape = 21, alpha = 0.75)+
  # scale_fill_manual(values = c("#5E976E",
  #                               "#58355E",
  #                               "#FFCA3A",
  #                               "#EC0B43",
  #                               "#63ADF2"),
  #                   name = NULL)+
  guides(fill = guide_legend(override.aes = list(size = 3)))+
  coord_map("moll") +
  theme_bw()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.background = element_blank(), 
        legend.position = c(0.075, 0.075),
        legend.justification = c(0, 0),
        legend.text = element_text(size = 8),
        legend.margin=margin(c(0,1,5,1)))

cowplot::plot_grid(map1,
                   labels = c("G)"),
                   ncol = 1,
                   label_x = .1,
                   label_y = .785)

ggsave("C:/Users/mikem/Documents/Research/GCD Analyses/MAP.tiff",
       height = 4.24, width = 5.75)


library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  # indices$REGION   # returns the continent (7 continent model)
  indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}

points = data.frame(lon = mdat1$Long,
                    lat = mdat1$Lat)

mdat1$Country = as.character(coords2continent(points))

mdatna <- mdat1 %>%
  filter(is.na(Country))

mdat1k <- mdat1 %>%
  filter(!is.na(Country))


mdatna$Country = c("United States of America", "Spain", "Australia", "Italy",
                   "United States of America", "Netherlands", "United States of America", "United States of America",
                   "United States of America", NA, "Germany", "Canada",
                   "Kenya", "United States of America", "United States of America", "Australia",
                   "Italy", "Taiwan", "United States of America", "Brazil",
                   NA, "New Zealand", "Madagascar", "Brazil",
                   "Ecuador", "China", "Sweden", "New Zealand",
                   "Norway", NA, NA, NA,
                   "Germany", "The Bahamas", "The Bahamas", "The Bahamas",
                   "The Bahamas", "India", "Portugal", "United States of America",
                   "Portugal")


mdat1 <- bind_rows(mdat1k, mdatna)

hiin <- c("Andorra","Australia","Austria","Belgium","Canada","Cyprus",
          "Czech Republic","Denmark","Estonia","Finland","France","Germany",
          "Greece","Hong Kong SAR","Iceland","Ireland","Israel","Italy",
          "Japan","Korea","Latvia","Lithuania","Luxembourg","Macao SAR",
          "Malta","Netherlands","New Zealand","Norway","Portugal",
          "Puerto Rico","San Marino","Singapore","Slovak Republic","Slovenia",
          "Spain","Sweden","Switzerland","Taiwan","United Kingdom",
          "United States of America")

mdat1$Income = ifelse(is.na(mdat1$Country),
                      NA,
                      ifelse(mdat1$Country %in% hiin,
                      "HIC",
                      "LMIC"))

write.csv(mdat1,
          "C:/Users/mmahon/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Research/GlobalChangeDriver_original/Data/MapLocationContinentInformation.csv")
