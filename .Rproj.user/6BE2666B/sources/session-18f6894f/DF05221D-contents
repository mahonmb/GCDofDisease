mdat1 <- read.csv("G:/Shared drives/GCD of Disease/Field Studies/FieldStudies.csv")

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
                 fill = Global.Change.Driver),
             size = 1.5, shape = 21, alpha = 0.75)+
  scale_fill_manual(values = c("#5E976E",
                                "#58355E",
                                "#FFCA3A",
                                "#EC0B43",
                                "#63ADF2"),
                    name = NULL)+
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


