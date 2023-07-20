#Heatmap code
##This needs to be run aftr you have run the best fitting interaction models
##for each of the study, host, and parasite moderators

test1 <- bind_rows(
  data.frame(emmeans(m2g, ~ Global.Change.Driver))[,1:2] %>%
    mutate(Secondary = "Overall",
           Name = "Overall"),
  data.frame(emmeans(mEndg, ~ Global.Change.Driver|Endpoint_Host_Parasite))[,1:3] %>%
  rename(Secondary = Endpoint_Host_Parasite) %>%
    mutate(Name = "Endpoint"),
data.frame(emmeans(mHost_Taxong, ~ Global.Change.Driver|Native.host.type))[,1:3] %>%
  rename(Secondary = Native.host.type) %>%
  mutate(Name = "Host Taxa"),
data.frame(emmeans(mEnemyg, ~ Global.Change.Driver|Enemy.type))[,1:3] %>%
  rename(Secondary = Enemy.type) %>%
  mutate(Name = "Enemy Taxa"),
data.frame(emmeans(mVectorg, ~ Global.Change.Driver|Vector))[,1:3] %>%
  rename(Secondary = Vector) %>%
  mutate(Name = "Vector",
         Secondary = ifelse(Secondary == "No", "Non-vector", "Vector")),
data.frame(emmeans(mVectorBg, ~ Global.Change.Driver|Vector.borne))[,1:3] %>%
  rename(Secondary = Vector.borne) %>%
  mutate(Name = "Vector-borne",
         Secondary = ifelse(Secondary == "No", "Not vector-borne", "Vector-borne")),
data.frame(emmeans(mEctoparasiteBg, ~ Global.Change.Driver|Ectoparasite))[,1:3] %>%
  rename(Secondary = Ectoparasite) %>%
  mutate(Name = "Ectoparasite",
         Secondary = ifelse(Secondary == "No", "Endoparasite", "Ectoparasite")),
data.frame(emmeans(mVenueg, ~ Global.Change.Driver|Venue))[,1:3] %>%
  rename(Secondary = Venue) %>%
  mutate(Name = "Venue"),
data.frame(emmeans(mHabitatg, ~ Global.Change.Driver|Habitat))[,1:3] %>%
  rename(Secondary = Habitat) %>%
  mutate(Name = "Habitat"),
data.frame(emmeans(mRouteIntg, ~ Global.Change.Driver|Route))[,1:3] %>%
  rename(Secondary = Route) %>%
  mutate(Name = "Route",
         Secondary = ifelse(Secondary == "Complex", "Complex route", "Direct route")),
data.frame(emmeans(mMacroIntg, ~ Global.Change.Driver|Macroparasite))[,1:3] %>%
  rename(Secondary = Macroparasite) %>%
  mutate(Name = "ParasiteSize",
         Secondary = ifelse(Secondary == "No", "Microparasite", "Macroparasite")),
data.frame(emmeans(mThermyIntg, ~ Global.Change.Driver|Ectothermic.host))[,1:3] %>%
  rename(Secondary = Ectothermic.host) %>%
  mutate(Name = "Host Thermy",
         Secondary = ifelse(Secondary == "No", "Endothermic host", "Ectothermic host")),
data.frame(emmeans(mStageg, ~ Global.Change.Driver|Free.living.stages))[,1:3] %>%
  rename(Secondary = Free.living.stages) %>%
  mutate(Name = "Free Living Stages",
         Secondary = ifelse(Secondary == "No", "No free living stages", "Free living stages")),
data.frame(emmeans(mHumang, ~ Global.Change.Driver|Human.Parasite))[,1:3] %>%
  rename(Secondary = Human.Parasite) %>%
  mutate(Name = "Human Parasite",
         Secondary = ifelse(Secondary == "No", "Non-human parasite", "Human parasite")),
data.frame(emmeans(mZooIntg, ~ Global.Change.Driver|Zoonotic))[,1:3] %>%
  rename(Secondary = Zoonotic) %>%
  mutate(Name = "Zoonotic",
         Secondary = ifelse(Secondary == "No", "Non-zoonotic parasite", "Zoonotic parasite"))
)


##Need to go back through the "Secondary column" and update the values that are
##uninformative (e.g., yes, no); match naming scheme below
test1

test1 <- complete(test1, Global.Change.Driver, Secondary)

test1$Secondary <- as.factor(test1$Secondary)
test1$Secondary <- factor(test1$Secondary, levels = c("Overall",
                                                    "Host",
                                                    "Parasite",
                                                    "Amphibian/Reptile",
                                                    "Arthropod",
                                                    "Bird",
                                                    "Mammal",
                                                    "Mollusk",
                                                    "Plant",
                                                    "Human parasite",
                                                    "Non-human parasite",
                                                    "Zoonotic parasite",
                                                    "Non-zoonotic parasite",
                                                    "Bacteria",
                                                    "Fungi",
                                                    "Helminth",
                                                    "Protist",
                                                    "Virus",
                                                    "Vector",
                                                    "Non-vector",
                                                    "Vector-borne",
                                                    "Not vector-borne",
                                                    "Endoparasite",
                                                    "Ectoparasite",
                                                    "Field",
                                                    "Lab",
                                                    "Freshwater",
                                                    "Marine",
                                                    "Terrestrial",
                                                    "Complex route",
                                                    "Direct route",
                                                    "Microparasite",
                                                    "Macroparasite",
                                                    "Ectothermic host",
                                                    "Endothermic host",
                                                    "No free living stages",
                                                    "Free living stages"))




test1$Global.Change.Driver <- as.factor(test1$Global.Change.Driver)
test1$Global.Change.Driver <- factor(test1$Global.Change.Driver, levels = c("BC","CP","CC","HLC","IS"))

ggplot(test1, aes(y = Secondary, x = Global.Change.Driver, fill = emmean)) +
  # geom_tile()+
  # scale_fill_gradient2(high = "#fc8d59",
  #                      mid = "#ffffbf",
  #                      low = "#91bfdb",
  #                      midpoint = 0)+
  geom_tile(color = "black", size = 0.25) +
  # scale_fill_gradientn(colors = c("#075AFF", "white", "#FF0000"),
  #                      na.value = "black",
  #                      breaks=c(-1, 0, 2),
  #                      labels=c(-1, 0, 2),
  #                      limits = c(-2,2),
  #                      oob = scales::squish)
  scale_fill_gradient2(high = "#FF001E",
                       mid = "#f7f7f7",
                       low = "#0080FF",
                       midpoint = 0,
                       na.value = "black",
                       name = "Hedge's G",
                       oob = scales::squish,
                       limits = c(-1.01,2.01),
                       breaks = c(-1,0,1,2),
                       labels = c("-1", "0", "1", "2+")
                       ) +
  geom_hline(yintercept = c(1.5, 3.5, 9.5, 11.5, 13.5,
                            18.5, 20.5, 22.5, 24.5, 26.5,
                            29.5, 31.5, 33.5, 35.5), size = 2)+
  coord_fixed(expand = c(0), ratio = 0.565)+
  theme_JR()+
  theme(axis.title = element_blank())

ggsave("./Figures/HEATMAPupdate.tiff",
       dpi = 300,
       height = 6.1,
       width = 5)
