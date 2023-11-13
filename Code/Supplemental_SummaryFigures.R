library(metafor)
library(MuMIn)
library(tidyverse)
library(multcomp)
library(emmeans)
library(ggeffects)
library(forcats)

##Make ggplot theme to use throughout
theme_JR <- function (base_size = 12, base_family = "")
{
  theme(
    panel.background = element_rect(fill=NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color="black"),
    legend.title = element_text(size=18,color="black"),
    legend.text = element_text(size=16,color="black"),
    legend.key = element_rect(fill=NA,color=NA),
    axis.text = element_text(size=12,color="black"),
    axis.title = element_text(size=16,color="black")
  )
}

fulldat2 <- read.csv("./Data/FinalDataWRound2.csv")

############################
library(car)
library(lme4)
library(ggeffects)

##########################################
ste <- function(x) sd(x, na.rm = T)/sqrt(length(x))


data.frame(fulldat2 %>%
  group_by(Global.Change.Driver) %>%
  summarize(meanSS = mean(Sample_size, na.rm = T),
            seSS = ste(Sample_size)))


fulldat2$SE_meta <- sqrt(fulldat2$SMDH_viC)
SEmod <- lmer(SE_meta ~ Global.Change.Driver + (1|Citation.number),
              data = fulldat2)
emmeans:::cld.emmGrid(emmeans(SEmod, specs = ~ Global.Change.Driver))

SEfigdat <- data.frame(ggemmeans(SEmod, terms = "Global.Change.Driver"))
SEfigdat$Global.Change.Driver = c("Biodiversity Change",
                                  "Climate Change",
                                  "Chemical Pollution",
                                  "Habitat Loss or Change",
                                  "Introduced Species")
SEfigdat$Global.Change.Driver = as.factor(SEfigdat$Global.Change.Driver)
# SEfigdat$Global.Change.Driver = factor(SEfigdat$Global.Change.Driver,
#                                         levels(SEfigdat$Global.Change.Driver)[c(1,3,2,4,5)])
SEfigdat$grouping = c("AB","A","A","B","A")
SEfigdat$EffLab = c(5,3,4,2,1)

SEfig <- ggplot(SEfigdat, aes(x = forcats::fct_rev(Global.Change.Driver),
                              y = predicted, color = Global.Change.Driver))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, size = 1)+
  geom_point(size = 3)+
  geom_text(aes(label = grouping,
                x = EffLab),
            color = "black",
            position = position_nudge(x = 0.25)
  )+
  scale_y_continuous(limits = c(0.4,0.8),
                     breaks = c(0.4,0.5,0.6,0.7,0.8))+
  scale_color_manual(values = c("#5E976E",
                                "#58355E",
                                "#FFCA3A",
                                "#EC0B43",
                                "#63ADF2"))+
  xlab("Global Change Driver") +               #relabel X and Y axes
  ylab("Standard error\nof effect size") +
  coord_flip()+
  theme_JR()+                                   #call your theme
  theme(legend.position = "none")
  


SSmod <- glmer(Sample_size ~ Global.Change.Driver + (1|Citation.number),
              family = poisson, data = fulldat2)
emmeans:::cld.emmGrid(emmeans(SSmod, specs = ~ Global.Change.Driver))

SSfigdat <- data.frame(ggemmeans(SSmod, terms = "Global.Change.Driver"))

SSfigdat$Global.Change.Driver = c("Biodiversity Change",
                                  "Climate Change",
                                  "Chemical Pollution",
                                  "Habitat Loss or Change",
                                  "Introduced Species")
SSfigdat$Global.Change.Driver = as.factor(SSfigdat$Global.Change.Driver)
# SEfigdat$Global.Change.Driver = factor(SEfigdat$Global.Change.Driver,
#                                         levels(SEfigdat$Global.Change.Driver)[c(1,3,2,4,5)])
SSfigdat$grouping = c("A","B","A","C","A")
SSfigdat$EffLab = c(5,3,4,2,1)

SSfig <- ggplot(SSfigdat, aes(x = forcats::fct_rev(Global.Change.Driver),
                              y = predicted, color = Global.Change.Driver))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, size = 1)+
  geom_point(size = 3)+
  geom_text(aes(label = grouping,
                x = (EffLab)),
            color = "black",
            position = position_nudge(x = 0.25)
  )+
  scale_y_continuous(trans = "log", breaks = c(25,50,100,150))+
  scale_color_manual(values = c("#5E976E",
                                "#58355E",
                                "#FFCA3A",
                                "#EC0B43",
                                "#63ADF2"))+
  xlab("Global Change Driver") +               #relabel X and Y axes
  ylab("Sample size\nper effect size") +
  coord_flip()+
  theme_JR()+                                   #call your theme
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank())


SESS_whole = cowplot::align_plots(SEfig, SSfig,
                                    align = 'h', axis = 'l')

cowplot::plot_grid(SESS_whole[[1]], SESS_whole[[2]],
                   labels = c("A)","B)"),
                   ncol = 2,
                   rel_widths = c(1,0.5),
                   label_x = c(0.89,0.775),
                   label_y = 0.925)

ggsave("./Figures/SS_SE_GCD_update.tiff",
       dpi = 300,
       width = 6,
       height = 4,
       units = "in")


#####Summary stats
##Table s2
##Groupings within global change drivers


data.frame(fulldat2 %>%
             filter(!is.na(Enemy.type) & Enemy.type != "Myxozoan" & Enemy.type != "Rotifer") %>%
  group_by(Enemy.type,
           Global.Change.Driver) %>%
  summarize(studies = length(unique(Citation.number)),
            count = n()))

data.frame(fulldat2 %>%
             filter(!is.na(Native.host.type)) %>%
             filter(!Native.host.type %in%  c("Coral","Echinoderm")) %>%
             group_by(Native.host.type,
                      Global.Change.Driver) %>%
             summarize(studies = length(unique(Citation.number)),
                       count = n()))

data.frame(fulldat2 %>%
             filter(!is.na(Human.Parasite)) %>%
             group_by(Human.Parasite,
                      Global.Change.Driver) %>%
             summarize(studies = length(unique(Citation.number)),
                       count = n()))

complete(data.frame(fulldat2 %>%
             filter(!is.na(Ectothermic.host)) %>%
             group_by(Ectothermic.host,
                      Global.Change.Driver) %>%
             summarize(studies = length(unique(Citation.number)),
                       count = n())),Ectothermic.host, Global.Change.Driver)

complete(data.frame(fulldat2 %>%
             filter(!is.na(Macroparasite)) %>%
             group_by(Macroparasite,
                      Global.Change.Driver) %>%
             summarize(studies = length(unique(Citation.number)),
                       count = n())),Macroparasite,Global.Change.Driver
                      )

data.frame(fulldat2 %>%
             filter(!is.na(Free.living.stages)) %>%
             group_by(Free.living.stages,
                      Global.Change.Driver) %>%
             summarize(studies = length(unique(Citation.number)),
                       count = n()))


data.frame(fulldat2 %>%
             filter(!is.na(Endpoint_Host_Parasite)) %>%
             group_by(Endpoint_Host_Parasite,
                      Global.Change.Driver) %>%
             summarize(studies = length(unique(Citation.number)),
                       count = n()))

data.frame(fulldat2 %>%
             filter(!is.na(Route)) %>%
             group_by(Route,
                      Global.Change.Driver) %>%
             summarize(studies = length(unique(Citation.number)),
                       count = n()))


complete(data.frame(fulldat2 %>%
             filter(!is.na(Vector.borne)) %>%
             group_by(Vector.borne,
                      Global.Change.Driver) %>%
             summarize(studies = length(unique(Citation.number)),
                       count = n())),Vector.borne, Global.Change.Driver
             
                      )

complete(data.frame(fulldat2 %>%
             filter(!is.na(Ectoparasite)) %>%
             group_by(Ectoparasite,
                      Global.Change.Driver) %>%
             summarize(studies = length(unique(Citation.number)),
                       count = n())),Ectoparasite, Global.Change.Driver
             )

complete(data.frame(fulldat2 %>%
                      filter(!is.na(Venue)) %>%
                      group_by(Venue,
                               Global.Change.Driver) %>%
                      summarize(studies = length(unique(Citation.number)),
                                count = n())), Venue, Global.Change.Driver)
         

complete(data.frame(fulldat2 %>%
                      filter(!is.na(Habitat)) %>%
                      group_by(Habitat,
                               Global.Change.Driver) %>%
                      summarize(studies = length(unique(Citation.number)),
                                count = n())),Habitat, Global.Change.Driver)

complete(data.frame(fulldat2 %>%
                      filter(!is.na(Zoonotic)) %>%
                      group_by(Zoonotic,
                               Global.Change.Driver) %>%
                      summarize(studies = length(unique(Citation.number)),
                                count = n())), Zoonotic, Global.Change.Driver)

complete(data.frame(fulldat2 %>%
                      filter(!is.na(Vector)) %>%
                      group_by(Vector,
                               Global.Change.Driver) %>%
                      summarize(studies = length(unique(Citation.number)),
                                count = n())), Vector, Global.Change.Driver)

print(complete(data.frame(fulldat2 %>%
                      filter(!is.na(ContinentUse)) %>%
                      group_by(ContinentUse,
                               Global.Change.Driver) %>%
                      summarize(studies = length(unique(Citation.number)),
                                count = n())), ContinentUse, Global.Change.Driver),n = 30)
##ANY OTHER DESCRIPTIVE STATS

##Total number of studies
length(unique(fulldat2$Citation.number))
##1002 studies - note 30 studies are from Mitchell et al (unique citation number for each family)
## so 1002 - 30 = 972
##Total Number of Effect Sizes
nrow(fulldat2)
##2938 observations

##Number of parasite species/taxa

data3 = fulldat2 %>%
  filter(complete.cases(Enemy_Random_Effect))

length(unique(data3$Enemy_Random_Effect))
##Each Introduced species combo is not its own taxa: 751

length(unique(paste(data3$Enemy_Random_Effect,
                    ifelse(grepl("_", data3$IS_Old_EnemyWInvasive),
                           sub(".*_", "", data3$IS_Old_EnemyWInvasive),
                           ""),
                    sep = "_")))
##Each Introduced species combo is its own taxa: 1282






data4 = fulldat2 %>%
  filter(complete.cases(Native.host))

length(unique(data4$Native.host))
##480 Unique host taxa

##

data5 = fulldat2 %>%
  filter(complete.cases(Enemy_Random_Effect)) %>%
  filter(complete.cases(Native.host)) %>%
  mutate(INT = paste(paste(Enemy_Random_Effect,
                           ifelse(grepl("_", IS_Old_EnemyWInvasive),
                                  sub(".*_", "", IS_Old_EnemyWInvasive),
                                  ""),
                           sep = "_"), Native.host, sep = "_"))

length(unique(data5$INT))
##1497 Unique parasite - host taxa interactions.


##Parasite groupings
data3 %>%
  filter(complete.cases(Enemy.type)) %>%
  group_by(Enemy.type) %>%
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

##Host groupings
data3 %>%
  filter(complete.cases(Native.host.type)) %>%
  group_by(Native.host.type) %>%
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

data3 %>%
  group_by(Global.Change.Driver) %>%
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))


fulldat2 %>%
  filter(complete.cases(Income)) %>%
  group_by(Income, Global.Change.Driver) %>%
  summarize(Studies = length(unique(Citation)),
    EffSize = n())

##Distribution of data across various contexts
##Distribution across host taxa
##Distribution across parasite taxa
##Distribution across habitats
##Distribution across drivers
##Distribution across observational/experimental

#Habitat
habplot = fulldat2 %>%
  filter(complete.cases(Habitat)) %>%
ggplot(aes(x = Habitat))+
  scale_y_continuous(limits = c(0, 2999),
                     breaks = seq(0, 3000, 500),
                     expand = c(0, 0))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of effect sizes")+
  xlab("Habitat")+
  theme_JR()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title.y = element_blank())

fulldat2$Global.Change.Driver2 <- factor(fulldat2$Global.Change.Driver)

fulldat2$Global.Change.Driver2 <- factor(fulldat2$Global.Change.Driver2,
                                      levels(fulldat2$Global.Change.Driver2)[c(1,3,2,4,5)])

#Global change driver
gcdplot = ggplot(fulldat2, aes(x = Global.Change.Driver2))+
  scale_y_continuous(limits = c(0, 1800),
                     breaks = seq(0,2000,500),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Biodiversity change",
                              "Chemical pollution",
                              "Climate change",
                              "Habitat loss/change",
                              "Introduced species"))+
  geom_bar(aes(fill = Global.Change.Driver),
           color = "black", width = 0.5)+
  scale_fill_manual(values = c("#5E976E",
                                "#FFCA3A",
                               "#58355E",
                                "#EC0B43",
                                "#63ADF2"))+
  ylab("Number of effect sizes")+
  xlab("Global change driver")+
  theme_JR()+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.8,vjust = 0.9),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.position = "none")

#Venue
venueplot = ggplot(fulldat2, aes(x = Venue))+
  scale_y_continuous(limits = c(0, 2999),
                     breaks = seq(0, 4000, 500),
                     expand = c(0, 0))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of effect sizes")+
  xlab("Venue")+
  theme_JR()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

#Parasite taxa
paraplot = fulldat2 %>%
  filter(complete.cases(Enemy.type)) %>%
  filter(!(Enemy.type %in% c("Myxozoan", "Rotifer"))) %>%
  ggplot(aes(x = Enemy.type))+
  scale_y_continuous(limits = c(0, 1050),
                     breaks = seq(0, 1250, 250),
                     expand = c(0, 0))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of effect sizes")+
  xlab("Parasite taxa")+
  theme_JR()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title.y = element_blank())

#host taxa
hostplot = fulldat2 %>%
  filter(complete.cases(Native.host.type)) %>%
  filter(!(Native.host.type %in% c("Echinoderm", "Coral"))) %>%
  ggplot(aes(x = Native.host.type))+
  scale_y_continuous(limits = c(0, 1800),
                     breaks = seq(0, 1500, 500),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Amphibian/Reptile", "Arthropod", "Bird", "Fish", "Mammal", "Mollusk", "Plant"))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of effect sizes")+
  xlab("Host taxa")+
  theme_JR()+
  theme(axis.text.x = element_text(angle = 30, hjust = .8, vjust = .9),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_blank())

#Human Parasite
humanplot = fulldat2 %>%
  filter(complete.cases(Human.Parasite)) %>%
  ggplot(aes(x = Human.Parasite))+
  scale_y_continuous(limits = c(0, 1900),
                     breaks = seq(0, 3000, 500),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Non-human\nparasite", "Human\nparasite"))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

fig1_WHOLE = cowplot::align_plots(gcdplot, paraplot, hostplot,
                                  venueplot, habplot, humanplot,
            align = 'hv', axis = 'l')

figure1 = cowplot::plot_grid(fig1_WHOLE[[1]], fig1_WHOLE[[2]], fig1_WHOLE[[3]],
                             fig1_WHOLE[[4]], fig1_WHOLE[[5]], fig1_WHOLE[[6]],
                             labels = c("A)","B)","C)","D)","E)", "F)"),
                             ncol = 3,
                             label_x = 0.23,
                             label_y = 0.975)

ggsave("./Figures/SummaryFigNEWUpdate.tiff",
       figure1,
       dpi = 300,
       width = 9,
       height = 6,
       scale = 1.25,
       units = "in")



##Supplemental?
#Ectothermic host
hostthemplot = fulldat2 %>%
  filter(complete.cases(Ectothermic.host)) %>%
  ggplot(aes(x = Ectothermic.host))+
  scale_y_continuous(limits = c(0, 1800),
                     breaks = seq(0, 1500, 500),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Endothermic\nHost", "Ectothermic\nHost"))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Vector
vectorplot = fulldat2 %>%
  filter(complete.cases(Vector)) %>%
  ggplot(aes(x = Vector))+
  scale_y_continuous(limits = c(0, 3000),
                     breaks = seq(0, 4000, 500),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Non-vector", "Vector"))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Vector-borne
vectorborneplot = fulldat2 %>%
  filter(complete.cases(Vector.borne)) %>%
  ggplot(aes(x = Vector.borne))+
  scale_y_continuous(limits = c(0, 2500),
                     breaks = seq(0, 2500, 500),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Non-vector-borne", "Vector-borne"))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Route
routeplot = fulldat2 %>%
  filter(complete.cases(Route)) %>%
  ggplot(aes(x = Route))+
  scale_y_continuous(limits = c(0, 2000),
                     breaks = seq(0, 2000, 500),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Complex\nTransmission", "Direct\nTransmission"))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))


#Free living stages
FLSplot = fulldat2 %>%
  filter(complete.cases(Free.living.stages)) %>%
  ggplot(aes(x = Free.living.stages))+
  scale_y_continuous(limits = c(0, 2200),
                     breaks = seq(0, 2500, 500),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("No Free Living\nStages", "Free Living\nStages"))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of Effect Sizes")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

###Endpoint plot here
endplot = fulldat2 %>%
  filter(complete.cases(Endpoint_Host_Parasite)) %>%
  ggplot(aes(x = Endpoint_Host_Parasite))+
  scale_y_continuous(limits = c(0, 2600),
                     breaks = seq(0, 3500, 500),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Host\nEndpoint", "Parasite\nEndpoint"))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Macroparasite
sizeplot = fulldat2 %>%
  filter(complete.cases(Macroparasite)) %>%
  ggplot(aes(x = Macroparasite))+
  scale_y_continuous(limits = c(0, 1700),
                     breaks = seq(0, 3000, 500),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Microparasite", "Macroparasite"))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Ectoparasite
paratypeplot = fulldat2 %>%
  filter(complete.cases(Ectoparasite)) %>%
  ggplot(aes(x = Ectoparasite))+
  scale_y_continuous(limits = c(0, 2500),
                     breaks = seq(0, 3000, 500),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Endoparasite", "Ectoparasite"))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of Effect Sizes")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15, hjust = 0.75))

##zoonotic plot
zoonplot = fulldat2 %>%
  filter(complete.cases(Zoonotic)) %>%
  ggplot(aes(x = Zoonotic))+
  scale_y_continuous(limits = c(0, 2500),
                     breaks = seq(0, 2500, 500),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Not Zoonotic", "Zoonotic"))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))


figsp_WHOLE = cowplot::align_plots(paratypeplot, hostthemplot,
                                   vectorplot, vectorborneplot,
                                   routeplot, FLSplot,
                                   endplot, sizeplot,
                                   zoonplot,
                                  align = 'hv', axis = 'l')
figureSpara = cowplot::plot_grid(figsp_WHOLE[[1]], figsp_WHOLE[[2]], figsp_WHOLE[[3]],
                                 figsp_WHOLE[[4]], figsp_WHOLE[[5]], figsp_WHOLE[[6]],
                                 figsp_WHOLE[[7]], figsp_WHOLE[[8]], figsp_WHOLE[[9]],
                             labels = c("A)","B)","C)","D)","E)","F)","G)","H)","I)"),
                             ncol = 5,
                             label_x = 0.22,
                             label_y = 0.975)

ggsave("./Figures/SummaryFigSParaNEWUpdate.tiff",
       figureSpara,
       dpi = 300,
       width = 6,
       height = 11.25,
       scale = 1.25,
       units = "in")

##Studies

#Global change driver
gcdplots = fulldat2 %>%
  group_by(Global.Change.Driver2) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Global.Change.Driver2, y = nstud))+
  scale_y_continuous(limits = c(0, 650),
                     breaks = seq(0,600,100),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Biodiversity change",
                              "Chemical pollution",
                              "Climate change",
                              "Habitat loss/change",
                              "Introduced species"))+
  geom_bar(aes(fill = Global.Change.Driver2), stat = "identity",
           color = "black", width = 0.5)+
  scale_fill_manual(values = c("#5E976E",
                                "#58355E",
                                "#FFCA3A",
                                "#EC0B43",
                                "#63ADF2"))+
  ylab("Number of studies")+
  xlab("Global change driver")+
  theme_JR()+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.8,vjust = 0.9),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.position = "none")


#Habitat
habplots = fulldat2 %>%
  filter(complete.cases(Habitat)) %>%
  group_by(Habitat) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Habitat, y = nstud))+
  scale_y_continuous(limits = c(0, 1000),
                     breaks = seq(0, 1000, 200),
                     expand = c(0, 0))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of studies")+
  xlab("Habitat")+
  theme_JR()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title.y = element_blank())

#Venue
venueplots =
  fulldat2 %>%
  group_by(Venue) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Venue, y = nstud))+
  scale_y_continuous(limits = c(0, 1000),
                     breaks = seq(0, 800, 200),
                     expand = c(0, 0))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of studies")+
  xlab("Venue")+
  theme_JR()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

#Parasite taxa
paraplots = fulldat2 %>%
  filter(complete.cases(Enemy.type)) %>%
  filter(!(Enemy.type %in% c("Myxozoan","Rotifer"))) %>%
  group_by(Enemy.type) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Enemy.type, y = nstud))+
  scale_y_continuous(limits = c(0, 335),
                     breaks = seq(0, 300, 50),
                     expand = c(0, 0))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of studies")+
  xlab("Parasite taxa")+
  theme_JR()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title.y = element_blank())

#host taxa
hostplots = fulldat2 %>%
  filter(complete.cases(Native.host.type)) %>%
  filter(!Native.host.type %in% c("Coral","Echinoderm")) %>%
  group_by(Native.host.type) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Native.host.type, y = nstud))+
  scale_y_continuous(limits = c(0, 650),
                     breaks = seq(0, 800, 100),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Amphibian/Reptile", "Arthropod", "Bird", "Fish", "Mammal", "Mollusk", "Plant"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of studies")+
  xlab("Host taxa")+
  theme_JR()+
  theme(axis.text.x = element_text(angle = 30, hjust = .8, vjust = .9),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_blank())

#Human Parasite
humanplots = fulldat2 %>%
  filter(complete.cases(Human.Parasite)) %>%
  group_by(Human.Parasite) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Human.Parasite, y = nstud))+
  scale_y_continuous(limits = c(0, 680),
                     breaks = seq(0, 600, 100),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Non-human\nparasite", "Human\nparasite"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

fig1_WHOLES = cowplot::align_plots(gcdplots, paraplots, hostplots,
                                   venueplots, habplots, humanplots,
                                  align = 'hv', axis = 'l')
figure1S = cowplot::plot_grid(fig1_WHOLES[[1]], fig1_WHOLES[[2]], fig1_WHOLES[[3]],
                              fig1_WHOLES[[4]], fig1_WHOLES[[5]], fig1_WHOLES[[6]],
                             labels = c("A)","B)","C)","D)","E)","F)"),
                             ncol = 3,
                             label_x = 0.2,
                             label_y = 0.975)

ggsave("./Figures/SummaryFigStudies3NEWupdate.tiff",
       figure1S,
       dpi = 300,
       width = 9,
       height = 6,
       scale = 1.25,
       units = "in")


####
#Ectothermic host
hostthemplots = fulldat2 %>%
  filter(complete.cases(Ectothermic.host)) %>%
  group_by(Ectothermic.host) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Ectothermic.host, y = nstud))+
  scale_y_continuous(limits = c(0, 675),
                     breaks = seq(0, 700, 100),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Endothermic\nHost", "Ectothermic\nHost"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Vector
vectorplots = fulldat2 %>%
  filter(complete.cases(Vector)) %>%
  group_by(Vector) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Vector, y = nstud))+
  scale_y_continuous(limits = c(0, 1000),
                     breaks = seq(0, 1000, 250),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Non-vector", "Vector"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Vector-borne
vectorborneplots = fulldat2 %>%
  filter(complete.cases(Vector.borne)) %>%
  group_by(Vector.borne) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Vector.borne, y = nstud))+
  scale_y_continuous(limits = c(0, 750),
                     breaks = seq(0, 700, 100),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Non-vector-borne", "Vector-borne"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Route
routeplots = fulldat2 %>%
  filter(complete.cases(Route)) %>%
  group_by(Route) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Route, y = nstud))+
  scale_y_continuous(limits = c(0, 650),
                     breaks = seq(0, 600, 100),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Complex\nTransmission", "Direct\nTransmission"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))


#Free living stages
FLSplots = fulldat2 %>%
  filter(complete.cases(Free.living.stages)) %>%
  group_by(Free.living.stages) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Free.living.stages, y = nstud))+
  scale_y_continuous(limits = c(0, 675),
                     breaks = seq(0, 750, 100),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("No Free Living\nStages", "Free Living\nStages"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of Studies")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Endpoint
Endplots = fulldat2 %>%
  filter(complete.cases(Endpoint_Host_Parasite)) %>%
  group_by(Endpoint_Host_Parasite) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Endpoint_Host_Parasite, y = nstud))+
  scale_y_continuous(limits = c(0,950),
                     breaks = seq(0, 1000, 250),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Host\nEndpoint", "Parasite\nEndpoint"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))


#Macroparasite
sizeplots = fulldat2 %>%
  filter(complete.cases(Macroparasite)) %>%
  group_by(Macroparasite) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Macroparasite, y = nstud))+
  scale_y_continuous(limits = c(0, 650),
                     breaks = seq(0, 600, 100),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Microparasite", "Macroparasite"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Ectoparasite
paratypeplots = fulldat2 %>%
  filter(complete.cases(Ectoparasite)) %>%
  group_by(Ectoparasite) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Ectoparasite, y = nstud))+
  scale_y_continuous(limits = c(0, 825),
                     breaks = seq(0, 800, 100),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Endoparasite", "Ectoparasite"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of Studies")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15, hjust = 0.75))

#zoonotic
zoonplots = fulldat2 %>%
  filter(complete.cases(Zoonotic)) %>%
  group_by(Zoonotic) %>%
  summarize(nstud = length(unique(Citation.number))) %>%
  ggplot(aes(x = Zoonotic, y = nstud))+
  scale_y_continuous(limits = c(0,700),
                     breaks = seq(0, 1000, 100),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Not Zoonotic", "Zoonotic"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))


figsp_WHOLES = cowplot::align_plots(paratypeplots, hostthemplots,
                                   vectorplots, vectorborneplots,
                                   routeplots, FLSplots,
                                   Endplots, sizeplots,
                                   zoonplots,
                                   align = 'hv', axis = 'l')

figureSparaS = cowplot::plot_grid(figsp_WHOLES[[1]], figsp_WHOLES[[2]], figsp_WHOLES[[3]],
                                  figsp_WHOLES[[4]], figsp_WHOLES[[5]], figsp_WHOLES[[6]],
                                  figsp_WHOLES[[7]], figsp_WHOLES[[8]], figsp_WHOLES[[9]],
                                 labels = c("J)","K)","L)","M)","N)","O)","P)","Q)","R)"),
                                 ncol = 5,
                                 label_x = 0.22,
                                 label_y = 0.975)

ggsave("./Figures/SummaryFigSParaStudiesNEWupdate.tiff",
       figureSparaS,
       dpi = 300,
       width = 6,
       height = 11.25,
       scale = 1.25,
       units = "in")

#####Number of parasite taxa
#Global change driver
gcdplott = fulldat2 %>%
  group_by(Global.Change.Driver) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  mutate(GCD = c("Biodiversity change",
           "Climate change",
           "Chemical pollution",
           "Habitat loss/change",
           "Introduced species")) %>%
  ggplot(aes(x = GCD, y = npara))+
  scale_y_continuous(limits = c(0, 550),
                     breaks = seq(0,1500,100),
                     expand = c(0, 0))+
  geom_bar(aes(fill = GCD), stat = "identity",
           color = "black", width = 0.5)+
  scale_fill_manual(values = c("#5E976E",
                                "#58355E",
                                "#FFCA3A",
                                "#EC0B43",
                                "#63ADF2"))+
  ylab("Number of parasite taxa")+
  xlab("Global change driver")+
  theme_JR()+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.8,vjust = 0.9),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(hjust = 1),
        legend.position = "none")


#Habitat
habplott = fulldat2 %>%
  filter(complete.cases(Habitat)) %>%
  group_by(Habitat) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  ggplot(aes(x = Habitat, y = npara))+
  scale_y_continuous(limits = c(0, 800),
                     breaks = seq(0, 2500, 250),
                     expand = c(0, 0))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of parasite taxa")+
  xlab("Habitat")+
  theme_JR()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title.y = element_blank())

#Venue
venueplott =
  fulldat2 %>%
  group_by(Venue) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  ggplot(aes(x = Venue, y = npara))+
  scale_y_continuous(limits = c(0, 800),
                     breaks = seq(0, 3000, 250),
                     expand = c(0, 0))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of parasite taxa")+
  xlab("Venue")+
  theme_JR()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

#Parasite taxa
paraplott = fulldat2 %>%
  filter(complete.cases(Enemy.type)) %>%
  filter(!(Enemy.type %in% c("Myxozoan","Rotifer"))) %>%
  group_by(Enemy.type) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  ggplot(aes(x = Enemy.type, y = npara))+
  scale_y_continuous(limits = c(0, 275),
                     breaks = seq(0, 800, 50),
                     expand = c(0, 0))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of parasite taxa")+
  xlab("Parasite taxa")+
  theme_JR()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title.y = element_blank())

#host taxa
hostplott = fulldat2 %>%
  filter(complete.cases(Native.host.type)) %>%
  filter(!(Native.host.type %in% c("Coral","Echinoderm"))) %>%
  group_by(Native.host.type) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  ggplot(aes(x = Native.host.type, y = npara))+
  scale_y_continuous(limits = c(0, 600),
                     breaks = seq(0, 1500, 100),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Amphibian/Reptile", "Arthropod", "Bird", "Fish", "Mammal", "Mollusk", "Plant"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of parasite taxa")+
  xlab("Host taxa")+
  theme_JR()+
  theme(axis.text.x = element_text(angle = 30, hjust = .8, vjust = .9),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_blank())


#Human Parasite
humanplott = fulldat2 %>%
  filter(complete.cases(Human.Parasite)) %>%
  group_by(Human.Parasite) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  ggplot(aes(x = Human.Parasite, y = npara))+
  scale_y_continuous(limits = c(0, 600),
                     breaks = seq(0, 2000, 200),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Non-human\nparasite", "Human\nparasite"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))


fig1_WHOLET = cowplot::align_plots(gcdplots, paraplots, hostplots,
                                   venueplots, habplots, humanplots,
                                   gcdplott, paraplott, hostplott,
                                   venueplott, habplott, humanplott,
                                   align = 'hv', axis = 'l')
figure1T = cowplot::plot_grid(fig1_WHOLET[[1]], fig1_WHOLET[[2]], fig1_WHOLET[[3]],
                              fig1_WHOLET[[4]], fig1_WHOLET[[5]], fig1_WHOLET[[6]],
                              fig1_WHOLET[[7]], fig1_WHOLET[[8]], fig1_WHOLET[[9]],
                              fig1_WHOLET[[10]], fig1_WHOLET[[11]], fig1_WHOLET[[12]],
                              labels = c("A)","B)","C)","D)","E)","F)",
                                         "G)","H)","I)","J)","K)","L)"),
                              ncol = 3,
                              label_x = 0.225,
                              label_y = 0.975)

ggsave("./Figures/SummaryFigStudiesParaTaxNEWupdate.tiff",
       figure1T,
       dpi = 300,
       width = 9,
       height = 12,
       scale = 1.25,
       units = "in")


####
#Ectothermic host
hostthemplott = fulldat2 %>%
  filter(complete.cases(Ectothermic.host)) %>%
  group_by(Ectothermic.host) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  ggplot(aes(x = Ectothermic.host, y = npara))+
  scale_y_continuous(limits = c(0, 500),
                     breaks = seq(0, 500, 100),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Endothermic\nHost", "Ectothermic\nHost"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Vector
vectorplott = fulldat2 %>%
  filter(complete.cases(Vector)) %>%
  group_by(Vector) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  ggplot(aes(x = Vector, y = npara))+
  scale_y_continuous(limits = c(0, 750),
                     breaks = seq(0, 600, 200),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Non-vector", "Vector"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Vector-borne
vectorborneplott = fulldat2 %>%
  filter(complete.cases(Vector.borne)) %>%
  group_by(Vector.borne) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  ggplot(aes(x = Vector.borne, y = npara))+
  scale_y_continuous(limits = c(0, 700),
                     breaks = seq(0, 600, 200),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Non-vector-borne", "Vector-borne"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Route
routeplott = fulldat2 %>%
  filter(complete.cases(Route)) %>%
  group_by(Route) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  ggplot(aes(x = Route, y = npara))+
  scale_y_continuous(limits = c(0, 600),
                     breaks = seq(0, 600, 200),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Complex\nTransmission", "Direct\nTransmission"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))


#Free living stages
FLSplott = fulldat2 %>%
  filter(complete.cases(Free.living.stages)) %>%
  group_by(Free.living.stages) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  ggplot(aes(x = Free.living.stages, y = npara))+
  scale_y_continuous(limits = c(0, 600),
                     breaks = seq(0, 600, 200),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("No Free Living\nStages", "Free Living\nStages"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of Parasite Taxa")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

##Endpoint
endplott = fulldat2 %>%
  filter(complete.cases(Endpoint_Host_Parasite)) %>%
  group_by(Endpoint_Host_Parasite) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  ggplot(aes(x = Endpoint_Host_Parasite, y = npara))+
  scale_y_continuous(limits = c(0, 750),
                     breaks = seq(0, 800, 200),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Host\nEndpoint", "Parasite\nEndpoint"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Macroparasite
sizeplott = fulldat2 %>%
  filter(complete.cases(Macroparasite)) %>%
  group_by(Macroparasite) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  ggplot(aes(x = Macroparasite, y = npara))+
  scale_y_continuous(limits = c(0, 500),
                     breaks = seq(0, 500, 100),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Microparasite", "Macroparasite"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15))

#Ectoparasite
paratypeplott = fulldat2 %>%
  filter(complete.cases(Ectoparasite)) %>%
  group_by(Ectoparasite) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  ggplot(aes(x = Ectoparasite, y = npara))+
  scale_y_continuous(limits = c(0, 600),
                     breaks = seq(0, 600, 200),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Endoparasite", "Ectoparasite"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of Parasite Taxa")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15, hjust = 0.75))

zoonplott = fulldat2 %>%
  filter(complete.cases(Zoonotic)) %>%
  group_by(Zoonotic) %>%
  summarize(npara = length(unique(Enemy_Random_Effect))) %>%
  ggplot(aes(x = Zoonotic, y = npara))+
  scale_y_continuous(limits = c(0, 600),
                     breaks = seq(0, 600, 200),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Not Zoonotic", "Zoonotic"))+
  geom_bar(stat="identity", color = "black", fill = "grey43", width = 0.5)+
  ylab("")+
  xlab("")+
  theme_JR()+
  theme(axis.title = element_text(size = 15, hjust = 0.75))

figsp_WHOLET = cowplot::align_plots(paratypeplott, hostthemplott,
                                    vectorplott, vectorborneplott,
                                    routeplott, FLSplott,
                                    endplott, sizeplott,
                                    zoonplott,
                                    align = 'hv', axis = 'l')

figureSparaT = cowplot::plot_grid(figsp_WHOLET[[1]], figsp_WHOLET[[2]], figsp_WHOLET[[3]],
                                  figsp_WHOLET[[4]], figsp_WHOLET[[5]], figsp_WHOLET[[6]],
                                  figsp_WHOLET[[7]], figsp_WHOLET[[8]], figsp_WHOLET[[9]],
                                  labels = c("S)","T)","U)","V)","W)","X)","Y)","Z)","a)"),
                                  ncol = 5,
                                  label_x = 0.22,
                                  label_y = 0.975)

ggsave("./Figures/SummaryFigSParaTaxaNEWupdate.tiff",
       figureSparaT,
       dpi = 300,
       width = 6,
       height = 11.25,
       scale = 1.25,
       units = "in")


figsuppswhole = cowplot::plot_grid(figureSpara,
                                   figureSparaS,
                                  figureSparaT,
                                  labels = NA,
                                  ncol = 1)


ggsave("./Figures/edf3.tiff",
       figsuppswhole,
       dpi = 300,
       width = 10.8333,
       height = 15,
       scale = 1.5,
       units = "in")




figsp_WHOLETOT = cowplot::align_plots(gcdplott, paraplott, hostplott,
                                    venueplott, habplott, humanplott,
                                    paratypeplott, hostthemplott,
                                    vectorplott, vectorborneplott,
                                    routeplott, FLSplott,
                                    endplott, sizeplott,
                                    zoonplott,
                                    align = 'hv', axis = 'l')

EDF2 = cowplot::plot_grid(figsp_WHOLETOT[[1]], figsp_WHOLETOT[[2]], figsp_WHOLETOT[[3]],
                          figsp_WHOLETOT[[4]], figsp_WHOLETOT[[5]], figsp_WHOLETOT[[6]],
                          figsp_WHOLETOT[[7]], figsp_WHOLETOT[[8]], figsp_WHOLETOT[[9]],
                          figsp_WHOLETOT[[10]], figsp_WHOLETOT[[11]], figsp_WHOLETOT[[12]],
                          figsp_WHOLETOT[[13]], figsp_WHOLETOT[[14]], figsp_WHOLETOT[[15]],
                              labels = c("A)","B)","C)",
                                         "D)","E)","F)",
                                         "G)","H)","I)",
                                         "J)","K)","L)",
                                         "M)",'N)',"O)"),
                              ncol = 3,
                              label_x = 0.225,
                              label_y = 0.975)

ggsave("./Figures/edf2.tiff",
       EDF2,
       dpi = 300,
       width = 9,
       height = 15,
       scale = 1.25,
       units = "in")
