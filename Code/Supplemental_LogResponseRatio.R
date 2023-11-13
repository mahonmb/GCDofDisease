##Repeat first two sets of analyses with LRR instead of SMDH
library(tidyverse); library(emmeans); library(metafor); library(cowplot)

fulldat2 <- read.csv("./Data/FinalDataWRound2.csv")


##Tuesday - need to flip sign of LRR to match SMHD
m2lrr <- rma.mv(LRR_yi, LRR_viC,
             mods = ~  Global.Change.Driver - 1,
             random = list(~1|Citation.number, ~1|ind_id),
             data = fulldat2)

m2lrrro <- robust(m2lrr, cluster=Citation.number, clubSandwich=TRUE)
m2lrrro

##Set up emmeans reference grid
m2glrr <- qdrg(object = m2lrrro, data = fulldat2)


m2emlrr <- emmeans(m2glrr, ~ Global.Change.Driver)
emmeans:::cld.emmGrid(m2emlrr)

figdat_GCDlrr <- data.frame(m2emlrr) %>%
  filter(Global.Change.Driver != "BC")
figdat_GCDlrr$grouping = c("*A", "*A", "*B", "*A")
figdat_GCDlrr$EffLab = c(3,4,2,1)

#create a new column for GCD labels used in figure 
figdat_GCDlrr$Global.Change.Driver2 <- c(
                                      "Climate Change", 
                                      "Chemical Pollution", 
                                      "Habitat Loss or Change",
                                      "Introduced Species")


figdat_GCDlrr$k = (fulldat2 %>%
                     filter(!is.na(LRR_yi) & !is.na(LRR_vi)) %>%
                  group_by(Global.Change.Driver) %>%
                  summarize(count = n()))$count

figdat_GCDlrr$n = (fulldat2 %>%
                     filter(!is.na(LRR_yi) & !is.na(LRR_vi)) %>%
                  group_by(Global.Change.Driver) %>%
                  summarize(count = length(unique(Citation.number))))$count


figdat_GCDlrr$GCD_xlab <- paste(figdat_GCDlrr$Global.Change.Driver2, "\n(n = ", 
                             figdat_GCDlrr$n, ", k = ",
                             figdat_GCDlrr$k, ")", sep = "")

#make the plot
EDF3A <- ggplot(figdat_GCDlrr, aes(x = GCD_xlab, 
                       y = emmean,
                       color = Global.Change.Driver2)) +
  geom_hline(yintercept = 0,                   #make a reference line at 0
             linetype = "dashed", 
             color = "black", 
             linewidth = 1) +
  geom_point(size = 3) +                               #plot points
  geom_errorbar(aes(ymin = asymp.LCL,        #include error bars
                    ymax = asymp.UCL),     
                width=0.25,
                size = 1) +                   #adjust width of bar
  geom_text(aes(label = grouping,
                x = EffLab),
            color = "black",
            position = position_nudge(x = 0.25,  y = 0.025))+
  # scale_y_continuous(limits = c(-0.275,1.05),      #change limits of Y axis
  #                    breaks = seq(-0.25,1,0.25))+      #set Y axis breaks
  
  scale_color_manual(values = c(#"#5E976E",
                                         "#58355E",
                                         "#FFCA3A",
                                         "#EC0B43",
                                         "#63ADF2"))+
                                           xlab("Global Change Driver") +               #relabel X and Y axes
  ylab("Log Response Ratio") +
  scale_x_discrete(limits = rev(levels(as.factor(figdat_GCDlrr$GCD_xlab))))+
  coord_flip()+
  theme_JR()+                                   #call your theme
  theme(legend.position = "none",
        axis.title.y = element_blank())

EDF3A

# Subdrivers 
###Need to remove some Effects, due to lack of replication
## Drop Fungicide & Sulfur containing
fulldat2ELLR <- fulldat2 %>%
  filter(!is.na(LRR_yi) & !is.na(LRR_viC)) %>%
  filter(!(Effect %in% c("Fungicide", "Sulfur Containing Compound")))

sort(unique(fulldat2ELLR$Effect))

mGCDriver_sublrr <- rma.mv(LRR_yi, LRR_viC,
                        mods = ~  Effect - 1,
                        random = list(~1|Citation.number, ~1|ind_id),
                        data = fulldat2ELLR)

mGCDriver_sublrrro <- robust(mGCDriver_sublrr, cluster=Citation.number, clubSandwich=TRUE)
mGCDriver_sublrrro


figdat_sublrr <- data.frame(est = mGCDriver_sublrrro$beta,
                         ci.low = mGCDriver_sublrrro$ci.lb,
                         ci.up = mGCDriver_sublrrro$ci.ub,
                         GCD = row.names(mGCDriver_sublrrro$beta))

figdat_sublrr$Effect = as.factor(gsub('Effect', '', figdat_sublrr$GCD))

figdat_sublrr$GCD = fct_rev(fulldat2ELLR$Global.Change.Driver[match(figdat_sublrr$Effect,
                                                                fulldat2ELLR$Effect)])

#
figdat_sublrr$GCD = fct_rev(figdat_sublrr$GCD)

figdat_sublrr$Effect <- str_to_sentence(figdat_sublrr$Effect)

figdat_sublrr$Effect[3] = "CO2"
figdat_sublrr$Effect[2] = "Climate variability or ENSO"
figdat_sublrr$Effect[17] = "UVB"

figdat_sublrr$Effect = as.factor(figdat_sublrr$Effect)

subknlrr = (fulldat2ELLR %>%
           group_by(Effect) %>%
           summarize(kcount = n(),
                     ncount = length(unique(Citation.number))))

figdat_sublrr$n = subknlrr$kcount[c(2,3,1,4:15,17,16)]
figdat_sublrr$k = subknlrr$kcount[c(2,3,1,4:15,17,16)]

figdat_sublrr$GCD_xlab <- as.factor(paste(figdat_sublrr$Effect, "\n(n = ", 
                                       figdat_sublrr$n, ", k = ",
                                       figdat_sublrr$k, ")", sep = ""))
# 


figdat_sublrr$GCD_xlab = factor(figdat_sublrr$GCD_xlab,
                             levels(figdat_sublrr$GCD_xlab)[c(14,10,5,
                                                              16,7,4,
                                                              12,11,2,1,
                                                              17,15,13,9,8,6,3
                                                              )])

# figdat_sub$GCD = factor(figdat_sub$GCD,
#                         levels(figdat_sub$GCD)[c(5,3,4,2,1)])


#make the plot
EDF3B <- ggplot(figdat_sublrr, aes(x = GCD_xlab, 
                       y=est,
                       color = GCD)) +            
  geom_hline(yintercept = 0,                   #make a reference line at 0
             linetype = "dashed", 
             color = "black", 
             size = 1) +
  geom_point(size = 2.5) +                               #plot points
  geom_errorbar(aes(ymin = ci.low,        #include error bars
                    ymax = ci.up),     
                width=0.33,
                linewidth = 0.6) +                   #adjust width of bar
  scale_y_continuous(limits = c(-4.25,6),      #change limits of Y axis
                     breaks = seq(-6,8,2))+      #set Y axis breaks
  xlab("") +               #relabel X and Y axes
  ylab("Log Response Ratio") +
  scale_color_manual(values = c(#"#5E976E",
                                         "#FFCA3A",
                                         "#58355E",
                                         "#EC0B43",
                                         "#63ADF2"))+
  coord_flip()+
  # scale_x_discrete(limits = rev(levels(figdat_sublrr$GCD_xlab)))+
  theme_JR()+                                   #call your theme
  theme(legend.position = "none",
        axis.text.y = element_text(size = 9),
        axis.title.y = element_blank())

EDF3B

EDF3_WHOLE = cowplot::align_plots(EDF3A,
                                  EDF3B,
                                  align = 'hv', axis = 'l')
EDF3_fig = cowplot::plot_grid(EDF3_WHOLE[[1]], 
                              EDF3_WHOLE[[2]], 
                              labels = c("A)","B)"),
                              ncol = 1,
                              rel_heights = c(0.5,1),
                              label_x = 0,
                              label_y = 0.975)
EDF3_fig



ggsave("./Figures/EDF4.tiff",
       dpi = 300,
       width = 4.4,
       height = 7.5,
       units = "in")
