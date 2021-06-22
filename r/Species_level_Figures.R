#Script to create Figures 1 and S1

library(ggplot2)
library(ggpubr)
library(cowplot)
library(viridis)
library(RColorBrewer)
library(gridExtra)
library(ggExtra)
library(ggridges)
library(patchwork)
library(tidyverse)

#Loading data
load("data/species_data.RData")

#Theme
white_theme <-theme(axis.ticks=element_line(colour="black"),
                       axis.text=element_text(size=10,colour="black"),
                       axis.title=element_text(size=12,face="bold"),
                       panel.grid.minor=element_blank(),
                       panel.background=element_rect(fill="white",colour="black"),
                       plot.background=element_rect(fill="transparent",colour=NA),
                       legend.key = element_rect(fill = "white"))

#Color palette
myPalette <- colorRampPalette(rev(brewer.pal(4, "Spectral")))

#Figure 1A

#Determine the number of fish species in each quadrant 
length(which(species_data$Climate_V.index<=50 & species_data$Fishing_V>50))/nrow(species_data) #1st quadrant
length(which(species_data$Climate_V.index>50 & species_data$Fishing_V>50))/nrow(species_data) #2nd quadrant
length(which(species_data$Climate_V.index<=50 & species_data$Fishing_V<=50))/nrow(species_data) #3rd quadrant
length(which(species_data$Climate_V.index>50 & species_data$Fishing_V<=50))/nrow(species_data) #4th quadrant

#Plot % of species in each quadrat
combined <- ggplot(species_data, aes(x = Climate_V.index, y = Fishing_V, colour = EnvTemp)) +
  stat_density2d( aes(fill = ..density..), geom = "raster", contour = FALSE, n = 200) +  theme(legend.position="none") +
  scale_fill_continuous(low = "#ffffff", high = "#075482")+
  geom_segment(aes(x = 50, y = 0, xend = 50 , yend =100),linetype="dashed",size=0.5,color="black")+
  geom_segment(aes(x = 0, y = 50, xend = 100 , yend =50),linetype="dashed",size=0.5,color="black")+
  geom_point(alpha=0,size=0,shape=16) +
  scale_y_continuous("Vulnerability to fishing",breaks=c(0,25,50,75,100),labels=c("0","25","50","75","100")) +
  scale_x_continuous("Vulnerability to climate change",breaks=c(0,25,50,75,100),labels=c("0","25","50","75","100")) +
  geom_smooth(aes(x = Climate_V.index, y = Fishing_V), colour = "black",method = "lm", se = F, size=1,linetype="dashed")+
  geom_smooth(aes(colour = EnvTemp), method = "lm", se = F, size=1.5)+
  #1st quadrat
  annotate("text", x = 25, y = 75, size=4, label = "22%", fontface = c("bold")) +
  #2nd quadrat
  annotate("text", x = 75, y = 75, size=4, label = "25%", fontface = c("bold")) +
  #3rd quadrat
  annotate("text", x = 25, y = 25, size=4, label = "26%", fontface = c("bold")) +
  #4th quadrat
  annotate("text", x = 75, y = 25, size=4, label = "27%", fontface = c("bold")) +
  #annotate("text", x = 1, y = 100, size=6, label = "a", fontface = c("bold")) +
  scale_colour_viridis(discrete=T, name="",labels=c("Cold","Subtropical","Temperate","Tropical")) +
  white_theme

panelA <- ggMarginal(combined, groupColour = T, xparams = list(size=1.5), yparams = list(size=1.5))
#panelA

#Figure 1B and legend
panelB <- ggplot(species_data,aes(y=Fishing_V, x=Climate_V.index,z=MicronutDensityScore) ) +
  stat_summary_2d() + 
  scale_fill_gradientn(name = "Micronutrient\ndensity (%)", colours = myPalette(100),limits=c(41,313)) +
  geom_point(shape = 16,alpha=0)+
  geom_segment(aes(x = 50, y = 0, xend = 50 , yend =100),linetype="dashed",size=0.5,color="darkgrey")+
  geom_segment(aes(x = 0, y = 50, xend = 100 , yend =50),linetype="dashed",size=0.5,color="darkgrey")+
  scale_y_continuous("Vulnerability to fishing",breaks=c(0,25,50,75,100),labels=c("0","25","50","75","100")) +
  scale_x_continuous("Vulnerability to climate change",breaks=c(0,25,50,75,100),labels=c("0","25","50","75","100")) +
  white_theme + theme(legend.position="none")

#panelB

pleg <- ggplot(species_data,aes(y=Fishing_V, x=Climate_V.index,z=MicronutDensityScore) ) +
  geom_segment(aes(x = 50, y = 0, xend = 50 , yend =100),size=0.5,color="darkgrey")+
  geom_segment(aes(x = 0, y = 50, xend = 100 , yend =50),size=0.5,color="darkgrey")+
  stat_summary_2d() + 
  scale_fill_gradientn(name = "Micronutrient\ndensity (%)", colours = myPalette(100),limits=c(41,313),
                       breaks=c(30,100,200,300,360),labels=c("30","100","200","300","360"))+
  geom_point(shape = 16,alpha=0.3)+
  scale_y_continuous("Vulnerability to fishing",breaks=c(0,25,50,75,100),labels=c("0","25","50","75","100")) +
  scale_x_continuous("Vulnerability to climate change",breaks=c(0,25,50,75,100),labels=c("0","25","50","75","100")) +
  white_theme 

legend_nut <- get_legend(
  pleg + 
    theme(legend.position = "bottom",legend.justification="center",legend.key.width = unit(3,"line"),
          legend.text=element_text(size=10),legend.title = element_text(size=12 ,face="bold"))
)

species_data$EnvTemp <- factor(species_data$EnvTemp,levels = c("polar_deep","temperate","subtropical","tropical"))

panelC <- ggplot(species_data, aes(y=MicronutDensityScore, x=EnvTemp, fill=EnvTemp)) +
  geom_violin(color="black")+
  scale_fill_viridis(discrete=T,name="Thermal regimes",labels=c("Cold","Temperate","Subtropical","Tropical")) +
  scale_y_continuous("Micronutrient density (%)",breaks=c(40,100,200,300),limits=c(35, 320),labels=c("40","100","200","300")) +
  scale_x_discrete("Thermal regime",labels=c("Cold","Temperate","Subtropical","Tropical"))+
  white_theme + theme(legend.position="none")

panelC

#Export Figure 1
tiff("figures/Figure_1.tiff", width=3600, height=1500, compression="lzw", res=300)
cowplot::plot_grid(panelA,panelB,panelC,
                   NULL,legend_nut,NULL,
                   nrow = 2, ncol = 3, rel_heights = c(1,.1), rel_widths = c(1,1,1),labels = c('A', 'B','C'),label_size=12)
graphics.off()

###################################################################################################
#Supplemental figures

#Figure S1

#theme
theme_set(theme_classic() + 
            theme(axis.text = element_text(colour='black'), 
                  axis.title = element_text(colour='black')))

#Color code
nut.cols<-c('Calcium'='#de2d26', 'Iron'='#636363', 'Zinc'='#3182bd', 'Vitamin A'='#31a354',
            'Selenium' = '#776EB0')

## estimate average nutrients per functional group (EnvTemp)
thermal <- species_data %>% select(species,EnvTemp,MicronutDensityScore,rda_Calcium,rda_Iron,rda_Selenium,rda_Zinc,rda_Vitamin_A) %>%
  group_by(EnvTemp) %>%
  dplyr::summarise(across(MicronutDensityScore:rda_Vitamin_A, list(mean = ~mean(.x)))) %>% # get mean per EnvTemp
  ## turn to long format
  pivot_longer(rda_Calcium_mean:rda_Vitamin_A_mean, names_to = 'nutrient', values_to = 'rda') %>%
  dplyr::group_by(EnvTemp) %>%
  dplyr::mutate(label_ypos=cumsum(rda) - 0.5*rda) ## for ggplot - label each fill segment, in the middle

## arrange levels 
thermal$nutrient<-factor(thermal$nutrient, levels = rev(unique(thermal$nutrient)))
thermal$lab<-thermal$nutrient; levels(thermal$lab) <-rev(c('Calcium', 'Iron', 'Selenium', 'Zinc', 'Vitamin A')) 
thermal$EnvTemp<-fct_reorder(thermal$EnvTemp, thermal$MicronutDensityScore_mean)

## plot EnvTemp nutrient adequacy (panel A)
gEnvTemp <-ggplot(thermal, aes(EnvTemp, rda)) + 
  geom_bar(stat='identity', aes(fill=lab),alpha=0.9) +
  geom_text(data = thermal %>% filter(rda>5) , 
            aes(y = label_ypos, label= paste0(round(rda, 0), '%')),  color="white", size=4)+
  annotate(geom="text", y=thermal$label_ypos[which(thermal$EnvTemp=="tropical")], x=4.5, label=c('Calcium', 'Iron', 'Selenium', 'Zinc', 'Vitamin A'),
           color=c('Calcium'='#de2d26', 'Iron'='#636363','Selenium' = '#776EB0', 'Zinc'='#3182bd', 'Vitamin A'='#31a354'),fontface='bold')+
  #geom_text(aes(EnvTemp, label_ypos, label = lab, col=lab), size=4.5, fontface='bold') +
  coord_flip() +
  guides(fill=FALSE) +
  scale_fill_manual(values=nut.cols) +
  scale_y_continuous(name="Micronutrient density (%)",breaks=c(0,50,100,150,200,250,300,350),limits=c(0, max(thermal$MicronutDensityScore_mean + 10)),
                     labels=c("0","50","100","150","200","250","300","350")) +
  scale_x_discrete(name="Thermal regime",labels=c("Cold","Temperate","Subtropical","Tropical"))+
  theme(axis.text=element_text(size=13,colour="black"),
        axis.title=element_text(size=16,face="bold"))

#Ridge plots (panels B-E)
cols <- c('rda_Calcium'='#de2d26', 'rda_Iron'='#636363', 'rda_Zinc'='#3182bd', 'rda_Vitamin_A'='#31a354','rda_Selenium' = '#776EB0')

thermalr <- species_data %>% select(EnvTemp,MicronutDensityScore,rda_Calcium,rda_Iron,rda_Selenium,rda_Zinc,rda_Vitamin_A) %>%
  group_by(EnvTemp) %>%
  pivot_longer(rda_Calcium:rda_Vitamin_A, names_to = 'nutrient', values_to = 'rda') 

thermalr$inter <- interaction(thermalr$EnvTemp,thermalr$nutrient)

thermalr$inter <- factor(thermalr$inter,levels =  c("polar_deep.rda_Vitamin_A",
                                                    "polar_deep.rda_Zinc",
                                                    "polar_deep.rda_Selenium",
                                                    "polar_deep.rda_Iron",
                                                    "polar_deep.rda_Calcium",
                                                    "temperate.rda_Vitamin_A",
                                                    "temperate.rda_Zinc",
                                                    "temperate.rda_Selenium",
                                                    "temperate.rda_Iron",
                                                    "temperate.rda_Calcium",
                                                    "subtropical.rda_Vitamin_A",
                                                    "subtropical.rda_Zinc",
                                                    "subtropical.rda_Selenium",
                                                    "subtropical.rda_Iron",
                                                    "subtropical.rda_Calcium",
                                                    "tropical.rda_Vitamin_A",
                                                    "tropical.rda_Zinc",
                                                    "tropical.rda_Selenium",
                                                    "tropical.rda_Iron",
                                                    "tropical.rda_Calcium"))

tro <- thermalr[which(thermalr$EnvTemp=="tropical"),]  

tr <- ggplot(tro, aes(y = inter)) +
  geom_density_ridges(aes(x = rda, fill = nutrient),alpha = .8, color = "white", from = 0, to = max(tro$rda)) +
  scale_x_continuous(name=" ",breaks=c(0,50,100))+
  scale_y_discrete(name="Tropical",labels=NULL)+
  scale_fill_manual(name = "Micronutrient",values=cols,labels=c("Calcium","Iron","Selenium","Vitamin A","Zinc")) +
  theme_classic()+
  theme(legend.position="none",panel.grid.major.x = element_line(),axis.text=element_text(size=13,colour="black"),
        axis.title=element_text(size=13)) 

tr

#
sub <- thermalr[which(thermalr$EnvTemp=="subtropical"),]  

su <- ggplot(sub, aes(y = inter)) +
  geom_density_ridges(aes(x = rda, fill = nutrient),alpha = .8, color = "white", from = 0, to = max(sub$rda)) +
  scale_x_continuous(name=" ",breaks=c(0,50,100))+
  scale_y_discrete(name="Subtropical",labels=NULL)+
  scale_fill_manual(name = "Micronutrient",values=cols,labels=c("Calcium","Iron","Selenium","Vitamin A","Zinc")) +
  theme_classic()+
  theme(legend.position="none",panel.grid.major.x = element_line(),axis.text=element_text(size=13,colour="black"),
        axis.title=element_text(size=13))

su

#
temp <- thermalr[which(thermalr$EnvTemp=="temperate"),]  

tem <- ggplot(temp, aes(y = inter)) +
  geom_density_ridges(aes(x = rda, fill = nutrient),alpha = .8, color = "white", from = 0, to = max(temp$rda)) +
  scale_x_continuous(name=" ",breaks=c(0,50,100))+
  scale_y_discrete(name="Temperate",labels=NULL)+
  scale_fill_manual(name = "Micronutrient",values=cols,labels=c("Calcium","Iron","Selenium","Vitamin A","Zinc")) +
  #theme_classic()+
  theme(legend.position="none",panel.grid.major.x = element_line(),axis.text=element_text(size=13,colour="black"),
        axis.title=element_text(size=13))

tem

cold <- thermalr[which(thermalr$EnvTemp=="polar_deep"),]  

col <- ggplot(cold, aes(y = inter)) +
  geom_density_ridges(aes(x = rda, fill = nutrient),alpha = .8, color = "white", from = 0, to = max(cold$rda)) +
  scale_x_continuous(name=" ",breaks=c(0,50,100))+
  scale_y_discrete(name="Cold",labels=NULL)+
  scale_fill_manual(name = "Micronutrient",values=cols,labels=c("Calcium","Iron","Selenium","Vitamin A","Zinc")) +
  theme_classic()+
  theme(legend.position="none",panel.grid.major.x = element_line(),axis.text=element_text(size=13,colour="black"),
        axis.title=element_text(size=13))

col

#export Figure S1
patchwork <- (gEnvTemp ) | (tr / su / tem / col)

jpeg("figures/Figure_S1.jpeg", res=300, width=4000, height=2400)
patchwork + plot_layout(widths = c(2, 1), guides = "auto") + plot_annotation(tag_levels = 'A') 
graphics.off()


#Figure S2 - Micronutrient density score versus each fishing and climate change vulnerability
CC <- ggplot(species_data, aes(y=MicronutDensityScore, x=Climate_V.index)) +
  annotate("text", x = 0, y = 400, size=6, label = "a", fontface = c("bold")) +
  geom_point(alpha=0.7,size=2)+
  #labs(x="",xlab=c("Cold","Subtropical","Temperate","Tropical"))+
  #scale_fill_viridis(discrete=T,name="Thermal regimes",labels=c("Cold","Subtropical","Temperate","Tropical")) +
  scale_y_continuous("Micronutrient density (%)",breaks=c(0,100,200,300,400),limits=c(0,400),labels=c("0","100","200","300","400")) +
  scale_x_continuous("Vulnerability to climate change",breaks=c(0,25,50,75,100),labels=c("0","25","50","75","100"),limits = c(0,100)) +
  geom_smooth(method = "lm", se = T, size=1,color="red")+
  white_theme

CC

Fishing <- ggplot(species_data, aes(y=MicronutDensityScore, x=Fishing_V)) +
  geom_point(alpha=0.7,size=2)+
  annotate("text", x = 0, y = 400, size=6, label = "b", fontface = c("bold")) +
  scale_y_continuous("Micronutrient density (%)",breaks=c(0,100,200,300,400),limits=c(0,400),labels=c("0","100","200","300","400")) +
  scale_x_continuous("Vulnerability to fishing",breaks=c(0,25,50,75,100),labels=c("0","25","50","75","100"),limits = c(0,100)) +
  geom_smooth(method = "lm", se = T, size=1,color="red")+
  white_theme

Fishing

jpeg("figures/Figure_S2.jpeg", res=400, width=4800, height=2400)
plot_grid(CC,Fishing, ncol =2)
graphics.off()

#end of script