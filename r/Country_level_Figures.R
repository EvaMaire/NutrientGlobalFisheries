#Script to produce Figures 2, 4, S3-S5

#Libraries
library(ggrepel)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(gridExtra)
library(plyr)
library(tidyverse)
library(ggExtra)
library(patchwork)

#Load data
load("data/country_data.RData")

#Rename
revalue(country_data$countryISO, c("Saint Helena, Ascension and Tristan da Cunha" = "SHN",
                                "French Southern Territories" = "ATF",
                                "Heard & McDonald Isl." = "HMD",
                                "Svalbard and Jan Mayen" = "SJM",
                                "Saint Pierre & Miquelon" ="SPM",
                                "American Samoa"="US Samoa",
                                "British Virgin Isl." ="VGB",
                                "Bosnia & Herzegovina" = "BIH",
                                "Papua New Guinea"="PNG",
                                "South Georgia & Sandwich Isl." ="SGS",
                                "Northern Marianas" = "MNP",
                                "Congo (ex-Zaire)" = "Congo",
                                "United Arab Emirates" = "UAE",
                                "Dominican Republic" = "Dominican Rep.",
                                "Equatorial Guinea" = "Eq. Guinea",
                                "Antigua & Barbuda" = "ATG",
                                "Sao Tome and Principe" = "STP",
                                "US Minor Outlying Isl."="UMI",
                                "Wallis & Futuna Isl."="WLF",
                                "Mozambique"="MOZ")) -> country_data$countryISO

#Color palette
myPalette <- colorRampPalette(rev(brewer.pal(4, "Spectral")))

#Theme
white_theme <-theme(axis.ticks=element_line(colour="black"),
                    axis.text=element_text(size=10,colour="black"),
                    axis.title=element_text(size=12,face="bold"),
                    panel.grid.minor=element_blank(),
                    panel.background=element_rect(fill="white",colour="black"),
                    plot.background=element_rect(fill="transparent",colour=NA),
                    legend.key = element_rect(fill = "white",colour=NA))

#Figure 2
#Plot both vulnerabilities with size as a function of micronutrient evenness and color as a function of micronutrient density
#Quartile 1
q1 <- which(country_data$quartile==1)
alp <- rep(0.1,nrow(country_data))
alp[q1] <- 0.9

#Create a new column which identifies which countries to display
country_data$lab <- rep(0,nrow(country_data))

country_data$lab[which(country_data$countryISO=="Seychelles")] <- 1
country_data$lab[which(country_data$countryISO=="Bahamas")] <- 1
country_data$lab[which(country_data$countryISO=="Greenland")] <- 1
country_data$lab[which(country_data$countryISO=="VGB")] <- 1

country_data$lab[which(country_data$countryISO=="SGS")] <- 1
country_data$lab[which(country_data$countryISO=="Finland")] <- 1
country_data$lab[which(country_data$countryISO=="Estonia")] <- 1
country_data$lab[which(country_data$countryISO=="Latvia")] <- 1

country_data$lab[which(country_data$countryISO=="Bulgaria")] <- 1
country_data$lab[which(country_data$countryISO=="Japan")] <- 1
country_data$lab[which(country_data$countryISO=="North Korea")] <- 1

quartile1 <- ggplot(country_data, aes(y=VulnF_W, x=VulnCC_W, fill=MicronutDensityScore_W, size = MicronutEvenness)) +
  geom_segment(aes(x = 50, y = 0, xend = 50 , yend =75),size=0.5,color="darkgrey")+
  geom_segment(aes(x = 0, y = 50, xend = 75 , yend =50),size=0.5,color="darkgrey")+
  annotate("text", x = 1, y = 75, size=6, label = "A", fontface = c("bold")) +
  geom_point(shape = 21,alpha=alp)+
  scale_size_area(breaks=c(0.1,.2,.4,.6), max_size=15, name='Nutrient evenness')+
  scale_fill_gradientn(colours = myPalette(100), limits=c(100,215))+
  scale_y_continuous("Vulnerability to fishing",breaks=c(0,25,50,75),labels=c("0","25","50","75"),limits=c(0,75)) +
  scale_x_continuous("Vulnerability to climate change",breaks=c(0,25,50,75),labels=c("0","25","50","75"),limits=c(0,75)) +
  geom_text_repel(data = country_data %>% filter(lab == 1) , aes(label=countryISO),
                  direction = "both",
                  # Add extra padding around each text label.
                  box.padding = unit(.6, 'cm'),
                  # Color of the line segments.
                  segment.color = 'black',
                  # Width and transparency of the line segments.
                  segment.alpha= .3,
                  min.segment.length = 0.2,
                  # Strength of the repulsion force.
                  force = 15,
                  # Maximum iterations of the naive repulsion algorithm O(n^2).
                  max.iter = 1e6,max.overlaps=20,
                  size=5)+
  white_theme + theme(legend.position="none")

quartile1

#Top quartile
q4 <- which(country_data$quartile == 4)
alp4 <- rep(0.1,nrow(country_data))
alp4[q4] <- 0.9

#Create a new column which identifies which countries to display
country_data$lab <- rep(0,nrow(country_data))

country_data$lab[which(country_data$countryISO=="Cambodia")] <- 1
country_data$lab[which(country_data$countryISO=="Kiribati")] <- 1
country_data$lab[which(country_data$countryISO=="Malaysia")] <- 1
country_data$lab[which(country_data$countryISO=="Timor Leste")] <- 1

country_data$lab[which(country_data$countryISO=="Namibia")] <- 1
country_data$lab[which(country_data$countryISO=="BIH")] <- 1
country_data$lab[which(country_data$countryISO=="Namibia")] <- 1
country_data$lab[which(country_data$countryISO=="MOZ")] <- 1
country_data$lab[which(country_data$countryISO=="Sierra Leone")] <- 1

country_data$lab[which(country_data$countryISO=="Mauritania")] <- 1
country_data$lab[which(country_data$countryISO=="Peru")] <- 1
country_data$lab[which(country_data$countryISO=="Georgia")] <- 1

country_data$lab[which(country_data$countryISO=="Indonesia")] <- 1
country_data$lab[which(country_data$countryISO=="Kiribati")] <- 1

quartile4 <- ggplot(country_data, aes(y=VulnF_W, x=VulnCC_W, fill=MicronutDensityScore_W, size = MicronutEvenness)) +
  geom_segment(aes(x = 50, y = 0, xend = 50 , yend =75),size=0.5,color="darkgrey")+
  geom_segment(aes(x = 0, y = 50, xend = 75 , yend =50),size=0.5,color="darkgrey")+
  annotate("text", x = 1, y = 75, size=6, label = "B", fontface = c("bold")) +
  geom_point(shape = 21,alpha=alp4)+
  scale_size_area(breaks=c(0.1,.2,.4,.6), max_size=15, name='Nutrient evenness')+
  scale_fill_gradientn(colours = myPalette(100), limits=c(100,215))+
  scale_y_continuous("",breaks=c(0,25,50,75),labels=c("0","25","50","75"),limits=c(0,75)) +
  scale_x_continuous("Vulnerability to climate change",breaks=c(0,25,50,75),labels=c("0","25","50","75"),limits=c(0,75)) +
  geom_text_repel(data = country_data %>% filter(lab == 1) , aes(label=countryISO),
                  direction = "both",
                  box.padding = unit(.6, 'cm'),
                  segment.color = 'black',
                  segment.alpha= .3,
                  min.segment.length = 0.3,
                  force = 15,
                  max.iter = 1e6,max.overlaps=20,
                  size=5)+
  white_theme + theme(legend.position="none")

quartile4

#legend
pleg2 <- ggplot(country_data, aes(y=VulnF_W, x=VulnCC_W, colour=MicronutDensityScore_W, size = MicronutEvenness)) +
  geom_segment(aes(x = 50, y = 0, xend = 50 , yend =75),size=0.5,color="darkgrey")+
  geom_segment(aes(x = 0, y = 50, xend = 75 , yend =50),size=0.5,color="darkgrey")+
  scale_size_area(breaks=c(0.1,.2,.4,.6), max_size=15, name='Micronutrient\nevenness')+
  scale_colour_gradientn(colours = myPalette(100), limits=c(100,215),
                         name="Micronutrient\ndensity (%)",breaks=c(100,150,200),labels=c("100","150","200"))+
  geom_point(shape = 21,alpha=0.9)+
  scale_y_continuous("Vulnerability to fishing",breaks=c(0,25,50,75),labels=c("0","25","50","75"),limits=c(0,75)) +
  scale_x_continuous("Vulnerability to climate change",breaks=c(0,25,50,75),labels=c("0","25","50","75"),limits=c(0,75)) +
  white_theme

p <- pleg2 + guides(colour = guide_colourbar(order=1),
                    size = guide_legend(order=2))

legend_b2 <- get_legend(
  p + 
    theme(legend.position = "bottom",legend.justification = "center",legend.direction = "horizontal",
          legend.text=element_text(size=14),legend.title = element_text(size=18 ,face="bold"),legend.key.width = unit(3,"line"))
)

#Export Figure 2
tiff("figures/Figure_2.tiff", width=3600, height=1700, compression="lzw", res=300) 
grid.arrange(quartile1, quartile4,
             legend_b2,
             ncol=2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(3, 3), heights = c(2.5, 0.3))
graphics.off()

#################################################################################
#Figure 4
code <- paste(country_data$color)
countries <- paste(country_data$countryISO)

#Remove countries with NA's for Nutritional Dependence Index (Selig et al. 2018) and/or PII
dat <- country_data[-which(is.na(country_data$NutritionalDep)==T),]
dat <- dat[-which(is.na(dat$mean_PII)==T),]

#Re-classify Nutritional Dep. Index into low, medium, high and very high
q <- quantile(dat$NutritionalDep,na.rm=T)

dat$NutritionalDepClass <- rep("low",nrow(dat))

for(k in 1:nrow(dat)){
  if(dat$NutritionalDep[k] > q[2] & dat$NutritionalDep[k] <= q[3]){
    dat$NutritionalDepClass[k] <- "medium"
  }
  if(dat$NutritionalDep[k] > q[3] & dat$NutritionalDep[k] <=75){
    dat$NutritionalDepClass[k] <- "high"
  }
  
  if(dat$NutritionalDep[k] > q[4]){
    dat$NutritionalDepClass[k] <- "very high"
  }
  
}# end of k

dat$NutritionalDepClass <- factor(dat$NutritionalDepClass, levels = c("low","medium","high","very high"))  # convert to factor to retain sorted order in plot.

#Display 'Mozambique' and not 'MOZ'
revalue(dat$countryISO, c("MOZ" = "Mozambique")) -> dat$countryISO

#Highlight some countries
dat$lab <- rep(0,nrow(dat))

#nutrient rich countries
dat$lab[which(dat$countryISO=="Indonesia")] <- 1 
dat$lab[which(dat$countryISO=="Georgia")] <- 1 
dat$lab[which(dat$countryISO=="Sierra Leone")] <- 1 
dat$lab[which(dat$countryISO=="Namibia")] <- 1 
dat$lab[which(dat$countryISO=="Cambodia")] <- 1 
dat$lab[which(dat$countryISO=="Mauritania")] <- 1 
dat$lab[which(dat$countryISO=="BIH")] <- 1 

dat$lab[which(dat$countryISO=="Timor Leste")] <- 1
dat$lab[which(dat$countryISO=="Sri Lanka")] <- 1
dat$lab[which(dat$countryISO=="Malaysia")] <- 1 

dat$lab[which(dat$countryISO=="Mozambique")] <- 1
dat$lab[which(dat$countryISO=="Kiribati")] <- 1

#nutrient poor
dat$lab[which(dat$countryISO=="Finland")] <- 1
dat$lab[which(dat$countryISO=="Bulgaria")] <- 1 
dat$lab[which(dat$countryISO=="Japan")] <- 1 
dat$lab[which(dat$countryISO=="North Korea")] <- 1
dat$lab[which(dat$countryISO=="Finland")] <- 1
dat$lab[which(dat$countryISO=="Estonia")] <- 1

alp <- rep(0.2,nrow(dat))
alp[which(dat$lab==1)] <- 1

Nut <- ggplot(dat, aes(y=MicronutDensityScore_W, x=mean_PII, fill=NutritionalDepClass,size=MicronutEvenness))  +
  geom_segment(aes(x = mean(mean_PII), y = 100, xend = mean(mean_PII,na.rm=T) , yend = 215),
               size=0.5, color="darkgrey",linetype="dashed")+
  geom_segment(aes(x = 0, y = mean(MicronutDensityScore_W), xend = 100, yend = mean(MicronutDensityScore_W)),
               size=0.5, color="darkgrey",linetype="dashed")+
  geom_point(shape=21,alpha=alp)+
  scale_size_area(breaks=c(0.1,.2,.4,.6), max_size=15, name='Micronutrient\nevenness',labels=c("0.1","0.2","0.4","0.6"))+
  scale_fill_brewer(palette = "YlGnBu", name="Nutritional\nDependence")+
  scale_y_continuous(limits=c(100,215),name="Micronutrient density (%)",breaks=c(100,150,200),labels=c("100","150","200"))+
  scale_x_continuous(breaks=c(0,25,50,75,100),labels=c("0","25","50","75","100"),limits = c(0,100),name="Prevalence of inadequate micronutrient intake (%)")+
  geom_text_repel(data = dat %>% filter(lab==1) , aes(label=countryISO),
                  direction = "both",
                  box.padding = unit(.4, 'cm'),
                  segment.alpha = .3,
                  force = 1, 
                  size= 5 )+
  white_theme + theme(legend.text=element_text(size=16),legend.title = element_text(size=18 ,face="bold"),legend.key = element_rect(colour = NA))+
  guides(fill = guide_legend(order =1, override.aes = list(   #Edit
    size=6)))

Nut

#Export Figure 4
tiff("figures/Figure_4.tiff", width=2800, height=2400, compression="lzw", res=300) 
Nut
graphics.off()

############################################################################################
# Supplemental Figures

#Figure S3
dat <- country_data

length(which(dat$VulnCC_W<=50 & dat$VulnF_W>50))/length(which(is.na(dat$VulnCC_W)==F)) #1st quandrant
length(which(dat$VulnCC_W>50 & dat$VulnF_W>50))/length(which(is.na(dat$VulnCC_W)==F)) #2nd quandrant
length(which(dat$VulnCC_W<=50 & dat$VulnF_W<=50))/length(which(is.na(dat$VulnCC_W)==F)) #3rd quandrant
length(which(dat$VulnCC_W>50 & dat$VulnF_W<=50))/length(which(is.na(dat$VulnCC_W)==F)) #4th quandrant

length(which(dat$VulnCC_W<=50 & dat$VulnF_W>50))
length(which(dat$VulnCC_W>50 & dat$VulnF_W>50))
length(which(dat$VulnCC_W<=50 & dat$VulnF_W<=50))
length(which(dat$VulnCC_W>50 & dat$VulnF_W<=50))

#Quantile95
quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

pbox1 <- ggplot(dat, aes(y=VulnF_W, x=quartile)) +
  stat_summary(fun.data = quantiles_95, geom="boxplot",fill="dark grey")+
  annotate("text", x = 0.6, y = 100, size=6, label = "a", fontface = c("bold")) +
  labs(x="",labels=c("Q1","Q2","Q3","Q4"))+
  scale_fill_brewer(palette="Dark2") +
  scale_y_continuous("Vulnerability to fishing",breaks=c(0,25,50,75,100),labels=c("0","25","50","75","100"),limits = c(0,100)) +
  scale_x_discrete(labels=c("Q1","Q2","Q3","Q4"))+
  white_theme + theme(legend.position="none")

pbox1

pbox2 <- ggplot(dat, aes(y=VulnCC_W, x=quartile)) +
  stat_summary(fun.data = quantiles_95, geom="boxplot",fill="dark grey")+
  annotate("text", x = 0.6, y = 100, size=6, label = "b", fontface = c("bold")) +
  labs(x="",labels=c("Q1","Q2","Q3","Q4"))+
  scale_y_continuous("Vulnerability to climate change",breaks=c(0,25,50,75,100),labels=c("0","25","50","75","100"),limits = c(0,100)) +
  scale_x_discrete(labels=c("Q1","Q2","Q3","Q4"))+
  white_theme + theme(legend.position="none")

pbox2


#Plot % and number of countries in each quadrant
Country <- ggplot(dat, aes(y=VulnF_W, x=VulnCC_W)) +
  geom_point(alpha=0.5,size=4,shape=16, colour="#075482")+
  annotate("text", x = 0, y = 100, size=6, label = "c", fontface = c("bold")) +
  geom_segment(aes(x = 50, y = 0, xend = 50 , yend =100),size=0.5,color="darkgrey")+
  geom_segment(aes(x = 0, y = 50, xend = 100 , yend =50),size=0.5,color="darkgrey")+
  #1st quandrant
  annotate("text", x = 25, y = 75, size=6, label = "n = 12 (8%)", fontface = c("bold")) +
  #2nd quandrant
  annotate("text", x = 75, y = 75, size=6, label = "n = 6 (4%)", fontface = c("bold")) +
  #3rd quandrant
  annotate("text", x = 25, y = 25, size=6, label = "n = 80 (51%)", fontface = c("bold")) +
  #4th quandrant
  annotate("text", x = 75, y = 25, size=6, label = "n = 59 (37%)", fontface = c("bold")) +
  scale_y_continuous("Vulnerability to fishing",breaks=c(0,25,50,75,100),labels=c("0","25","50","75","100")) +
  scale_x_continuous("Vulnerability to climate change",breaks=c(0,25,50,75,100),labels=c("0","25","50","75","100")) +
  white_theme

Country 

patchwork <- (pbox1 + pbox2 + Country)

jpeg("figures/Figure_S3.jpeg", res=300, width=6000, height=2000)
patchwork
graphics.off()

#Figure S4
fnut2 <- country_data
code <- paste(country_data$col)
countries <- paste(country_data$keepISO)

#Standardise Taxa Richness
standardise <- function(x){(x-mean(x))/(sd(x))} 
fnut2$STaxRichness <- standardise(fnut2$TaxRichness)

#Outliers?
library(mgcv)
mod1 = gam(MicronutDensityScore_W ~ s(STaxRichness, k=3) , data = fnut2)
inf1 = influence.gam(mod1)
hist(inf1)
fnut2[which(inf1>0.6),] #USA

#evenness
mod2 = gam(MicronutEvenness ~ s(MicronutDensityScore_W,k=3) , data = fnut2)

inf2 = influence.gam(mod2)
hist(inf2)
plot(mod2)
#no outlier

#Change name (too long)
#plyr::revalue(fnut2$countryISO, c("Bosnia & Herzegovina" = "BIH")) -> fnut2$countryISO #Already changed

fnut2$lab <- rep(0,nrow(fnut2))
fnut2$lab[which(fnut2$countryISO=="Latvia")] <- 1
fnut2$lab[which(fnut2$countryISO=="Finland")] <- 1
fnut2$lab[which(fnut2$countryISO=="Croatia")] <- 1
fnut2$lab[which(fnut2$countryISO=="BIH")] <- 1
fnut2$lab[which(fnut2$countryISO=="Estonia")] <- 1
fnut2$lab[which(fnut2$countryISO=="Montenegro")] <- 1

code2 <- paste(fnut2$col)
countries2 <- paste(fnut2$countryISO)

#evenness Quartile 1
white_themejpg <-theme(axis.ticks=element_line(colour="black"),
                       axis.text=element_text(size=13,colour="black"),
                       axis.title=element_text(size=18,face="bold"),
                       axis.text.y = element_text(angle = 0),
                       panel.grid.minor=element_blank(),
                       panel.background=element_rect(fill="white",colour="black"),
                       plot.background=element_rect(fill="transparent",colour=NA),
                       legend.key = element_rect(fill = "white"))

pnut2 <- ggplot(data=fnut2,aes(x=MicronutDensityScore_W,y=MicronutEvenness,colour=countryISO))+
  annotate("text", x = 215, y = 0.8, size=6, label = "a", fontface = c("bold")) +
  scale_colour_manual(breaks= countries2, values = code2)+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 3),se = T, col="darkgrey",level = 0.95,fill="#e9e8e8")+
  #geom_smooth(method="lm", formula = y ~ poly(x, 3), se = TRUE, fullrange = FALSE,col="darkgrey",level = 0.95,fill="#e9e8e8")+
  geom_point(size=4)+
  geom_text_repel(data = fnut2 %>% filter(lab==1) , aes(label=countryISO), size=4,box.padding = 0.6)+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8),limits = c(0,0.80),name="Micronutrient evenness")+
  scale_x_continuous(limits=c(100,215),
                     name="Micronutrient density (%)",breaks=c(100,150,200),labels=c("100","150","200"))+
  white_themejpg + theme(legend.position="none")

pnut2

#Remove 1 outlier
outl <- which(fnut2$countryISO=="United States") #Remove USA
d <- fnut2[-outl,]

prichness <- ggplot(data=d,aes(x=STaxRichness,y=MicronutEvenness,colour=countryISO))+
  annotate("text", x = 5.29, y = 0.8, size=6, label = "b", fontface = c("bold")) +
  scale_colour_manual(breaks= countries2, values = code2)+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 3),se = T, col="darkgrey",level = 0.95,fill="#e9e8e8")+
  geom_point(data=fnut2,aes(x=STaxRichness,y=MicronutEvenness,colour=countryISO),size=4)+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8),limits = c(0,0.80),name="Micronutrient evenness")+
  scale_x_continuous(name="Standardised Taxa Richness")+
  white_themejpg + theme(legend.position="none")

prichness

#Legend
myPalette <- colorRampPalette(rev(brewer.pal(4, "Spectral")))

pleg2 <- ggplot(country_data, aes(y=VulnF_W, x=VulnCC_W, colour=MicronutDensityScore_W)) +
  geom_segment(aes(x = 50, y = 0, xend = 50 , yend =75),size=0.5,color="darkgrey")+
  geom_segment(aes(x = 0, y = 50, xend = 75 , yend =50),size=0.5,color="darkgrey")+
  annotate("text", x = 10, y = 75, size=6, label = "Children under 5")+
  scale_size_area(breaks=c(0.1,.2,.4,.6), max_size=15, name='Micronutrient\nevenness')+
  scale_colour_gradientn(colours = myPalette(100), limits=c(100,215),
                         name="Micronutrient\ndensity (%)",breaks=c(100,150,200),labels=c("100","150","200"))+
  geom_point(shape = 21,alpha=0.9)+
  
  scale_y_continuous("Vulnerability to fishing",breaks=c(0,25,50,75),labels=c("0","25","50","75"),limits=c(0,75)) +
  scale_x_continuous("Vulnerability to climate change",breaks=c(0,25,50,75),labels=c("0","25","50","75"),limits=c(0,75)) +
  white_themejpg

legend_b2 <- get_legend(
  pleg2 + 
    theme(legend.position = "bottom",legend.justification = "center",legend.direction = "horizontal",legend.key.width = unit(3,"line"),
          legend.text=element_text(size=15),legend.title = element_text(size=18 ,face="bold"))
)


prichnessnut <- ggplot(data=d,aes(x=STaxRichness,y=MicronutDensityScore_W,colour=countryISO))+
  annotate("text", x = 5.29, y = 215, size=6, label = "c", fontface = c("bold")) +
  scale_colour_manual(breaks= countries2, values = code2)+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 3),se = T, col="darkgrey",level = 0.95,fill="#e9e8e8")+
  #geom_smooth(method="lm", formula = y ~ poly(x, 3), se = TRUE, fullrange = FALSE,col="darkgrey",level = 0.95,fill="#e9e8e8")+
  geom_point(data=fnut2,aes(x=STaxRichness,y=MicronutDensityScore_W,colour=countryISO),size=4)+
  scale_y_continuous(breaks=c(100,150,200),labels=c("100","150","200"),limits = c(100,215),name="Micronutrient density (%)")+
  scale_x_continuous(name="Standardised Taxa Richness")+
  white_themejpg + theme(legend.position="none")

prichnessnut

jpeg("figures/Figure_S4.jpeg", res=300, width=6000, height=2600)
cowplot::plot_grid(pnut2,prichness,prichnessnut,
                   NULL,legend_b2,NULL,
                   nrow = 2, ncol = 3, rel_heights = c(1,.1),rel_widths = c(1,1))
graphics.off()


#Figure S5 - Correlogram
library(corrgram)
library(corrplot)

#Remove countries with NA's for Nutritional Dependence Index (Selig et al. 2018) and/or PII
dat <- country_data[-which(is.na(country_data$NutritionalDep)==T),]
dat <- dat[-which(is.na(dat$mean_PII)==T),]

#Select variables
d <- dat[,c("NutritionalDep","mean_PII","Vitamin_A_PII","Zinc_PII","Iron_PII","Calcium_PII")]

colnames(d)<-c("Nutritional Dependence","Averaged PII",
               "Vitamin A PII","Zinc PII","Iron PII","Calcium PII")
ccor <- d
cor_mat <- cor(ccor)

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# matrix of the p-value of the correlation
p.mat <- cor.mtest(ccor)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

jpeg("figures/Figure_S5.jpeg", res=300, width=2000, height=2000)
corrplot(cor_mat, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.cex = 0.9, number.cex = .7, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 1, insig = "blank",  
         # hide correlation coefficient on the principal diagonal
         diag=FALSE )
graphics.off() 

#end of script