#Script to run the log micronutrient density score (linear) model and re-produce Figure 3

#Landings
library(ggrepel)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(gridExtra)
library(plyr)
library(tidyverse)
library(ggExtra)
library(lme4)

#Load data
load("data/country_data.RData")

#Remove countries with missing values for HDI
dat <- country_data[-which(is.na(country_data$hdi2018)==T),]
summary(dat)

#Standardise covariates
standardise <- function(x){(x-mean(x))/(sd(x))} 

dat$logNUT <- log(dat$MicronutDensityScore_W)
dat$HDIs <- standardise(dat$hdi2018)
dat$SRs<- standardise(dat$TaxRichness)
dat$yield_5Ys<- standardise(dat$yield_5Y)
dat$cumulAreas <- standardise(dat$cumulArea)
dat$RegionWBs <- relevel(as.factor(dat$RegionWB),ref="Latin America & Caribbean")

# Investigate relationships between response variable and covariates
# 1 - Response variable: LOG micronutrient density scores
hist(dat$logNUT) #bimodal distribution -> check residuals!

# 2 - VIF
library(usdm)
df <- dat[,c("HDIs","SRs","yield_5Ys","cumulAreas")]
usdm::vif(df) # < 2

#Linear model
M1 <- lm(logNUT~HDIs+RegionWBs+SRs+yield_5Ys+cumulAreas,data=dat)
summary(M1)
cor(dat$logNUT,predict(M1))^2 # 0.41

#Check predicted versus observed values
pred=predict(M1)
plot(dat$logNUT,pred,xlim=c(4.5,5.5),ylim=c(4.5,5.5))
segments(4.5,4.5,5.5,5.5)
cor.test(dat$logNUT,pred) # 0.64

#Check residuals
res<-residuals(M1)
hist(res) #Residuals are normally distributed

#Plot
library(stats)
ci95 <- confint(M1, level = 0.95)
ci75 <- confint(M1, level = 0.75)
mean <- coef(M1)

Var <- c("Latin America & Caribbean","HDI","East Asia & Pacific","Europe & Central Asia","Middle East & North Africa","North America",                  
         "South Asia","Sub-Saharan Africa","Richness","Yield","Total Area")


datFig <- data.frame(Var,mean,ci75,ci95)
names(datFig)<-c("Var","mean","ll_75","ul_75","ll_95","ul_95")

#Add reference 'Americas' for RegionWBs
datFig[1,c(2:6)]<-c(0,0,0,0,0)

#Rename and reorder covariate effects
row.names(datFig) <- c("Latin America & Caribbean","HDI","East Asia & Pacific","Europe & Central Asia","Middle East & North Africa","North America",                  
                       "South Asia","Sub-Saharan Africa","Richness","Yield","Total Area")

roworder <- rev(c("HDI","Richness","Yield","Total Area",
                  "North America","Europe & Central Asia",
                  "South Asia","East Asia & Pacific","Sub-Saharan Africa","Middle East & North Africa",
                  "Latin America & Caribbean"))

datFig2 <- datFig[match(roworder,row.names(datFig)),]
datFig2$mean<-as.numeric(datFig2$mean)
datFig2$ll_75<-as.numeric(datFig2$ll_75)
datFig2$ul_75<-as.numeric(datFig2$ul_75)
datFig2$ll_95<-as.numeric(datFig2$ll_95)
datFig2$ul_95<-as.numeric(datFig2$ul_95)
datFig2

datFig2$Var <- factor(datFig2$Var, levels = datFig2$Var)

white_theme <-theme(axis.ticks=element_line(colour="black"),
                       axis.text=element_text(size=10,colour="black"),
                       axis.title=element_text(size=12),
                       panel.grid.minor=element_blank(),
                       panel.background=element_rect(fill="white",colour="black"),
                       plot.background=element_rect(fill="transparent",colour=NA),
                       legend.key = element_rect(fill = "white"))

#Adapt shape of points
shape<-c(22,21,21,21,21,21,21,21,21,21,21)
col<-rep("black",nrow(datFig2))
col[c(1,2,3,5)] <-"white"

effect <- ggplot(datFig2,aes(x=Var, y=mean)) + 
  geom_hline(yintercept=0, lwd=0.5, lty=2)+
  geom_linerange(aes(x = Var,ymin = ll_95, ymax = ul_95),lwd=0.5)+
  geom_linerange(aes(x = Var,ymin = ll_75,ymax = ul_75),lwd=1.5)+
  geom_point(stat='identity', shape=shape ,size=5,colour="black",fill=col)  +
  scale_y_continuous(limits=c(-0.38,0.13), breaks=c(-.3,-.2,-.1,0,.1),name="Effect on log micronutrient density")+
  scale_x_discrete(name="")+
  coord_flip()+
  white_theme

effect

tiff("figures/Figure_3.tiff", width=2400, height=2000, compression="lzw", res=300) 
effect
graphics.off()

#End of script

