#Extract and prepare catch data from the SAU database to replicate analyses
#Example for Algeria

#Libraries
library(tidyverse)
library(vegan)

#Load nutrient concentrations at the species, genus and family level
load("data/species_data.RData")
load("data/genus_data.RData")
load("data/family_data.RData")

#Load SAU catch data for Algeria from 2010 to 2014
load("data/catch_data_Algeria_2010_2014.RData")
data <- catch_data_Algeria_2010_2014

#summarise data by taxon_scientific_name, year and catch status and fishing entity name
summ <- data %>%
  group_by(taxon_scientific_name,year,catch_status_name,fishing_entity_name) %>%
  summarise(catch_tot = sum(catch_sum))

#Merge nutrient data
nutSp<- species_data[,c("species",
                        "Protein_mu","Zinc_mu","Iron_mu","Calcium_mu","Omega_3_mu","Vitamin_A_mu","Selenium_mu",
                        "MicronutDensityScore",
                        "Fishing_V","Climate_V.index")]

nutGenus<- genus_data[,c("Genus",
                         "Protein_mu","Zinc_mu","Iron_mu","Calcium_mu","Omega_3_mu","Vitamin_A_mu","Selenium_mu",
                         "MicronutDensityScore",
                         "Fishing_V","Climate_V.index")]

colnames(nutGenus)[1] <- "species"

nutFamily<- family_data[,c("Family",
                           "Protein_mu","Zinc_mu","Iron_mu","Calcium_mu","Omega_3_mu","Vitamin_A_mu","Selenium_mu",
                           "MicronutDensityScore",
                           "Fishing_V","Climate_V.index")]

colnames(nutFamily)[1] <- "species"

nutSAU <- unique(rbind(nutSp,nutGenus,nutFamily))
head(nutSAU)

#list of taxa covered by nutrient data
allTaxaNut <- unique(nutSAU$species)

#Add nutrient data to Algerian catch data 
nut <- merge(summ,nutSAU,by.x="taxon_scientific_name",by.y="species",all.x=T)

#Load list of taxa reported in the SAU database and classified as marine finfish or invertebrates 
check <- read.csv("data/summarySAU.csv")

#and merge with catch data 
nut2 <- merge(nut,check,by="taxon_scientific_name",all.x=T)

#Keep 'landings' data => remove discards
unique(nut$catch_status_name)

dat <- nut2 %>% 
  dplyr::select(taxon_scientific_name,year,catch_status_name,catch_tot, 
                Protein_mu,Zinc_mu,Iron_mu,Calcium_mu,Omega_3_mu,Vitamin_A_mu,Selenium_mu,MicronutDensityScore,Fishing_V,Climate_V.index,class,nut) %>% 
  dplyr::filter(catch_status_name == "Landings") 

#Years included
years <- seq(2010,2014,1)

#proportion of catch data covered by nutrient and vulnerability data
prop_nutdata <- vector()

#Biomass-averaged micronutrient density and vulnerability indexes: empty vectors to fill in
MicronutDensityScore_w <- vector()
VulnF_w <- vector()
VulnCC_w <- vector()

#################################################################################
# Calculate biomass-weighted micronutrient density and vulnerability

for (k in 1:length(years)){
  
  keepY <- dat[which(dat$year==years[k]),] #Keep year k
  
  #Determine % of catch data are covered by nutrient and vulnerability data
  if(dim(keepY)[1]!=0){
    
    keepFish <- keepY[which((keepY$taxon_scientific_name%in%allTaxaNut)==T),] #Keep species covered by nutrient data
    
    if(dim(keepFish)[1]!=0){
      #% of catch covered?
      na1 <- which(is.na(keepFish$Fishing_V)==T)
      na2 <- which(is.na(keepFish$MicronutDensityScore)==T)
      na3 <- which(is.na(keepFish$Climate_V.index)==T)
      na <- unique(c(na1,na2,na3))
      
      if(length(na)!=0){ 
        prop_nutdata[k] <- sum(keepFish[-na,"catch_tot"])/sum(keepY[which(keepY$class=="marine_finfish"),"catch_tot"])
      }else{
        prop_nutdata[k] <- sum(keepFish[,"catch_tot"])/sum(keepY[which(keepY$class=="marine_finfish"),"catch_tot"])
      }
      
      #Create empty vectors to run loop on species
      weight_sp <- vector()
      WNUT <- vector()
      WVulF <- vector()
      WVulCC <- vector()
      
      stat <- keepFish %>% dplyr::select(taxon_scientific_name,catch_tot) %>%
        dplyr::group_by(taxon_scientific_name) %>%
        dplyr::summarise(Catch = sum(catch_tot))
      
      species <- unique(stat$taxon_scientific_name)
      
      for (j in 1:length(species)){
        good <- which((stat$taxon_scientific_name%in%species[j])==T)
        weight_sp[j] <- sum(stat$Catch[good])/sum(stat$Catch)
        
        good2 <- which((keepFish$taxon_scientific_name%in%species[j])==T)
        WNUT[j] <- as.numeric(keepFish[good2[1],"MicronutDensityScore"])*weight_sp[j]
        WVulF[j] <- as.numeric(keepFish[good2[1],"Fishing_V"])*weight_sp[j]
        WVulCC[j] <- as.numeric(keepFish[good2[1],"Climate_V.index"])*weight_sp[j]
        
      }#end of j
      
      bad1 <- which(is.na(WNUT)==T)
      bad2 <- which(is.na(WVulF)==T)
      bad3 <- which(is.na(WVulCC)==T)
      bad <- unique(c(bad1,bad2,bad3))
      
      if(length(bad)!=0){
        
        MicronutDensityScore_w[k] <- sum(WNUT[-bad],na.rm=T) / sum(weight_sp[-bad])
        VulnF_w[k] <- sum(WVulF[-bad],na.rm=T) / sum(weight_sp[-bad])
        VulnCC_w[k] <- sum(WVulCC[-bad],na.rm=T) / sum(weight_sp[-bad])
        
      }else{
        
        MicronutDensityScore_w[k] <- sum(WNUT,na.rm=T) / sum(weight_sp)
        VulnF_w[k] <- sum(WVulF,na.rm=T) / sum(weight_sp)
        VulnCC_w [k] <- sum(WVulCC,na.rm=T) / sum(weight_sp)
        
      }
      
    }else{
      MicronutDensityScore_w[k] <- NA
      VulnF_w[k] <- NA
      VulnF_w[k] <- NA
      
    }#end of else
  }else{
    MicronutDensityScore_w[k] <- NA
    VulnF_w[k] <- NA
    VulnF_w[k] <- NA
    
  }#end of else
} #end of k  

#Compute the micronutrient evenness of fish catches
dat$MicronutDensityScoreW <- dat$catch_tot*dat$MicronutDensityScore

#And remove species for which vulnerability and nutrient data are missing, in order to match species used to compute micronutrient density score
na1 <- which(is.na(dat$Fishing_V)==T)
na2 <- which(is.na(dat$MicronutDensityScore)==T)
na3 <- which(is.na(dat$Climate_V.index)==T)
na <- unique(c(na1,na2,na3))
keepFish <- dat[-na,]

taxaTot <- unique(keepFish$taxon_scientific_name)

#create empty matrix years x taxa and compute micronutrient density values
mat <- matrix(0,ncol=length(taxaTot),nrow=length(years))
rownames(mat) <- years
colnames(mat) <- taxaTot

#Compute Hmax (Shannon's diversity index)
Hmax <- log(length(taxaTot))

for(j in 1:length(taxaTot)){
  
  keepTax <- keepFish[which(keepFish$taxon_scientific_name==taxaTot[j]),]
  
  for(i in 1:length(years)){
    x <- which(keepTax$year==years[i])
    if(length(x)>=1){
      mat[i,j]<- sum(keepTax[x,"MicronutDensityScoreW"])
    }else{
      mat[i,j]<-0
    }
  }#end of i
}# end of j

shannonCnut = diversity(mat)/Hmax

#Average values over the period 2010-2014
MicronutEvenness <- mean(shannonCnut)
MicronutDensityScore_W <- mean(MicronutDensityScore_w)
VulnCC_W <- mean(VulnCC_w)
VulnF_W <- mean(VulnF_w)

Algeria_data <- data.frame(MicronutEvenness,MicronutDensityScore_W,VulnCC_W,VulnF_W)
Algeria_data

#End of script