################################################
#                                              #
#   Speed change before and after intervention #
#                                              #
################################################

#This script runs the Condatis function to calculate the speed metric in the landscapes selected in step 1

#The script iterates the calculation of speed using different dispersal distances within the range estimated in the pilot project in Cumbria

#The result is a table with the speed corresponding to a particular dispersal distance

#The area under the curve (AUC) of dispersal distance vs speed is calculated for both landscapes, i.e. habitat without B-line project and habitat with B-line project intervention. Then, the percentage of change of speed between both landscape curves is calculated.

#INPUTS:
# Hab - raster of the habitat you wish to measure connectivity over (habitat.tif/ habitat_bl.tif obtained with the layer preparation script step 1)
# st - raster of location of sources and targets (stXX.tif obtained with the layer preparation script)
# R - R value of the species moving (number of movers produced per km^2 of habitat), fixed to 1000 for all species
# disper - range of dispersal distance 

library(raster)
library(sf)
library(tidyverse)
library(rgdal)
library(dplyr)
library(ggplot2)
library(maptools)
library(scales)
library(DescTools)
#memory.limit(size=300000)

# Run Condatis with dispersal distance iteration --------------------------

#Raster of AOI without B-line project
hab<- raster("spatialdata/habitat.tif")
st<- raster("spatialdata/stSN.tif")
R<-1000

#Range defined between 10m and maximum distance between the source and target[Dispersal
disper <-c(10^seq(-1.7,0.5,0.1))

test_result<-data.frame()
for(i in disper) {
  test<-CondatisNR(hab=hab, st=st, R=R, disp=i)
  test_result<- rbind(test_result, test$conductance)
}

colnames(test_result)<-c("disp" , "conductance")

con<- data.frame(test_result %>%
                   group_by(disp)%>%
                   summarise(Conduct = mean(conductance)))

write.csv(con, "conductance/testSN.csv")



#Raster of AOI including B-line projects
hab<-raster("spatialdata/habitatBL.tif") 
st<- raster("spatialdata/stSN.tif")
R<-1000

disper <-c(10^seq(-1.6,0.6,0.2))

test_result<-data.frame()
for(i in disper) {
  test<-CondatisNR(hab=hab, st=st, R=R, disp=i)
  test_result<- rbind(test_result, test$conductance)
}

colnames(test_result)<-c("disp" , "conductance")

con<- data.frame(test_result %>%
                   group_by(disp)%>%
                   summarise(Conduct = mean(conductance)))

write.csv(con, "conductance/test_blSN.csv")

# Plot results ------------------------------------------------------------

#Joining results of the conductance of landscapes with ('B-line') and without ('No B-line') B-line project intervention
cond<- data.frame(read.csv("conductance/testSN.csv"))
cond_bl<- data.frame(read.csv("conductance/test_blSN.csv"))
conductance<-data.frame(cond$disp, cond$Conduct, cond_bl$Conduct)
colnames(conductance)<-c('disp_dist', 'before SSproj','after SSproj')

#Rearranging the conductance data frame to plot both landscapes
conductance.long <- conductance %>% 
  select('disp_dist', 'before SSproj','after SSproj') %>% 
  pivot_longer(-disp_dist, names_to = "Variable", values_to = "speed")


#plot absolute dispersal distance vs speed
ggplot(conductance.long, aes(disp_dist, speed, colour = Variable)) + 
  geom_point(size = 5)+
  labs(x = 'Dispersal distance [km]', y='Speed')+
  theme(text = element_text(size = 30), legend.position="right",
        legend.title=element_blank())

#plot absolute dispersal distance vs log speed
ggplot(conductance.long, aes(disp_dist, log10(speed), colour = Variable)) + 
  geom_point(size = 5)+
  labs(x = 'Dispersal distance [km]', y='log(Speed)')+
  theme(text = element_text(size = 30))


#plot log dispersal distance vs log speed
pdf("conductance/plotSN.pdf")
plot<-ggplot(conductance.long, aes(log10(disp_dist), log10(speed), colour = Variable))+ 
  geom_point(size = 5)+
  labs(x = 'log (Dispersal distance) [km]', y='log(Speed)' )+
  scale_x_continuous(breaks=c(-1.5,-1,-0.5,0,0.5))+
  theme(text = element_text(size = 30), legend.position="right",
        legend.title=element_blank())
dev.off()


# Estimate change of speed due to intervention ----------------------------

nobl_area<-AUC(conductance$disp_dist, conductance$`before SSproj`)
nobl_area
bl_area<-AUC(conductance$disp_dist, conductance$`after SSproj`)
bl_area
change<-bl_area-nobl_area
perc_change<-(change/nobl_area)*100
perc_change

write.csv(perc_change,"conductance/test_3kchange.csv")
