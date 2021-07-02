#Exersize2 - Betadiversity 
#Megan Vokes 
#Due : 02/07/2021



library(tidyverse)
library(betapart)
library(vegan)
library(gridExtra)
library(BiodiversityR)
library(grid)
library(gridBase)
library(tidyr)


# Question1 ---------------------------------------------------------------



# Load Data ---------------------------------------------------------------


spp <- read.csv("Quantitative_Ecology-main/exercises/diversity/SeaweedsSpp.csv")
spp <- dplyr::select(spp, -1)





# Decompose total Sørensen dissimilarity into turnover and nestedness-resultant components:
Y.core <- betapart.core(spp)
Y.pair <- beta.pair(Y.core, index.family = "sor")

# Let Y1 be the turnover component (beta-sim):
Y1 <- as.matrix(Y.pair$beta.sim)

# Let Y2 be the nestedness-resultant component (beta-sne):
Y2 <- as.matrix(Y.pair$beta.sne)

round(Y1[1:10, 1:10], 4)
round(Y2[1:10, 1:10], 4)

Y1_1<- data.frame(Y1[1:1, 1:58])

ggplot(data = Y1_1, (aes(x = 1:58, y = Y1_1[,1])))+
  geom_line(size = 1.5, col = "Green")+
  labs(x = "Coastal Sites West to East (km)", y = "Species Turnover (βsim)",
                                           title = "Graph showing Species Turnover Across Coastal Sites of South Africa (West to East) ") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60)) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) + 
  theme(panel.background = element_rect(fill = "Dark seagreen1",color = "Seagreen"),legend.position="none")



# Explanation -------------------------------------------------------------

#The graph shows us that with distance there is a higher species turnover , 
#meaning that as the number of sites(and total distance) increases the more species 
# are lost or gained without the alpha diversity changing . The number of species across
#the site is the same but what the species has changed . This is why the graph shows a 
#much higher species turnover at 60km from the original site compared to at 10km , 
#as distance increases the sites may be exposed to different environmental conditions 
#which can affect which species occurs there ( one instead of another due to suitability)
# without necessarily effecting how many species occur . 


# Question2 ---------------------------------------------------------------
#Nestedness-resultant beta diversity refers to the the situation in which across site their 
#is a change in alpha diversity (the number of species) as a result of loss or gain species and the 
#sample with the lower alpha diversity is a subset of the sample with the higher beta-diversity .
#this is true for the West Coast of South Africa (BMP)where algal species are arranged according 
#to a factor other than temperature gradient . This could possibly be attributed 
#to the influence of up welling or other contributing factors . however the biodiversity is lower 
#on the west coast due to lack of steep environmental gradients . Hence the nestedness-
#resultant beta diversity is high , because the same species repeat at sites at different frequencies . 
#this is in direct contrast to the south and eastern coastlines of South Africa(AMP) which feature more 
#temperature gradients and transitional zones which have a greater influence how the species are arranged 
#and also which species occur and therefore a higher species turnover is apparent for this region . 
#In The BMP their are fewer species and their arrangement is governed by factors other 
#than steep temperature gradients thus their biodiversity is largely represented by Nest-Res beta diversity 
#as the species that occur remain mostly the same but the frequencies at which site they occur change . 
#The paper used to formulate this discussion :Smit et al 2017  

