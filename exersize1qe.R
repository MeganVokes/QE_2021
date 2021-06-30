#QE Exersize 1 
#Due : July 2nd 2021
#Megan Vokes 
library(tidyverse)
library(betapart)
library(vegan)
library(gridExtra)
library(BiodiversityR)
library(grid)
library(gridBase)
library(tidyr)




spp <- read.csv("Quantitative_Ecology-main/exercises/diversity/SeaweedsSpp.csv")# loads the data 
spp <- dplyr::select(spp, -1)
dim(spp)# gives dimensions of the matrix

spp[1:5, 1:5]#for first 5 rows and coloumns 
spp[(nrow(spp) - 5):nrow(spp), (ncol(spp) - 5):ncol(spp)]# for last 5

# Question1 ---------------------------------------------------------------
#The dissimilarity matrix is square because unlike the raw data the number 
#of columns is equal to the number of rows . This means that the matrix is 
#symmetrical due to the pairwise comparisons that are used to create it . 


# Question2  --------------------------------------------------------------
#The diagonal represents the pairwise comparison of a site against itself 
# hence why the value is always 0 because a site compared to itself is always 
#identical. This given that the values of the dis matrix range from 0(identical) 
#to 1 (completely different)


# Question3 ---------------------------------------------------------------
#The non diagonal elements of the dissimilarity matrix represent the pairwise 
#comparisons of each site to every other site. Thereby showing the measure (0-1) 
#of similarity between each site and every other site . 



# Question4 ---------------------------------------------------------------

Sor <- vegdist(spp, binary = TRUE) # Vegdist creates the matrix # binary = TRUE sets to presence/absence data
Sor_df <- round(as.matrix(Sor), 4)# rounds off to 4 decimal places 
dim(Sor_df)
view(Sor_df)

Sor_Graph<- as_tibble(Sor_df[1:1, 1:58]) # gives the first row only 

ggplot(data = Sor_Graph, aes(y = value, x = 1:58))+
  scale_color_manual(values = c("Limegreen"))+
  geom_line(aes (col = "Limegreen"),lwd = 1.5)+
  labs(x = "Coastal Sites", y = "Dissimalirty Value",
       title = "Graph showing Dissimalirty over Disance Across Coastal Sites of South Africa (West to East) ") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60)) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) + 
  theme(panel.background = element_rect(fill = "Dark seagreen1",color = "Seagreen"),legend.position="none")








# Question5 ---------------------------------------------------------------
#The shape of the matrix is representative of the fact that geographical distance 
#has an effect on ecological distance . The more geographically distant two sites 
# are the more environmental features and conditions differ , this in turn effect the 
# species that occur . This is because some species are better suited to certain environmental
# conditions than others , so the types of species change as the geographical distance grows
#Therefore the matrix square shape lends itself to the ecological understanding that site which are closer
#(and therefore have more similar environments) will have more similar species . 





