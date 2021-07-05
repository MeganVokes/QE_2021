#Topic 5 Questions 
#Quantitative Ecology
#Megan Vokes
#Due : 05/07/2021
library(ggplot2)
library(dplyr)
library(tidyverse)
library(betapart)
library(vegan)
library(gridExtra)
library(BiodiversityR)
library(grid)
library(gridBase)
library(tidyr)
library(geodist)
library(readr)


Doubs <- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")


# Question 1 --------------------------------------------------------------

Doubs <- dplyr::select(doub_data, -1)
dim(Doubs)# 30 rows 27 col
# Question 1 --------------------------------------------------------------

Doubs<- dplyr::select(Doubs, -1)
dim(Doubs)# 30 rows 27 col

# The dataset is comprised of 30 rows and 27 coloums and is rectangular .
#The rows show sites and the columns show species 



# Question 2 --------------------------------------------------------------

#Bray Curtis because we are looking for dissimilarity , and Jaccard looks at 
#similarity 

# Question 3 --------------------------------------------------------------

Doubs_Matrix <- vegdist(Doubs, method="bray", binary=FALSE, diag=TRUE
    )

as.matrix(Doubs_Matrix)
dim(Doubs_Matrix)

# Question 4 --------------------------------------------------------------
#The results show how dissimilar the sites is to each other. 
#The greater the distance between sites the more dissimilar they are.
#  The data is square as each site is being compared to
#itself and every other site. This is shown by the zero diagonal  where each site is compared to itself.

# Question 5 --------------------------------------------------------------
#There are spikes which occur in the dissimilarity. Some sites are completely
#dissimilar (dissimilarity = 1) compared to other creating spikes in the index.
#The sites are also occurring along a latitudinal gradient.
#There is also a site that has no fish species being studied.

# Question 6 --------------------------------------------------------------

Doubs_Graph <-as_tibble(Doubs_Matrix[1:1, 1:30]) # gives the first row only # i ran into an error that i could not solve .
#Error in Doubs_Matrix[1:1, 1:30] : incorrect number of dimensions






# Question 7 --------------------------------------------------------------





# Question 8 --------------------------------------------------------------


DSor <- (decostand(Doubs, method = "pa")) #converts the abundance data to presence/absence data.
DSor

sorensen <- vegdist(DSor, binary = TRUE) #binary = TRUE is used for presence absence data to produce a dissimilarity index and apply sorenson's index.
sorensen_df <- as.matrix(as.matrix(sorensen))
dim(sorensen_df)
view(sorensen_df)

Sor <- as_tibble(sorensen_df[1, 1:30])
Sor






# Question 9  -------------------------------------------------------------

Sor_plot <- ggplot(data = Sor, aes(x = 1:30, y = value)) +
  geom_line(aes(colour = "seagreen")) +
  labs(x = "Site along river side", y = "Dissimilarity",
       title = "Site species dissimilarity along sites ",
       subtitle = "Presence/Absence data") +
  theme_bw() +
  theme(legend.position = "none")

##The dissimilarity for the presence absence data follows the same trend as the dissimilarity index
#for abundance data. The pattern still shows the presence of lower points in dissimilarity, followed
#by a gradual increase. 
#The patterns have slight differences due to the weighting that the some
#datasets have on species data. Presence absence data gives more weight to rare species as they
#are weighted the same as common species. Abundance data give more weight to commons species.





