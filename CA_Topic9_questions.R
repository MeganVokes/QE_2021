#CA Questions
#Topic 9
#Megan Vokes 

library(vegan)
library(tidyverse)

# Question 1 --------------------------------------------------------------
#The patterns seen in the four panels of the above figure can be explained as 
#such :
# The four panels represent the top most abundant species within the CA1( CA1
#contains 51.5% of the variation),individually plotted .
#  
# Satr- The first panel shows the abundance of species Satr in bubbles whose 
#size is dependent on abundance . The green/blue lines are sort of like contour
#lines ( in that they connect sites of similar species abundance like geographical 
#contour lines connect sites of the same height/elevation). In panel one we see 
# that the species Satr is most abundant on the upper and lower right side of the
# graph and that the abundance follows (sort of ) a gradient from high abundance in 
#the right sites to low abundance in the left sites .The environmental factors plotted 
# in panel four apply to all the panels as such we can extrapolate that Satr does
# better as a species in high altitudes with greater slopes as shown by the 
# significant red factors slo and alt in panel four.
#Scer-The second panel show the larger bubbles ( and therefore the greater abundance)
# can be found on the left of the graph . The contour lines are much closer together
# and have a steep u shape . This tells us the abundance is more concentrated in those 
#sites and the a gradient down in the direction of the sites on the right is followed . 
# this is possibly due to the significant bod and other factors in this part of 
# the river where these sites are found .the smaller bubbles throughout the graph
#show a very low abundance
#Teso-The third panel shows the species Teso has its highest abundance in the bottom
#right quadrant (The largest bubbles occur here) . the contour lines are curved
# in a very wide u shape and a gradient from high to lower abundance does occur 
# buts still mostly within the bottom right quadrant . In the other quadrants very 
#low abundances of the species does occur . This pattern of abundance may be due to 
# the fact that oxygen is highly present in the area of the river where these 
#sites are as seen by the red significant environmental factors plotted in panel 
#four .
#Cogo-The fourth panel shows large abundance of the species Cogo in the bottom right 
#panel with circular contour lines that show a relatively steady gradient between the 
#highest abundances and the lower ones ,still howevr in the bottom right quadrant 
# the rest of the graph has very low abundances of this species throughout the sites
# as shown by the very small bubbles . This may be due to the fact that there is a 
#significant oxygen factor present in the part of the river where these sites are , 
#as shown in the plotted red enviromental factors . 



# Question2A --------------------------------------------------------------
# BIRD DATA:

# Load the data

ybirds.spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_spe.txt', row.names = 1)
birds_spe <-  ybirds.spe # Just renaming

birds_spe # View the data in the console

dim(birds_spe)

# 50 rows & 59 columns

head(birds_spe)

# Do the CA:
birds_ca<- cca(birds_spe) # Finds the CA
birds_ca

summary(birds_ca)

# Calculate the total inertia manually:

round(sum(birds_ca$CA$eig), 5)

# 2.00751

# The inertia for the first axis (CA1) is:

round(birds_ca$CA$eig[1], 5)

# CA1 = 0.74767

# The inertia of CA1 and CA2 is:

round(sum(birds_ca$CA$eig[1:2]), 5)

# 1.08145

# The fraction of the variance explained by CA1 and CA2 is:

round(sum(birds_ca$CA$eig[1:2]) / sum(birds_ca$CA$eig) * 100, 2) # result in %

# 53.87%

# Make the plots:

# In the console:
# par(mfrow = c(1,1)) # To remove graphs from being put together

plot(birds_ca, scaling = 1, main = "CA bird abundances - biplot scaling 1")
plot(birds_ca, scaling = 2, main = "CA bird abundances - biplot scaling 2")

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(birds_spe, tmp <- ordisurf(birds_ca ~ ALA , bubble = 3,
                                family = quasipoisson, knots = 2, col = 6,
                                display = "sites", main = "ALA"))
abline(h = 0, v = 0, lty = 3)
with(birds_spe, tmp <- ordisurf(birds_ca ~ WRN, bubble = 3,
                                family = quasipoisson, knots = 2, col = 6,
                                display = "sites", main = "WRN"))
abline(h = 0, v = 0, lty = 3)
with(birds_spe, tmp <- ordisurf(birds_ca ~ SWP, bubble = 3,
                                family = quasipoisson, knots = 2, col = 6,
                                display = "sites", main = "SWP"))
abline(h = 0, v = 0, lty = 3)
with(birds_spe, tmp <- ordisurf(birds_ca ~ ILT, bubble = 3,
                                family = quasipoisson, knots = 2, col = 6,
                                display = "sites", main = "ILT"))
abline(h = 0, v = 0, lty = 3)

# Load environmental data:

bird_env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt', row.names = 1) %>%
  select(-`Veg.`, -Veg_ext)
bird_env <- dplyr::select(bird_env, - 1)

bird_env

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default

(birds.env_ca <- envfit(birds_ca, bird_env, scaling = 2))
plot(birds.env_ca, col = "grey40")
plot(birds.env_ca, p.max = 0.05, col = "blue") # plot significant variables with a different colour


# Question2B --------------------------------------------------------------
# ALPINE DATA:

# Load the data:

library(readr)
alpine_sp <- read_csv("Quantitative_Ecology-main/jupyter_lab/aravo.csv") %>%
  select(-`X1`,-ZoogD)

view(alpine_sp)

dim(alpine_sp) # Read dimensions
# 75 rows & 82 columns
head(alpine_sp)

# Do the CA:

alpine_ca <- cca(alpine_sp) # Finds the CA
alpine_ca

summary(alpine_ca)

# Calculate the total inertia manually:

round(sum(alpine_ca$CA$eig), 5)

# 4.21441

# The inertia for the first axis (CA1) is:

round(alpine_ca$CA$eig[1], 5)

# CA1 = 0.066068

# The inertia of CA1 and CA2 is:

round(sum(alpine_ca$CA$eig[1:2]), 5)

# 1.08111

# The fraction of the variance explained by CA1 and CA2 is:

round(sum(alpine_ca$CA$eig[1:2]) / sum(alpine_ca$CA$eig) * 100, 2) # result in %

# 25.65%

# Make the plots:

# In the console:
# par(mfrow = c(1,1)) # To remove graphs from being put together

plot(alpine_ca, scaling = 1, main = "CA vascular plant abundances - biplot scaling 1")
plot(alpine_ca, scaling = 2, main = "CA vascular plant abundances - biplot scaling 2")

# Check summary for high species scores:

summary(alpine_ca)

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(alpine_sp, tmp <- ordisurf(alpine_ca ~ Bart.alpi , bubble = 3,
                                family = quasipoisson, knots = 2, col = 6,
                                display = "sites", main = "Bartsia alpina L."))
abline(h = 0, v = 0, lty = 3)
with(alpine_sp, tmp <- ordisurf(alpine_ca ~ Sali.retu, bubble = 3,
                                family = quasipoisson, knots = 2, col = 6,
                                display = "sites", main = "Salix reticulata L."))
abline(h = 0, v = 0, lty = 3)
with(alpine_sp, tmp <- ordisurf(alpine_ca ~ Alch.pent, bubble = 3,
                                family = quasipoisson, knots = 2, col = 6,
                                display = "sites", main = "Alchemilla pentaphyllea L."))
abline(h = 0, v = 0, lty = 3)
with(alpine_sp, tmp <- ordisurf(alpine_ca ~ Poa.supi, bubble = 3,
                                family = quasipoisson, knots = 2, col = 6,
                                display = "sites", main = "Poa supina Schrader"))
abline(h = 0, v = 0, lty = 3)


# Load environmental data:
library(readr)
alpine_env <- read_csv("Quantitative_Ecology-main/aravo_env.csv") %>%
  select(-X1) # Descriptive variables must be taken out

alpine_env

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default

(alpine.env_ca <- envfit(alpine_ca, alpine_env, scaling = 2))
plot(alpine.env_ca, col = "grey40")
plot(alpine.env_ca, p.max = 0.05, col = "blue") # plot significant variables with a different colour



# Question3A --------------------------------------------------------------
#The four panels show the four most abundant species within the CA1 analysis ( the Ca1 analysis will show)
# the majority of the species variation between the sites .
#ALA - The high abundance of this species is shown by the larger circles in the top
#left hand corner of the graph the circles decrease in size down a gradient that 
# a downward path on the left of the graph,the rest of the very low abundances are spread 
#like the bottom of a u shaped curve in the top right and left corners of the two lower 
#quadrants and the bottom left and right of the the two upper quadrants .This is due to the 
#environmental factors plotted in the fourth panel graph. In the case of ALA the factors present at 
#the specific sites that influence its high abundance are GC-Ground cover and EXP- Exposure .
#These factors influence the abundance of ALA and the species ability to deal with these 
#factors helps determine said abundance . In this case species ALA is able to deal with 
#higher exposure levels(range from 0-low to 10 -high) and with higher levels of ground cover 
#( this is understood from scaling bi plots of the environmental factors which show high correlations of
# these  factors . )
# WRN- Shows its high abundance with larger circles along the downward line side of the upside down u shaped curve
# this species has a high abundance in a large number of sites . In the cluster of sites near the  top and bottom 
# right half of the upper left quadrant this species is most abundant . in the middle of these two clusters there 
#is a site with a lower but still significant abundance . The rest of the sites have this species present but in very
#small amounts . the environmental factors affecting this species abundance within sites is also higher ground cover 
# and higher exposure as well as conifer percentage (CP),Slope(SP)and MDB . this tells us that species WRN
#does better with a higher conifer percentage and with more slope . 
#SWP - Abundance is concentrated in the bottom left corner of the upper right quadrant . the rest of the abundance 
#follows the graph curve but is made up of very small bubbles ( because very low abundance ) this may be due environmental
#factors such as SC- shrub cover and tree density- TD . This tells us that species SWP occurs most abundantly in sites 
#with higher tree density and higher shrub cover.as seen from the significant environmental factors plotted in panel 4
#ILT-the abundance is concentrated in the bottom left corner of the upper right quadrant . the rest of the abundance 
#follows the graph curve but is made up of very small bubbles ( because very low abundance ). Environmental factors 
#contributing to this are Higher tree density as well as higher tree foliage density and higher tree species diversity.
#this means that the above factors help determine the abundance of species ILT. 

# Question3B --------------------------------------------------------------

#Bartsia alpina L. is found mainly in an area with a factor of
# physical distance. It is concentrated in one area where the largest bubbles overlap.

# Species Salix reticulata L. is found in the same area as that of Bartesia alpina.
# They coexist in an area with minimal significant .
# environmental factors

# Species Alchemilla pentaphyllea L. is  concentrated in an area with the factors of form
# and snow. This species  has a gradient of high abundance to low
# abundance due to the u-shaped contour lines. This species is found throughout
# the two environmental factors.It thrives
# in any type of slope. It is recorded that this species of plant was highly
# abundant when the data was taken in 1997 - 1999 because the snow 
# factors was  high.

# Species Poa supina Schrader is a species that is concentrated in in an area where
# the most dominant factor is Form. The form former indicates the
# shape of the slope. It is coexisting with the A. pentaphyllea .

