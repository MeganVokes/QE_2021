#nMDS Questions
#Megan Vokes 
#QE

library(tidyverse)
library(vegan)





# Question 1 --------------------------------------------------------------
 
data(package = "vegan")
Mite <- data(mite)
Mite_env <- data("mite.env")

# Unconstrained Ordination Technique 1
Mite_env <- dplyr::select(mite.env,-Substrate , -Shrub,-Topo)
head(Mite_env)


env_pca <- rda(Mite_env, scale = TRUE)
env_pca

summary(env_pca)

biplot(env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# Now we create biplots using the cleanplot.pca() function that comes with
# Numerical Ecology in R book:

source("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")

cleanplot.pca(env_pca, scaling = 1)
cleanplot.pca(env_pca, scaling = 2)





# Unconstrained Ordination Method 2 ---------------------------------------

# Work out the dissimilarity matrix - Bray-Curtis:

Mite_bray <- vegdist(mite)

Mite_bray # To view the dissimilarity matrix

# Work out the PCoA:

# spe_pcoa <- cmdscale(spe_bray, k = nrow(spe) - 1, eig = TRUE)
Mite_pcoa <- capscale(Mite_bray ~ 1)
Mite_pcoa

summary(Mite_pcoa)

Mite_pcoa <- capscale(mite ~ 1, distance = "bray")
Mite_pcoa

# This method shows the species scores

# To see what is inside the results....
str(Mite_pcoa)

# The percentage inertia explained by the first three axes is therefore:

round(sum(Mite_pcoa$CA$eig[1:3]) / sum(Mite_pcoa$CA$eig) * 100, 2)
# 54.77%

# Ordination diagrams:

plot(Mite_pcoa, scaling = 1, main = "PCoA Oribatid mites - biplot scaling 1")
plot(Mite_pcoa, scaling = 2, main = "PCoA Oribatid mites - biplot scaling 2")

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(mite, tmp <- ordisurf(Mite_pcoa ~ RARD, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "RARD"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(Mite_pcoa ~ Miniglmn, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Miniglmn"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(Mite_pcoa ~ Protopl, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Protopl"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(Mite_pcoa ~ Trimalc2, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Trimalc2"))
abline(h = 0, v = 0, lty = 3)

Mite_pcoa_env <- envfit(Mite_pcoa, mite.env, scaling = 2)
plot(Mite_pcoa_env, p.max = 0.05, col = "red")

#Discussion
#PCA - the four panels shoe the four species with the largest negative of positive values in PCA1 . 
#in the fourth panel the environmental factors are also plotted. 
# The larger bubbles show abundabce and the green countour lines connect sites with similar abundances . 
# in the firs panel the sites with the highest abundance can be seen in the upper right quadrant .In the seconfd panel the 
# the abundace is concentrated in the sites showning in the upper right quadrant a similar abundance is also plotted for the 
#third panel . These three species have similar distributionnand frequecies across the sites . the species represented by
# the fourth panel shows a very different abundance concentration in sites that the other three soecies have 
#relatively low abundance in .This is possibliy due to the enviromental factor of Water content of the substrate 
#we can see that in the sites in which the species in thhe fourth panel are most abundant that WatrCont is high .This may be the
#the reason for this species sucees in that area . 






















# Question 2 --------------------------------------------------------------

Dune <- data(dune) 
Dune_env <- data("dune.env")
#Unconstrained Ordination Technique 3


dim(dune) # Read dimensions
# 75 rows & 82 columns
head(dune)

# Do the CA:

Dune_ca <- cca(dune) # Finds the CA
Dune_ca

summary(Dune_ca)

# Calculate the total inertia manually:

round(sum(Dune_ca$CA$eig), 5)

# 4.21441

# The inertia for the first axis (CA1) is:

round(Dune_ca$CA$eig[1], 5)

# CA1 = 0.066068

# The inertia of CA1 and CA2 is:
 
round(sum(Dune_ca$CA$eig[1:2]), 5)

# 1.08111

# The fraction of the variance explained by CA1 and CA2 is:

round(sum(Dune_ca$CA$eig[1:2]) / sum(Dune_ca$CA$eig) * 100, 2) # result in %

# 25.65%

# Make the plots:

# In the console:
# par(mfrow = c(1,1)) # To remove graphs from being put together

plot(Dune_ca, scaling = 1, main = "CA Vegetation in Dutch Dune Meadows. - biplot scaling 1")
plot(Dune_ca, scaling = 2, main = "CA Vegetation in Dutch Dune Meadows - biplot scaling 2")

# Check summary for high species scores:

summary(Dune_ca)

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(dune, tmp <- ordisurf(Dune_ca ~ Callcusp, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Callcusp"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(Dune_ca ~ Comapalu, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Comapalu"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(Dune_ca ~ Eleopalu, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Eleopalu"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(Dune_ca ~ Ranuflam, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Ranuflam"))
abline(h = 0, v = 0, lty = 3)


# puts  environmental variables in a CA (panel 4)
(Dune_ca_env <- envfit(Dune_ca,dune.env, scaling = 2))
plot(Dune_ca_env, p.max = 0.05, col = "Navy") # plot significant variables with a
# different colour



#Unconstrained Ordination Method 4
dune_nmds <- metaMDS(dune, distance = "bray")

nmds <- metaMDS(dune, metric = "gower")

nmds

summary(nmds)

# Plots:

par(mfrow = c(2, 2))
stressplot(dune_nmds, main = "Shepard plot")
ordiplot(dune_nmds, type = "t", cex = 1.5, main = paste0("nMDS stress = ", round(dune_nmds$stress, 2)))
gof = goodness(dune_nmds)
plot(dune_nmds, type = "t", main = "Goodness of fit")
points(dune_nmds, display = "sites", cex = gof * 200)

nmds$stress

# Ordination diagrams:

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(dune, tmp <- ordisurf(dune_nmds ~ Callcusp, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Callcusp"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_nmds  ~Comapalu , bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Comapalu"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_nmds ~ Eleopalu , bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Eleopalu"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_nmds ~ Ranuflam , bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Ranuflam"))
abline(h = 0, v = 0, lty = 3)

# Add the environmental variables:

(spe_nmds_env <- envfit(dune_nmds, dune.env))
plot(spe_nmds_env, p.max = 0.05, col = "red")



#Discussion
#CA
#The four panels show the four most abundant species within the CA1 analysis ( the Ca1 analysis will show)
# the majority of the species variation between the sites .
#Abundance is shown by the size of the bubbles . Across The four panels we can see that the abundance is largely 
#concentrated on the right side of the graphs (for all four highest contributing species )
#We also the environmental factors that influence abundance in the bottom left panel . The significant factors to
# The abundance of these species at these sites is moisture 5 and A1
#nMDS
#the Shepard/stress plot shows the relationship between the dissimilarity matrix pf the data and the rank ordered 
#distance . This shown by the red line . the surrounding scattered plotted points show us that the stress value 
#does not exceed 0.2 and is relatively low .
#in the bottom left panel .the size of the bubble is proportional to how well the rank ordered distances match up 
# to the dissimilarity matrix
#Sites 17,19,18,11,12 and 14 are most heavily influenced by the species .

