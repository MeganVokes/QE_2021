# PCA Questions 
# Megan Vokes 
# Date : 12/07/2021

library(tidyverse)
library(vegan)
library(stats)
library(readr)

# Question A --------------------------------------------------------------
# The data always shows a linear relationship . Its unable to display all the details because it cam only show 3 planes , the effects 
# of the other details are little and so they matter less 
# Question B --------------------------------------------------------------
#1
# Birds



ybirds.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt', row.names = 1)
ybirds.env <- dplyr::select(ybirds.env,-Veg., -Veg_ext)
head(ybirds.env)


env_pca <- rda(ybirds.env, scale = TRUE)
env_pca

summary(env_pca)

biplot(env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# Now we create biplots using the cleanplot.pca() function that comes with
# Numerical Ecology in R book:

source("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")

cleanplot.pca(env_pca, scaling = 1)
cleanplot.pca(env_pca, scaling = 2)









#Aravo
Aravo_env <- read.csv("Quantitative_Ecology-main/jupyter_lab/aravo.csv") %>% 
select(-X, -ZoogD) # Descriptive variables must be taken out

dim(Aravo_env) # Read dimensions
# 75 rows & 5 columns
head(Aravo_env)

Aravo_pca <- rda(Aravo_env, scale = TRUE)
Aravo_pca

summary(Aravo_pca)# provides a more detailed summary that shows eigen values , site scaling etc. 

# Called ordination diagrams.
# Use the biplot() function.

biplot(Aravo_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(Aravo_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# Now we create biplots using the cleanplot.pca() function that comes with
# Numerical Ecology in R book:

source("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")

cleanplot.pca(Aravo_pca, scaling = 1)
cleanplot.pca(Aravo_pca, scaling = 2)


#2
#A - 