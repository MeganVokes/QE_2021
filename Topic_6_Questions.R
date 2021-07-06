# Correlation and Associations Exercise 
# Megan Vokes

library(ggpubr)
library(tidyverse)
library(vegan)
library(Hmisc) # for rcorr()
library(corrplot)
library(RColorBrewer)


# Load the data and check stucture ----------------------------------------

Env <- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
head(Env, 1)
# drop the first column
Env <- dplyr::select(Env, -1)
head(Env)

Doubs <- round(cor(Env), 2)
Doubs_Env <- rcorr(as.matrix(Env))


# Question A--------------------------------------------------------------

#1
Env_pearson <- cor(Doubs)
corrplot(Env_pearson, method = "circle") 
        
#2  
view(Doubs_Env)
# Top Two Positive

#Top Two Negative 



#3



# Question B --------------------------------------------------------------

Doubs<- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
Doubs <- dplyr::select(Doubs, -1)
head(Doubs)

Doubs_t <- t(Doubs)         

#1 
# the data needs to be transposed beacuse the original data has the species names as coloumns and the sites as rows 
#in order for us to to compare species between sites rather than site similarity we need to make the x and y axises 
#species in order to compare 


#2
dim(Doubs_t)
# the transposed species table has 27 row and 30 coloumns   
#it now shows species on the y axis and sites on the x axis 
#it nows shows us how many sites in which each species is found ,rather than how may 
#species per site







# Question 5 --------------------------------------------------------------
Doubs_assoc1 <- vegdist(Doubs_t, method = "jaccard")
as.matrix((Doubs_assoc1))[1:10, 1:10] # display only a portion of the data
Doubs_assoc2 <- vegdist(Doubs_t, method = "jaccard", binary = TRUE)
as.matrix((Doubs_assoc2))[1:10, 1:10] # display only a portion of the data


#1
#Association matrix looks at the comparisons bewteen pairs of decrispters 
#two types of association MatrixModels
#Q mode - when pairs of ojects are compared- in Q mode the associations are 
#measured by Similarity (or dis) like in the case of Jaccard similarity
#R mode - when pairs of descriptors are paired - in R mode the dependences are
#measured by correlation


