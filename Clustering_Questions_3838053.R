#Clustering analysis 
#Megan Vokes 


library(tidyverse) 
library(cluster)
library(ggcorrplot)
library(factoextra)
library(vegan)
library(ggpubr)

# 
SDGs <- read_csv("Quantitative_Ecology-main//exercises/WHO/SDG_complete.csv")

 
SDGs[1:5, 1:8]

unique(SDGs$ParentLocation)

length(SDGs$Location)

#  correlation matrix: 
correlation <- round(cor(SDGs[3:ncol(SDGs)]), 1)
ggcorrplot(correlation, type = 'upper', outline.col = "white", 
           colors = c("navy", "white", "#FC4E07"), 
           lab = TRUE)

# Data is too large to see the collinearity, therefore do the PCA:

SDGs_std <- decostand(SDGs[3:ncol(SDGs)], method = "standardize") # Standardise the data 

# SDGs_euc <- vegdist(SDGs_std, method = "euclidian")

rownames(SDGs_std) <- SDGs$Location # carry location names into output

# Silhouette analysis: 

plt1 <- fviz_nbclust(SDGs_std, cluster::pam, method = "silhouette")

# Elbow analysis: 
plt2 <- fviz_nbclust(SDGs_std, cluster::pam, method = "wss")

# Gap statistics: 

plt3 <- fviz_nbclust(SDGs_std, cluster::pam, method = "gap_stat")

# Create plots in one plane: 

ggarrange(plt1, plt2, plt3, nrow = 3)


# Cluster Analysis: 
SDGs_pam <- pam(SDGs_std, metric = "euclidean", k = 3)

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex", 
             palette = c("seagreen4", "purple", "navy blue"), ellipse.alpha = 0.05) +
  geom_text(aes(label = SDGs$Location), size = 2.75)

# South Africa: 
SDGs <- SDGs |> 
  mutate(col_vec = ifelse(Location == "South Africa", "black", "grey50"),
         scale_vec = ifelse(Location == "South Africa", 3.5, 2.5))

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex",
             palette = c("seagreen4","purple", "navy blue"),
             ellipse.alpha = 0.05, pointsize = 2.0) +
  geom_text(aes(label = SDGs$Location), size = SDGs$scale_vec, col = SDGs$col_vec)


# Another cluster: 
fviz_cluster(SDGs_pam, palette = c("seagreen4", "purple", "navy blue"), 
             ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, 
             pointsize = SDGs$scale_vec * 0.8) +
  theme_bw()

# Cluster fidelity: 
SDGs_centroids <- SDGs %>% 
  mutate(cluster = SDGs_pam$clustering) %>% 
  group_by(cluster) %>% 
  summarise_at(vars(other_1:SDG3.b_5), median, na.rm = TRUE)
SDGs_centroids

pairs(SDGs[, 3:10], col = c("seagreen4", "purple", "navy blue")[SDGs_pam$clustering])



# Question1 ---------------------------------------------------------------

# When  3 or more clusters are used with  the pam() function, an error occurs.
# that says there is insufficient values found in the manual scale. 
# Certain datasets,  can only produce a certain amount of clusters.
# The grouping of the clusters in the data is also set at a specific amount 
#The pam() function reduces the sum of dissimilarities rather than 
# that of the sum of squared euclidean distances. Using more clusters than the data 
# can group together causes the clusters to overlap  making the information 
# unreliable as the locatins can be found in multiple clusters. 


# Question2 ---------------------------------------------------------------


# Using 3 clusters should suffice as the countries can be placed according to their
# financial status. Clusters will be formed such as the first world countries etc.


# Question 3 --------------------------------------------------------------



SDGs_kmeans <- kmeans(SDGs_std, centers = 3)
fviz_cluster(SDGs_kmeans, data = SDGs_std,
             geom = "text",
             ellipse.type = "confidence",)

# Using the pam() function is similar to that of the kmeans method. 


# Question 4 --------------------------------------------------------------



# The clusters are grouped together by parent locations. All the 
# African , South American , European  and North American
# Countries are grouped together in different clusters. The clusters show that the 
# countries are all grouped based on  the financial 
# status of the country looked at. Due to  poor financial status, 
# African and South American countries are grouped together. 


# Question 5 --------------------------------------------------------------


# South Africa is found in an area where it is supposed to b grouped together as 
# a developing country. 
# The huge gap bewteen the financia satuses of the suth african people means that the financials  
# of the country dont reach the necessary levels to achieve the SDGs despite the multitudes of south africas natural 
#resources ( these are also grossly mismanaged )

