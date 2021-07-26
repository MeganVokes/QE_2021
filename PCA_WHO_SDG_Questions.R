# PCA-SDG
# Megan Vokes 

#The WHO has designated a set of Sustainable Development Goals meant to measure the progress made by the 
#countries of the world toward Reaching certain goals . 
#The following is an analysis of the factors pertaining to the "Good Health and well being "section of the SDGs

# Question1 ---------------------------------------------------------------
# First we load the required packages 
library(tidyverse)
library(vegan)
library(missMDA) # to impute missing values
library(ggcorrplot) # for the correlations


# The second step is to load the Data . All the data is available online and as such the data is being loaded via Url 
# and not from a file . The Data is then transformed in various ways .
# 
#The function filter(Period ==) is used across all the data sets to keep only the most recent recording of that factor .
#in most cases the year filtered out is 2016 (as the most recent recording ), however some data sets are filtered for 2015 
# or other years depending on when the last (and therefore most recent) recording of the factor was . In some case 
# an aditional row was filtered out ill make note of those as they occur 
#
# Following this the select () fuction was used to remove all other coloumns excepting for Indicator,Parent Location
#Location , and fact Value Numeric 
# 
# The last Function used for all of the datasets is the Mutate() fuction used to change the names of the datasets into a more 
#workable format .  Create a new column labeled SDG with rows all having the label specified in "".
#By transforming the data in this way we can view only the necessary indicators for certian timeframes for easier analysis
SDG1.a <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG1.a_domestic_health_expenditure.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG1.a")
#
SDG3.1_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.1_maternal_mort.csv") %>%
  filter(Period == 2016,
         Indicator == "Maternal mortality ratio (per 100 000 live births)") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.1_1")
#
SDG3.1_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.1_skilled_births.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.1_2")
# 
SDG3.2_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_neonatal_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_1")
# 
SDG3.2_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_under_5_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_2")
# 
SDG3.2_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_infant_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_3")
# 
SDG3.3_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_new_HIV_infections.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_1")
# 
SDG3.3_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_TB.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_2")
# 
SDG3.3_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_malaria.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_3")
# 
SDG3.3_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_hepatitis_B.csv") %>%
  filter(Period == 2015) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_4")
# 
SDG3.3_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_NCD_interventions.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_5")
# 
SDG3.4_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_adult_death_prob.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_1")
# 
SDG3.4_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Diabetes mellitus") %>%# in the case of this factor 2 additional rows are being filtered out 
  mutate(Indicator = Dim2) %>%#the mutate function is being used change a column name to match the other data sets (useful when analysing)
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_2")

SDG3.4_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Cardiovascular diseases") %>%# in the case of this factor 2 additional rows are being filtered out 
  mutate(Indicator = Dim2) %>%#the mutate function is being used change a column name to match the other data sets (useful when analysing)
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_3")

SDG3.4_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Respiratory diseases") %>%
  mutate(Indicator = Dim2) %>%#the mutate function is being used change a column name to match the other data sets (useful when analysing)
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_4")
# 
SDG3.4_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_suicides.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_5")
# l.csv"
SDG3.4_6 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_data_total.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_6")
# 
SDG3.5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.5_alcohol_consumption.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.5")
# 
SDG3.6 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.6_traffic_deaths_prop.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.6")
# 
SDG3.7 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.7_adolescent_births.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.7")
# 
SDG3.8_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.7_adolescent_births.csv") %>%
  filter(Period == "2013-2017") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.8_1")
# 
SDG3.8_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.8_UHC_index_of_service_coverage.csv") %>%
  filter(Period == 2017) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.8_2")
# 
SDG3.9_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.9_unintentional_poisoning_prop.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.9_1")
# 
SDG3.9_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.9_WASH_mortalities.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.9_3")
#
SDG16.1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG16.1_homicides.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG16.1")
# 
SDG3.a <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.a_tobacco_control.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.a")
# 
SDG3.b_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_dev_assistence_for_med_research.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_1")
# 
SDG3.b_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_measles_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_2")
# 
SDG3.b_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_diphtheria_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_3")
# 
SDG3.b_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_pneumococcal_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_4")
# 
SDG3.b_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_HPV_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_5")
# 
SDG3.c_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Medical doctors (per 10,000)") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_1")

SDG3.c_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Nursing and midwifery personnel (per 10,000)") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_2")

SDG3.c_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Dentists (per 10,000)") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_3")

SDG3.c_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Pharmacists  (per 10,000)") %>%# in the case of this factor an additional row is being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_4")
# 
SDG3.d_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.d_health_risks.csv")  %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.d_1")
# 
other_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_Other_life_expectancy.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes",
         Indicator == "Life expectancy at birth (years)") %>%# in the case of this factor 2 additional rows are being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "other_1")
# 
other_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_Other_life_expectancy.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes",
         Indicator == "Life expectancy at age 60 (years)") %>%# in the case of this factor 2 additional rows are being filtered out 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "other_2")


##
#The rbind fuction is used to merge/join/bring together all the rows of the loaded and adjusted datasets under the different columns
#hence the need for columns with the same names and macthing numbers of coloumns 
# do.call carries out a function call from a function or list arguements given to it.
health <- do.call("rbind", lapply(ls(),get))
head(health)
#
unique(health[, c(5, 1)])
# the unique function extracts (removing duplicates)
#  the [] brackets specify that only the variables in column 5 and 1 must be retained.
# With this method, a list of all the SDGs is created.
health_wide <- health %>%
  arrange(Location) %>%#arranges the data alphabetically based on location.
  select(-Indicator) %>%#select works as above but the - means that coloumn is removed instead of all the others 
  pivot_wider(names_from = SDG, values_from = FactValueNumeric) %>%
  as_tibble()#makes a table
# pivtor_wider increases the number of columns and decreases the number of
# rows, making the data wider. It splits  one column into 
#  respective columns in order to make available hidden numerical values.
# names from SDG are extracted and made into columns, and their corresponding values for
# FactValueNumeric are placed accordingly.
#
popl <- read_csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_population.csv") %>%
  filter(Year == 2016) %>% # retain only the population data for the year 2016.
  rename(popl_size = `Population (in thousands) total`, #renames column `Population (in thousand) total` to popl_size.
         Location = Country) %>% # renames Country into Location to match health_wide data set.
  select(Location, popl_size) %>% # retain only columns pol_size and Location
  mutate(popl_size = as.numeric(gsub("[[:space:]]", "", popl_size)) * 1000) # creates a new column - popl_size where population size is  a numerical value.

health_wide <- health_wide %>%
  left_join(popl) #joins population data per country with health_wide data.
head(health_wide)
glimpse(health_wide)
# data sets are joined based on variable column they have in common (Location)

health_wide <- health_wide %>%
  mutate(SDG3.4_4 = SDG3.4_4 / popl_size * 100000,
         SDG3.4_3 = SDG3.4_3 / popl_size * 100000,
         SDG3.4_2 = SDG3.4_2 / popl_size * 100000,
         SDG3.4_6 = SDG3.4_6 / 100,
         SDG3.2_2 = SDG3.2_2 / popl_size * 100000,
         SDG3.2_3 = SDG3.2_3 / popl_size * 100000,
         SDG3.2_1 = SDG3.2_1 / popl_size * 100000)

# certain columns are standardised in order to have all variables expressed as a unit of population size.



## Histograms of missing values, and correlations --------------

health_wide$na_count <- apply(health_wide[, 3:(ncol(health_wide) - 1)], 1,
                              function(x) sum(is.na(x)))
# na_column is made by which sums up all the NA values within a specific row.
hist(health_wide$na_count, breaks = 14, plot = TRUE)
# creates a histogramn comparing the the frequency of the NA values at equal frequencies divided into 14 bins
# remove rows where there are more than 10 NAs
health_wide <- health_wide %>%
  filter(na_count <= 10) %>%
  select(-na_count)


# calculate pairwise correlations
corr <- round(cor(health_wide[, 3:(ncol(health_wide) - 1)]), 1)
view(corr)

# visualization of the correlation matrix
ggcorrplot(corr, type = 'upper', outline.col = "grey60", # type species that the upper values (above the linear line) must be plotted.
           # aesthetics are set: the values are outlines in grey
           colors = c("#1679a1", "white", "#f8766d"), # the colours used for the gradient legend is set.
           lab = TRUE) # labels must be shown.
# Impute remaining NAs:---------------------------------------------------------
health_wide_complete <- imputePCA(health_wide[, 3:(ncol(health_wide) - 1)])$completeObs
# The missing values are shown in the dataset, in order to do the PCA.

# Scale and center the data and do the PCA ------
health_wide_complete_std <- decostand(health_wide_complete, method = "standardize") # the data is standardsied.
health_pca <- rda(health_wide_complete_std) # pca is calculated
health_pca # Information on intertia, eigenvalues for each ordinations can be seen.
# Through the
summary(health_pca) #provides more information such as species scores and site scores.
# Importance component can also be viewed> It shows the proportion of variation
# represented in each ordination. Ordinations have an increasingly lower eigenvalue
# the more ordination are constructed.


# Graphs ------------------------------------------------------------------
par(mfrow = c(1, 2))
biplot(health_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))#relationships between "sites" are shown
biplot(health_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))#relationships between "species" are shown
## The chosen ordinations are PC1 and PC2 which hold the bulk of the information of variation.

#For easier visualisation and understanding the plots have more aestetic value added to them 
pl1 <- ordiplot(health_pca, type = "none", scaling = 1, main = "PCA WHO/SDG scaling 1") # main = title.
points(pl1, "sites", pch = 21, cex = 1.0, col = "grey20", bg = "grey80") # the points showing  "sites" are given a set of aesthetics.
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE) # the points representing "species" are given a set of aesthetics.
text(pl1, "species", col = "blue4", cex = 0.9) # test regarding "species" are given a specific aesthetic
text(pl1, "sites", col = "red4", cex = 0.9)# test regarding "sites" are given a specfic aesthetic

pl2 <- ordiplot(health_pca, type = "none", scaling = 2, main = "PCA WHO/SDG scaling 2")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)


#Ordination plot
site_scores <- tibble(ParentLocation = health_wide$ParentLocation,
                     Location = health_wide$Location) # site_scores are made into table data and coloumns are added.
# Parent location is gotten from ParentLocation in the health_wide data set. This is specified using the $. The same applies to Location.

site_scores <- tibble(cbind(site_scores, scores(health_pca, display = "sites", choices = c(1:7)))) #
# site_scores is combined with the first 7 PCs in site data from health_pca.

species_scores <- data.frame(scores(health_pca, display = "species", choices = c(1:7)))
# the species scores data for the first 7 PCs from health_pca are assigned as a data frame called species_scores.

species_scores$species <- rownames(species_scores) # species column in species_scores is renamed to species_scores.  
species_scores <- tibble(species_scores) # species_scores data frame is converted into a table format.

ggplot(data = site_scores, aes(x = PC1, y = PC2)) + # the data from site_scores is used to great a plot with ggplot. The axis are set.
  geom_point(aes(col = ParentLocation)) + # Points are differentiated by colour based on ParentLocation.
  geom_segment(data = species_scores, # geom_segment draws a straight line based on x, y and xend, and yend points.
               aes(x = 0, y = 0, xend = PC1, yend = PC2), # the straight line is drawn from the origin to the PCs site scores.
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), # an arrow is added  to show direction. (helps with interpretation of data).
               color = "lightseagreen", alpha = 1, size = 0.3) + # colour, size, and transparency of arrow and line is set.
  geom_text(data = species_scores, # geom_text is used to insert and edit text that appears on the plot.
            aes(x = PC1, y = PC2, label = species), # x and y positions are set as PC1 and PC 2 respectively, and the nature of the text (label) is set to "species" (i.e. SDGs).
            color = "black") + # Colour of text is set.
  xlab("PC1") + ylab("PC2") + # x labels and y labels are specified.
  
  scale_y_continuous(breaks = c(-8,-7,-6,-5,-4,-3,-2,-1,0,1)) + #dictates the scale on the y axis
  
  ggtitle("WHO SDGs, Scaling 2") # The title of the plot is specified in "".
# With the use of ggplot, the plot's aesthetics can be edited be allow for easier
# and more effective interpretations.



# Question2 ---------------------------------------------------------------

#Biplot Scaling 1- Shows the majority of the varience between sites shown by PC1 and PC2
#The majority of the sites are clustered together on the grapgh due to the fact that those countries use the same indicators
#for thier SDG measurement. The outlier countries have differing factors by which thier SDG are measured . This can be 
#this can be attributed to the fact that these outlier contries may not have the data available to measure their 
#SDGs like the concentrated countries , owing to socio-economic factors . The trend seen in the graph is 
#change from more impoverished area to more financially stable areas
# can be seen from left to right.

#Biplot Scalling 2-  the correlation between measurements used to attain SDG is shown
#  this plot, follows the trend referenced in biplot scalling 1 and 
#we can see negative correlations between measurements on left
# to measurements on the right had side of the plot. 
#The factors that have the most influence on a countries SDGs depends on the countries 
#Economic level . Poorer countries SDGs are more influenced by HIV,Malaria and Child mortality 
#These countries share positive correlations with these specific factors 
#richer countries share a negative correlation with these factors as thier SDGs are more inflenced by
#higheer life expectancy and health expenditure etc. Ths supports the trend and tells us that healthcare acess 
#is severly unequal across countries . 


###Key Countries 
view(site_scores)
site_scores <- site_scores %>%
  filter(Location %in% c("South Africa", "Argentina", "Indonesia", "United States of America"))
# Filters the data so that the only site scores present are that of the stipulated locations.


ggplot(data = site_scores, aes(x = PC1, y = PC2)) + # the data from site_scores is used to great a plot with ggplot. The axis are set.
  geom_point(aes(col = Location),size = 4) +# Points are differentiated by colour based Location.
  scale_color_manual(values = c("dark red","green","dark orchid ", " orange"))+
  geom_segment(data = species_scores, # geom_segment draws a straight line based on x, y and xend, and yend points.
               aes(x = 0, y = 0, xend = PC1, yend = PC2), # the straight line is drawn from the center (origin) to the PCs site scores.
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), # an arrow is added to the lines to show direction. (assist in interpretation of data).
               color = "lightseagreen", alpha = 1, size = 0.3) + # colour, size, and transparency of arrow and line is set.
  geom_text(data = species_scores, # geom_text is used to insert and edit text that appears on the plot.
            aes(x = PC1, y = PC2, label = species), # x and y positions are set as PC1 and PC 2 respectively, and tthe text  is set to "species" (i.e. SDGs).
            color = "black") + # Colour of text is set.
  xlab("PC1") + ylab("PC2") + # x labels and y labels are specified.
  ggtitle("WHO SDGs (Country based), Scaling 2")
 ###
#South Africa's SDG, is more strongly influenced and correlated to high
# values in deaths by non-communicable diseases and Crude suicide
# rates (per 100 000 population). the USA are positively correlated to all the factors that have
# high values in wealthier countries  South Africa has a
# SDG which is also most  influenced by all factors in the upper quadrants of the plot which are the factors 
#that have more influence on the poor countries .South Africa has an interesting yet very prevalent wealth split . 
#There is  poverty , a staggered middle class and a extreme waelth class . Access and equality are issues of contention within 
#the country. 


# Question3  --------------------------------------------------------------
# The data used comes from various years as not all of the factors are not measured
#at the same time or on a regular basis. That being said the measurements are 5 or more 
# years old (kinda outdated ) Thus they may not reflect the true nature of a country or region within the present time 
#Their is also the fact to consider that this system is infinetly skewed toward wealthier countries who can manage these
#sustainability goals by the deadline . poorer countries often have histories featuring politcal unrest that has a negative
#influence on their ability to reach the goals . 



