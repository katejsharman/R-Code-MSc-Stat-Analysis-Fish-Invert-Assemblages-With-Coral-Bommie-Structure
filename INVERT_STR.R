######################################################################
##                            Kate Sharman                          ##
##                             24/09/2019                           ##            
##           ANOVA script for invert.richness/habitats              ##
######################################################################

# Question: Do different habitat structure attributes affect the Invert species richness of inverts in bommie

########################################################################
# Previous step: import data set InvertCat; 
########################################################################

########################################################################
# load packages from R
library(lattice) # for multipanel graphs
library(latticeExtra) # for multipanel graphs
library(HH) # for the Brown and Forsyth test
library(car) # for the Levene Test
library(nortest) # for the Anderson-Darling test
########################################################################
rm()
invert.cat <- data.frame(InvertCat)
########################################################################
# Step 0: Inspect the file

invert.cat  # all the data
head(invert.cat) # first 6 rows
head(invert.cat,10) # first 10 rows
str(invert.cat)
#all okay I think

summary(invert.cat) # quick summary 
########################################################################

########################################################################
# Step 1: Outliers y and x

par(mfrow = c(1, 2)) # 1 row and 2 columns 

# boxplot of Invert_rich
boxplot(invert.cat$Invert_rich, 
        main = "Invert species richness") # title label

# dotplot of Invert_rich
dotchart(invert.cat$Invert_rich, 
         xlab = "Range of data",
         ylab = "Values")
boxplot(invert.cat$Invert_rich, 
        main = "Invert species richness") # title label

# their appears to one outlier however none will be removed as all are actual observations
########################################################################


########################################################################
# Step 2: Homogeneity of variances

# conditional boxplot: Invert_rich by habitat structure variable (indepedently)
par(mfrow = c(2,3))
boxplot(Invert_rich ~ Dist_neighbour, data=invert.cat, 
        xlab="Distance (m)",
        ylab="Invert species richness", 
        main="Conditional Boxplot")
#
boxplot(Invert_rich ~ Neighbour_5m, data=invert.cat, 
        xlab="Number of neighbours within 5m",
        ylab="Invert species richness", 
        main="Conditional Boxplot")
#
boxplot(Invert_rich ~ Habitat, data=invert.cat, 
        xlab="Habitat",
        ylab="Invert species richness", 
        main="Conditional Boxplot")
#
boxplot(Invert_rich ~ HAS_2, data=invert.cat, 
        xlab="HAS_2",
        ylab="Invert species richness", 
        main="Conditional Boxplot")
#
boxplot(Invert_rich ~ Rugosity, data=invert.cat, 
        xlab="Rugosity",
        ylab="Invert species richness", 
        main="Conditional Boxplot")
#

# to investigate further:
bartlett.test(Invert_rich ~ Rugosity, data=invert.cat)
bartlett.test(Invert_rich ~ HAS_2, data=invert.cat)
bartlett.test(Invert_rich ~ Habitat, data=invert.cat)
bartlett.test(Invert_rich ~ Neighbour_5m, data=invert.cat)
bartlett.test(Invert_rich ~ Dist_neighbour, data=invert.cat)
#

#Levene test

leveneTest(Invert_rich ~ Dist_neighbour, data=invert.cat)

leveneTest(Invert_rich ~ Neighbour_5m, data=invert.cat)

leveneTest(Invert_rich ~ Habitat, data=invert.cat)

leveneTest(Invert_rich ~ HAS_2, data=invert.cat)

leveneTest(Invert_rich ~ Rugosity, data=invert.cat)
# All return a P>0.05 therefore assumption of homogeneity is accepted




########################################################################
# Step 3: Normality
par(mfrow = c(1,1))
# histogram for the whole Invert_rich data set
hist(invert.cat$Invert_rich,
     xlab = "Invert species richness", 
     breaks = 20,
     main = "Histogram", 
     ylab = "Frequency",
     col ="grey") 
# does NOT indicate a normal distribution par(mfrow = c(2,3))
# histogram for Invert_rich data by habitat structure variable (indepedently)
histogram( ~ Invert_rich | Dist_neighbour , 
           data=invert.cat,
           xlab = "Invert species richness", 
           breaks = 10, 
           col="grey",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )
#
histogram( ~ Invert_rich | Neighbour_5m , 
           data=invert.cat,
           xlab = "Invert species richness", 
           breaks = 10, 
           col="grey",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )
#
histogram( ~ Invert_rich | HAS_2 , 
           data=invert.cat,
           xlab = "Invert species richness", 
           breaks = 10, 
           col="grey",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )
#
histogram( ~ Invert_rich | Habitat , 
           data=invert.cat,
           xlab = "Invert species richness", 
           breaks = 10, 
           col="grey",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )
#
histogram( ~ Invert_rich | Rugosity , 
           data=invert.cat,
           xlab = "Invert species richness", 
           breaks = 10, 
           col="grey",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )
#
# does not suggest normailty within levels of each independent variable
# formal tests for normality:
# Shapiro-Wilk test
shapiro.test(invert.cat$Invert_rich)
shapiro.test(invert.cat$Invert_rich[invert.cat$Dist_neighbour])
shapiro.test(invert.cat$Invert_rich[invert.cat$Neighbour_5m])#
shapiro.test(invert.cat$Invert_rich[invert.cat$Rugosity]) 
shapiro.test(invert.cat$Invert_rich[invert.cat$HAS_2]) 
shapiro.test(invert.cat$Invert_rich[invert.cat$Habitat]) 
# all return p<0.05 therefore data normality not present however this will be tested in the residuals

###########################################################################################


###########################################################################################
# Steps 4-7 of data exsploration are not required for this test
###########################################################################################

###########################################################################################
# ANOVA


ANOVA<-aov(Invert_rich ~ Habitat * HAS_2 * Rugosity, data = InvertCat)
summary(ANOVA)
# only Habitat, HAS_2 and Rugosity significant
# no significant interactions found
TukeyHSD(ANOVA)
#
#
# Habitat
#diff       lwr        upr    p adj
#Seagrass-Bommie -1.543367 -2.412308 -0.6744263 0.000593
#
#
# HAS:
#diff         lwr        upr     p adj
#Low-High    -2.5527524 -3.90852150 -1.1969833 0.0000479
#Medium-High -1.9412096 -3.25523291 -0.6271863 0.0017829
#Medium-Low   0.6115428 -0.04969613  1.2727817 0.0762862
#
#
# Rugosity:
#diff        lwr        upr     p adj
#Low-High     2.1855224  0.5193142  3.8517305 0.0064026
#Medium-High  0.1688604 -0.4718843  0.8096050 0.8072782
#Medium-Low  -2.0166620 -3.7039244 -0.3293996 0.0145857
#
#

#
#
# To visualise differences of invertebrate species richness within the structural variables:
par(mfrow =c(1,3))
boxplot(Invert_rich ~ Habitat, data=invert.cat, 
        xlab="Habitat",
        ylab="Invert species richness", 
        main="Conditional Boxplot")
#
#
boxplot(Invert_rich ~ HAS_2, data=invert.cat, 
        xlab="HAS",
        ylab="Invert species richness", 
        main="Conditional Boxplot")
#
#
boxplot(Invert_rich ~ Rugosity, data=invert.cat, 
        xlab="Rugosity",
        ylab="Invert species richness", 
        main="Conditional Boxplot")

par(mfrow =c(3,2))
plot(ANOVA)


qqnorm(ANOVA$resid)
qqline(ANOVA$resid)
hist(ANOVA$resid)
# residuals vs fitted: relatively shapeless therefore good
# normal q-q:normal distribution
# scale-location :potentially has homogeneity of variance
# residuals vs leverage: no point is near cook's distance
#                       therefore no influential data points


library(moments)


skewness(ANOVA$resid)
# distribution is approximately symmetric but slightly skewed as returned value = 0.4728
# therefore probably not normally distributed 
#
E1 <- resid(ANOVA)   #Variable E1 contains now the distribution of residuals
F1 <- fitted(ANOVA)  #variable F1 contains now the distribution of the expected outcome of Y based on values of X
shapiro.test(E1)
#P<0.05 therefore the residuala are not normally distribution
#
# so lets try ...
#
library(MASS)
par(mfrow = c(1,1))
b=boxcox(Invert_rich2 ~ Habitat * HAS_2 * Rugosity, data = InvertCat)
b
lamda=b$x
lik=b$y
bc=cbind(lamda,lik)
bc
bc[order(-lik),]

TRANSANOVA<-aov(Invert_rich2^(1/3) ~ Habitat * HAS_2 * Rugosity, data = InvertCat) # as lamda=0.383838
summary(TRANSANOVA)
# Only HAS_2 significant differences ... Habitat and HAS
TukeyHSD(TRANSANOVA) # Only need to look at HAS_2 and Habitat output:
#
#
# 
#Habitat
#diff        lwr        upr    p adj
#Seagrass-Bommie -0.428452 -0.6599203 -0.1969836 0.000352
#
#
#
#HAS_2:
#diff        lwr         upr     p adj
#Low-High    -0.5272311 -0.8883806 -0.16608166 0.0020514
#Medium-High -0.3272367 -0.6772660  0.02279249 0.0722238
#Medium-Low   0.1999944  0.0238537  0.37613506 0.0217060HAS_2
#
#
# to visualise differences:
#
#
# Habitat:
boxplot(Invert_rich2 ~ Habitat, data=invert.cat, 
        xlab="Habitat",
        ylab="Invert species richness", 
        main="Conditional Boxplot")
#
#
# HAS
boxplot(Invert_rich2 ~ HAS_2, data=invert.cat, 
        xlab="HAS_2",
        ylab="Invert species richness", 
        main="Conditional Boxplot")



# comparing residuals from before and after boxcox transformation

windows()
par(mfrow =c(3,2))
plot(ANOVA)
qqnorm(ANOVA$resid)
qqline(ANOVA$resid)
hist(ANOVA$resid)

#

windows()
par(mfrow =c(3,2))
plot(TRANSANOVA)
qqnorm(TRANSANOVA$resid)
qqline(TRANSANOVA$resid)
hist(TRANSANOVA$resid)
#
# Data normality looks less likely now
#
# Compare the skewness of data for both ANOVAs
skewness(ANOVA$resid)
skewness(TRANSANOVA$resid)
# Transformed ANOVA (TRANSANOVA) is highly skewed (-1.325) the origional ANOVA
# (ANOVA) is less skewed (0.472)
E2 <- resid(TRANSANOVA)   #Variable E1 contains now the distribution of residuals
F2 <- fitted(TRANSANOVA)  #variable F1 contains now the distribution of the expected outcome of Y based on values of X
shapiro.test(E2)#
###########################################################################################

#
#
# AS IT DOES NOT HAVE normality how source says: "violations of normality are not usually a death sentence for validity" due to large sample size
#######################################################################################
# END