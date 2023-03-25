######################################################################
##                            Kate Sharman                          ##
##                             24/09/2019                           ##            
##  ANOVA script for sp.richness#habitats ##
######################################################################

# Question: Do different habitat structure attributes aggect the species richness of fish in bommies

########################################################################
# Previous step: import data set Category; 
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
cat.data <- data.frame(Category)

########################################################################
# Step 0: Inspect the file

cat.data  # all the data
head(cat.data) # first 6 rows
head(cat.data,10) # first 10 rows
str(cat.data)
#all okay I think

summary(cat.data) # quick summary 
########################################################################

########################################################################
# Step 1: Outliers y and x

par(mfrow = c(1, 2)) # 1 row and 2 columns 

# boxplot of Sp_rich
boxplot(cat.data$Sp_rich, 
        main = "Fish species richness", ylab="Range of data") # title label

# dotplot of Sp_rich
dotchart(cat.data$Sp_rich, 
         xlab = "Range of data",
         ylab = "Values", main="Fish species richness")



# their appears to be a couple of outliers however none will be removed as all are actual observations
########################################################################


########################################################################
# Step 2: Homogeneity of variances

# conditional boxplot: Sp_rich by habitat structure variable (indepedently)
par(mfrow = c(3,1))

#
boxplot(Sp_rich ~ Habitat, data=cat.data, 
        xlab="Habitat",
        ylab="Species richness", 
        main="Conditional Boxplot")
#
boxplot(Sp_rich ~ HAS_2, data=cat.data, 
        xlab="HAS_2",
        ylab="Species richness", 
        main="Conditional Boxplot")
#
boxplot(Sp_rich ~ Rugosity, data=cat.data, 
        xlab="Rugosity",
        ylab="Species richness", 
        main="Conditional Boxplot")
#
# results show that the variation in the observations
# from the habitat and HAS may be similar, therefore 
# they may not violate the assumption of homogeneity of variance
# however show that the variance in rugosity may not
# have homogeneity of variance

# to investigate further:
# You can also use a test: Bartlett Test
bartlett.test(Sp_rich ~ Rugosity, data=cat.data)
bartlett.test(Sp_rich ~ HAS_2, data=cat.data)
bartlett.test(Sp_rich ~ Habitat, data=cat.data)
bartlett.test(Sp_rich ~ Neighbour_5m, data=cat.data)
bartlett.test(Sp_rich ~ Dist_neighbour, data=cat.data)

#Levene test

leveneTest(Sp_rich ~ Dist_neighbour, data=cat.data)

leveneTest(Sp_rich ~ Neighbour_5m, data=cat.data)

leveneTest(Sp_rich ~ Habitat, data=cat.data)

leveneTest(Sp_rich ~ HAS_2, data=cat.data)

leveneTest(Sp_rich ~ Rugosity, data=cat.data)
# All return a P>0.05 therefore assumption of homogeneity is accepted




########################################################################
# Step 3: Normality
par(mfrow = c(1,1))
# histogram for the whole Sp_rich data set
hist(cat.data$Sp_rich,
     xlab = "Species richness", 
     breaks = 20,
     main = "Histogram", 
     ylab = "Frequency",
     col ="grey") 
# does NOT indicate a normal distribution par(mfrow = c(2,3))
# histogram for Sp_rich data by habitat structure variable (indepedently)

#
histogram( ~ Sp_rich | HAS_2 , 
           data=cat.data,
           xlab = "Species richness", 
           breaks = 10, 
           col="grey",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )
#
histogram( ~ Sp_rich | Habitat , 
           data=cat.data,
           xlab = "Species richness", 
           breaks = 10, 
           col="grey",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )
#
histogram( ~ Sp_rich | Rugosity , 
           data=cat.data,
           xlab = "Species richness", 
           breaks = 10, 
           col="grey",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )
#
# does not suggest normailty within levels of each independent variable
# formal tests for normality:
# Shapiro-Wilk test
shapiro.test(cat.data$Sp_rich)
shapiro.test(cat.data$Sp_rich[cat.data$Dist_neighbour])
shapiro.test(cat.data$Sp_rich[cat.data$Neighbour_5m])#
shapiro.test(cat.data$Sp_rich[cat.data$Rugosity]) 
shapiro.test(cat.data$Sp_rich[cat.data$HAS_2]) 
shapiro.test(cat.data$Sp_rich[cat.data$Habitat]) 
# all return p<0.05 therefore data normality not present however this will be tested in the residuals

#### Multicolinearity########################################################################################

cor(data[,c("HAS_2", "Rugosity")], use="complete", method="pearson")
cor(data[,c("HAS_2", "Rugosity")], use="complete", method="spearman")


Data.num = data[c("Dist_neighbour","Neighbour_5m", "HAS", "Rugosity")]
library(PerformanceAnalytics)
chart.Correlation(Data.num, method="pearson",histogram=TRUE, pch=16)
chart.Correlation(Data.num, method="spearman",histogram=TRUE, pch=16)
###########################################################################################
# Steps 4-7 of data exsploration are not required for this test
###########################################################################################

###########################################################################################
# ANOVA


ANOVA<-aov(Sp_rich ~ Habitat * HAS_2 * Rugosity, data = Category)
summary(ANOVA)

# only HAS_2 significant (df = 2  sum sq = 967.7   mean sq = 483.8  f=37.724 Pr(>F)5.35e-14 ***)
TukeyHSD(ANOVA)
# only need to look at values for HAS as nothing else was fond to be significant:
#$HAS_2
#diff        lwr       upr   p adj
#Low-High    -9.684293 -12.590810 -6.777777 0.0e+00
#Medium-High -6.224378  -9.041399 -3.407357 1.7e-06
#Medium-Low   3.459915   2.042342  4.877488 1.0e-07
boxplot(Sp_rich ~ HAS_2, data=cat.data, 
        xlab="HAS_2",
        ylab="Species richness", 
        main="Conditional Boxplot")

par(mfrow =c(3,2))
plot(ANOVA)


qqnorm(ANOVA$resid)
qqline(ANOVA$resid)
hist(ANOVA$resid)
# residuals vs fitted: relatively shapeless therefore okay however
#                     slightly concerned about values 8*10 missing data
# normal q-q:normal distribution
# scale-location :potentially has homogeneity of variance
# residuals vs leverage: no point is near cook's distance
#                         therefore no influential data points


library(moments)


skewness(ANOVA$resid)
# distribution is highly skewed as returned value = 1.0627
# therefore probably not normally distributed 
#
E1 <- resid(ANOVA)   #Variable E1 contains now the distribution of residuals
F1 <- fitted(ANOVA)  #variable F1 contains now the distribution of the expected outcome of Y based on values of X
shapiro.test(E1)
#
#
#




#######################


STOP HERE FORE WRITE UP!
  
  ############
library(MASS)
par(mfrow = c(1,1))
b=boxcox(Sp_rich2 ~ Habitat * HAS_2 * Rugosity, data = Category)
b
lamda=b$x
lik=b$y
bc=cbind(lamda,lik)
bc
bc[order(-lik),]

TRANSANOVA<-aov(Sp_rich2^(1/3) ~ Habitat * HAS_2 * Rugosity, data = Category) # as lamda=0.34343434
summary(TRANSANOVA)
# Only HAS_2 significant (df=2  sum sq=22.32  f=11.161  Pr(>F)=37.952 4.6e-14 ***)
TukeyHSD(TRANSANOVA)
# Only need to look at HAS_2 output:
# $HAS_2
#diff        lwr        upr     p adj
#Low-High    -1.2197251 -1.6598502 -0.7796001 0.0000000
#Medium-High -0.5521385 -0.9787116 -0.1255654 0.0072658
#Medium-Low   0.6675866  0.4529278  0.8822455 0.0000000
boxplot(Sp_rich2 ~ HAS_2, data=cat.data, 
        xlab="HAS_2",
        ylab="Species richness", 
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
# Both similar )as previously explained however the distribution/boxplot
# of the transformed ANOVA (TRANSANOVA) looks like if follow normality more
#
# Compare the skewness of data for both ANOVAs
skewness(ANOVA$resid)
skewness(TRANSANOVA$resid)
# Non transformed ANOVA (ANOVA) is highly skewed (1.063) the transformed ANOVA
# (TRANSANOVA) is less skewed (-0.6)
#
E2 <- resid(TRANSANOVA)   #Variable E1 contains now the distribution of residuals
F2 <- fitted(TRANSANOVA)  #variable F1 contains now the distribution of the expected outcome of Y based on values of X
shapiro.test(E2)
#
#
#
#
# SOURCE@https://sites.ualberta.ca/~lkgray/uploads/7/3/6/2/7362679/slides_-_anova_assumptions.pdf 
###########################################################################################

#######################################################################################
# END