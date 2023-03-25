######################################################################
##                            Kate Sharman                          ##
##                             24/09/2019                           ##            
##  Test multiple linear regression script for sp.richness#habitats ##
######################################################################

# Question: Do different habitat structure attributes aggect the species richness of fish in bommies

########################################################################
# Previous step: import data set Test3; 
########################################################################

########################################################################
# Previous step: load packages from R
library(lattice) # for multipanel graphs
library(latticeExtra) # for multipanel graphs
library(HH) # for the Brown and Forsyth test
library(car) # for the Levene Test
library(nortest) # for the Anderson-Darling test
########################################################################
rm()
data <- data.frame(Test3)

########################################################################
# Step 0: Inspect the file

data  # all the data
head(data) # first 6 rows
head(data,10) # first 10 rows
str(data)
#does everything look ok? Are all habitat attributes a factor? If not you need to transform it into a factor
#      Actually, I don't think I need to do this as only the habitat is a factor                   
#data$Dist_neighbour<-factor(data$Dist_neighbour)
#data$Neighbour_5m<-factor(data$Neighbour_5m)
#data$HASr<-factor(data$HAS)
#data$Neighbour_5m<-factor(data$Neighbour_5m)
#str(data) # check they are now factors

summary(data) # quick summary 
########################################################################

########################################################################
# Step 1: Outliers y and x

par(mfrow = c(1, 2)) # 1 row and 2 columns 

# boxplot of Sp_rich2
boxplot(data$Sp_rich2, 
        main = "Species richness") # title label

# dotplot of Sp_rich2
dotchart(data$Sp_rich2, 
         xlab = "Range of data",
         ylab = "Values")
# their appears to be a couple of outliers however none will be removed as all are actual observations
########################################################################


########################################################################
# Step 2: Homogeneity of variances

# conditional boxplot: Sp_rich2 by habitat structure variable (indepedently)
par(mfrow = c(2,3))
boxplot(Sp_rich2 ~ Dist_neighbour, data=data, 
        xlab="Distance (m)",
        ylab="Species richness", 
        main="Conditional Boxplot")
#
boxplot(Sp_rich2 ~ Neighbour_5m, data=data, 
        xlab="Number of neighbours within 5m",
        ylab="Species richness", 
        main="Conditional Boxplot")
#
boxplot(Sp_rich2 ~ Habitat, data=data, 
        xlab="Habitat",
        ylab="Species richness", 
        main="Conditional Boxplot")
#
boxplot(Sp_rich2 ~ HAS, data=data, 
        xlab="HAS",
        ylab="Species richness", 
        main="Conditional Boxplot")
#
boxplot(Sp_rich2 ~ Rugosity, data=data, 
        xlab="Rugosity",
        ylab="Species richness", 
        main="Conditional Boxplot")
#
boxplot(Sp_rich2 ~ Refuge_size_categories, data=data, 
        xlab="Number of refugse size categories",
        ylab="Species richness", 
        main="Conditional Boxplot")
# results show that the variation in the observations
# from the habitat may be similar, therefore 
# they may not violate the assumption of homogeneity of variance
# however show that the variance in all other structural attributes may not
# have homogeneity of variance

# to investigate further:

# Bartlett Test
bartlett.test(Sp_rich2 ~ Dist_neighbour, data=data)
#data:  Sp_rich2 by Dist_neighbour
#Bartlett's K-squared = 4.216, df = 4, p-value = 0.3776
# p>0.05 therefore the results of this test indicate that they do 
# not violate the assumption of homogeneity of variance
bartlett.test(Sp_rich2 ~ Neighbour_5m, data=data)
#data:  Sp_rich2 by Neighbour_5m
#Bartlett's K-squared = 3.5336, df = 3, p-value = 0.3164
# p>0.05 therefore the results of this test indicate that they do 
# not violate the assumption of homogeneity of variance
bartlett.test(Sp_rich2 ~ Habitat, data=data)
#data:  Sp_rich2 by Habitat
#Bartlett's K-squared = 0.27302, df = 1, p-value = 0.6013
# p>0.05 therefore the results of this test indicate that they do 
# not violate the assumption of homogeneity of variance
bartlett.test(Sp_rich2 ~ HAS, data=data)
#data:  Sp_rich2 by HAS
#Bartlett's K-squared = 24.218, df = 4, p-value = 7.221e-05
# p<0.05 therefore the results of this test indicate that they do 
# violate the assumption of homogeneity of variance
bartlett.test(Sp_rich2 ~ Rugosity, data=data)
#data:  Sp_rich2 by Rugosity
#Bartlett's K-squared = 1.7833, df = 2, p-value = 0.41# p>0.05 therefore the results of this test indicate that they do 
# not violate the assumption of homogeneity of variance
bartlett.test(Sp_rich2 ~ Refuge_size_categories, data=data)
#data:  Sp_rich2 by Refuge_size_categories
B#artlett's K-squared = 22.796, df = 4, p-value = 0.0001391
# p>0.05 therefore the results of this test indicate that they do 
# violate the assumption of homogeneity of variance





# Levene Test (same overall outcome as bartlett test just have not copied and pastewd new results)

leveneTest(Sp_rich2 ~ Dist_neighbour, data=data)

leveneTest(Sp_rich2 ~ Neighbour_5m, data=data)

leveneTest(Sp_rich2 ~ Habitat, data=data)

leveneTest(Sp_rich2 ~ HAS, data=data)

leveneTest(Sp_rich2 ~ Rugosity, data=data)

leveneTest(Sp_rich2 ~ Refuge_size_categories, data=data)







###########
#
#
#
#

# Brown and Forsyth test (similar Homogeneity of Variance Plot)
hov(Sp_rich2 ~ treatment, data = data)
# F = 0.3775, df:treatment = 3, df:Residuals = 60, p-value = 0.7695
# simililary, this test also supports homogeneity

hovPlot(Sp_rich2 ~ treatment, data = data)
# even though results are not similiar between all groups they have similar variationn
# assumption of homogeneity has been met
# and therefore a one-way repeated measures ANOVA can be used
########################################################################

########################################################################
# Step 3: Normality
par(mfrow = c(1,1))
# histogram for the whole Sp_rich2 data set
hist(data$Sp_rich2,
     xlab = "Species richness", 
     breaks = 20,
     main = "Histogram", 
     ylab = "Frequency",
     col ="grey") 
# does NOT indicate a normal distribution par(mfrow = c(2,3))
# histogram for Sp_rich2 data by habitat structure variable (indepedently)
histogram( ~ Sp_rich2 | Dist_neighbour , 
           data=data,
           xlab = "Species richness", 
           breaks = 10, 
           col="grey",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )

# qq-plots for all treatments
qqnorm( data$Sp_rich2,
        main = "Sp_rich2")
qqline( data$Sp_rich2)

# qq-plots for all treatments
par(mfrow = c(2, 3)) # fit all 4 treatments graphs onto one page

qqnorm( data$Sp_rich2[data$Dis_neighbour=="Low"],
        main = "Low")
qqline( data$Sp_rich2[data$treatment=="sugar"])

qqnorm( data$Sp_rich2[data$treatment=="vinegar"],
        main = "vinegar")
qqline( data$Sp_rich2[data$treatment=="vinegar"])

qqnorm( data$Sp_rich2[data$treatment=="water"],
        main = "water")
qqline( data$Sp_rich2[data$treatment=="water"])

qqnorm( data$Sp_rich2[data$treatment=="alcohol"],
        main = "alcohol")
qqline( data$Sp_rich2[data$treatment=="alcohol"])

# formal tests for normality:
# Shapiro-Wilk test
shapiro.test(data$Sp_rich2) # W = 0.94934, p-value = 0.01062
shapiro.test(data$Sp_rich2[data$Dist_neighbour])
shapiro.test(data$Sp_rich2[data$Neighbour_5m])# W = 0.92025, p-value = 0.1703
shapiro.test(data$Sp_rich2[data$Rugosity]) # W = 0.95733, p-value = 0.6138
shapiro.test(data$Sp_rich2[data$HAS]) # W = 0.94253, p-value = 0.3811
shapiro.test(data$Sp_rich2[data$Habitat]) # W = 0.9495, p-value = 0.4818
# as the value p gets smaller, the probability of normal distribution also gets smaller
# each housing treatment has normal distribution

# can also use the Anderson-Darling test:
ad.test(data$Sp_rich2) # A = 0.99587, p-value = 0.01177
ad.test(data$Sp_rich2[data$treatment=="sugar"]) # A = 0.56475, p-value = 0.1196
ad.test(data$Sp_rich2[data$treatment=="vinegar"]) # A = 0.28833, p-value = 0.5704
ad.test(data$Sp_rich2[data$treatment=="water"]) # A = 0.46122, p-value = 0.2247
ad.test(data$Sp_rich2[data$treatment=="alcohol"]) # A = 0.284, p-value = 0.5831
# H1 the distribution of the data does not follow a normal distribution
# Ho the distribution of the data follows a normal distribution
# Ho is accepted as each treatment has normal distribution
# this meets the assumptions of a one-way repeated measures ANOVA

###########################################################################################

###########################################################################################
# Step 4: Trouble with Zeroes
sum(data$Sp_rich2 == 0)
sum(data$Sp_rich2 == 0) / nrow(data)
par(mfrow = c(1,1))
plot(table(data$Sp_rich2)) 
# there are no zeroes to worry about
###########################################################################################

###########################################################################################
# Steps 5-7 of data exsploration are not required for this test
###########################################################################################

###########################################################################################
# ANOVA


ANOVA4<-aov(Sp_rich2 ~ Habitat * HAS * HAS_2 * HAS_2.1 * Rugosity, data = Test3)
summary(ANOVA4)
TukeyHSD(ANOVA4)
boxplot(Sp_rich2 ~ HAS, data=data, 
        xlab="HAS",
        ylab="Species richness", 
        main="Conditional Boxplot")

par(mfrow =c(3,2))
plot(ANOVA4)


qqnorm(ANOVA4$resid)
qqline(ANOVA$resid)
hist(ANOVA4$resid)
library(moments)


skewness(ANOVA4$resid)

library(MASS)
par(mfrow = c(1,1))
b=boxcox(Sp_rich2 ~ Habitat * HAS * HAS_2 * HAS_2.1 * Rugosity, data = Test3)
b
lamda=b$x
lik=b$y
bc=cbind(lamda,lik)
bc
bc[order(-lik),]


#
#
#
#
#
#
ANOVA5<-aov(Sp_rich2^(1/3) ~ Habitat * HAS * HAS_2 * HAS_2.1 * Rugosity, data = Test3)
summary(ANOVA5)
TukeyHSD(ANOVA5)
boxplot(Sp_rich2 ~ HAS, data=data, 
        xlab="HAS",
        ylab="Species richness", 
        main="Conditional Boxplot")

par(mfrow =c(2,2))

qqnorm(ANOVA5$resid)
qqline(ANOVA5$resid)
hist(ANOVA5$resid)
qqnorm(ANOVA4$resid)
qqline(ANOVA$resid)
hist(ANOVA4$resid)

library(moments)


# comparing residuals from before and after boxcox transformation

windows()
par(mfrow =c(3,2))
plot(ANOVA4)


qqnorm(ANOVA4$resid)
qqline(ANOVA$resid)
hist(ANOVA4$resid)

#
#
#

windows()
par(mfrow =c(3,2))
plot(ANOVA5)


qqnorm(ANOVA5$resid)
qqline(ANOVA5$resid)
hist(ANOVA5$resid)

skewness(ANOVA4$resid)
skewness(ANOVA5$resid)
# As a general rule of thumb: If skewness is less than -1 or greater than 1, 
# the distribution is highly skewed. If skewness is between -1 and -0.5 or between
# 0.5 and 1, the distribution is moderately skewed. If skewness is between -0.5 and
# 0.5, the distribution is approximately symmetric.
#





















E1 <- resid(ANOVA4)   #Variable E1 contains now the distribution of residuals
F1 <- fitted(ANOVA4)  #variable F1 contains now the distribution of the expected outcome of Y based on values of X

plot(x = F1, 
     y = E1,                     #scatterplot residuals vs fitted (expected) values
     xlab = "Fitted values",
     ylab = "Residuals", 
     main = "Homogeneity?")
abline(h = 0, v = 0, lty = 2) 

# OUTPUT
# Analysis of Variance Table
# Response: Sp_rich2
#            Df Sum Sq Mean Sq F value    Pr(>F)    
#  treatment  3 23.271  7.7570   42.09 9.028e-15 ***
#  Residuals 60 11.058  0.1843                      
# ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# therefore reject hypothesis that group means are equal. The type of drink
# given to a domesticated mouse has a significant impact on its hormonal

# however further tests (post-hoc) are need to identify which treatments (type of drink) differ 
# from the others a the ANOVA only shows that there is not an effect of a factor

##########################################################################################
##########################################################################################
# POST-HOC tests

# ======  Pairwise test: 
# to see which treatment/treatments is/are significantly different from the others
# comparing them pair by pair:
pairwise.t.test(data$Sp_rich2, data$treatment,
                p.adj = "bonf")
# Pairwise comparisons using t tests with pooled SD 
# data:  data$Sp_rich2 and data$treatment 
#           alcohol   sugar   vinegar 
#  sugar    2.8e-10   -       -   
#  vinegar  2.3e-11   1.00    -   
#  water    4.7e-14   0.17    0.67
# P value adjustment method: bonferroni 
# sugar is significantly different from all other housing treatments

# ==========Tukey's post-hoc test:
TukeyHSD(ANOVA_treatment)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# Fit: aov(formula = Sp_rich2 ~ treatment, data = data)
# $treatment
#                 diff             lwr        upr        p adj
#sugar-alcohol   -1.21588542 -1.61696845 -0.8148024 0.0000000
#vinegar-alcohol  0.34218750 -0.05889554  0.7432705 0.1205381
#water-alcohol    0.09765625 -0.30342679  0.4987393 0.9174370
#vinegar-sugar    1.55807292  1.15698988  1.9591560 0.0000000
#water-sugar      1.31354167  0.91245863  1.7146247 0.0000000
#water-vinegar   -0.24453125 -0.64561429  0.1565518 0.3802466

# again the output shows that sugar has a significantly lower hormonal level compared
# to all other drinks, no other null hypothesis are rejected

# the bonferroni pairwise t test will be reported as it uses a 
# generalised assumption as opposed to an exact, used by the Tukey
###########################################################################################

#######################################################################################
# END