#################################
#                               #
# Generalised linear regression #
#################################



########################################################
# Question: Is the species richness of a bommie affected 
# by habitat / rugosity / HAS / Distance to nearest neghbour / 
# number of neighbouring bommies within 5m
########################################################

########################################################################
#Previous step: import Number.txt; 
# in this case our explanatory variables are habitat / rugosity / HAS /
# Distance to nearest neghbour / number of neighbouring bommies within 5m  
# the response variable (y) species richness
########################################################################
# Previous step: import data set InvertNum; 
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
data <- data.frame(InvertNum)
data
########################################################################
# Step 0: Inspect the file

data  # all the data
head(data) # first 6 rows
head(data,10) # first 10 rows
str(data)
# Are these okay? There's a mix of factor, int and num???

summary(data) # quick summary 
########################################################################

########################################################################
# Step 1: Outliers y and x

par(mfrow = c(1, 2)) # 1 row and 2 columns 

# boxplot of Invert_rich
boxplot(data$Invert_rich, 
        main = "Invertebrate group richness", ylab="Range of data") # title label

# dotplot of Invert_rich
dotchart(data$Invert_rich, 
         xlab = "Range of data",
         ylab = "Values", main="Invertebrate group richness")
# their appears to be a couple of outliers however none will be removed as all are actual observations
########################################################################


########################################################################
# Step 2: Homogeneity of variances

# NOT NEEDED ... will be checked for residuals later

########################################################################
# Step 3: Data normality

# NOT NEEDED ... will be checked for residuals later

########################################################################
#Step 4 of data exploration: Trouble with Zeroes

# will not be an issue as assuming a Poisson distribution which can cope with 0s in data
########################################################################
#Step 5 independence of data 

# will not be an issues as independent due to study design

##########################################################################################
# strength of the relationship between all variables......
# COLLINEARITY
# the correlation matrix for both parametric and non parametric
# Should I used the pearson or spearman as I can't do the bartlett test on each variable?
# as I get the following error: Error in bartlett.test.default(c(3L, 6L, 0L, 4L, 2L, 2L, 4L, 6L, 5L, 5L,  : there must be at least 2 observations in each group
cor(data[,c("Dist_neighbour","Neighbour_5m", "HAS", "Rugosity")], use="complete", method="pearson")
cor(data[,c("Dist_neighbour","Neighbour_5m", "HAS", "Rugosity")], use="complete", method="spearman")


Data.num = data[c("Dist_neighbour","Neighbour_5m", "HAS", "Rugosity")]
library(PerformanceAnalytics)
chart.Correlation(Data.num, method="pearson",histogram=TRUE, pch=16)
chart.Correlation(Data.num, method="spearman",histogram=TRUE, pch=16)

# returned r values<0.7 therefore no need to remove any of the independent
# /descriptive variables as there is no collinearity
########################################################

GLM<-glm(Invert_rich~ Dist_neighbour + Neighbour_5m + HAS + Rugosity,
         data = data, family = poisson(link = "log"))
avPlots(GLM, ask =F)
summary(GLM)
# P<0.005 on HAS and Rugosity
# but overdispersion as:      Null deviance: 225.05  on 162  degrees of freedom
#Residual deviance: 187.23  on 158  degrees of freedom         AIC: 620.62
GLM
par(mfrow =c(2,2))
plot(GLM)
# residuals vs fitted: relatively shapeless therefore okay
# normal q-q:normal distribution
# scale-location :generally has homogeneity of variance
# residuals vs leverage: none point is almost at cook's
# distanc ehowever is short of ot therefore no influential
# data points


########################################
# quasipoisson family is said to help reduce overdispersion ...
quasiGLM<-glm(Invert_rich~ Dist_neighbour + Neighbour_5m + HAS + Rugosity,
         data = data, family = quasipoisson(link = "log"))
avPlots(quasiGLM, ask =F)
summary(quasiGLM)
window()
par(mfrow =c(3,2))
plot(quasiGLM)
qqnorm(quasiGLM$resid)
qqline(quasiGLM$resid)
hist(quasiGLM$resid)
# the overdispersion did not change
# negative binomial regression is for modelling over-dispersed count variables
library(MASS)
GLMnb<-glm.nb(Invert_rich~ Dist_neighbour + Neighbour_5m + HAS + Rugosity,
              data = data)
avPlots(GLMnb, ask =F)
summary(GLMnb)
# The residual deviance is still the same therefore will continue with originoal GLM (GLM) ...
#
##########################

# AIC stepwise backward selection
step(GLM, direction="backward")
# So the best regression model is: Invert_rich ~ Neighbour_5m + HAS + Rugosity ... therefore ...
GLM2<-glm(Invert_rich~ Neighbour_5m + HAS + Rugosity,
          data = data, family = poisson(link = "log"))
GLM2
summary(GLM2)
# comparing the maximal and stepwise backward selection
#summary(GLM2)
#Call:
#  glm(formula = Invert_rich ~ Neighbour_5m + HAS + Rugosity, family = poisson(link = "log"), 
#      data = data)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.8805  -0.7872  -0.1235   0.6165   2.8274  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.025199   0.305785  -0.082  0.93432    
#Neighbour_5m  0.018831   0.009041   2.083  0.03728 *  
#  HAS           0.082752   0.015784   5.243 1.58e-07 ***
#  Rugosity     -1.324068   0.412136  -3.213  0.00131 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#(Dispersion parameter for poisson family taken to be 1)
#
#Null deviance: 225.05  on 162  degrees of freedom
#Residual deviance: 187.77  on 159  degrees of freedom
#AIC: 619.17
#
#Number of Fisher Scoring iterations: 5

#
#
summary(GLM)
#Call:
#glm(formula = Invert_rich ~ Dist_neighbour + Neighbour_5m + HAS + 
#      Rugosity, family = poisson(link = "log"), data = data)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.8618  -0.8020  -0.1277   0.6277   2.8133  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     0.023222   0.312319   0.074  0.94073    
#Dist_neighbour -0.012673   0.017855  -0.710  0.47784    
#Neighbour_5m    0.016402   0.009642   1.701  0.08891 .  
#HAS             0.081768   0.015842   5.161 2.45e-07 ***
#  Rugosity       -1.297441   0.414760  -3.128  0.00176 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#(Dispersion parameter for poisson family taken to be 1)
#Null deviance: 225.05  on 162  degrees of freedom
#Residual deviance: 187.23  on 158  degrees of freedom
#AIC: 620.62
#Number of Fisher Scoring iterations: 5
#
# 


# Both models have a equal amount of overdispersion ~ residual deviance = 187.77 on 159 df. 
#

#
# For now I am proceeding with the best regression model (GLM2)
# what are the implications of a model having over dispersal?
#
#
###########################
# Validating the 'best' model (which I have chosen to be GLM2 
#
#
#
################################################
#Model validation
#1. Homogeneity
#2. Independence
#3. Influential observations
#4. Normality
#5. Does it all make sense?


#Standard graphical output lm:
par(mfrow = c(2, 2))
plot(GLM2)
# residuals vs fitted: relatively shapeless therefore okay 
# normal q-q:normal distribution
# scale-location :generally has homogeneity of variance
# residuals vs leverage: no point is near cook's distance
#                         therefore no influential data points
#
#



######### 1. Homogeneity of residuals#########################
#compare the fitted value for Y vs the residuals


par(mfrow = c(1, 1))


E1 <- resid(GLM2)   #Variable E1 contains now the distribution of residuals
F1 <- fitted(GLM2)  #variable F1 contains now the distribution of the expected outcome of Y based on values of X

plot(x = F1, 
     y = E1,                     #scatterplot residuals vs fitted (expected) values
     xlab = "Fitted values",
     ylab = "Residuals", 
     main = "Homogeneity?")
abline(h = 0, v = 0, lty = 2)    #draws a horizontal line based on Residual = 0

# May have homogeneity of residuals as a cloud like shape around line residual=0 

######### 2. Independence  ###############
#Plot residuals versus covariates
par(mfrow = c(1, 3))
plot(x = data$Neighbour_5m, 
     y = E1,
     xlab = "Neighbour_5m   Invert_richness",
     ylab = "residuals",
     main = "Independence?")
abline(0,0,lty=2)

plot(x = data$HAS, 
     y = E1,
     xlab = "HAS   Invert_richness",
     ylab = "residuals",
     main = "Independence?")
abline(0,0,lty=2)

plot(x = data$Rugosity, 
     y = E1,
     xlab = "Rugosity   Invert_richness",
     ylab = "residuals",
     main = "Independence?")
abline(0,0,lty=2)

# Neighbour_5m is a 'cloud around 0' = data normality
# HAS is a 'cloud around 0' = data normality
# Rugosity is a 'cloud around 0' however more towards upper scale of rug-invert  = data normality
# also due to the design of the data collection method all datapoints are independent 

######### 3. Influential observations ####################
par(mfrow = c(1, 1))
plot(GLM2,which=4) # to obtain Cook's distance

# No values > 1. Largest value is just # 0.10
# therefore no influential data points

######### 4. Normality of residuals#########################


# example of a normal distribution with sample size = 1000
rnorm(1000)
# and the histogram should look like...
par(mfrow = c(2,2))
hist(rnorm(1000))
## so for the current data (Number.text = data) with 163 data it should look..
hist(rnorm(163))

# shows what the distribution of residuals looks like...
hist(E1, main = "Normality")
#similar to what was expected? Better!
#qq-plot
qqnorm(E1)
qqline(E1) # suggests normality
shapiro.test(E1) # P>0.05 therefore normality of residuals!


######## Does it all make sense? ##############
# Assumption of homogeneity of variance of residuals (homoscedastisity) Yes
# Assumption of avoidance of influential observations. Yes
# Assumption of Normality of residuals. Yes
### FINAL DECISION? To accept the model HOWEVER WHAT DO I DO ABOUT THERE BEING OVERDISPERSION?!?!?!?! 

################################################
#What does the numerical output mean?
summary(GLM2)
# Equation to estimate of (Invert_rich) = -0.025199 + 0.018831 * Neighbour_5m + 0.082752 * HAS + -1.324068 * Rugosity
# Is this the correct way to interpret it? 

#Plot GLMnb2 model:
par(mfrow = c(1,3))
termplot(GLM2, 
         partial.resid = TRUE)
# HAS looks like it influences the Invert species richness more than the Rugosity which in turn
#influences it better than Neighbour_5m
#
#
#
# How does over dispersion impact???
#uses the following 2 commands to calculate the p value!!!
nul=glm(Invert_rich~ 1,
        data = data, family = poisson(link = "log"))
anova(GLM2, nul, test = "Chisq")
# 1 - (Residual Deviance/Null Deviance) = R squared
1-(187.77/225.05)
# Estimate of (Invert_rich) = -0.025199 + 0.018831 * Neighbour_5m + 0.082752 * HAS + -1.324068 * Rugosity
#CONCLUSION: our model is significant, it shows a positive relationship between distance_neighbour and Invert_richness
#of a bommie and a positive relationship between HAS and Invert_richness of a bommie
# however the intercepet is not significantly different from 0 ()

# however ... is there a way to say how what percentage of data is explained by it like in multiple regression models?
# How would I report it in the text, would I just quote the above equation and say that the HAS has a stronger positive relationship
# with Invert_richness (Est=0.179, z=10.978, p=16x(10^16)) than Dist_neighbour (Est=0.032, z=2.796, p=0.0052)?
# Also, what values would I include in a table in the appendices?

#END



