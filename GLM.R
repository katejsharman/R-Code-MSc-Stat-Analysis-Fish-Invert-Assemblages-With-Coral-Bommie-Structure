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
# Previous step: import data set Number; 
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
data <- data.frame(Number)
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

# boxplot of Sp_rich
boxplot(data$Sp_rich, 
        main = "Species richness") # title label

# dotplot of Sp_rich
dotchart(data$Sp_rich, 
         xlab = "Range of data",
         ylab = "Values")
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

GLM<-glm(Sp_rich~ Dist_neighbour + Neighbour_5m + HAS + Rugosity,
         data = data, family = poisson(link = "log"))
avPlots(GLM, ask =F)
summary(GLM)
# P<0.005 on intercept, Dist_neighbour and HAS
# but overdispersion as:     Null deviance: 581.40  on 162  degrees of freedom
# Residual deviance: 340.79  on 158  degrees of freedom       AIC: 852.6
GLM
par(mfrow =c(2,2))
plot(GLM)
library(AER)
dispersiontest(GLM)

# residuals vs fitted: relatively shapeless therefore okay
# normal q-q:normal distribution
# scale-location :generally has homogeneity of variance
# residuals vs leverage: none point is almost at cook's
# distanc ehowever is short of ot therefore no influential
# data points


########################################
# quasipoisson family is said to help reduce overdispersion ...
quasiGLM<-glm(Sp_rich~ Dist_neighbour + Neighbour_5m + HAS + Rugosity,
         data = data, family = quasipoisson(link = "log"))
avPlots(quasiGLM, ask =F)
summary(quasiGLM)
dispersiontest(quasiGLM)
window()
par(mfrow =c(3,2))
plot(quasiGLM)


qqnorm(quasiGLM$resid)
qqline(quasiGLM$resid)
hist(quasiGLM$resid)

# source for nb = https://bookdown.org/schmettow/NewStats/GLM.html#count_data
# the overdispersion did not change
# negative binomial regression is for modelling over-dispersed count variables
library(MASS)
GLMnb<-glm.nb(Sp_rich~ Dist_neighbour + Neighbour_5m + HAS + Rugosity,
              data = data)
avPlots(GLMnb, ask =F)
summary(GLMnb)
GLMnb
dispersiontest(GLMnb)
1-(1/340)

1-(340/1)

# The residual deviance was reduced from 320.79 on 158 df (quasipoisson)
# to 191.23 on 158df. Overdispersion still exists but at a 
# smaller magnitude? Is this correct? #
#
##########################

# AIC stepwise backward selection
step(GLMnb, direction="backward")
# So the best regression model is: Sp_rich ~ Dist_neighbour + HAS
GLMnb2<-glm.nb(Sp_rich~ Dist_neighbour + HAS,
              data = data)
# comparing the maximal and stepwise backward selection
summary(GLMnb2)
summary(GLMnb)
# I also did the AIC stepwise backward selection on the quasiGLM however it's
# not possible and returns an error. I tried it on the origional GLM just to see...
step(GLM, direction="backward")
# So the best regression model is: Sp_rich ~ Dist_neighbour + HAS
GLM2<-glm(Sp_rich~ Dist_neighbour + HAS,
          data = data, family = quasipoisson(link = "log"))
summary(GLM2)
GLM2
1-(345.6/581.4) # 0.4055728 therefore explains 40.6% of variability in species richness which suggests that factors other than those here influence the species richness.


# The lowest by far residual deviance (which is 191.50 on 160 df) is returned on the
# updated GLMnb (called GLMnb2) wheres the updated GLM (called GLM2) is 343.55 on 160 df.
#
#
# # What do I do now as there is overdispersion? I tried to limit it with 
# family= quasipoisson and then the negative binomial its still there but 
# with a smaller ratio. Do I stop there and report the numbers from the summary
# and then say but it's over dispersed.
#
# I have been reading about overdispersal for hours. It occurs because the
# mean and variance components of a GLM are related and depends on the same
# parameter that is being predicted through the independent vector. Apparantly
# it means that the estimated standard errors and test statistics the overall
# goodness-of-fit will be distorted and adjustments must be made but I have no
# idea what this actually means, how to word it or what aditionally adjustments must be made?
#
#Do I have to back transform it? If so I have absoltely no idea how to do this, I've looking online and can't find anything
# Do I still need to validate the model if it is overdispersed???
# If so then see below ...
# Validating the 'best' model now (which I have chosen to be GLMnb2 due to the smaller residual deviance
# to df however overdispersion is still present)
#
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
plot(GLMnb2)
# residuals vs fitted: relatively shapeless therefore okay however
#                     should I be concerned about the arc?
# normal q-q:normal distribution
# scale-location :generally has homogeneity of variance
# residuals vs leverage: no point is near cook's distance
#                         therefore no influential data points
#
#



######### 1. Homogeneity of residuals#########################
#compare the fitted value for Y vs the residuals


par(mfrow = c(1, 1))


E1 <- resid(GLMnb2)   #Variable E1 contains now the distribution of residuals
F1 <- fitted(GLMnb2)  #variable F1 contains now the distribution of the expected outcome of Y based on values of X

plot(x = F1, 
     y = E1,                     #scatterplot residuals vs fitted (expected) values
     xlab = "Fitted values",
     ylab = "Residuals", 
     main = "Homogeneity?")
abline(h = 0, v = 0, lty = 2)    #draws a horizontal line based on Residual = 0

# May have homogeneity of residuals as a cloud like shape around line residual=0 however some extreme values on the right

######### 2. Independence  ###############
#Plot residuals versus covariates
par(mfrow = c(1, 2))
plot(x = data$Dist_neighbour, 
     y = E1,
     xlab = "Sp_richness",
     ylab = "residuals",
     main = "Independence?")
abline(0,0,lty=2)

plot(x = data$HAS, 
     y = E1,
     xlab = "Sp_richness",
     ylab = "residuals",
     main = "Independence?")
abline(0,0,lty=2)

# Dist_neighbour has no pattern between Sp_rich:1-5 however very few values>5.
# HAS is a 'cloud around 0' = data normality also due to the design of the data
# collection method all datapoints are independent 

######### 3. Influential observations ####################
par(mfrow = c(1, 1))
plot(GLMnb2,which=4) # to obtain Cook's distance

# No values > 1. Largest value is just over 0.25
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
summary(GLMnb2)
# Equation to estimate of (Sp_rich) = -1.70735 + 0.03221 * Dist_neighbour + 0.17883 * HAS
# Is this correct or do I need to back-transform that data? I fuond a blog that said that it should be backtransformed
# but I couldn't find anything else on it and I have NO idea how to do this?

#Plot GLMnb2 model:
par(mfrow = c(1,2))
termplot(GLMnb2, 
         partial.resid = TRUE)
# HAS looks like it predicts the Sp_richness more than the Dist_neighbour as the datapoint follow
# the HAS line.
# Could the Dist-neighbour looking like it is not a good predictor of Sp_richness because of there
# being overdispersion???



# Estimate of (Sp_rich) = -1.70735 + 0.03221 * Dist_neighbour + 0.17883 * HAS
#CONCLUSION: our model is significant, it shows a positive relationship between distance_neighbour and Sp_richness
#of a bommie and a positive relationship between HAS and Sp_richness of a bommie

# however ... is there a way to say how what percentage of data is explained by it like in multiple regression models?
# How would I report it in the text, would I just quote the above equation and say that the HAS has a stronger positive relationship
# with sp_richness (Est=0.179, z=10.978, p=16x(10^16)) than Dist_neighbour (Est=0.032, z=2.796, p=0.0052)?
# Also, what values would I include in a table in the appendices?

#END


1-(317/171)
1-(191/317)
