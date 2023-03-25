######################################################################
##                            Kate Sharman                          ##
##                             24/09/2019                           ##            
##           Kruskal Wallis for structure between habitat           ##
######################################################################



########################################################################
#Previous step: import data set str_habber.txt; 
#Ho: there were no differences in the HAS or Rugosity as the habitat changes
#(from seagrass - bommie - crest)
#######################################################################
########################################################################
rm()
str_hab <- data.frame(Structure.habitat)
#######################################################################
#Step 0: Inspect the file
str_hab# you can see all your data
names(str_hab) # You have a data set with 3 variables
str(str_hab) # you can see the structure of your data
# looks good

summary(str_hab) # quick way of seeing a summary 
# of the different variables

########################################################################

########################################################################
#Step 1: Outliers y and x

par(mfrow = c(1, 2))
### boxplot
boxplot(str_hab$Rugosity, 
        main = "Rugosity", ylab="Range of data") # main indicates the title label
### dotplot
dotchart(str_hab$Rugosity, 
         xlab = "Range of data", # xlab indicates the label for x axis
         ylab = "Values", main="Rugosity") # ylab indicates the label for y axis
### boxplot
boxplot(str_hab$HAS, 
        main = "HAS", ylab="Range of data") # main indicates the title label
### dotplot
dotchart(str_hab$HAS, 
         xlab = "Range of data", # xlab indicates the label for x axis
         ylab = "Values", main="HAS") # ylab indicates the label for y axis
# potentially a couple of outliers with small rugosity, no outliesrs in HAS
# all are actual observations so overall deemed no outliers to be removed
########################################################################

########################################################################
#Step 2: Homogeneity of variances... as parametric assumption

#conditional boxplot: Rugosity by Habitat

boxplot(Rugosity ~ Habitat, data=str_hab, 
        xlab="Habitat",
        ylab="Rugosity", 
        main="Conditional Boxplot")
#conditional boxplot: HAS by Habitat
boxplot(HAS ~ Habitat, data=str_hab, 
        xlab="Habitat",
        ylab="HAS", 
        main="Conditional Boxplot")

# You can also use a test: Bartlett Test
bartlett.test(Rugosity ~ Habitat, data=str_hab)
bartlett.test(HAS ~ Habitat, data=str_hab)
#P>0.05 for rugosity therefore homogeneity of variance
#P>0.05 for HAS therefore homogeneity of variance
#
# Levene Test
library(car)
leveneTest(Rugosity ~ Habitat, data=str_hab)
leveneTest(HAS ~ Habitat, data=str_hab)
#P>0.05 for rugosity therefore homogeneity of variance
#P>0.05 for HAS therefore homogeneity of variance
########################################################################

########################################################################
# Step 3: Normality....ANOVA is a parametric test...

#histogram for the whole RUGOSITY data
par(mfrow = c(1, 2))
hist(str_hab$Rugosity,
     xlab = "Rugosity",  
     breaks = 10,
     main = "Distribution of Rugosity", 
     ylab = "Frequency",
     col ="gray78") 
# looks skewed
# 
#histogram for the whole HAS data
par(mfrow = c(1, 1))
hist(str_hab$HAS,
     xlab = "HAS",  
     breaks = 10,
     main = "Distribution of HAS", 
     ylab = "Frequency",
     col ="gray78")
# looks normally distributed
#
#
#histogram for rugosity data BY Habitat
par(mfrow = c(1, 2))
library (lattice)
histogram( ~ Rugosity | Habitat , 
           data=str_hab,
           breaks = 10, 
           col="gray78",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )

library (lattice)
histogram( ~ HAS | Habitat , 
           data=str_hab,
           breaks = 10, #
           col="gray78",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )

shapiro.test(str_hab$Rugosity)
shapiro.test(str_hab$Rugosity[str_hab$Habitat=="Seagrass"])
shapiro.test(str_hab$Rugosity[str_hab$Habitat=="Bommie"])
shapiro.test(str_hab$Rugosity[str_hab$Habitat=="Crest"])
# P<0.05 for overall and bommie. P>0.05 for seagrass and crest
# therefore overall rugosity is not normally distributed
#
#shapiro.test(str_hab$Rugosity)
shapiro.test(str_hab$HAS)
shapiro.test(str_hab$HAS[str_hab$Habitat=="Seagrass"])
shapiro.test(str_hab$HAS[str_hab$Habitat=="Bommie"])
shapiro.test(str_hab$HAS[str_hab$Habitat=="Crest"])
# P>0.05 for overall, bommie and crest however not for seagrass
# therefore overall habitats not normally distributed
#
#
# Both rugosity and HAS has homogeneity of variance but not data normality
# source saying: "violations of normality are not usually a death sentence for validity#
# therefore a one way anova will be conducted for both rugosity and HAS
#
# https://sites.ualberta.ca/~lkgray/uploads/7/3/6/2/7362679/slides_-_anova_assumptions.pdf
# 
#
####################################################################
# ANOVA for HAS:
ANOVA_HAS <- aov(HAS~Habitat,
                   data=str_hab)
anova(ANOVA_HAS)
# CONCLUSION: reject H0 that HAS means are equal. The habitat
# of the bommie had a significant impact on its HAS. 
#
#
# POST-HOC tukey test: 
TukeyHSD(ANOVA_HAS)
#the OUPUT shows that the HAS of a bommie is significantly higher
# in crest than bommie, bommie-seagrass and crest-seagrass
#no other H0 are rejected. 
#
#
####################################################################
# ANOVA for Rugosity:
ANOVA_Rugosity <- aov(Rugosity~Habitat,
                 data=str_hab)
anova(ANOVA_Rugosity)
# CONCLUSION: reject H0 that Rugosity means are equal. The habitat
# of the bommie had a significant impact on its rugosity. 
#
#
# POST-HOC tukey test: 
TukeyHSD(ANOVA_Rugosity)

#the OUPUT shows that H0 are rejected for seagrass=bommie and seagrass-crest (P>0.05)
# therefore the rugosity of a bommie in these habitats are not dffferent
# however the rugosity of a bommie is significantly higher
# in crest than bommie habitat
#
#
#
# CHECK RESIDUALS:
#
par(mfrow =c(3,2))
plot(ANOVA_HAS)
qqnorm(ANOVA_HAS$resid)
qqline(ANOVA_HAS$resid)
hist(ANOVA_HAS$resid)

#
#
skewness(ANOVA_HAS$resid)
#
#
E1 <- resid(ANOVA_HAS)   #Variable E1 contains now the distribution of residuals
F1 <- fitted(ANOVA_HAS)  #variable F1 contains now the distribution of the expected outcome of Y based on values of X
shapiro.test(E1)
#
#
#
par(mfrow =c(3,2))
plot(ANOVA_Rugosity)
qqnorm(ANOVA_Rugosity$resid)
qqline(ANOVA_Rugosity$resid)
hist(ANOVA_Rugosity$resid)

#
#
skewness(ANOVA_Rugosity$resid)
#
#
E2 <- resid(ANOVA_Rugosity)   #Variable E1 contains now the distribution of residuals
F2 <- fitted(ANOVA_Rugosity)  #variable F1 contains now the distribution of the expected outcome of Y based on values of X
shapiro.test(E2)
#
#
#######################################################################
# END
#######################################################################
