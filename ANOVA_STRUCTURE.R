#####################################################################################
##                                   Kate Sharman                                  ##
##                                    24/09/2019                                   ##            
##  SPAT_ANOVA script for sp.richness$ neighbours / neighbour ddistance / habitat  ##
#####################################################################################

# Question: Do different habitat structure attributes aggect the species richness of fish in bommies

########################################################################
# Previous step: import data set Category; 
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
spat.data <- data.frame(Category)

########################################################################
# Step 0: Inspect the file

spat.data  # all the data
head(spat.data) # first 6 rows
head(data,10) # first 10 rows
str(spat.data)
#all looks good

summary(spat.data) # quick summary 
########################################################################

########################################################################
# Step 1: Outliers y and x

par(mfrow = c(1, 2)) # 1 row and 2 columns 

# boxplot of Sp_rich
boxplot(spat.data$Sp_rich, 
        main = "Species richness") # title label

# dotplot of Sp_rich
dotchart(spat.data$Sp_rich, 
         xlab = "Range of data",
         ylab = "Values")
# their appears to be a couple of outliers however none will be removed as all are actual observations
########################################################################


########################################################################
# Step 2: Homogeneity of variances

# conditional boxplot: Sp_rich by habitat structure variable (indepedently)
par(mfrow = c(2,3))
boxplot(Sp_rich ~ Dist_neighbour, data=spat.data, 
        xlab="Distance (m)",
        ylab="Species richness", 
        main="Conditional Boxplot")
#
boxplot(Sp_rich ~ Neighbour_5m, data=spat.data, 
        xlab="Number of neighbours within 5m",
        ylab="Species richness", 
        main="Conditional Boxplot")
#
boxplot(Sp_rich ~ Habitat, data=spat.data, 
        xlab="Habitat",
        ylab="Species richness", 
        main="Conditional Boxplot")
#
boxplot(Sp_rich ~ HAS_2, data=spat.data, 
        xlab="HAS_2",
        ylab="Species richness", 
        main="Conditional Boxplot")
#
boxplot(Sp_rich ~ Rugosity, data=spat.data, 
        xlab="Rugosity",
        ylab="Species richness", 
        main="Conditional Boxplot")
##
# results show that the variation in the observations
# from the habitat may be similar, therefore 
# they may not violate the assumption of homogeneity of variance
# however show that the variance in all other structural attributes may not
# have homogeneity of variance
# to investigate further:

# to investigate further:

#Levene test

leveneTest(Sp_rich ~ Dist_neighbour, data=spat.data)

leveneTest(Sp_rich ~ Neighbour_5m, data=spat.data)

leveneTest(Sp_rich ~ Habitat, data=spat.data)

leveneTest(Sp_rich ~ HAS_2, data=spat.data)

leveneTest(Sp_rich ~ Rugosity, data=spat.data)
# All return a P>0.05 therefore assumption of homogeneity is accepted

########################################################################

########################################################################
# Step 3: Normality
par(mfrow = c(1,1))
# histogram for the whole Sp_rich data set
hist(spat.data$Sp_rich,
     xlab = "Species richness", 
     breaks = 20,
     main = "Histogram", 
     ylab = "Frequency",
     col ="grey") 

histogram( ~ Sp_rich | Dist_neighbour , 
           data=spat.data,
           xlab = "Species richness", 
           breaks = 10, 
           col="grey",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )
#
histogram( ~ Sp_rich | Neighbour_5m , 
           data=spat.data,
           xlab = "Species richness", 
           breaks = 10, 
           col="grey",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )
#
histogram( ~ Sp_rich | HAS_2 , 
           data=spat.data,
           xlab = "Species richness", 
           breaks = 10, 
           col="grey",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )
#
histogram( ~ Sp_rich | Habitat , 
           data=spat.data,
           xlab = "Species richness", 
           breaks = 10, 
           col="grey",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )
#
histogram( ~ Sp_rich | Rugosity , 
           data=spat.data,
           xlab = "Species richness", 
           breaks = 10, 
           col="grey",
           scales= list(y=list(relation="free"),
                        x=list(relation="free") ) )
# does not suggest normailty within levels of each independent variable
# formal tests for normality:
# Shapiro-Wilk test
shapiro.test(spat.data$Sp_rich)
shapiro.test(spat.data$Sp_rich[spat.data$Dist_neighbour])
shapiro.test(spat.data$Sp_rich[spat.data$Neighbour_5m])
shapiro.test(spat.data$Sp_rich[spat.data$Rugosity]) 
shapiro.test(spat.data$Sp_rich[spat.data$HAS_2]) 
shapiro.test(spat.data$Sp_rich[spat.data$Habitat]) 
# all return p<0.05 therefore data normality not present however this will be tested in the residuals

###########################################################################################


###########################################################################################
# Steps 4-7 of data exsploration are not required for this test
###########################################################################################

###########################################################################################
# SPAT_ANOVA

SPAT_ANOVA<-aov(Sp_rich ~ Habitat * Dist_neighbour * Neighbour_5m, data = spat.data)
summary(SPAT_ANOVA)
# For some reason the code is wrong as it is not calculating the 3 way interaction and is also missing
# a 2 way interaction - it worked for the other 3 way anova in the "ANOVA.R" script, do you have any idea?
# I even wrote the long version of the equation and it still missed out the 3 way and a 2 way interaction ...
SPAT2_ANOVA<-aov(Sp_rich ~ Dist_neighbour + Neighbour_5m + Habitat + Dist_neighbour:Neighbour_5m + Dist_neighbour:Habitat + Dist_neighbour:Habitat + Dist_neighbour * Neighbour_5m * Habitat, data = Category)
summary(SPAT2_ANOVA)
# Only 2 way interaction between Dist_neighbour:Neighbour_5m significant (df=6  sum sq=395.9   meansq=65.99   f value=3.841 Pr(>F)0.00138)
# however I need to get the code above to calculate the missing 2 way and 3 way interaction this may have an impact on the result
#
# As I could not solve this issue I carried on anyway for now ...
TukeyHSD(SPAT_ANOVA)
# only need to focues on 2 way interaction between he number of bommie neighbours within 5m and the distance to the nearest bommie:
# $`Dist_neighbour:Neighbour_5m`
#diff          lwr        upr     p adj
#Low:High-High:High                               NA           NA         NA        NA
#Medium:High-High:High                            NA           NA         NA        NA
#Very high:High-High:High                         NA           NA         NA        NA
#Very low:High-High:High                          NA           NA         NA        NA
#High:Low-High:High                               NA           NA         NA        NA
#Low:Low-High:High                                NA           NA         NA        NA
#Medium:Low-High:High                             NA           NA         NA        NA
#Very high:Low-High:High                          NA           NA         NA        NA
#Very low:Low-High:High                           NA           NA         NA        NA
#High:Medium-High:High                            NA           NA         NA        NA
#Low:Medium-High:High                             NA           NA         NA        NA
#Medium:Medium-High:High                          NA           NA         NA        NA
#Very high:Medium-High:High                       NA           NA         NA        NA
#Very low:Medium-High:High                        NA           NA         NA        NA
#High:Very high-High:High                         NA           NA         NA        NA
#Low:Very high-High:High                          NA           NA         NA        NA
#Medium:Very high-High:High                       NA           NA         NA        NA
#Very high:Very high-High:High                    NA           NA         NA        NA
#Very low:Very high-High:High                     NA           NA         NA        NA
#Medium:High-Low:High                    -3.75491962 -19.38553583 11.8756966 0.9999977
#Very high:High-Low:High                          NA           NA         NA        NA
#Very low:High-Low:High                  -0.87389675  -7.41265264  5.6648591 1.0000000
#High:Low-Low:High                       -1.45338008 -10.19116017  7.2844000 1.0000000
#Low:Low-Low:High                         0.03180345  -5.45426241  5.5178693 1.0000000
#Medium:Low-Low:High                      1.41841216  -4.71241992  7.5492442 0.9999987
#Very high:Low-Low:High                  -0.02692873  -6.75327498  6.6994175 1.0000000
#Very low:Low-Low:High                   -4.52016820 -16.02399024  6.9836538 0.9970029
#High:Medium-Low:High                    10.62586058  -0.87796146 22.1296826 0.1095868
#Low:Medium-Low:High                     -0.08155503  -5.15028046  4.9871704 1.0000000
#Medium:Medium-Low:High                  -0.54498837  -7.27133462  6.1813579 1.0000000
#Very high:Medium-Low:High                        NA           NA         NA        NA
#Very low:Medium-Low:High                 0.37744865  -5.14880871  5.9037060 1.0000000
#High:Very high-Low:High                          NA           NA         NA        NA
#Low:Very high-Low:High                  -2.01992955  -9.61504381  5.5751847 0.9999882
#Medium:Very high-Low:High                        NA           NA         NA        NA
#Very high:Very high-Low:High                     NA           NA         NA        NA
#Very low:Very high-Low:High             -0.33726064  -7.06360689  6.3890856 1.0000000
#Very high:High-Medium:High                       NA           NA         NA        NA
#Very low:High-Medium:High                2.88102287 -12.81458578 18.5766315 1.0000000
#High:Low-Medium:High                     2.30153955 -14.43003550 19.0331146 1.0000000
#Low:Low-Medium:High                      3.78672308 -11.50032115 19.0737673 0.9999962
#Medium:Low-Medium:High                   5.17333178 -10.35676495 20.7034285 0.9996637
#Very high:Low-Medium:High                3.72799089 -12.04668934 19.5026711 0.9999982
#Very low:Low-Medium:High                -0.76524857 -19.09377072 17.5632736 1.0000000
#High:Medium-Medium:High                 14.38078021  -3.94774194 32.7093024 0.3453210
#Low:Medium-Medium:High                   3.67336459 -11.46891991 18.8156491 0.9999973
#Medium:Medium-Medium:High                3.20993126 -12.56474897 18.9846115 0.9999999
#Very high:Medium-Medium:High                     NA           NA         NA        NA
#Very low:Medium-Medium:High              4.13236827 -11.16914547 19.4338820 0.9999851
#High:Very high-Medium:High                       NA           NA         NA        NA
#Low:Very high-Medium:High                1.73499008 -14.42924709 17.8992272 1.0000000
#Medium:Very high-Medium:High                     NA           NA         NA        NA
#Very high:Very high-Medium:High                  NA           NA         NA        NA
#Very low:Very high-Medium:High           3.41765898 -12.35702125 19.1923392 0.9999996
#Very low:High-Very high:High                     NA           NA         NA        NA
#High:Low-Very high:High                          NA           NA         NA        NA
#Low:Low-Very high:High                           NA           NA         NA        NA
#Medium:Low-Very high:High                        NA           NA         NA        NA
#Very high:Low-Very high:High                     NA           NA         NA        NA
#Very low:Low-Very high:High                      NA           NA         NA        NA
#High:Medium-Very high:High                       NA           NA         NA        NA
#Low:Medium-Very high:High                        NA           NA         NA        NA
#Medium:Medium-Very high:High                     NA           NA         NA        NA
#Very high:Medium-Very high:High                  NA           NA         NA        NA
#Very low:Medium-Very high:High                   NA           NA         NA        NA
#High:Very high-Very high:High                    NA           NA         NA        NA
#Low:Very high-Very high:High                     NA           NA         NA        NA
#Medium:Very high-Very high:High                  NA           NA         NA        NA
#Very high:Very high-Very high:High               NA           NA         NA        NA
#Very low:Very high-Very high:High                NA           NA         NA        NA
#High:Low-Very low:High                  -0.57948333  -9.43300065  8.2740340 1.0000000
#Low:Low-Very low:High                    0.90570020  -4.76288751  6.5742879 1.0000000
#Medium:Low-Very low:High                 2.29230891  -4.00237694  8.5869948 0.9988683
#Very high:Low-Very low:High              0.84696802  -6.02905568  7.7229917 1.0000000
#Very low:Low-Very low:High              -3.64627145 -15.23824667  7.9457038 0.9998516
#High:Medium-Very low:High               11.49975733  -0.09221789 23.0917326 0.0545098
#Low:Medium-Very low:High                 0.79234172  -4.47339142  6.0580749 1.0000000
#Medium:Medium-Very low:High              0.32890839  -6.54711531  7.2049321 1.0000000
#Very high:Medium-Very low:High                   NA           NA         NA        NA
#Very low:Medium-Very low:High            1.25134540  -4.45614866  6.9588395 0.9999995
#High:Very high-Very low:High                     NA           NA         NA        NA
#Low:Very high-Very low:High             -1.14603280  -8.87401628  6.5819507 1.0000000
#Medium:Very high-Very low:High                   NA           NA         NA        NA
#Very high:Very high-Very low:High                NA           NA         NA        NA
#Very low:Very high-Very low:High         0.53663611  -6.33938759  7.4126598 1.0000000
#Low:Low-High:Low                         1.48518353  -6.62199594  9.5923630 1.0000000
#Medium:Low-High:Low                      2.87179224  -5.68487431 11.4284588 0.9996266
#Very high:Low-High:Low                   1.42645135  -7.56650002 10.4194027 1.0000000
#Very low:Low-High:Low                   -3.06678812 -16.02701042  9.8934342 0.9999982
#High:Medium-High:Low                    12.07924066  -0.88098164 25.0394630 0.1010741
#Low:Medium-High:Low                      1.37182505  -6.45897427  9.2026244 1.0000000
#Medium:Medium-High:Low                   0.90839171  -8.08455965  9.9013431 1.0000000
#Very high:Medium-High:Low                        NA           NA         NA        NA
#Very low:Medium-High:Low                 1.83082873  -6.30360185  9.9652593 0.9999992
#High:Very high-High:Low                          NA           NA         NA        NA
#Low:Very high-High:Low                  -0.56654947 -10.22652882  9.0934299 1.0000000
#Medium:Very high-High:Low                        NA           NA         NA        NA
#Very high:Very high-High:Low                     NA           NA         NA        NA
#Very low:Very high-High:Low              1.11611944  -7.87683193 10.1090708 1.0000000
#Medium:Low-Low:Low                       1.38660871  -3.80614202  6.5793594 0.9999875
#Very high:Low-Low:Low                   -0.05873218  -5.94271825  5.8252539 1.0000000
#Very low:Low-Low:Low                    -4.55197165 -15.58444552  6.4805022 0.9945765
#High:Medium-Low:Low                     10.59405713  -0.43841674 21.6265310 0.0761741
#Low:Medium-Low:Low                      -0.11335849  -3.99530846  3.7685915 1.0000000
#Medium:Medium-Low:Low                   -0.57679182  -6.46077788  5.3071942 1.0000000
#Very high:Medium-Low:Low                         NA           NA         NA        NA
#Very low:Medium-Low:Low                  0.34564520  -4.11721022  4.8085006 1.0000000
#High:Very high-Low:Low                           NA           NA         NA        NA
#Low:Very high-Low:Low                   -2.05173300  -8.91200404  4.8085380 0.9999293
#Medium:Very high-Low:Low                         NA           NA         NA        NA
#Very high:Very high-Low:Low                      NA           NA         NA        NA
#Very low:Very high-Low:Low              -0.36906409  -6.25305016  5.5149220 1.0000000
#Very high:Low-Medium:Low                -1.44534089  -7.93467640  5.0439946 0.9999993
#Very low:Low-Medium:Low                 -5.93858036 -17.30544726  5.4282865 0.9370351
#High:Medium-Medium:Low                   9.20744842  -2.15941848 20.5743153 0.2885074
#Low:Medium-Medium:Low                   -1.49996719  -6.24967444  3.2497401 0.9998428
#Medium:Medium-Medium:Low                -1.96340053  -8.45273604  4.5259350 0.9999160
#Very high:Medium-Medium:Low                      NA           NA         NA        NA
#Very low:Medium-Medium:Low              -1.04096351  -6.27615805  4.1942310 0.9999999
#High:Very high-Medium:Low                        NA           NA         NA        NA
#Low:Very high-Medium:Low                -3.43834171 -10.82437597  3.9476926 0.9791972
#Medium:Very high-Medium:Low                      NA           NA         NA        NA
#Very high:Very high-Medium:Low                   NA           NA         NA        NA
#Very low:Very high-Medium:Low           -1.75567280  -8.24500831  4.7336627 0.9999846
#Very low:Low-Very high:Low              -4.49323947 -16.19205543  7.2055765 0.9977475
#High:Medium-Very high:Low               10.65278931  -1.04602665 22.3516053 0.1240971
#Low:Medium-Very high:Low                -0.05462630  -5.55156643  5.4423138 1.0000000
#Medium:Medium-Very high:Low             -0.51805963  -7.57271110  6.5365918 1.0000000
#Very high:Medium-Very high:Low                   NA           NA         NA        NA
#Very low:Medium-Very high:Low            0.40437738  -5.51709995  6.3258547 1.0000000
#High:Very high-Very high:Low                     NA           NA         NA        NA
#Low:Very high-Very high:Low             -1.99300081  -9.88034093  5.8943393 0.9999948
#Medium:Very high-Very high:Low                   NA           NA         NA        NA
#Very high:Very high-Very high:Low                NA           NA         NA        NA
#Very low:Very high-Very high:Low        -0.31033191  -7.36498337  6.7443196 1.0000000
#High:Medium-Very low:Low                15.14602878   0.18085311 30.1112044 0.0437680
#Low:Medium-Very low:Low                  4.43861316  -6.39238580 15.2696121 0.9949975
#Medium:Medium-Very low:Low               3.97517983  -7.72363613 15.6739958 0.9995566
#Very high:Medium-Very low:Low                    NA           NA         NA        NA
#Very low:Medium-Very low:Low             4.89761685  -6.15489787 15.9501316 0.9877249
#High:Very high-Very low:Low                      NA           NA         NA        NA
#Low:Very high-Very low:Low               2.50023865  -9.71877611 14.7192534 0.9999998
#Medium:Very high-Very low:Low                    NA           NA         NA        NA
#Very high:Very high-Very low:Low                 NA           NA         NA        NA
#Very low:Very high-Very low:Low          4.18290756  -7.51590841 15.8817235 0.9991130
#Low:Medium-High:Medium                 -10.70741561 -21.53841458  0.1235833 0.0565749
#Medium:Medium-High:Medium              -11.17084895 -22.86966491  0.5279670 0.0804869
#Very high:Medium-High:Medium                     NA           NA         NA        NA
#Very low:Medium-High:Medium            -10.24841193 -21.30092665  0.8041028 0.1058801
#High:Very high-High:Medium                       NA           NA         NA        NA
#Low:Very high-High:Medium              -12.64579013 -24.86480489 -0.4267754 0.0338205
#Medium:Very high-High:Medium                     NA           NA         NA        NA
#Very high:Very high-High:Medium                  NA           NA         NA        NA
#Very low:Very high-High:Medium         -10.96312122 -22.66193719  0.7356947 0.0961502
#Medium:Medium-Low:Medium                -0.46343333  -5.96037347  5.0335068 1.0000000
#Very high:Medium-Low:Medium                      NA           NA         NA        NA
#Very low:Medium-Low:Medium               0.45900368  -3.47954140  4.3975488 1.0000000
#High:Very high-Low:Medium                        NA           NA         NA        NA
#Low:Very high-Low:Medium                -1.93837451  -8.46971263  4.5929636 0.9999369
#Medium:Very high-Low:Medium                      NA           NA         NA        NA
#Very high:Very high-Low:Medium                   NA           NA         NA        NA
#Very low:Very high-Low:Medium           -0.25570561  -5.75264574  5.2412345 1.0000000
#Very high:Medium-Medium:Medium                   NA           NA         NA        NA
#Very low:Medium-Medium:Medium            0.92243701  -4.99904031  6.8439143 1.0000000
#High:Very high-Medium:Medium                     NA           NA         NA        NA
#Low:Very high-Medium:Medium             -1.47494118  -9.36228130  6.4123989 1.0000000
#Medium:Very high-Medium:Medium                   NA           NA         NA        NA
#Very high:Very high-Medium:Medium                NA           NA         NA        NA
#Very low:Very high-Medium:Medium         0.20772772  -6.84692374  7.2623792 1.0000000
#Very low:Medium-Very high:Medium                 NA           NA         NA        NA
#High:Very high-Very high:Medium                  NA           NA         NA        NA
#Low:Very high-Very high:Medium                   NA           NA         NA        NA
#Medium:Very high-Very high:Medium                NA           NA         NA        NA
#Very high:Very high-Very high:Medium             NA           NA         NA        NA
#Very low:Very high-Very high:Medium              NA           NA         NA        NA
#High:Very high-Very low:Medium                   NA           NA         NA        NA
#Low:Very high-Very low:Medium           -2.39737820  -9.28983208  4.4950757 0.9993887
#Medium:Very high-Very low:Medium                 NA           NA         NA        NA
#Very high:Very high-Very low:Medium              NA           NA         NA        NA
#Very low:Very high-Very low:Medium      -0.71470929  -6.63618662  5.2067680 1.0000000
#Low:Very high-High:Very high                     NA           NA         NA        NA
#Medium:Very high-High:Very high                  NA           NA         NA        NA
#Very high:Very high-High:Very high               NA           NA         NA        NA
#Very low:Very high-High:Very high                NA           NA         NA        NA
#Medium:Very high-Low:Very high                   NA           NA         NA        NA
#Very high:Very high-Low:Very high                NA           NA         NA        NA
#Very low:Very high-Low:Very high         1.68266891  -6.20467121  9.5700090 0.9999997
#Very high:Very high-Medium:Very high             NA           NA         NA        NA
#Very low:Very high-Medium:Very high              NA           NA         NA        NA
#Very low:Very high-Very high:Very high           NA           NA         NA        NA
#
#
#
#
# As there are lots of 'NAs'should I report that though there was a significant value found,
# the ammount of retunred NAs in the post hoc shows that there were not enough repeats of all
# combinations of the 2 variables at each level therefore this is limited and no difference can be
# confidently reported.
# Also .... the ANOVA code did not work properly as it missed out a 2 way and the 3 way interaction
#
#
#
# checking assumptions:
par(mfrow =c(3,2))
plot(SPAT_ANOVA)
qqnorm(SPAT_ANOVA$resid)
qqline(SPAT_ANOVA$resid)
hist(SPAT_ANOVA$resid)
# residuals vs fitted: concerned about lack of values from 8-16
# normal q-q:normal distribution
# scale-location :unlikely to have homogeneity of variance
# residuals vs leverage: no point is near cook's distance
#                         therefore no influential data points
library(moments)
skewness(SPAT_ANOVA$resid)
# skewed = 0.75 therefore unlikely to be normally distributed 
#therefore apply a boxcox transformation to help with non normality and heterogeneity of data
library(MASS)
par(mfrow = c(1,1))
b=boxcox(Sp_rich2 ~ Habitat * HAS_2 * Rugosity, data = Category)
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
TRANSSPAT_ANOVA<-aov(Sp_rich2^(1/3) ~ Habitat * HAS_2 * Rugosity, data = Category) # as lamda=0.34343434
summary(TRANSSPAT_ANOVA)
# Only HAS_2 with a significant p value (df=2  sum sq=22.32  mean sq=11.161  f=37.952 Pr(>F)=4.6e-14 ***)
TukeyHSD(TRANSSPAT_ANOVA)
# Only need to look at HAS_2@
#$HAS_2
#diff        lwr        upr     p adj
#Low-High    -1.2197251 -1.6598502 -0.7796001 0.0000000
#Medium-High -0.5521385 -0.9787116 -0.1255654 0.0072658
#Medium-Low   0.6675866  0.4529278  0.8822455 0.0000000
boxplot(Sp_rich2 ~ HAS_2, data=spat.data, 
        xlab="HAS_2",
        ylab="Species richness", 
        main="Conditional Boxplot")




# comparing residuals from before and after boxcox transformation

windows()
par(mfrow =c(3,2))
plot(SPAT_ANOVA)
qqnorm(SPAT_ANOVA$resid)
qqline(SPAT_ANOVA$resid)
hist(SPAT_ANOVA$resid)

#
#
#

windows()
par(mfrow =c(3,2))
plot(TRANSSPAT_ANOVA)
qqnorm(TRANSSPAT_ANOVA$resid)
qqline(TRANSSPAT_ANOVA$resid)
hist(TRANSSPAT_ANOVA$resid)
# normality of data looks more ikely
# homogeneity of data looks more likely
# no influential data points
#
#
#
skewness(SPAT_ANOVA$resid)
skewness(TRANSSPAT_ANOVA$resid)
# # Non transformed SPAT_ANOVA  is more skewed (0.75) the transformed ANOVA
# (TRANSANOVA) which is less skewed (-0.6)
#
#
###########################################################################################

#######################################################################################
# END