library(car)
library(MASS)
#Creates a fit to determine adjusted box office based off year, studio, and theaters showing
fit <- lm(MyData$Adjusted.Life~MyData$Theaters+MyData$OTHER+MyData$FOX+MyData$Sgem+MyData$Warner+MyData$Uni+MyData$Tris+MyData$Sony+MyData$MGM+MyData$IMDb+MyData$RottenT)
#Preforms stepwise regression
step <- stepAIC(fit, direction="both")
fit2 <-lm(MyData$Adjusted.Life~MyData$Theaters + MyData$FOX + MyData$RottenT + MyData$MGM + MyData$Sgem)
fit22 <- lm((((MyData$Adjusted.Life)^.40-1)/.40)~ MyData$Theaters + MyData$FOX + MyData$RottenT+ MyData$MGM + MyData$Sgem)
fit21 <- lm(log(MyData$Adjusted.Life)~ MyData$Theaters + MyData$FOX + MyData$RottenT + MyData$MGM + MyData$Sgem)
summary(fit2)
boxcox(fit2)
summary(fit22)
summary(fit21)
hist(studres(fit22), main="Histogram of Studentized Residuals on Box-Cox Transformed Fit")
#Preforms an anova test (See if parameters are significant)
anova2=anova(fit2)
anova4=anova(fit22)
#Checks to see if autocorrelated
durbinWatsonTest(fit22)
#Checks if outliers
outlierTest(fit22)
#Checks Constant Variance
ncvTest(fit22)
spreadLevelPlot(fit22)
#Checks if Normal
##Number of Theaters
qqnorm(MyData$Theaters, main="QQ Plot",xlab="Theater Number Quantiles")
qqline(MyData$Theaters)
##If Made by SGEM
qqnorm(MyData$Sgem, main="QQ Plot")
##If Made by Warner
qqnorm(MyData$Warner, main="QQ Plot")
##Rotten Tomato Score
qqnorm(MyData$IMDb,main="QQ Plot",xlab="IMDb Score Quantile")
qqline(MyData$IMDb)
##Multicolinearity
vif(fit22) # variance inflation factors
sqrt(vif(fit22)) > 2 # problem?
##Linearity
crPlots(fit22)
#Test Equality of Means
frame1=(data.frame(b,MyData$Adjusted.Life))
b=as.factor(c(rep("Other",13),rep("MGM",3),rep("Sony",2),rep("TriStar",3),rep("Universal",6),rep("Warner",2),rep("Screen Gem",6),rep("Fox",8)))
summary(aov(MyData$Adjusted.Life~b))
TukeyHSD(aov(MyData$Adjusted.Life~b),conf.level=.9)

##Mean based on studio
meanother=mean(frame1[1:13,]$MyData.Adjusted.Life)
s2other=var(frame1[1:13,]$MyData.Adjusted.Life)
meanfox=mean(frame1[37:43,]$MyData.Adjusted.Life)
s2fox=var(frame1[c(37:43),]$MyData.Adjusted.Life)
meanSG=mean(frame1[c(30:35),]$MyData.Adjusted.Life)
s2SG=var(frame1[c(30:35),]$MyData.Adjusted.Life)
meanWarner=mean(frame1[c(28:29),]$MyData.Adjusted.Life)
s2Warner=var(frame1[c(28:29),]$MyData.Adjusted.Life)
meanUni=mean(frame1[c(22:27),]$MyData.Adjusted.Life)
s2Uni=var(frame1[c(22:27),]$MyData.Adjusted.Life)
meanTris=mean(frame1[c(19:21),]$MyData.Adjusted.Life)
s2Tris=var(frame1[c(19:21),]$MyData.Adjusted.Life)
meanSony=mean(frame1[c(17:18),]$MyData.Adjusted.Life)
s2Sony=var(frame1[c(17:18),]$MyData.Adjusted.Life)
meanMGM=mean(frame1[c(14:16),]$MyData.Adjusted.Life)
s2MGM=var(frame1[c(14:16),]$MyData.Adjusted.Life)
meanParamount=mean(frame1[c(22:23),]$MyData.Adjusted.Life)
s2Paramount=var(frame1[c(22:23),]$MyData.Adjusted.Life)

means=c(meanother, meanfox, meanSG, meanWarner, meanUni,meanTris, meanSony, meanMGM,meanParamount)
sd=c(s2other, s2fox,s2SG, s2Warner,s2Uni,s2Tris ,s2Sony, s2MGM,s2Paramount)^.5
rr=data.frame(means,sd)
boxplot(MyData$Adjusted.Life~b,ylab="Adjusted Gross Revenue")

plot(MyData$IMDb,MyData$RottenT, main="Rotten Tomatoe Score vs IMDb Score",xlab="IMDb Score",ylab="Rotten Tomaotoe Score")
summary(lm(MyData$IMDb~MyData$RottenT))
