# Patrick McHugh, Tyler Poelking
# Stat 3301
# Final Project
library(ggplot2)
library(GGally)
library(car)
library(scatterplot3d)
library(gridExtra)
library(leaps)


# Variables to keep:


# Variables to throw out

# Pool.QC: Pool Quality, but only 9 houses have pools.
# Pool Area: Same as above. Only 9 houses with pools.


#Ty
housingDataAll <- read_csv("~/Desktop/All Stuff/School Stuff/R Stuff/HousingPrices/housing_data.csv")


#get data types of each columns
sapply(housingDataAll, class)

#subset to integer columns
nums <- sapply(housingDataAll, is.integer)
housing_continuous = housingDataAll[ , nums]
sapply(housing_continuous, class)

#make a scatterplot of the continuous variables
plot(housing_continuous)


housingDataAll = subset(housingDataAll, )
plot(housingDataAll)


# Remove data affecting only a small number of houses.
housingData <- subset(housingDataAll, select = -c(44,68,70,71,73,74))
housingData$BsmtFin.SF=housingData$BsmtFin.SF.1+housingData$BsmtFin.SF.2
housingData <- subset(housingDataAll, select = -c(Pool.Area,X1st.Flr.SF, X2nd.Flr.SF, Function))
# Sum various bath attributes into one column
housingData$Total.Baths <- housingData$Full.Bath + housingData$Bsmt.Full.Bath + .5*housingData$Half.Bath + .5*housingData$Bsmt.Half.Bath
housingData$true.age <- as.numeric(apply(housingData, 1, function(x) max(x[18], x[19])))
housingData$Age.Factor <- ifelse(housingData$Year.Built > 1932, ifelse(housingData$Year.Built > 1985, ifelse(housingData$Year.Built>2002, "Very New", "New-Ish"), "Medium"), "Old")

#Remove consolidated attributes
housingData <- subset(housingDataAll, select = -c(Full.Bath, Bsmt.Full.Bath,Half.Bath,Bsmt.Half.Bath))


#YEAR EXPLORATORY

#Plot Year
plot(SalePrice~Year.Built, data=housingData, xlab="Year Built", ylab="Sale Price", main="Sale Price vs Year Built", cex=0.5)

#Add Age Factor based on Year Built
housingData$Age <- ifelse(housingData$Year.Built > 1970, "New", "Old")

maxRsq <- 0
oneVarModels <- list()
for (i in 1:77){
  colname <- colnames(housingData)[i]
  plot(housingData[,i], housingData[,78], xlab=colnames(housingData)[i], ylab="Sale Price")
  oneVarModels[[i]] <- lm(housingData[,78] ~ housingData[,paste(colname)])
  if (lm(housingData[,78] ~ housingData[,paste(colname)])$sigma)
}

##MODEL BUILDING

install.packages("MASS")
library("MASS")
null = lm(SalePrice ~ 1, data=housingData)
full = lm(SalePrice ~ .,data=housingData)
n <- nrow(housingData)
stepAIC(null, scope=list(lower=null, upper=full), direction="forward", k=log(n))


summary(lm(formula = SalePrice ~ Overall.Qual + Gr.Liv.Area + BsmtFin.SF.1 + 
       Neighborhood + Kitchen.Qual + MS.SubClass + Bsmt.Exposure + 
       Exter.Qual + Garage.Area + Overall.Cond + Year.Built + Lot.Area + 
       Total.Bsmt.SF + Bedroom.AbvGr + Bsmt.Qual + Bldg.Type + Functional + 
       Screen.Porch + BsmtFin.SF.2 + Mas.Vnr.Area + Pool.QC + Fireplaces + 
       Year.Remod.Add + Garage.Yr.Blt + Garage.Cars + Pool.Area + 
       Mas.Vnr.Type, data = housingData))


#Added Variable Plot for Overall.Qual and Overall.Cond
summary(lm(SalePrice~Overall.Qual, data=housingData))
e1 = resid(lm(SalePrice ~ Overall.Qual, data=housingData)) 
e2 = resid(lm(Overall.Cond~Overall.Qual, data=housingData))
plot(e1~e2)
summary(lm(e1~e2))$r.sq
#Small r.sq. Adding Overall.Cond does not explain more variability.
#Can remove Overall.Cond since Overall.Qual single linear regression model has > r^2
housingData <- subset(housingDataAll, select = -c(Overall.Cond))

#Test whether having Just Exter.Qual enough
null.model = lm(SalePrice ~ Exter.Qual, data=housingData) 
RSS.null = sum(resid(null.model)^2) 
df.null = null.model$df.residual 
alt.model = lm(SalePrice ~ Exter.Qual + Exter.Cond, data=housingData) 
RSS.alt = sum(resid(alt.model)^2) 
df.alt = alt.model$df.residual 
Fstat = ((RSS.null - RSS.alt)/(df.null - df.alt)) / (RSS.alt/df.alt) 
pf(Fstat, df1 = df.null - df.alt, df2 = df.alt, lower.tail=F)

#Test whether having Just Bsmt.Cond enough
null.model = lm(SalePrice ~ Bsmt.Cond, data=housingData) 
RSS.null = sum(resid(null.model)^2) 
df.null = null.model$df.residual 
alt.model = lm(SalePrice ~ Bsmt.Qual + Bsmt.Cond, data=housingData) 
RSS.alt = sum(resid(alt.model)^2) 
df.alt = alt.model$df.residual 
Fstat = ((RSS.null - RSS.alt)/(df.null - df.alt)) / (RSS.alt/df.alt) 
pf(Fstat, df1 = df.null - df.alt, df2 = df.alt, lower.tail=F)

null.model = lm(SalePrice ~ Neighborhood, data=housingData) 
RSS.null = sum(resid(null.model)^2) 
df.null = null.model$df.residual 
alt.model = lm(SalePrice ~ Neighborhood + Overall.Qual, data=housingData) 
RSS.alt = sum(resid(alt.model)^2) 
df.alt = alt.model$df.residual 
Fstat = ((RSS.null - RSS.alt)/(df.null - df.alt)) / (RSS.alt/df.alt) 
pf(Fstat, df1 = df.null - df.alt, df2 = df.alt, lower.tail=F)


#Select one basement attribute with best r^2
bsmt=c("Bsmt.Cond","Bsmt.Qual","Bsmt.Exposure","Bsmt.BsmtFin.Type.1","BsmtFin.SF.1","BsmtFin.Type.2","BsmtFin.SF.2","Bsmt.Unf.SF","Total.Bsmt.SF")
bsmtAts=append(bsmtAts, c("Bsmt.Cond",summary(lm(SalePrice~Bsmt.Cond+Neighborhood+Overall.Qual, data=housingData))$r.sq))
bsmtAts=append(bsmtAts, c("Bsmt.Qual",summary(lm(SalePrice~Bsmt.Qual+Neighborhood+Overall.Qual, data=housingData))$r.sq))
bsmtAts=append(bsmtAts, c("Bsmt.Exposure",summary(lm(SalePrice~Bsmt.Exposure+Neighborhood+Overall.Qual, data=housingData))$r.sq))
bsmtAts=append(bsmtAts, c("Bsmt.BsmtFin.Type.1",summary(lm(SalePrice~BsmtFin.Type.1+Neighborhood+Overall.Qual, data=housingData))$r.sq))
bsmtAts=append(bsmtAts, c("BsmtFin.SF.1",summary(lm(SalePrice~BsmtFin.SF.1+Neighborhood+Overall.Qual, data=housingData))$r.sq))
bsmtAts=append(bsmtAts, c("BsmtFin.Type.2",summary(lm(SalePrice~BsmtFin.Type.2+Neighborhood+Overall.Qual, data=housingData))$r.sq))
bsmtAts=append(bsmtAts, c("BsmtFin.SF.2",summary(lm(SalePrice~BsmtFin.SF.2+Neighborhood+Overall.Qual, data=housingData))$r.sq))
bsmtAts=append(bsmtAts, c("Bsmt.Unf.SF",summary(lm(SalePrice~Bsmt.Unf.SF+Neighborhood+Overall.Qual, data=housingData))$r.sq))
bsmtAts=append(bsmtAts, c("Total.Bsmt.SF",summary(lm(SalePrice~Total.Bsmt.SF+Neighborhood+Overall.Qual, data=housingData))$r.sq))

#Bsmt.Qual has highest r^2 of 0.453558

#Next Highest is Total.Bsmt.SF
e1 = resid(lm(SalePrice ~ Lot.Area, data=housingData)) 
e2 = resid(lm(ConsolArea~Lot.Area, data=housingData))
plot(e1~e2)
summary(lm(e1~e2))$r.sq
#Small r.sq. Adding Overall.Cond does not explain more variability.
#Can remove Overall.Cond since Overall.Qual single linear regression model has > r^2
housingData <- subset(housingDataAll, select = -c(Overall.Cond))


scatterplotMatrix(formula = ~ Bsmt.Cond + Bsmt.Qual + 
                      Bsmt.Exposure+BsmtFin.Type.1+BsmtFin.SF.1+BsmtFin.Type.2+BsmtFin.SF.2+Bsmt.Unf.SF+Total.Bsmt.SF, data=housingData, diagonal="none", 
                  smoother=FALSE, col=c("red", "black", "black"), 
                  lwd=2, pch=16)


anova(lm(SalePrice ~ Overall.Qual+ Neighborhood+ Gr.Liv.Area+ Exter.Cond+Exter.Qual, data=housingData))
#Exter.Cond Does not add as much as Exter.Qual

anova(lm(SalePrice ~ Overall.Qual+ Neighborhood+ Gr.Liv.Area+ Garage.Cond+Garage.Qual, data=housingData))
#Garage qual ads about as much as garage qual

housingData$Grg.Gen.Test<- as.factor(housingData$Garage.Qual)
housingData$Grg.Gen.Test<- sapply(as.character(as.factor(housingData$Garage.Qual)), switch,Ex=5,Gd=4,TA=3,Fa=2,Po=1,No=0)
housingData$Grg.Gen.Test

changelevels <- function(newCol, colOne, colTwo) {
    temp1 <- sapply(as.character(as.factor(colOne)), switch,Ex=5,Gd=4,TA=3,Fa=2,Po=1,NoB=0)
    temp2 <- sapply(as.character(as.factor(colTwo)), switch,Ex=5,Gd=4,TA=3,Fa=2,Po=1,NoB=0)
    newCol<-(temp1+temp2)/2
    
}

housingData$Grg.Gen.Test <- changelevels(housingData$Grg.Gen.Test,housingData$Garage.Cond, housingData$Garage.Qual )
housingData$Exter.Gen <- changelevels(housingData$Exter.Gen,housingData$Exter.Cond, housingData$Exter.Qual )
housingData$Bsmt.Gen <- changelevels(housingData$Bsmt.Gen,housingData$Bsmt.Cond, housingData$Bsmt.Qual )

HeatAts=c("Heating", "Heating.QC", "Central.Air")
GrgAts=c("Garage.Type", "Garage.Yr.Blt", "Garage.Finish", "Garage.Cars","Garage.Area", "Garage.Qual", "Garage.Cond")
Areas=c("Lot.Area", "Total.Bsmt.SF","Gr.Live.Area", "Garage.Area", "Wood.Deck.SF","Open.Porch.SF","Enclosed.Porch", "X3Ssn.Porch", "Screen.Porch", "Gr.Liv.Area")
External=c("Exter.Qual", "Exter.Cond","Foundation","Mas.Vnr.Area","Mas.Vnr.Type","Exterior.1","Exterior.2","Roof.Matl","Roof.Style")
Area=c("Lot.Area","Total.Bsmt.SF","Gr.Liv.Area","Garage.Area")
#+ housingData$Wood.Deck.SF+housingData$Open.Porch.SF+housingData$Enclosed.Porch +housingData$X3Ssn.Porch+ housingData$Screen.Porch)

Total.Bsmt.SF
Garage.Area
Exter.Qual
BsmtFin.SF.1

qplot(x=Year.Built, y=SalePrice, data=housingData, ylab="Sale Price", xlab="Year Built", main="Sale Price vs Year Built Smoothed") + 
    geom_smooth(method="loess", se=F, span=0.75)


housingData$true.age <- as.numeric(apply(housingData, 1, function(x) max(x[18], x[19])))


backModel=lm(SalePrice~Neighborhood+as.factor(Overall.Qual)+BsmtFin.SF+Total.Bsmt.SF+Gr.Liv.Area+Garage.Area+Lot.Area+Bldg.Type+Overall.Cond+Mas.Vnr.Area+Bsmt.Exposure+Bedroom.AbvGr+Kitchen.Qual+Age.Factor+Age.Factor:Year.Built, data=housingData)

#Diagnostics
#plot of standardized residuals ri versus ﬁtted values yi
plot(x=fitted(backModel), y=rstandard(backModel), xlab="Fitted Values", ylab="Residuals", main="Residuals vs. Fitted Values",cex=0.5); abline(h=0)

#Plot of standardized residuals versus (each) predictor variables.
par(mfrow=c(2,2))
predictors=c("Neighborhood","BsmtFin.SF","Total.Bsmt.SF","Gr.Liv.Area","Garage.Area","Lot.Area","Bldg.Type","Overall.Cond","Mas.Vnr.Area","Bsmt.Exposure","Bedroom.AbvGr","Kitchen.Qual")
for (i in predictors){
col <- housingData[,paste(i)]    
plot(col, rstandard(backModel), xlab="Predictor", ylab="Stand Residuals",main=i, cex=0.5); abline(h=0)
}
plot(housingData$Overall.Qual, rstandard(backModel), xlab="Predictor", ylab="Stand Residuals",main="Overall.Qual as Numeric", cex=0.5); abline(h=0)
plot(as.factor(housingData$Overall.Qual), rstandard(backModel), xlab="Predictor", ylab="Stand Residuals",main="Overall.Qual as Factor", cex=0.5); abline(h=0)
plot(as.factor(housingData$Age.Factor), rstandard(backModel), xlab="Predictor", ylab="Stand Residuals",main="Age", cex=0.5); abline(h=0)


#Plot Stand resids against some potential predictors we didnt include
plot(housingData$Exter.Qual, rstandard(backModel), xlab="Predictor", ylab="Residuals",main="Exter.Qual", cex=0.5); abline(h=0)
plot(housingData$Bsmt.Qual, rstandard(backModel), xlab="Predictor", ylab="Residuals",main="Bsmt.Qual", cex=0.5); abline(h=0)
plot(housingData$Year.Built, rstandard(backModel), xlab="Predictor", ylab="Residuals",main="Year.Built", cex=0.5); abline(h=0)


temp3=lm(SalePrice~Age.Factor:Year.Built, data=housingData)
plot(x=fitted(temp3), rstandard(backModel), xlab="Fitted", ylab="Residuals",main="Age.Factor:Year.Built", cex=0.5); abline(h=0)

megaSearch<-regsubsets(SalePrice~Neighborhood+Overall.Qual+BsmtFin.SF+Total.Bsmt.SF+Exter.Qual+Gr.Liv.Area+Garage.Area+Lot.Area+Condition.1+Bldg.Type+Overall.Cond+Year.Built+Mas.Vnr.Type+Mas.Vnr.Area+Bsmt.Qual+Bsmt.Exposure+Bedroom.AbvGr+Kitchen.AbvGr+Kitchen.Qual+Total.Baths+Age.Factor+true.age+Age.Factor:true.age,data=housingData,nvmax=23, force.in=c(1:7),really.big=T)
