forest2

#1

maple=tapply(forest2$Maple, forest2$Area, mean)
beech=tapply(forest2$Beech, forest2$Area, mean)

newData=cbind(maple,beech)

barplot(newData,beside=T,col=c("#ee7700","#3333ff")
        ,main="Mean Basal Areas in Four Quadrants",xlab="Area",ylab="Basal Area")

legend("topleft",rownames(newData),fill=c("#ee7700","#3333ff"))

#Do the areas seem to differ with respect to basal area for these two species?
# Basal areas in Maple trees appear to be significantly larger in central and pasture, while 
# Basal Areas in Beech trees appear to be significantly larger in chestnut and west-facing

#2
forest2$MapMinusBeech <- forest2$Maple-forest2$Beech

CentralData=forest2[which(as.character(forest2$Area)=="central"),]
ChestnutData=forest2[which(as.character(forest2$Area)=="chestnut"),]
PastureData=forest2[which(as.character(forest2$Area)=="pasture"),]
WestFacingData=forest2[which(as.character(forest2$Area)=="west-facing"),]

t.test(as.numeric(CentralData$MapMinusBeech), alternative="greater")

t.test(as.numeric(ChestnutData$MapMinusBeech), alternative="less")

t.test(as.numeric(PastureData$MapMinusBeech), alternative="greater")

t.test(as.numeric(WestFacingData$MapMinusBeech), alternative="less")

##We need 12 Hypothesis tests. Use this to compare each plot with the other 3, for each species
Maple.Central <- CentralData$Maple
Beech.Central <- CentralData$Beech
Maple.Chestnut <- ChestnutData$Maple
Beech.Chestnut <- ChestnutData$Beech
Maple.Pasture <- PastureData$Maple
Beech.Pasture <- PastureData$Beech
Maple.WestFacing <- WestFacingData$Maple
Beech.WestFacing <- WestFacingData$Beech

## Maple Central vs Maple Pasture
t.test(x = Maple.Central, y = Maple.Pasture, alternative = "two.sided")

## Maple Central vs Maple WF
t.test(x = Maple.Central, y = Maple.WestFacing, alternative = "two.sided")

## Maple Central vs Maple Chestnut
t.test(x = Maple.Central, y = Maple.Chestnut, alternative = "two.sided")

## Maple Pasture vs Maple WF
t.test(x = Maple.Pasture, y = Maple.WestFacing, alternative = "two.sided")

## Maple Pasture vs Maple Chestnut
t.test(x = Maple.Pasture, y = Maple.Chestnut, alternative = "two.sided")

## Maple WF vs Maple Chestnut
t.test(x = Maple.WestFacing, y = Maple.Chestnut, alternative = "two.sided")

## Beech Central vs Beech Pasture
t.test(x = Beech.Central, y = Beech.Pasture, alternative = "two.sided")

## Beech Central vs Beech WF
t.test(x = Beech.Central, y = Beech.WestFacing, alternative = "two.sided")

## Beech Central vs Beech Chestnut
t.test(x = Beech.Central, y = Beech.Chestnut, alternative = "two.sided")

## Beech Pasture vs Beech WF
t.test(x = Beech.Pasture, y = Beech.WestFacing, alternative = "two.sided")

## Beech Pasture vs Beech Chestnut
t.test(x = Beech.Pasture, y = Beech.Chestnut, alternative = "two.sided")

## Beech WF vs Beech Chestnut
t.test(x = Beech.WestFacing, y = Beech.Chestnut, alternative = "two.sided")


#3

## Assuming no difference in the mean basal area for any of the forest areas for either species,
##how many of these tests would be expected to show a significant result (e.g., to reject \(H_0\))?





#NOT NEEDED
qqnorm(CentralData$MapMinusBeech)
qqline(CentralData$MapMinusBeech)

qqnorm(ChestnutData$MapMinusBeech)
qqline(ChestnutData$MapMinusBeech)

qqnorm(PastureData$MapMinusBeech)
qqline(PastureData$MapMinusBeech)

qqnorm(WestFacingData$MapMinusBeech)
qqline(WestFacingData$MapMinusBeech)