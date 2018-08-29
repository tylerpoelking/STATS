load('~/Desktop/All Stuff/School Stuff/STATS/Data/credit.Rdata')
print(length(newcredit))

#Summary for initial analysis
summary(newcredit)

#Continuous variables to put in scatterplot
keeps <- c("Income", "Limit", "Rating", "Cards", "Age", "Education", "Balance")
newcreditCont = newcredit[keeps]

newcredit$Private =as.factor(college$Private)
attach(newcredit)

#Scatterplot variables
pairs(newcreditCont)


#Of all the continuous variables, Limit and Rating appear to have the strongest (positive) correlation with Balance. Both correlations seem almost equal, which is not a suprise, since Limit and Rating themselves have an extremely high correlation between each other. Income is also highly correlated. My assumption is that we will only need one of these in the model as to avoid multicollinearity. The reltionship appears linear but it may be of a higher or lower degree, we will have to test this. Cards, Age, and Education don't have high correlation, so the degree of information each of these would add to our model stands questionable.


#Categorical variables and their affects on Balance are not easily analyized in a scatterplot such as the one above. We are going to construct boxplots, plotting Balance against these each categorical variable, as seen below.  These charts are much more interpretable and make analysis easier. 
#Student
boxplot(Balance~Student, main="Balance by Student", 
xlab="Student", ylab="Balance")
#Married
boxplot(Balance~Married, main="Balance by Married", 
xlab="Married", ylab="Balance")
#Ethnicity
boxplot(Balance~Ethnicity, main="Balance by Ethnicity", 
xlab="Ethnicity", ylab="Balance")

#The scale of the y-axis on each of these plots is the same, which allows us to compare between plots. Of the three categorical variables charted, the Student has the most prominent difference in meen balance and thus might contribute the most to our model. The mean Balance between ethnicities varries some too, so we may still use Ethnicity. The two box and whiskers in the 'Balance by Married' chart, however, are quite similar, indicating marrital status does not impact Balance.


#Rating? or Limit?
lmfit = lm( Balance ~ Rating)
summary(lmfit)

lmfit2 = lm( Balance ~ Limit)
summary(lmfit2)
#The model using Rating resulted in a smaller Residual standard error and larger Multiple R-squared (though they were both close, of course), so we will stick with that. 




#Seeing how addition of student affects fit
lmfit3 = lm( Balance ~ Rating+ Student)
summary(lmfit3)
#Adding student decreased our RSE and increased our R-squared, awesome, definitely going to add to our model. Now that we have the essentials, lets see how the remaining variables effect the simple linear model that models Balance. 


#Income
model = lm( Balance ~ Rating+ Student + Income)
print(sprintf('%s : %s' ,'Income', summary(model)$sigma))

#Married
model = lm( Balance ~ Rating+ Student + Married)
print(sprintf('%s : %s' ,'Married', summary(model)$sigma))

#Ethnicity
model = lm( Balance ~ Rating+ Student + Ethnicity)
print(sprintf('%s : %s' ,'Ethnicity', summary(model)$sigma))

#Cards
model = lm( Balance ~ Rating+ Student + Cards)
print(sprintf('%s : %s' ,'Cards', summary(model)$sigma))

#Age
model = lm( Balance ~ Rating+ Student + Age)
print(sprintf('%s : %s' ,'Age', summary(model)$sigma))

#Education
model = lm( Balance ~ Rating+ Education + Age)
print(sprintf('%s : %s' ,'Education', summary(model)$sigma))
#Income is certainly a must add. From here, we will use an anova table to explore the addition of any other variables. I'll form the anova table in order of the variables above that corresponded to the smallest residual standard error first. 




lmfit4 = lm( Balance ~ Rating+ Student + Income + Age + Ethnicity + Cards + Married + Education)
anova(lmfit4)
#None of the other variables after Income add significant information to the model according to the F-statistic.  


#Testing potential forms/degree polynomials to use in model
finalModel1 =lm( Balance ~ Rating+ Student + Income + Age)
summary(finalModel1)
finalModel2 =lm( Balance ~ I(Rating^2)+ Student + Income + Age)
summary(finalModel2)
pwrs = c(.4,.5,.6,.7,.8,.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)
#Other Quotient?
for (i in pwrs){
    #model = lm(Balance ~ poly(Rating, i))
    model = lm(Balance ~ I(Rating^i)+ Student + Income + Age)
    
    print(sprintf('%s : %s' ,i, summary(model)$sigma))
}

#Adding a second degree polynomial to the continuous variable Rating did not add much information at all. 1.0 also has the lowest residual sum of squares when testing all possible models with the Rating quotient ranging from 0.4-2.0. For sake of simplicity and interpretability, we will keep the degree equal to 1.0. 


#Now that we have a working model we are happy with, confirm its validity with residual plots
fits <- fitted(finalModel1)
## calculate the deviance residuals
dev.resids  <- resid(finalModel1)
plot(fits, dev.resids,
     xlab="fitted values", ylab="Pearson residuals" ,main = "Model Resids")
abline(h=0, lty=2)

#QQ to see if residuals follow normal
qqnorm(finalModel1$residuals)
qqline(finalModel1$residuals)



#The redisuals look good. They follow the normal qqline well, they experience the same variance across all fitted values (homoscedasticity), and they are centered about zero. This indicates a strong linear model that will predict credit balance accurately.
