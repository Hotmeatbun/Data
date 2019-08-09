money<- read.csv("money_vs_payload.txt",header = TRUE, sep = ',')
attach(money)
colnames(money)
str(money)
#handle missing value, since only one obs, we just exclude it.
money<- na.exclude(money)
#density function of paid_amount
hist((money$paid_amount), probability = TRUE)
#linear model we build
model <- lm(paid_amount~square_footage+number_of_bedrooms, data = money)
summary(model)
full <- lm(paid_amount~square_footage+number_of_bedrooms+as.factor(suburb))
#stepAIC
library(MASS)
step <- stepAIC(full, direction = "backward")
#prediction
plot(fitted.values(model), money$paid_amount, col="blue")
#residual plot     
residual<-money$paid_amount-fitted.values(model)
par(mfrow=c(1,2))
hist(residual) 
#boxplot of paid_amount~number of bedrooms
boxplot(paid_amount~number_of_bedrooms)


