library(NHANES)
data("NHANES")

NHANES_2 <- subset(NHANES, Age > 20 & Age <= 60 & !is.na(BMI) & !is.na(Testosterone) & Gender == "male")

range(NHANES_2$Age)
sum(is.na(NHANES_2$BMI))
sum(is.na(NHANES_2$Testosterone))
sum(NHANES_2$Gender == "female")

plot(NHANES_2$BMI, NHANES_2$Testosterone, xlab = "BMI (kg/m2)", ylab = "Testosterone (ng/dL)")
abline(lm(Testosterone ~ BMI, data = NHANES_2), col = "red")

linreg <- lm(Testosterone ~ BMI, data = NHANES_2)
summary(linreg)

plot(linreg)

