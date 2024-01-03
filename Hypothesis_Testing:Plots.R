library(NHANES)
library(FSA)

data("NHANES")

NHANES_2 <- subset(NHANES, Age >= 18 & !is.na(BMI) & !is.na(PhysActiveDays) & !is.na(Diabetes))

min(NHANES$Age)
min(NHANES_2$Age)

sum(is.na(NHANES$BMI))
sum(is.na(NHANES$PhysActiveDays))
sum(is.na(NHANES$Diabetes))
sum(is.na(NHANES_2$BMI))
sum(is.na(NHANES_2$PhysActiveDays))
sum(is.na(NHANES_2$Diabetes))

table(NHANES_2$Diabetes, exclude = F)

mean(NHANES_2$BMI)
sd(NHANES_2$BMI)
tapply(NHANES_2$BMI, NHANES_2$Diabetes, mean)
tapply(NHANES_2$BMI, NHANES_2$Diabetes, sd)

median(NHANES_2$PhysActiveDays)
tapply(NHANES_2$PhysActiveDays, NHANES_2$Diabetes, median)

qplot(NHANES_2$BMI, geom = "histogram", binwidth = 1, xlab = "BMI (kg/m^2)", fill = I("blue"), color = I("red"))
t.test(BMI ~ Diabetes, data = NHANES_2, var.equal = TRUE)

boxplot(PhysActiveDays ~ Diabetes, data = NHANES_2)
wilcox.test(PhysActiveDays ~ Diabetes, data = NHANES_2)

NHANES_3 <- subset(NHANES, Age >= 18 & !is.na(BMI) & !is.na(TotChol) & !is.na(UrineVol1))

min(NHANES$Age)
min(NHANES_3$Age)

sum(is.na(NHANES$BMI))
sum(is.na(NHANES$TotChol))
sum(is.na(NHANES$UrineVol1))
sum(is.na(NHANES_3$BMI))
sum(is.na(NHANES_3$TotChol))
sum(is.na(NHANES_3$UrineVol1))

NHANES_3$BMIcat <- ifelse(NHANES_3$BMI < 18.5, "Underweight", 
                        ifelse(NHANES_3$BMI >= 18.5 & NHANES_3$BMI < 25, "Healthy", 
                               ifelse(NHANES_3$BMI >=25 & NHANES_3$BMI < 30, "Overweight",
                                      ifelse(NHANES_3$BMI >= 30, "Obese", NA))))

NHANES_3$BMIcat <- factor(NHANES_3$BMIcat)
data.class(NHANES_3$BMIcat)
table(NHANES_3$BMIcat, exclude = F)

mean(NHANES_3$TotChol)
sd(NHANES_3$TotChol)
tapply(NHANES_3$TotChol, NHANES_3$BMIcat, mean)
tapply(NHANES_3$TotChol, NHANES_3$BMIcat, sd)

fit <- aov(TotChol ~ BMIcat, data = NHANES_3)
summary(fit)
TukeyHSD(fit)

tapply(NHANES_3$UrineVol1, NHANES_3$BMIcat, quantile)

kruskal.test(UrineVol1 ~ BMIcat, data = NHANES_3)
dunnTest(UrineVol1 ~ BMIcat, data = NHANES_3)

sum(is.na(NHANES_3$PhysActive))
NHANES.table <- table(NHANES_3$PhysActive, NHANES_3$BMIcat, exclude = F)
NHANES.table

chisq.test(NHANES.table)
