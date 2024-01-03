library(NHANES)
data("NHANES")

NHANES_Final <- subset(NHANES, !is.na(Marijuana) & !is.na(HardDrugs) & !is.na(RegularMarij))
sum(is.na(NHANES_Final$Marijuana))
sum(is.na(NHANES_Final$HardDrugs))
sum(is.na(NHANES_Final$RegularMarij))


NHANES_Final$Marijuana <- factor(NHANES_Final$Marijuana)
data.class(NHANES_Final$Marijuana)
NHANES_Final$Marijuana <- relevel(NHANES_Final$Marijuana, ref = "No")

NHANES_Final$HardDrugs <- factor(NHANES_Final$HardDrugs)
data.class(NHANES_Final$HardDrugs)
NHANES_Final$HardDrugs <- relevel(NHANES_Final$HardDrugs, ref = "No")

NHANES_Final$RegularMarij <- factor(NHANES_Final$RegularMarij)
data.class(NHANES_Final$RegularMarij)
NHANES_Final$RegularMarij <- relevel(NHANES_Final$RegularMarij, ref = "No")

NHANES_Final$Gender <- factor(NHANES_Final$Gender)
data.class(NHANES_Final$Gender)
NHANES_Final$Gender <- relevel(NHANES_Final$Gender, ref = "female")

table(NHANES_Final$HardDrugs, exclude = F)
table(NHANES_Final$Marijuana, exclude = F)
table(NHANES_Final$RegularMarij, exclude = F)
table(NHANES_Final$Gender, exclude = F)


Table_1 <- table(NHANES_Final$Marijuana, NHANES_Final$HardDrugs, exclude = F)
chi_results_1 <- chisq.test(Table_1)
chi_results_1$expected
chi_results_1

Table_2 <- table(NHANES_Final$RegularMarij, NHANES_Final$HardDrugs, exclude = F)
chi_results_2 <- chisq.test(Table_2)
chi_results_2$expected
chi_results_2

Table_3 <- table(NHANES_Final$Gender, NHANES_Final$HardDrugs, exclude = F)
chi_results_3 <- chisq.test(Table_3)
chi_results_3$expected
chi_results_3

model1 <- glm(HardDrugs ~ Marijuana, family = binomial (link = 'logit'), data = NHANES_Final)
summary(model1)

exp(cbind(coef(model1), confint(model1)))

library(ResourceSelection)

Predictions1 <- NHANES_Final[,c("ID", "Marijuana", "HardDrugs")]
Predictions1$HardDrugs01 <- ifelse(NHANES_Final$HardDrugs == "Yes", 1, 0)
Predictions1$predict <- fitted(model1)
View(Predictions1)

hoslem.test(Predictions1$HardDrugs01, Predictions1$predict)
pchisq(model1$null.deviance - model1$deviance, df = 1, lower.tail = F)

model2 <- glm(HardDrugs ~ Marijuana + RegularMarij + Gender, family = binomial (link = 'logit'), data = NHANES_Final)
summary(model2)

exp(cbind(coef(model2), confint(model2)))

Predictions2 <- NHANES_Final[,c("ID", "Marijuana", "RegularMarij", "Gender", "HardDrugs")]
Predictions2$HardDrugs01 <- ifelse(NHANES_Final$HardDrugs == "Yes", 1, 0)
Predictions2$predict <- fitted(model2)
View(Predictions2)

hoslem.test(Predictions2$HardDrugs01, Predictions2$predict)
pchisq(model2$null.deviance - model2$deviance, df = 3, lower.tail = F)

library(pROC)

roc2 <- roc(model2$y, model2$fitted, ci = TRUE, plot = TRUE, print.auc = TRUE)
roc2

