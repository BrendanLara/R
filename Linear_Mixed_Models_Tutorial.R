# Followed this tutorial (not my own code)
# https://ourcodingclub.github.io/tutorials/mixed-models/


# explore data

load("dragons.RData")
head(dragons)

hist(dragons$testScore)

dragons$bodyLength2 <- scale(dragons$bodyLength, center = TRUE, scale = TRUE)

# fit linear model to data

basic.lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(basic.lm)

library(tidyverse)

(prelim_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore)) +
    geom_point() +
    geom_smooth(method = "lm"))

plot(basic.lm, which = 1)
plot(basic.lm, which = 2)
boxplot(testScore ~ mountainRange, data = dragons)

(colour_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))

# multiple linear models

(split_plot <- ggplot(aes(bodyLength, testScore), data = dragons) + 
    geom_point() + 
    facet_wrap(~ mountainRange) + 
    xlab("length") + 
    ylab("test score"))

mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)

# mixed effects models

library(lme4)

# first mixed model

mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)

plot(mixed.lmer)

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))

# second mixed model

dragons <- within(dragons, sample <- factor(mountainRange:site))

mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)
summary(mixed.WRONG)

mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)
summary(mixed.lmer2)

(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +  
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), size = 1) + 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))
)

# random slopes

mixed.ranslope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange/site), data = dragons) 

summary(mixed.ranslope)

(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.ranslope)), aes(y = pred), size = 1) + 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))
)

# presenting results

library(ggeffects)

pred.mm <- ggpredict(mixed.lmer2, terms = c("bodyLength2"))

(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +
    geom_point(data = dragons,
               aes(x = bodyLength2, y = testScore, colour = mountainRange)) + 
    labs(x = "Body Length (indexed)", y = "Test Score", 
         title = "Body length does not affect intelligence in dragons") + 
    theme_minimal()
)

ggpredict(mixed.lmer2, terms = c("bodyLength2", "mountainRange"), type = "re") %>% 
  plot() +
  labs(x = "Body Length", y = "Test Score", title = "Effect of body size on intelligence in dragons") + 
  theme_minimal()

library(sjPlot)

(re.effects <- plot_model(mixed.ranslope, type = "re", show.values = TRUE))
summary(mixed.ranslope)

library(stargazer)

stargazer(mixed.lmer2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

full.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), 
                  data = dragons, REML = FALSE)
reduced.lmer <- lmer(testScore ~ 1 + (1|mountainRange) + (1|sample), 
                     data = dragons, REML = FALSE)

anova(reduced.lmer, full.lmer)

