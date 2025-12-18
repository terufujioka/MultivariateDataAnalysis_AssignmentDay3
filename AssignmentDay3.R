library(lme4)
library(ggplot2)
library(performance)

data("sleepstudy")

ggplot(sleepstudy, aes(x = Days + 1, y = Reaction, group = Subject, colour = Subject)) +
  geom_line(alpha = 0.7) +
  geom_point(size = 1) +
  theme_minimal() +
  labs(title = "Spaghetti Plot of Reaction Time over Days",
       y = "Reaction Time (ms)", 
       x = "Days of Sleep Deprivation",
       color = "Subject ID") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  guides(color = "none") +
  scale_color_viridis_d(option = "mako", begin = 0, end = 0.8)

#ランダム切片モデル
model_intercept <- lmer(Reaction ~ 1 + Days + (1 | Subject), data = sleepstudy)
summary(model_intercept)
#icc(model_intercept)

#ランダム傾きモデル
model_slope <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), data = sleepstudy)
summary(model_slope)

#モデル比較
anova(model_intercept, model_slope, refit = FALSE)
