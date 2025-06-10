data <- read.csv("https://raw.githubusercontent.com/marcel0501/Esercizi-Mod-Stat/012d99d04de5c4284e97a2bb74de302d972f4fd0/airfare.csv")

m1 <- lm(fare ~ dist + passen + concen, data = data)
summary(m1)
m1$coefficients

corr <- cor(data)
corr

summary(data)

library(psych)
pairs.panels(data,lm=T)

library(corrplot)
par(mfrow=c(1,1))
corrplot(corr)

