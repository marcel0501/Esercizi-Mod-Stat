best_consume <- which(data$km_per_liter ==max(data$km_per_liter))
best_consume

minor_consumo <- data[best_consume, ]
minor_consumo

numeric <- unlist(lapply(data, is.numeric))
Corr <- round(cor(data[,numeric]), 3)
Corr

pairs.panels(data[,numeric],lm=T)
par(mfrow=c(1,1))

corrplot(Corr)

summary(data)
m1 =lm(price~km_per_liter+weight_kg,data)
summary(m1)
ls(m1)
m1.summary <- summary(m1)
ls(m1.summary)

m1.summary$adj.r.squared
m1.summary$sigma

st_obs <- data[1, ]
obs = st_obs$price
m1.pred <- predict(m1, newdata = st_obs)
residuo <- obs - m1.pred
cat("Observed Value", obs, "\n",
    "Predicted Value", m1.pred, "\n",
    "Residual", residuo, "\n")

library(scatterplot3d)
par(mfrow=c(1,1))
grafico3d = scatterplot3d(data$km_per_liter,data$weight_kg,data$price,
                          pch=20, highlight.3d = TRUE, type = "h",angle = 45,
                          xlab = "km_per_liter",ylab="Weight", zlab="Price"
                          )

grafico3d$plane3d(m1,col="pink")

summary(m1$residuals)
par(mfrow=c(1,3))
plot(m1$fitted.values, m1$residuals, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values",
      col = "lightblue"
     )
abline(h=0, col="purple")
plot(data$km_per_liter, m1$residuals, 
     xlab = "Km", ylab = "Residuals",
     main = "KM vs Index",
      col = "lightblue"
     )
abline(h=0, col="purple")
plot(data$weight_kg, m1$residuals, 
     xlab = "Weight", ylab = "Residuals",
     main = "Weight vs Index",
      col = "lightblue"
     )
abline(h=0, col="purple")

par(mfrow=c(1,1))
smoothScatter(m1$fitted.values, m1$residuals, 
              ,nrpoints = 1)
plot(m1,which=1)

qqnorm(m1$residuals,main="QQ Plot of Residuals",
       xlab="Theoretical Quantiles", ylab="Sample Quantiles",
     col="lightblue", pch=19)
qqline(m1$residuals)
plot(m1,which=2)
summary(hatvalues(m1))

#Residui Standardizzati
resid_stand <- rstandard(m1)
par(mfrow=c(1,1))
qqnorm(resid_stand, main="QQ Plot of Standardized Residuals",
       xlab="Theoretical Quantilesss", ylab="Sample Quantiles",
       col="lightblue", pch=19)
qqline(resid_stand)
plot(m1,which=2)
summary(hatvalues(m1))


length(m1$coefficients)/nrow(data)
par(mfrow=c(2,2))

plot(hatvalues(m1), ylab="Leverage", xlab="Indice", type="h" )
abline(h=2*mean(hatvalues(m1)))

plot(x=hatvalues(m1), y=rstudent(m1), ylab="R Student", xlab="Leverage" )
abline(v=2*mean(hatvalues(m1)))
abline(h=c(-2,2))

plot(cooks.distance(m1), ylab="Cook's D", xlab="Indice", type="h" )
abline(h=4/(nrow(data)-length(m1$coefficients)  ))

which( hatvalues(m1)>  2*mean(hatvalues(m1)))
which( abs(rstudent(m1))>2   )

which(cooks.distance(m1)> 4/(nrow(data)-length(m1$coefficients) ))
