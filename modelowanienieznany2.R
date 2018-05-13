library(readr)
library(ggplot2)
library(MASS)
library(stats)
library(lmtest)
library(car)
library(RCurl)

dane <- read.csv(text=getURL("https://raw.githubusercontent.com/sonjawap/final/master/Kaggle.csv"), row.names = 1)
dane
#####################Zadanie####################

zmienna1 <- dane$Human.Development.Index.HDI.2014
zmienna2 <- dane$Carbon.dioxide.emissions.per.capita.2011.Tones

layout(matrix(c(1,2,3,4),1,1))  
plot(zmienna1)
summary(zmienna1)
m <- mean(zmienna1)
std <- sqrt(var(zmienna1))
hist(zmienna1, freq=F)

ggplot(dane, aes(x=zmienna2, y=zmienna1)) +   geom_point(shape=4) +  geom_smooth(method=lm)

##############korelacja
cor(zmienna2, zmienna1) # wspolczynnik korelacji

#############model regresji liniowej
model1 <- lm(zmienna1~zmienna2)
summary(model1)
coef(model1)

###############
model1_parametry <- coefficients(model1) # model coefficients parametry modelu
y_teoretyczne <- fitted(model1) # wartosci teoretyczne - predicted values
model1_reszty <- resid(model1) #reszty modelu

#########Anscombe - dlaczego testujemy modele ##############

summary(anscombe)
ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  mods[[i]] <- lmi <- lm(ff, data = anscombe)
  print(anova(lmi))
}

op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
       xlim = c(3, 19), ylim = c(3, 13))
  abline(mods[[i]], col = "blue")
}
mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
par(op)


################# Testy ######################

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # 4 graphs/page 
plot(model1)

####################testy######################
# niezaleznosc / autokorelacja reszt, test Durbina-Watsona H0: brak autokorelacji
dwtest(model1)
# normalnosc rozkladu reszt
layout(matrix(c(1,2,3,4),1,1))
hist(model1_reszty, freq=F)
curve(dnorm(x, mean=mean(model1_reszty), sd=sqrt(var(model1_reszty))), add=TRUE)
shapiro.test(model1_reszty) # H0:rozklad normalny
# heteroskedastycznosc - jednorodnosc wariancji reszt
bptest(model1) #H0:homoskedastycznosc - czy wariancja zalezy od zmiennej objasnianej
# Miara cooka wykres
influencePlot(model1,	id.method="identify", main="Influence Plot") #miara Cooka
# test specyfikacji RESET
resettest(model1) #H0:poprawna postac funkcyjna