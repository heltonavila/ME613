## cars
head(cars)

## speed: velocidade veicular em mph
## dist: distancia de frenagem em 'ft'
library(tidyverse)
cars = cars %>% as_tibble %>% 
  mutate(speed = speed*1.6,
         dist = dist*0.3048)

cars %>% ggplot(aes(x=speed, y=dist)) +
  geom_point() + theme_bw() +
  xlab("Velocidade (km/h)") +
  ylab("Distância Frenagem (m)") +
  ggtitle("Relação entre Velocidade e Distância de Frenagem")

## sum ((xi-xbarra)*(yi-ybarra))
Sxy = with(cars, sum((dist-mean(dist))*(speed-mean(speed))))
Sxx = with(cars, sum((speed-mean(speed))^2))
b1 = Sxy/Sxx
b0 = with(cars, mean(dist)-b1*mean(speed))
## b0 + b1*speed

y = with(cars, dist-mean(dist))
x = with(cars, speed - mean(speed))
Sxy_cent = sum(x*y)
Sxx_cent = sum(x^2)
b1_cent = Sxy_cent/Sxx_cent
b0_cent = mean(y)-b1*mean(x)
cars %>% ggplot(aes(x=speed-mean(speed),
                    y=dist-mean(dist))) +
  geom_point() + theme_bw() +
  xlab("Velocidade (km/h)") +
  ylab("Distância Frenagem (m)") +
  ggtitle("Relação entre Velocidade e Distância de Frenagem")
## b0_cent + b1_cent*speed_cent
## 0.75*speed_cent

## y: metros
## mean(y): metros
## var(y): metros^2
## sd(y): metros
y_pad = (cars$dist-mean(cars$dist))/sd(cars$dist)
x_pad = with(cars, (speed-mean(speed))/sd(speed))
Sxy_pad = sum((x_pad-mean(x_pad))*(y_pad-mean(y_pad)))
Sxx_pad = sum((x_pad-mean(x_pad))^2)
b1_pad = Sxy_pad/Sxx_pad
b0_pad = mean(y_pad) - b1_pad*mean(x_pad)
with(cars, cor(speed, dist))
## 0.81*x_pad

fit = lm(dist ~ speed, data=cars)
summary(fit)

cars2 = cars %>% mutate(dist=(dist-mean(dist))/sd(dist),
                        speed=(speed-mean(speed))/sd(speed))
summary(lm(dist ~ speed, data=cars2))

###
b0
b1
y_chapeu = b0+b1*cars$speed
plot(cars$speed, cars$dist)
lines(cars$speed, y_chapeu, col=2, lwd=2)
cars$dist - y_chapeu
cars = cars %>%
  mutate(y_chapeu = b0 + b1*speed,
         rsd = dist - y_chapeu)

cars %>% 
  ggplot(aes(speed, dist)) +
  geom_point() +
  geom_abline(slope=b1, intercept=b0,
              size=2, colour="red") +
  theme_bw() +
  xlab("Velocidade (km/h)") +
  ylab("Distância de Frenagem (m)") +
  ggtitle("Regressão Frenagem x Velocidade")

cars %>% 
  ggplot(aes(speed, rsd)) +
  geom_point() +
  theme_bw() +
  xlab("Velocidade (km/h)") +
  ylab("Resíduo") +
  ggtitle("Resíduo Regressão") +
  geom_hline(yintercept = 0, colour="red")

library(car)
fit = lm(dist ~ speed, data=cars)
ncvTest(fit)
### H0: variância é constante 

summary(fit)
EQM = sum(cars$rsd^2)/(nrow(cars)-2)
## EQM estima a variancia sigma^2

cars
SQR = sum((cars$y_chapeu-mean(cars$dist))^2) ## 1
SQE = sum(cars$rsd^2) ## n-2
SQT = sum((cars$dist-mean(cars$dist))^2) ## n-1
R2 = SQR/SQT
QMR = SQR/1
QME = SQE/48
test_F = QMR/QME

var_b1 = QME/sum((cars$speed-mean(cars$speed))^2)
## (b1_chapeu - valor_h0)/dp(b1_chapeu)
## H0: b1 = 0
## H1: b1 <> 0
T_obs = (b1-0)/sqrt(var_b1)
## Prob(T > 9.46) + Prob(T < -9.46)
pt(abs(T_obs), df = 48, lower.tail = FALSE) +
  pt(-abs(T_obs), df = 48, lower.tail = TRUE)
2*pt(abs(T_obs), df = 48, lower.tail = FALSE)
