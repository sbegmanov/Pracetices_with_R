library(ggplot2)
data("midwest")
library(mosaic)
library(lattice)
View(midwest)
attach(midwest)

mw <- midwest
state = factor(state)
inmetro = factor(inmetro)
t =  table(state, inmetro)
print(t)
barplot(t)

plotCumfreq(~poptotal, options(scipen = 1))
hist(area)
density(area)
mean(poptotal)
weighted.mean(poptotal, percblack)
median(poptotal)
sd(poptotal)
IQR(poptotal)

ggplot(mw, aes(popasian, poptotal)) +
  geom_point() +
  xlim(0, 1000)

mean(poptotal[inmetro == 1] > mean(poptotal))

par(mfrow = c(1, 2))
boxplot(poptotal[inmetro == 1])
boxplot(poptotal[inmetro == 0])
quantile(poptotal)
qqplot(poptotal, popblack)
pie(popadults[inmetro == 1], popadults[inmetro == 0])
plot(poptotal, popblack)


data("faithful")
attach(faithful)
plotCumfreq(~eruptions)

table = table(poptotal, inmetro)
print(table)
prop.table(table)
addmargins(table)

###############################################################################
sample.space <- c(0, 1)
theta <- 0.5
N <- 1000
flips <- sample(sample.space,
                size = N,
                replace = TRUE,
                prob = c(theta, 1 - theta))
print(flips)

sum(flips != 1) / N
#######################################################################

n = 5000
day_n = 1:365
ans = rep(NA, 25)

for(k in 1:25){
  count = 0
  for ( i in 1:n ){
    class = sample(day_n, k, replace = TRUE)
    if( length( unique(class) ) < length(class) ){
      count = count + 1
    }
    ans[k] = count / n
    
  }

}

print(ans)


plot(1:25, ans, type = 'b', col = "blue", lwd = 2, xlab = "Number of pupil",
     ylab = "Probability", main = "Birthday problem")
abline(h = 0.5, col = "red", lty = 2)














