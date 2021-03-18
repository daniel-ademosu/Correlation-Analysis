pdf("visualization.pdf")
library(readr)
FantasyPros_Fantasy_Basketball_Overall_2019_Projections <- read_csv("FantasyPros_Fantasy_Basketball_Overall_2019_Projections.csv")
View(FantasyPros_Fantasy_Basketball_Overall_2019_Projections)
x <-FantasyPros_Fantasy_Basketball_Overall_2019_Projections$MIN
y <-FantasyPros_Fantasy_Basketball_Overall_2019_Projections$PTS
plot(x, y, las=1, xlab = "Time played (minutes)" , ylab = "Number of points gained" , main = "Timed played in minutes vs Number of points gained \nat the 2019-2020 season of the NBA" , pch =20 , frame =T)
model <- lm(y ~ x,data =FantasyPros_Fantasy_Basketball_Overall_2019_Projections)
abline(model,col ="red")
cor(x,y,use ="pairwise.complete.obs",)
histogram <-hist(y,10, main = "Distribution of number of points gained\nat the 2019-2020 season of the NBA",xlab = "Number of points" ,ylab = "frequency", col="blue")
x1 <- seq(0, 2600, 1)
mn <- mean(y)
print(mn)
y1 <-dnorm(x1,mean = mn,sd=sd(y))
box.size<-diff(histogram$mids[1:2]) *length(y)
y1<-y1 *box.size
lines(x1, y1, col="red")
dev.off()