library(lubridate)
library(rhr)
library(adehabitat)



## 
n <- 10000  # 8 h, each min one point
x <- cumsum(runif(n, -10, 10))
y <- cumsum(runif(n, -10, 10))

d <- ymd("20120101") + minutes(1:n)
id <- sample(1:32, length(d), T)

dat <- data.frame(x=x, y=y, date=d)
res <- rhrSchoener.intervals(dat, 300, 10, TRUE)

secs <- c(60, 60^2, 60^3, 60^3*24, 60^3*24*7)
names.unit <- c("sec", "min", "hour", "day", "week")
time <- res$sampling
which.unit <- which(trunc(time[1] / secs) == 0)[1]
time <- time / secs[which.unit]
plot(time, res$schoener, xlab=paste0("Sampling interval [", names.unit[which.unit], "]"))

# with adehabitat
data(puechcirc)
puechcirc <- ltraj2traj(puechcirc)
puechcirc$date[1:10]
     
schoener(puechcirc, keep = c(5*60, 15*60))

# with rhr

dat <- data.frame(puechcirc[, c("burst", "x", "y", "date")])
dat <- dat[complete.cases(dat),]

sapply(split(dat[, c("x", "y", "date")], dat$burst), function(x) rhrSchoener(x, c(5*60, 15*60), TRUE))

