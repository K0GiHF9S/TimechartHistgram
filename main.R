args <- commandArgs(trailingOnly = T)

source("./separate.R")

data.all <- lapply(args, function(arg) {
  return(separate(read.csv(arg,header = T)))
})

len <- 100

makeSampleData <- function(meanRun, meanCycle, sd){
  run = rnorm(len, mean =  meanRun, sd = sd)
  cycle = rnorm(len, mean =  meanCycle, sd = sd)
  return(data.frame(RUN=run, CYCLE=cycle))
}

png("image.png", pointsize = 12, bg = "white", width = 600,height = 400,res = NA)

par(las=2, family = "Ms Gothic", xaxs="i", yaxs="i", cex.main = 2, cex.lab = 1.2, cex.axis = 1.1)
x1 <- makeSampleData(meanRun = 10, meanCycle = 1000, 1)
hist(x1$RUN, breaks = seq(6, 14, 1), freq = F, xlab = "time", ylab = "•p“x", ylim = c(0, 0.50), main = "A", col="#FF000050")
abline(v = mean(x1$RUN), col = "#FF000050", lwd = 4)
x1 <- makeSampleData(meanRun = 10, meanCycle = 1000, 1)
hist(x1$RUN, freq = F, add = T, density=10, col = "blue")
abline(v = mean(x1$RUN), col = "blue", lwd = 4)
x1 <- makeSampleData(meanRun = 10, meanCycle = 1000, 1)
hist(x1$RUN, freq = F, add = T, density=10, angle = -30, col = "green")
abline(v = mean(x1$RUN), col = "green", lwd = 4)
legend("topright", legend=c("A", "B", "C", "A", "B", "C"), col=rep(c("#FF000050", "blue", "green"), 2), pch = c(15, 15, 15, -1, -1, -1), lty = c(0, 0, 0, 1, 1, 1))

dev.off()

#x2 <- makeSampleData(meanRun = 50, meanCycle = 3000, 1)
#x3 <- makeSampleData(meanRun = 30, meanCycle = 2000, 1)



