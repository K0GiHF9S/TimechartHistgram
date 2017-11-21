args <- commandArgs(trailingOnly = T)
times <- 1.0
legend <- c("A", "B", "C")
col <- c("#FF000050", "blue", "green")
density <- c(0, 10, 10)
title <- list(
  "1" = 1,
  "2" = 2,
  "3" = 3,
  "4" = 4,
  "5" = 5,
  "6" = 6
  )
width.bar <- list("act" = 
                    list(
                      "1" = 0.5,
                      "2" = 1,
                      "3" = 1
                    ),
                  "cycle" =
                    list(
                      "1" = 1,
                      "2" = 1,
                      "3" = 1
                    )
)
ymax <- list("act" = 
                    list(
                      "1" = 1.0,
                      "2" = 1.0,
                      "3" = 1.0
                      ),
                  "cycle" =
                    list(
                      "1" = 1.0,
                      "2" = 1.0,
                      "3" = 1.0
                    )
)

args <- sort(args)
source("./readBinFiles.R")

data.all <- readBinFiles(args, times)

source("./separate.R")

data.all <- lapply(data.all, function(data) {
  return(separate(data))
})

kind.all <- NULL
for(data.one in data.all){
  kind.all <- c(kind.all, names(data.one))
}
kind.all <- unique(kind.all)

act.min.list = vector("list" , 3)
names(act.min.list) <- kind.all
act.max.list =vector("list" , 3)
names(act.max.list) <- kind.all
cycle.min.list = vector("list" , 3)
names(cycle.min.list) <- kind.all
cycle.max.list = vector("list" , 3)
names(cycle.max.list) <- kind.all
for(data.one in data.all){
  for(kind in kind.all){
    act.min.list[[kind]] <- min(act.min.list[[kind]],data.one[[kind]]$act)
    act.max.list[[kind]] <- max(act.max.list[[kind]],data.one[[kind]]$act)
    cycle.min.list[[kind]] <- min(cycle.min.list[[kind]],data.one[[kind]]$cycle)
    cycle.max.list[[kind]] <- max(cycle.max.list[[kind]],data.one[[kind]]$cycle)
  }
}

min.list <- list(act = lapply(act.min.list, floor), cycle = lapply(cycle.min.list, floor))
max.list <- list(act = lapply(act.max.list, ceiling), cycle = lapply(cycle.max.list, ceiling))


for(data.kind in c("act", "cycle")){
  for(kind in kind.all){
    par(new = F)
    i <- 1
    legent.list <- as.vector(NULL)
    png(sprintf("%s%s.png", data.kind, kind), pointsize = 12, bg = "white", width = 600, height = 400, res = NA)
    par(las=1, family = "Ms Gothic", xaxs="i", yaxs="i", cex.main = 2, cex.lab = 1.2, cex.axis = 1.1)
    for(data.one in data.all){
      if(!is.null(data.one[[kind]][[data.kind]])){
        hist(data.one[[kind]][[data.kind]], freq = F, density=density[i], col=col[i], breaks = seq(min.list[[data.kind]][[kind]], max.list[[data.kind]][[kind]], width.bar[[data.kind]][[kind]]), xlab = "time", ylab = "Š„‡", xlim = c(min.list[[data.kind]][[kind]], max.list[[data.kind]][[kind]]),ylim = c(0, ymax[[data.kind]][[kind]]), main = title[[kind]])
        tmp.mean <- mean(data.one[[kind]][[data.kind]])
        abline(v = tmp.mean, col = col[i], lwd = 4)
        legent.list = c(legent.list, legend[i], sprintf("•½‹Ï:%d",round(tmp.mean)))
        par(new = T)
        i <- i + 1
      }
    }
    box()
    legend("topright",legend=legent.list, col=rep(col, each = 2), pch = rep(c(15, -1), 2), lty = rep(c(0, 1), 2))
    dev.off()
  }
}



