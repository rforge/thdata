
data("geyser", package = "MASS")
library("survival")

f <- do.call("interaction", 
        lapply(c(2, 3, 4), function(x) geyser$duration == x))[, drop = TRUE]
levels(f) <- c("exact", "short", "medium", "long")
geyser$f <- f
tapply(geyser$duration, geyser$f, summary)

left <- 2
right <- 4
geyser$cduration <- cbind(left = geyser$duration, right = geyser$duration)
geyser$cduration[geyser$f == "short","left"] <- 0
geyser$cduration[geyser$f == "short","right"] <- left
geyser$cduration[geyser$f == "medium","left"] <- left
geyser$cduration[geyser$f == "medium","right"] <- right
geyser$cduration[geyser$f == "long","left"] <- right
geyser$cduration[geyser$f == "long","right"] <- Inf

geyser$duration <- with(geyser, Surv(time = cduration[,"left"], 
                                     time2 = cduration[, "right"], 
                                     type = "interval2"))
rm(f)
rm(left)
rm(right)
geyser$cduration <- NULL
geyser$f <- NULL
