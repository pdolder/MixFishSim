

library(MixFishSim)

closed_areas <- matrix(round(runif(100,1,100),0), nc = 2)
colnames(closed_areas) <- c("x", "y")

new.point<-c(10,34)


fun1 <- function () {
	cl <- mapply(x1 = closed_areas[,"x"],
       y1 = closed_areas[,"y"],
       FUN = function(x1,y1) {
	as.integer(x1) == as.integer(new.point[1]) & 
	as.integer(y1) == as.integer(new.point[2])})

	# If new.point is in closure areas, repick, else break
	Closure <- ifelse(any(cl), TRUE, FALSE) 

}

fun2 <- function () {
	paste(new.point, collapse="") %in% paste(closed_areas[,"x"],
					 closed_areas[,"y"], sep="")

}

fun3 <- function (variables) {
	as.integer(paste(new.point, collapse="")) %in% paste(as.integer(closed_areas[,"x"]),
					 as.integer(closed_areas[,"y"]), sep="")
}


library(microbenchmark)


microbenchmark("mapply" = fun1,
	       "straight" = fun2,
	       "with as integer" = fun3,
	       times = 1000)
