euclideanDistance <- function(u, v)
{
	sqrt(sum((u - v)^2))
}
 
sortObjectsByDist <- function(xl, z, metricFunction =
euclideanDistance)
{
	l <- dim(xl)[1]
	n <- dim(xl)[2] - 1
	distances <- matrix(NA, l, 2)
	for (i in 1:l)
	{
		distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
	}

	orderedXl <- xl[order(distances[, 2]), ]
	return (orderedXl);
}

kNN <- function(xl, z, k)
{
	orderedXl <- sortObjectsByDist(xl, z)
	n <- dim(orderedXl)[2] - 1
	classes <- orderedXl[1:k, n + 1]
	counts <- table(classes)
	class <- names(which.max(counts))
	##cat(class," ")
	return (class)
}

kwNN <- function(xl, z, k,q)
{
	 m <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
	xl <- sortObjectsByDist(xl, z)
	n <- dim(xl)[2] - 1
	classes <- xl[1:k, n + 1]
	for(i in 1:k)
	{
		w<-q ^ i
		m[classes[i]]<-m[classes[i]]+w
	}
	class <- names(which.max(m))
	return (class)
}



colors <- c("setosa" = "red", "versicolor" = "green3",
"virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
z <- c(4.5, 1.8) 
xl <- iris[, 3:5] 
class <- kwNN(xl, z, k=10,2)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)
