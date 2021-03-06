euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

c1 <- function(r) 
{
  if (abs(r) > 1) 
  {
    return (0)
  }
  return ((3/4) * (1 - r*r))
}
# Квартическое
c2 <- function(r) 
{
  if (abs(r) > 1)
  {
    return (0)
  }
  return ((15/16) * (1 - r*r)^2)
}
# Треугольное
c3 <- function(r) 
{
  if (abs(r) > 1) 
  {
    return (0)
  }
  return (1 - abs(r))
}
# Гауссовское
c4 <- function(r) 
{
  (2*pi)^0.5 * exp(-0.5 * r*r)
}
# Прямоугольное
c5 <- function(r) 
{
  if (abs(r) > 1) 
  {
    return (0)
  }
  return (0.5)
}

sortObjectsByDist <- function(xl, z, metricFunction =
                                euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- matrix(NA, l, 2)
  
  for (i in 1:l)
  {
    distances[i, ] <- c( metricFunction(xl[i, 1:n], z),xl[i,3])
  }
  return (distances);
}



parsen <- function(xl, z, h,K)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- xl
  m <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  classes <- xl[1:l, n + 1]
  for(i in 1:l)
  {
    w<-K(distances[i,1]/h)
    m[classes[i]]<-m[classes[i]]+w
  }
  if(m[1]!=0 || m[2]!=0 || m[3]!=0)class <- names(which.max(m))
  else class<-"not_class"
  if(class==0)class<-"grey"
  return (class)
}


plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)

colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue","not_class" = "grey")
xl <- iris[, 3:5]
k <-150
LOO<-rep(0,k)

segment <- seq(from = 0, to = 1, by = 0.05)

n<-150
K<-seq(0,3,length.out = 61)
accuracy<-c(0,k)
for(i in 1:n)
{
  z<-c(xl[i,1],xl[i,2])
  x<-sortObjectsByDist(xl[, 1:3][-i,], z)
  
  for(j in 1:61)
  {
    class<-parsen(x,z,K[j],c1)
    if(class != xl[i,3] )LOO[j]<-LOO[j]+1
    points(z[1], z[2], pch = 22, bg = colors[class], asp = 1) 
  }
}
for (i in 1:61)
{
  accuracy[i]<-LOO[i]/n
}

plot(K,accuracy,type = "l")
