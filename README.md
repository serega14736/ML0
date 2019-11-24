# СМПР #

## 1. Метод 1NN ##
Метод ближайших соседей (1NN). Имеется 3 класса объектов, дан некоторый объект, который необходимо классифицировать , т.е. определить , к какому классу он принадлежит.

Классифицируемый объект относим к тому классу, к которому принадлежит ближайший по заданной метрике "сосед" из выборки:

![w](https://latex.codecogs.com/gif.latex?w%28i%2C%20u%29%20%3D%20%5Bi%20%3D%201%5D%3B)

В реализованном методе выбрана евклидова метрика.

В качестве выборки был взят набор "Ирисы Фишера" 

Карта классификации выглядит следующим образом:  

![1NN](https://github.com/serega14736/ML0/blob/master/img/karta1NN.png)

## Пример 

Рассмотрим выборку "Ирисы Фишера" и некую точку z(1.9,1) 

Применим метод 1NN и получим , что z принадлежит классу "красных кружочков" 

![1NN](https://github.com/serega14736/ML0/blob/master/img/1nn1.png)


### **Преимущества:** 

1. Простота реализации.

2. При *k*, подобранном около оптимального, алгоритм "неплохо" классифицирует.

## **Недостатки** 

1.Неустойчивость к погрешностям.

2.Отсутствие параметров, которые можно было бы настраивать по выборке. Алгоритм полностью зависит от того, насколько удачно выбрана метрика.

3.Низкое качество классификации.

## 2. KNN. Метод k-ближайших соседей: ##

Алгоритм k ближайших соседей - kNN относит объект u к тому классу элементов которого больше среди k ближайших соседей ![equation](http://latex.codecogs.com/gif.latex?x_u^{i},&space;i=1,...,k:)

![equation](http://latex.codecogs.com/gif.latex?w(i,&space;u)&space;=&space;[i&space;\leq&space;k];&space;a(u;&space;X^l,&space;k)&space;=&space;argmax_{y\epsilon&space;Y}&space;\sum^k_{i&space;=&space;1}{[y^i_{u}&space;=&space;y]})

где k -параметр

## Реализация kNN функции ##

``` R
kNN <- function(xl, z, k)
{
	orderedXl <- sortObjectsByDist(xl, z)
	n <- dim(orderedXl)[2] - 1
	classes <- orderedXl[1:k, n + 1]
	counts <- table(classes)
	class <- names(which.max(counts))
	return (class)
}
```

## Пример ##

Рассмотрим точку Z(3.5, 1.5) на выборке "Ирисы Фишера". Применим метод k-ближайших соседей и получим , что Z принадлежит к классу "зеленых кружочков".

![1NN](https://github.com/serega14736/ML0/blob/master/img/KNN1.png)

## Карта классификации метода k-ближайших соседей ##

![1NN](https://github.com/serega14736/ML0/blob/master/img/kartaKNN.png)

## Выберем оптимальное k, воспользовавшись критерием скользящего контроля LOO ##

![1NN](https://github.com/serega14736/ML0/blob/master/img/LOOKNN1.png)

Таким образом оптимальное k=6, а LOO=0.333

## 3. kwNN. Метод k-взвешеных ближайших соседей: ##

Метод k-взвешеных ближайших соседей(kwNN). По сравнению с kNN, kwNN принимает во внимание не только колличество соседей определенного класса но и удаленность от классифицируемого обьекта. Для каждого класса определяется оценка близости, у какого класса больше оценка близости тот класс и присваивается классифицируемому обьекту.

Формула алгоритма kwNN:

<a href="https://www.codecogs.com/eqnedit.php?latex=w(i,u)&space;=&space;[i\leqslant&space;k]w(i)\&space;a(u,x^{l},k)&space;=&space;arg\max\limits_{y\in&space;Y}\sum\limits_{i=1}^{k}[y^i_u&space;=&space;y]w(i)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w(i,u)&space;=&space;[i\leqslant&space;k]w(i)\&space;a(u,x^{l},k)&space;=&space;arg\max\limits_{y\in&space;Y}\sum\limits_{i=1}^{k}[y^i_u&space;=&space;y]w(i)" title="w(i,u) = [i\leqslant k]w(i)\ a(u,x^{l},k) = arg\max\limits_{y\in Y}\sum\limits_{i=1}^{k}[y^i_u = y]w(i)" /></a>



Реализация kwNN фунции

``` R
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
```
## Пример ## 

Рассмотрим точку Z(4.5, 1.8) на выборке "Ирисы Фишера". Применим метод kwNN и получим , что Z принадлежит к классу "зеленых кружочков".

![1NN](https://github.com/serega14736/ML0/blob/master/img/KwNN2.png)

## Карта классификации метода k-взвешеных ближайших соседей ## 

![1NN](https://github.com/serega14736/ML0/blob/master/img/kartaKWNN.png)

## LOO KwNN ##

![1NN](https://github.com/serega14736/ML0/blob/master/img/LOO_KwNN.png)

Получается q=1 LOO=0.0333.

## Преимущество метода kwNN над kNN ##

Метод KNN

![1NN](https://github.com/serega14736/ML0/blob/master/img/kNNprimer.png)

Метод KwNN

![1NN](https://github.com/serega14736/ML0/blob/master/img/kwNNprimer.png)

## 4. Метод Парзеновского окна ##

Рассмотрим весовую функцию w(i,u) как функцию не от ранга соседа, а как функцию от расстояния 
![equation](http://latex.codecogs.com/gif.latex?\rho(u,x_u^i):)

![equation](http://latex.codecogs.com/gif.latex?w(i,u)&space;=&space;K(\frac{1}{h}&space;\rho&space;(u,x_u^i)))

где K(z) - невозрастающая на ![equation](http://latex.codecogs.com/gif.latex?[0,\infty&space;]) (гипотеза комактности) функция яда. В этом случае метричесикй классификатор примет следующий вид:

![equation](http://latex.codecogs.com/gif.latex?a(u;X^l,h)&space;=&space;argmax_{y\epsilon&space;Y}&space;\sum_{i:y_u^i=y}{K(\frac{\rho(u,x_u^i)}{h})})

Этот алгоритм - алогритм парзеновского окна. h - ширина окна.

Формулы ядер:

![1NN](https://github.com/serega14736/ML0/blob/master/img/ParsenFormul.png)

## Ядро Епанечникова ##

![1NN](https://github.com/serega14736/ML0/blob/master/img/LOO_panechnikov.png)

h=0.35 LOO=0.04

![1NN](https://github.com/serega14736/ML0/blob/master/img/Karta_Epanechnikov.png)

Реальзация фунцкии:

``` R
c1 <- function(r) 
{
  if (abs(r) > 1) 
  {
    return (0)
  }
  return ((3/4) * (1 - r*r))
}
```

## Ядро Квадратическое ##

![1NN](https://github.com/serega14736/ML0/blob/master/img/LOO_Kvadrat.png)

h=0.35 LOO=0.04	

![1NN](https://github.com/serega14736/ML0/blob/master/img/Karta_Kvadrat.png)

Реальзация фунцкии:

``` R
c2 <- function(r) 
{
  if (abs(r) > 1)
  {
    return (0)
  }
  return ((15/16) * (1 - r*r)^2)
}
```
## Ядро Треугольное ##

![1NN](https://github.com/serega14736/ML0/blob/master/img/LOO_Treyg.png)

h=0.35 LOO=0.04	

![1NN](https://github.com/serega14736/ML0/blob/master/img/Karta_Treyg.png)

Реальзация фунцкии:

``` R
c3 <- function(r) 
{
  if (abs(r) > 1) 
  {
    return (0)
  }
  return (1 - abs(r))
}
```
## Ядро Гауссовское ##

![1NN](https://github.com/serega14736/ML0/blob/master/img/LOO_Gaus.png)

h=0.1 LOO=0.04	

![1NN](https://github.com/serega14736/ML0/blob/master/img/Karta_Gaus.png)

Реальзация фунцкии:

```R
c4 <- function(r) 
{
  (2*pi)^0.5 * exp(-0.5 * r*r)
}
```

## Ядро Прямоугольное ##

![1NN](https://github.com/serega14736/ML0/blob/master/img/LOO_Pramoygol.png)

h=0.35 LOO=0.04	

![1NN](https://github.com/serega14736/ML0/blob/master/img/Karta_Pramoygol.png)

Реальзация фунцкии:

```R 
c5 <- function(r) 
{
  if (abs(r) > 1) 
  {
    return (0)
  }
  return (0.5)
}
```

## Преимущества: ##
1) При правильно выбраном h алгоритм способен классифицировать объект с хорошим качеством;

2) Алгоритм прост в реализации;

3) Учитывются все точки с одинаковым расстоянием;

## Недостатки: ##
1) Нужно подбирать h для каждой выборке

2) Требуется хранить выборку целиком;

## 5. Метод потенциальных функций ##
Если в методе парзеновского окна центр окна поместить в классифицируемый объект, то получим метод потенциальных функций: 

![equation](http://latex.codecogs.com/gif.latex?a(u;X^l)&space;=&space;argmax_{y\epsilon&space;Y}&space;\sum_{i:y_u^i=y}{\gamma_i&space;K(\frac{\rho(u,x_i)}{h_i})},&space;\gamma_i&space;\geq&space;0,&space;h_i&space;>&space;0)

Теперь ширина окна h зависит не от классифицуруемого объекта, а от обучающего x.

## реализация функции: ##
```R
potential <- function(xl, z,gamma, h) {
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- sortObjectsByDist(xl, z)
  m <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  classes <- xl[1:l, n + 1]
  for(i in 1:l)
  {
    w <- gamma[i] * c4(distances[i,2]/h)
    m[classes[i]] <- m[classes[i]] + w
  }
  if(m[1]!=0 || m[2]!=0 || m[3]!=0)class <- names(which.max(m))
  else class <- "not_class"
  if(class==0)class <- "grey"
  return (class)
}
h <- 1
gamma <- rep(0,n)
p <- c(0,n)
E <- 5
Q <- E + 1
while(Q > E)
{
  t <- 1
  while(t)
  {
    i <- sample(1:n, 1)

    z <- c(xl[i,1], xl[i,2])
    class <- potential(xl, z, gamma, h)
    if(class!=xl[i,3])
    {
      gamma[i] <- gamma[i] + 1
      t <- 0
    }
  }
  Q <- 0
  for(i in 1:n)
  {
    z <- c(xl[i,1], xl[i,2])
    class <- potential(xl, z, gamma, h)
    if(class != xl[i,3])Q <- Q+1
  }
}
n <- length(gamma)
gammamax <- max(gamma)
for(i in 1:n)
{
  x <- xl[i,1]
  y <- xl[i,2]
  r <- h
  if(gamma[i] > 0)
  {
    print(gamma[i]/E/gammamax)
    color <- adjustcolor(colors[xl[i,3]], gamma[i]/E/gammamax)
    draw.circle(x, y, r, 50, border = color, col = color)
  }
}
```
## Визуализация потенциалов ##
## Гауссовское максимум ошибок = 5 ##
![1NN](https://github.com/serega14736/ML0/blob/master/img/Potenc_gaus.png)
## Карты классификации Гауса ##
![1NN](https://github.com/serega14736/ML0/blob/master/img/PotencGaus_map.png)

## Треугольное максимум ошибок =5 ##
![1NN](https://github.com/serega14736/ML0/blob/master/img/Potenc_treyg.png)
## Карты классификации Треугольника ##
![1NN](https://github.com/serega14736/ML0/blob/master/img/PotencTreyg_map.png)
## 6. STOLP. ##
Выделяют несколько видов объектов обучения:

1) Эталонные — типичные представители классов. Если классифицируемый объект близок к эталону, то, скорее всего, он принадлежит тому же классу.

2) Неинформативные — плотно окружены другими объектами того же класса. Если их удалить из выборки, это практически не отразится на качестве классификации.

3) Выбросы — находятся в окружении объектов чужого класса. Как правило, их удаление только улучшает качество классификации.

Алгорим STOLP исключает из выборки выбросы и неинформативные объекты, оставляя лишь нужное количество эталонных. Таким образом улучшается качество классификации, сокращается объем данных и уменьшается время классификации объектов. Другими словами STOLP — алгоритм сжатия данных.

Используется функция отступа:

![1NN](https://camo.githubusercontent.com/c8bddb8a997db6324c19e1f4b28b4bffd86f666f/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f4d253238785f69253239253230253344253230575f253742795f69253744253238785f692532392532302d253230253543756e64657273657425374279253230253543696e253230592532302535437365746d696e7573253230795f692537442537426d6178253744575f79253238785f69253239253239253239)

 где ![1NN](https://camo.githubusercontent.com/6b7709629eecb9d52e0928c4b41ec76271043f89/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f575f79253238785f69253239) - весовая функция и зависит от выбранного алгоритма классификации.

## Типы объектов в зависимости от выступа ##

![1NN](https://github.com/serega14736/ML0/blob/master/img/STOLP.jpg)

Э — эталонные (можно оставить только их);

Н — неинформативные (можно удалить из выборки);

П — пограничные (их классификация неустойчива);

О — ошибочные (причина ошибки — плохая модель);

Ш — шумовые (причина ошибки — плохие данные)

## Преимущества отбора эталонов: ##

1) Сокращается число хранимых объектов;

2) Сокращается время классификации;

3) Объекты распределяются по величине отступов;

## Недостатки алгоритма ##

1) Относительно низкая эффективность
2) необходимость задавать параметр ![equation](https://latex.codecogs.com/gif.latex?%5Cdelta) ;

## 7. Линии уровня нормального распределения ##

Вероятностное распределение с плотностью 
<a href="https://www.codecogs.com/eqnedit.php?latex=N(x,\mu,\Sigma)&space;=&space;\frac{1}{(2\pi)^n\left&space;|&space;\Sigma&space;\right&space;|}exp(-\frac{1}{2}(x-\mu)^T&space;\Sigma^{-1}(x-\mu))" target="_blank"><img src="https://latex.codecogs.com/gif.latex?N(x,\mu,\Sigma)&space;=&space;\frac{1}{(2\pi)^n\left&space;|&space;\Sigma&space;\right&space;|}exp(-\frac{1}{2}(x-\mu)^T&space;\Sigma^{-1}(x-\mu))" title="N(x,\mu,\Sigma) = \frac{1}{(2\pi)^n\left | \Sigma \right |}exp(-\frac{1}{2}(x-\mu)^T \Sigma^{-1}(x-\mu))" /></a>

называется  n-мерным многомерным нормальным распределением с математическим ожиданием 
<a href="https://www.codecogs.com/eqnedit.php?latex=\mu&space;\in&space;R^n" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu&space;\in&space;R^n" title="\mu \in R^n" /></a>

и ковариоционной матрицей 
<a href="https://www.codecogs.com/eqnedit.php?latex=\Sigma&space;\in&space;R^{n*n}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\Sigma&space;\in&space;R^{n*n}" title="\Sigma \in R^{n*n}" /></a>

Предполагается, что матрица симетрична, невырожденная, положительно определенная.

## Реализация ##
``` R
line_norm <- function(center,A)
{
  det <- det(A)
  a <- A[2,2]/det
  b <- -A[2,1]/det
  c <- -A[1,2]/det
  d <- A[1,1]/det
  
  x0 <- center[1]
  y0 <- center[2]
  
  X <- seq(-2.5, 2.5, 0.1)
  Y <- seq(-2.5, 2.5, 0.1)
  
  
  A <- d
  B <- a
  C <- -c -b
  D <- -2*d*x0 + y0*(c+b)
  E <- -2*a*y0 + x0*(c+b)
  F <- d*x0^2 + a*y0^2 + x0*y0*(-c-b)
  
  func <- function(x, y) {
    1/(2*pi*sqrt(det))*exp((-1/2)*(x^2*A + y^2*B + x*y*C + x*D + y*E + F))
  }
  Z <- outer(X, Y, func)
  
  contour(X, Y, Z)
}
```
## Некорелированы ##
![1NN](https://github.com/serega14736/ML0/blob/master/img/elipse.png)
## Одинаковая дисперсия ##
![1NN](https://github.com/serega14736/ML0/blob/master/img/sphera.png)
## Корелированы ##
![1NN](https://github.com/serega14736/ML0/blob/master/img/elips_povorot.png)
