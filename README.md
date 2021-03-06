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

Ещё один способ задать веса соседям — определить <a href="https://www.codecogs.com/eqnedit.php?latex=w_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w_i" title="w_i" /></a> как функцию от расстояния <a href="https://www.codecogs.com/eqnedit.php?latex=\rho(u,x_{i,u})" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\rho(u,x_{i,u})" title="\rho(u,x_{i,u})" /></a> а не от ранга соседа <a href="https://www.codecogs.com/eqnedit.php?latex=i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?i" title="i" /></a>. Введём функцию ядра <a href="https://www.codecogs.com/eqnedit.php?latex=K(z)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?K(z)" title="K(z)" /></a> , невозрастающую на <a href="https://www.codecogs.com/eqnedit.php?latex=[0,\infty]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?[0,\infty]" title="[0,\infty]" /></a>,  и рассмотрим алгоритм <a href="https://www.codecogs.com/eqnedit.php?latex=a(u;X^l,h,K)=arg{\underset{y\in&space;Y}{max}}&space;\sum^l_{i=1}&space;[y_{i,u}=y]K(\frac{{\rho(u,x_{u,i})}&space;}{h})" target="_blank"><img src="https://latex.codecogs.com/gif.latex?a(u;X^l,h,K)=arg{\underset{y\in&space;Y}{max}}&space;\sum^l_{i=1}&space;[y_{i,u}=y]K(\frac{{\rho(u,x_{u,i})}&space;}{h})" title="a(u;X^l,h,K)=arg{\underset{y\in Y}{max}} \sum^l_{i=1} [y_{i,u}=y]K(\frac{{\rho(u,x_{u,i})} }{h})" /></a>. Параметр h называется шириной окна и играет примерно ту же роль, что и число соседей k.

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
Метод потенциальных функций - метрический классификатор, частный случай метода ближайших соседей. Позволяет с помощью простого алгоритма оценивать вес объектов обучающей выборки при решении задачи классификации.

Если в методе парзеновского окна центр окна поместить в классифицируемый объект, то получим метод потенциальных функций: 

![equation](http://latex.codecogs.com/gif.latex?a(u;X^l)&space;=&space;argmax_{y\epsilon&space;Y}&space;\sum_{i:y_u^i=y}{\gamma_i&space;K(\frac{\rho(u,x_i)}{h_i})},&space;\gamma_i&space;\geq&space;0,&space;h_i&space;>&space;0)

где h зависит не от классифицуруемого объекта, а от обучающего x.

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
(матрица симетрична, невырожденная, положительно определенная)



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

## Наивный нормальный байесовский классификатор ##

Байесовский классификатор — широкий класс алгоритмов классификации, основанный на принципе максимума апостериорной вероятности. Для классифицируемого объекта вычисляются функции правдоподобия каждого из классов, по ним вычисляются апостериорные вероятности классов. Объект относится к тому классу, для которого апостериорная вероятность максимальна.

Наивный байесовский классификатор максимизирует апостериорную вероятность класса, в этом случае классификатор имеет вид:

<a href="https://www.codecogs.com/eqnedit.php?latex=h(x)=\arg\max_{y&space;\in&space;Y}p(y|x)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?h(x)=\arg\max_{y&space;\in&space;Y}p(y|x)" title="h(x)=\arg\max_{y \in Y}p(y|x)" /></a>

По теореме байеса формулу можно переписать в виде:

<a href="https://www.codecogs.com/eqnedit.php?latex=h(x)=\arg\max_{y&space;\in&space;Y}p(y|x)=\arg\max_{y&space;\in&space;Y}\frac{p(x|y)p(y)}{p(x)}=\arg\max_{y&space;\in&space;Y}p(x|y)p(y)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?h(x)=\arg\max_{y&space;\in&space;Y}p(y|x)=\arg\max_{y&space;\in&space;Y}\frac{p(x|y)p(y)}{p(x)}=\arg\max_{y&space;\in&space;Y}p(x|y)p(y)" title="h(x)=\arg\max_{y \in Y}p(y|x)=\arg\max_{y \in Y}\frac{p(x|y)p(y)}{p(x)}=\arg\max_{y \in Y}p(x|y)p(y)" /></a>

Решающее правило принимает вид:

![1NN](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/naivv.gif)

## Реализация на R ##
```R
naiv <- function(x, mu, sigma, lamda, P){
	n <- 2
	res <- log(lamda*P)
	
	for(i in 1 : n){
		pyj <- (1/(sigma[i]*sqrt(2*pi))) * exp(-1 * ((x[i] - mu[i])^2)/(2*sigma[i]^2))
    	res <- res + log(pyj)
	}
	
	return(res)
}
```
## Пример  ##
![1NN](https://github.com/serega14736/ML0/blob/master/img/NaivBaess.jpeg)

## Линейные алгоритмы классификации ##

Линейный классификатор — алгоритм классификации, основанный на построении линейной разделяющей поверхности. В случае двух классов разделяющей поверхностью является гиперплоскость, которая делит пространство признаков на два полупространства. В случае большего числа классов разделяющая поверхность кусочно-линейна.

Пусть объекты описываются n числовыми признаками ![1NN](http://www.machinelearning.ru/mimetex/?f_j:\:%20X\to\mathbb{R},\;%20j=1,\ldots,n). Тогда пространство признаковых описаний объектов есть ![1NN](http://www.machinelearning.ru/mimetex/?X=\mathbb{R}^n). Пусть Y — конечное множество номеров (имён, меток) классов.

### Случай двух классов ###

Положим ![1NN](http://www.machinelearning.ru/mimetex/?Y=\{-1,+1\})

Линейным классификатором называется алгоритм классификации ![1NN](http://www.machinelearning.ru/mimetex/?a:\;%20X\to%20Y) вида

![1NN](http://www.machinelearning.ru/mimetex/?a(x,w)%20=%20\mathrm{sign}\left(%20\sum_{j=1}^n%20w_j%20f_j(x)%20-%20w_0%20\right)%20=%20\mathrm{sign}\langle%20x,w%20\rangle),
где ![1NN](http://www.machinelearning.ru/mimetex/?w_j) — вес j-го признака, ![1NN](http://www.machinelearning.ru/mimetex/?w_0) — порог принятия решения, ![1NN](http://www.machinelearning.ru/mimetex/?w=(w_0,w_1,\ldots,w_n)) — вектор весов, ![1NN](http://www.machinelearning.ru/mimetex/?\langle%20x,w%20\rangle) — скалярное произведение признакового описания объекта на вектор весов. Предполагается, что искусственно введён «константный» нулевой признак: ![1NN](http://www.machinelearning.ru/mimetex/?f_{0}(x)=-1)

### Случай произвольного числа классов ###

Линейный классификатор определяется выражением

![1NN](http://www.machinelearning.ru/mimetex/?a(x,w)%20=%20\mathrm{arg}\max_{y\in%20Y}\,%20\sum_{j=0}^n%20w_{yj}%20f_j(x)%20=%20\mathrm{arg}\max_{y\in%20Y}\,%20\langle%20x,w_y%20\rangle),
где каждому классу соотвествует свой вектор весов ![1NN](http://www.machinelearning.ru/mimetex/?w_y=(w_{y0},w_{y1},\ldots,w_{yn})).

## Adaline ##
Adaline (Адаптивный линейный элемент)  - это линейный алгоритм классификации, в котором используется квадратичная функция потерь. Возьмем в качестве функции потерь <a href="https://www.codecogs.com/eqnedit.php?latex=L'(M)&space;=&space;2(\left&space;\langle&space;w,x_i&space;\right&space;\rangle&space;y_i&space;-1)&space;x_i&space;y_i&space;=&space;2(\left&space;\langle&space;w,x_i&space;\right&space;\rangle&space;-&space;y_i)x_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?L'(M)&space;=&space;2(\left&space;\langle&space;w,x_i&space;\right&space;\rangle&space;y_i&space;-1)&space;x_i&space;y_i&space;=&space;2(\left&space;\langle&space;w,x_i&space;\right&space;\rangle&space;-&space;y_i)x_i" title="L'(M) = 2(\left \langle w,x_i \right \rangle y_i -1) x_i y_i = 2(\left \langle w,x_i \right \rangle - y_i)x_i" /></a> и следовательно получим правило обновения:
<a href="https://www.codecogs.com/eqnedit.php?latex=w&space;=&space;w&space;-&space;\eta(\langle&space;w,x_i&space;\rangle-y_i)x_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w&space;=&space;w&space;-&space;\eta(\langle&space;w,x_i&space;\rangle-y_i)x_i" title="w = w - \eta(\langle w,x_i \rangle-y_i)x_i" /></a>

## Реализация ##  
``` R
# Квадратичная функция потерь
loss_ada <- function(xi, yi, w) {
	mi <- c(crossprod(w, xi)) * yi
	l <- (mi - 1)^2
	return(l)
}

# Функция обнуления весов
upd_ada <- function(xi, yi, w, eta) {
	wx <- c(crossprod(w, xi))
	ld <- (wx - yi) * xi
	W <- w - eta * ld
	return(W)
}
```

Работа алгоритма при помощи ADALINE

![1NN](https://github.com/serega14736/ML0/blob/master/img/Adaline.png)
