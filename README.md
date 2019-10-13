# СМПР #

## Метод 1NN ##
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

```
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
```
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

## Метод парзеновского окна ##

В данном алгоритме выбирается следующий способ задать веса соседям : определить ![w_i](https://latex.codecogs.com/gif.latex?w_i) как функцию от расстояния ![rho](https://latex.codecogs.com/gif.latex?%24%5Crho%28u%2Cx_u%5E%7B%28i%29%7D%29%24), а не от ранга соседа i. Введём функцию ядра K(z), весовую функцию следующим образом:  
![w_for_parzen](https://latex.codecogs.com/gif.latex?%24%24w%28u%2Ci%29%20%3D%20K%5Cleft%20%28%5Cfrac%7B1%7D%7Bh%7D%5Crho%28u%2Cx_u%5E%7B%28i%29%7D%29%20%5Cright%20%29%24%24),
где параметр h - ширина окна.  
Данный параметр будем подбирать по оценке скользящего контроля LOO и для различных ядер оптимальное значение ширины окна будет отличаться.
