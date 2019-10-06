# SMPR #

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

![1NN](https://github.com/serega14736/ML0/blob/master/img/KNN.png)

## Карта классификации метода k-ближайших соседей ##

![1NN](https://github.com/serega14736/ML0/blob/master/img/kartaKNN.png)

## 3. kwNN. Метод k-взвешеных ближайших соседей: ##

Метод k-взвешеных ближайших соседей(kwNN). По сравнению с kNN, kwNN принимает во внимание не только колличество соседей определенного класса но и удаленность от классифицируемого обьекта. Для каждого класса определяется оценка близости, у какого класса больше оценка близости тот класс и присваивается классифицируемому обьекту.

Формула алгоритма kwNN:

<a href="https://www.codecogs.com/eqnedit.php?latex=w(i,u)&space;=&space;[i\leqslant&space;k]w(i)\&space;a(u,x^{l},k)&space;=&space;arg\max\limits_{y\in&space;Y}\sum\limits_{i=1}^{k}[y^i_u&space;=&space;y]w(i)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w(i,u)&space;=&space;[i\leqslant&space;k]w(i)\&space;a(u,x^{l},k)&space;=&space;arg\max\limits_{y\in&space;Y}\sum\limits_{i=1}^{k}[y^i_u&space;=&space;y]w(i)" title="w(i,u) = [i\leqslant k]w(i)\ a(u,x^{l},k) = arg\max\limits_{y\in Y}\sum\limits_{i=1}^{k}[y^i_u = y]w(i)" /></a>


