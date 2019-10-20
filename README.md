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

## Метод Парзеновского окна ##

Рассмотрим весовую функцию w(i,u) как функцию не от ранга соседа, а как функцию от расстояния 
![equation](http://latex.codecogs.com/gif.latex?\rho(u,x_u^i):)

![equation](http://latex.codecogs.com/gif.latex?w(i,u)&space;=&space;K(\frac{1}{h}&space;\rho&space;(u,x_u^i)))

где K(z) - невозрастающая на ![equation](http://latex.codecogs.com/gif.latex?[0,\infty&space;]) (гипотеза комактности) функция яда. В этом случае метричесикй классификатор примет следующий вид:

![equation](http://latex.codecogs.com/gif.latex?a(u;X^l,h)&space;=&space;argmax_{y\epsilon&space;Y}&space;\sum_{i:y_u^i=y}{K(\frac{\rho(u,x_u^i)}{h})})

Этот алгоритм - алогритм парзеновского окна. h - ширина окна, подбирается по LOO.
Рассмотрим ядра: прямоугольно, епачниково, квадратное, гауссовское, треугольное. На датасете ирисов и подберем оптимальный h.
в итоге получим:

![1NN](https://github.com/serega14736/ML0/blob/master/img/loo_parsen_results.png)

Красные точки на изображениях - оптимальные h для выбора. 

## Плюсы Парзеновского окна: ##
При правильно выбраном h алгоритм способен классифицировать объект с хорошим качеством;

Алгоритм прост в реализации;

Учитывются все точки с одинаковым расстоянием;

## Минусы: ##

Нужно подбирать h для каждой выборке

Требуется хранить выборку целиком;

## Карта классификации для парзеновского окна с ядром епачникова: ##

![1NN](https://github.com/serega14736/ML0/blob/master/img/kartaParsen.png)

## Метод потенциальных функций ##
Если в методе парзеновского окна центр окна поместить в классифицируемый объект, то получим метод потенциальных функций: 

![equation](http://latex.codecogs.com/gif.latex?a(u;X^l)&space;=&space;argmax_{y\epsilon&space;Y}&space;\sum_{i:y_u^i=y}{\gamma_i&space;K(\frac{\rho(u,x_i)}{h_i})},&space;\gamma_i&space;\geq&space;0,&space;h_i&space;>&space;0)

Теперь ширина окна h зависит не от классифицуруемого объекта, а от обучающего x.


