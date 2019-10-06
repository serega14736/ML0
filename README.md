# SMPR #

## Метод 1NN ##
Метод ближайших соседей (1NN). Имеется 3 класса объектов, дан некоторый объект, который необходимо классифицировать , т.е. определить , к какому классу он принадлежит.

Классифицируемый объект относим к тому классу, к которому принадлежит ближайший по заданной метрике "сосед" из выборки:

![w](https://latex.codecogs.com/gif.latex?w%28i%2C%20u%29%20%3D%20%5Bi%20%3D%201%5D%3B)

В реализованном методе выбрана евклидова метрика.  
В качестве выборки был взят набор "Ирисы Фишера"  
Карта классификации выглядит следующим образом:  

![1NN](https://github.com/serega14736/ML0/blob/master/1nn.png)
