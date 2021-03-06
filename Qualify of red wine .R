#install packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("psych")

# open file in tabular form
rm(list=ls())
options(warn=-1)
library(dplyr)
library(ggplot2)
library(corrplot)
library(readr)
Read.csv2("D:/winequality-red.csv")
View(wine)

# L???c nh???ng d??? l???u c� m???c ???nh hu???ng l???n t??? t???p g???c
wine_DF=wine[c(2,5,8,11,12)]
attach(wine_DF)
apply(is.na(wine_DF),2,which)
# L�m r� d??? li???u (Data visualization)
# T�nh to�n d??? li???u th???ng k�
Mean= apply(wine_DF, 2 , mean)
Median = apply(wine_DF, 2 , median)
Sd = apply(wine_DF, 2 , sd)
Min= apply(wine_DF, 2 , min)
Max= apply(wine_DF, 2 , max)
dinhluong = data.frame(Mean, Median, Sd, Min, Max )

# bi???u d??? ph�n ph???i c???a c�c bi???n
hist(quality , col = "blue")
hist(density, col = "red")
hist(alcohol, col = "pink" )
hist(chlorides, col = "yellow") 
hist(volatile.acidity)

# M???i quan h??? gi???a c�c b�n d???n ch???t lu???ng
pairs(`volatile acidity`~quality, main= "Moi quan he Volatile acidity va Quality")
pairs(chlorides~quality, main= "Moi quan he Chloride va Quality")
pairs(density~quality, main= "Moi quan he Density va Quality")
pairs(alcohol~quality, main= "Moi quan he Alcohol va Quality")
#X�y d???ng m� h�nh h???i quy tuy???n t�nh
#Ki???m tra m???c ???nh hu???ng
m1=lm(quality~`volatile acidity`+chlorides+density+alcohol)
summary(m1)
#m� h�nh m2
m2=lm(quality~`volatile acidity`+density+alcohol)
#D�ng Anova d??? ki???m tra m� h�nh ph� h???p
anova(m1,m2)
# V??? bi???u d??? bi???u th??? quan h??? gi???a gi� tr??? d??? b�o v� sai s??? h???i quy c???a m� h�nh m1
plot(fitted(m1),resid(m1))
plot(m1,which=1)
