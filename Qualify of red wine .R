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

# L???c nh???ng d??? l???u có m???c ???nh hu???ng l???n t??? t???p g???c
wine_DF=wine[c(2,5,8,11,12)]
attach(wine_DF)
apply(is.na(wine_DF),2,which)
# Làm rõ d??? li???u (Data visualization)
# Tính toán d??? li???u th???ng kê
Mean= apply(wine_DF, 2 , mean)
Median = apply(wine_DF, 2 , median)
Sd = apply(wine_DF, 2 , sd)
Min= apply(wine_DF, 2 , min)
Max= apply(wine_DF, 2 , max)
dinhluong = data.frame(Mean, Median, Sd, Min, Max )

# bi???u d??? phân ph???i c???a các bi???n
hist(quality , col = "blue")
hist(density, col = "red")
hist(alcohol, col = "pink" )
hist(chlorides, col = "yellow") 
hist(volatile.acidity)

# M???i quan h??? gi???a các bán d???n ch???t lu???ng
pairs(`volatile acidity`~quality, main= "Moi quan he Volatile acidity va Quality")
pairs(chlorides~quality, main= "Moi quan he Chloride va Quality")
pairs(density~quality, main= "Moi quan he Density va Quality")
pairs(alcohol~quality, main= "Moi quan he Alcohol va Quality")
#Xây d???ng mô hình h???i quy tuy???n tính
#Ki???m tra m???c ???nh hu???ng
m1=lm(quality~`volatile acidity`+chlorides+density+alcohol)
summary(m1)
#mô hình m2
m2=lm(quality~`volatile acidity`+density+alcohol)
#Dùng Anova d??? ki???m tra mô hình phù h???p
anova(m1,m2)
# V??? bi???u d??? bi???u th??? quan h??? gi???a giá tr??? d??? báo và sai s??? h???i quy c???a mô hình m1
plot(fitted(m1),resid(m1))
plot(m1,which=1)
