#Часть 1: R
#1
parf <-read.csv('parfume.csv')
#2
parf <-parf[parf$vote!=2,]
View (parf)
#3
library(tidyverse)
parf_test <-parf[parf$group=='test',]
parf_control <-parf[parf$group=='control',]
barplot(table(parf_control$vote), main = "Barplot для контрольной группы", 
        col = 'lightblue', ylab = "counts",xlab= 'control')
barplot(table(parf_test$vote), main = "Barplot для тестовой группы", 
        col = 'blue', ylab = "counts",xlab= 'test')
#4
test <- parf_test$vote
control <- parf_control$vote
#5
#Нулевая гипотеза - рекламный видеоролик в цветном варианте 
#и видеоролик в чернобелом варианте одинаково популярны среди девушек
#H0: p_цвет=p_чб

# Односторонняя альтернативная гипотеза - черно-белый ролик будет
#более предпочтителен,чем цветной.
#H1: p_цвет<p_чб

#6
p_test <- mean(test)
p_control <- mean(control)
p_test - p_control

N <- 1000

set.seed(123) # для воспроизводимости

differences <- rep(NA, N) 
for(i in 1:N){
  s1 <- sample(control, replace = TRUE) # выборка для control с возвращением
  s2 <- sample(test, replace = TRUE) # выборка для test с возвращением
  p1 <- sum(s1)/length(s1) # доля 1 в control
  p2 <- sum(s2)/length(s2) # доля 1 в test
  p_diff <- p2 - p1 # разница в долях
  differences[i] <- p_diff # записываем ее в differences
}
differences_cent <- differences - mean(differences)
diff_df <- data.frame(dif = differences_cent)

ggplot(data = diff_df, aes(x = dif)) + 
  geom_histogram(fill = "blue", color = "navy") + 
  geom_vline(xintercept = 0.223, color = "red", lty = 2)

sum(differences_cent > 0.223)

#7
#Статистический вывод
#Полученный p-value меньше уровня значимости 0.05.
#Следовательно, нулевую гипотезу можно отвергнуть на уровне значимости 0.05 на имеющихся данных
# в пользу альтернативной гипотезы.

#Содержительный вывод
#  Среди девушек от 18 до 25 лет черно-белый ролик предподчительней, 
# чем цветной.