#michalczyk-iris

# komentarz: wiem, ze nie trzeba dawać tego library ciagle, ale tak latwiej bylo skopiowac
#r3-zad1
library(dplyr)
data("iris")
select(iris, Species, Sepal.Length)

#r3_zad2
library(dplyr)
data("iris")
iris %>%
  filter(Petal.Width > 2) %>%
  count(.)

#r3_zad3
library(dplyr)
data("iris")
iris %>%
  filter(Sepal.Width < 4, Species != "setosa") %>%
  arrange(Petal.Length)

#r3_zad4
library(dplyr)
data("iris")
iris %>%
  filter(Sepal.Length < 4.5) %>%
  distinct(Species) %>%
  count()

#r3_zad5
library(dplyr)
data("iris")
new_iris = mutate(iris, Petal_dim = iris$Petal.Length * iris$Petal.Width)
new_iris

#r3_zad6
library(dplyr)
library(ggplot2)
data("iris")
#virg = filter(iris, Species == "virginica")
ggplot(filter(iris, Species == "virginica"), aes(Sepal.Length, Sepal.Width)) + geom_point()

#r3_zad7
library(dplyr)
library(ggplot2)
iris %>%
  group_by(Species) %>%
  count()

#r3_zad8
library(dplyr)
library(ggplot2)
iris %>%
  group_by(Species) %>%
  mutate(Avg_dim = mean(Petal.Width) * mean(Petal.Length)) %>%
  filter(Avg_dim != 13 | Species == "virginica")
