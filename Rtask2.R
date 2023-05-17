#1. Напишите функцию, чтобы найти максимум трех чисел.
max_3 <- function(x,y,z)
{
  if(x <= y) {
    if(y >= z) {return(y)} else {return(z)}
  }
  else {
    if (x >= z) {return(x)} else {return(z)}
  }
}
max_3(1,3,4)

#2. Напишите функцию для суммирования 4 чисел вектора с(8, 2, 5, 0).
#Ожидаемый результат:15
sum_v <- function(v)
{
  s <- 0
  for (elem in v)
  {
    s <- s + elem
  }
  return(s)
}
x <- c(8, 2, 5, 0)
sum_v(x)

#3. Напишите функцию для перемножения всех чисел вектора. Обратите внимание, что длина вектора может быть любой.
prod_v <- function(v)
{
  s <- 1
  for (elem in v)
  {
    s <- s * elem
  }
  return(s)
}
x <- c(8, 2, 5, -3)
prod_v(x)

#4. Напишите функцию для обращения строки.
#Ожидаемый результат:"dcba4321"
reverse_s <- function(s)
{
  arr =  strsplit(s, "")[[1]]
  ans = paste(arr[length(arr):1], collapse = "")
  return(ans)
}
reverse_s("1234abcd")

#5. Напишите функцию для вычисления факториала числа (неотрицательное целое число). Функция принимает число в качестве аргумента.
fact <-function(n)
{
  if (n<=1) return(1)
  else return((n*fact(n-1)))
}
fact(10)
fact(5)

#6. Напишите функцию, чтобы проверить, находится ли число в заданном диапазоне.
In_int<-function(n, a, b)
  return((n>=a)&(n<=b))
In_int(20,1,3)
In_int(2,1,3)
In_int(0,1,3)

#7. Напишите функцию, которая принимает строку и рассчитывает количество букв верхнего и нижнего регистра. Выведите элементы функции.
#Пример строки: «Быстрая Лиса - 58»
#Ожидаемый результат:Количество букв в верхнем регистре:2
#Количество строчных букв:9
analyse <- function(s) {
  U <- grepl("[[:upper:]]", strsplit(s, "")[[1]])
  L <- grepl("[[:lower:]]", strsplit(s, "")[[1]])
  
  U <- length(which(unlist(U)))
  L <- length(which(unlist(L)))
  
  print(paste('Количество букв в верхнем регистре:', U))
  print(paste('Количество строчных букв:', L))
}
analyse('Быстрая Лиса-58')

# 8. Напишите функцию, которая берет вектор и возвращает новый вектор с уникальными элементами из исходного.
#Входящий вектор:(1, 2, 3, 3, 3, 3, 4, 5)
#Ожидаемый результат:(1, 2, 3, 4, 5)
uniq <- function(v){
  return(v[!duplicated(v)])
}
uniq(c(1,2,3,3,3,3,4,5))

#9. Напишите функцию для печати четных чисел из заданного вектора.
#Входящий вектор:(1, 2, 3, 4, 5, 6, 7, 8, 9)
#Ожидаемый результат:(2, 4, 6, 8)
even_numbered <-function(v)
  return(v[v%%2==0])
even_numbered(c(1, 2, 3, 4, 5, 6, 7, 8, 9))

#10. Напишите функцию, которая проверяет, является ли переданная строка палиндромом или нет.
#Примечание. Палиндром - это слово, фраза или последовательность, которые читаются так же, как и вперёд, например, топот.
is_pal <-function(s){
  s_reversed <- revers_str(s)
  return(s == s_reversed)
}

#11. Напишите функцию, чтобы проверить, является ли строка панграммой или нет. Примечание. Панграммы - это слова или предложения, содержащие каждую букву алфавита хотя бы один раз. Например: «The quick brown fox jumps over the lazy dog»
is_pangramm <- function(s)
{
  s = strsplit(tolower(s), "")[[1]]
  s = s[!duplicated(s)]
  return(all(letters %in% s))
}
is_pangramm('The quick brown fox jumps over the lazy dog')
is_pangramm('The quick brown fox jumps over the lazy do')

#12. Напишите функцию, которая принимает в качестве входных данных последовательность слов, разделенных дефисами, и печатает слова в последовательности, разделенной дефисами, после сортировки по алфавиту.
#Образцы элементов:green - red - yellow - black
#Ожидаемый результат:black - green - red - yellow
sorter <-function(s)
  return(paste0(sort(strsplit(s, " - ")[[1]]), collapse = " - "))
sorter("green - red - yellow - black")

#13. Напишите функцию, чтобы создать список(list), значения которого представляют собой квадрат чисел от 1 до 30 (оба включены).
sq <- function()
{
  return(list(c(1:30)^2))
}
sq()

#14. Определите количество локальных переменных, объявленных в функции(любая на Ваш выбор, где есть локальные переменные).

#???

#15. Напишите функцию, которая ищет в указанной папке и подпапках файлы “.csv”. Функция должна наследовать аргументы от list.files()
csv_by<- function(path){
  csv_files <- list.files(path = path, pattern = "\\.csv$")
  return(csv_files)
}

#16. Отобразите методы для функции print()
methods(print)
#17. Создайте объект S4 класса book, задав параметры автора, года издания, названия, цвета обложки. Напишите метод “print.book” для этого класса. Создайте объект S4 класса documentary_book, наследующий от book, который дополнительно будет включать параметр “history period”.
setClass("book", representation(
  author = "character",
  year = "numeric",
  name = "character",
  color = "character")
)
setMethod("show","book",
          function(object) {
            cat("Автор:", object@author, "\n")
            cat("Год издания:", object@year, "\n")
            cat("Название:", object@name, "\n")
            cat("Цвет обложки", object@color, "\n")
          })
setClass("documentary_book", 
         slots = (history_period = "character"),
        contains('book')
)

