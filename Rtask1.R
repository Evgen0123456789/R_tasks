#USArrests

data(USArrests)
View(USArrests)

US<-USArrests

#1. количество строк и столбцов(dim, ncol, nrow)
dim(US)
ncol(US)
nrow(US)

#2. вывести на консоль содержание первых строк (head, tail, обращение по индексам строк)
head(US)
tail(US)

#3. вывести на консоль 16-20 элементы 3 столбца, обратившись к нему по имени (обращение по именам столбцов, индексам строк)
US$UrbanPop[16:20]

#4. тип/структуру/подтип данных для датасета в целом и каждого столбца (mode, class, typeof, цикл for, print)
mode(US)
class(US)
typeof(US)

for(elem in 1:ncol(US)) {
  print(colnames(US)[elem])
  print(mode(US[,elem]))
  print(class(US[,elem]))
  print(typeof(US[,elem]))
  cat('\n')
}

#5. имена столбцов и строк (colnames, rownames, dimnames)
colnames(US)
rownames(US)
dimnames(US)

#6. сумму, среднее, дисперсию, среднеквадратическое отклонение всего датасета (sum, mean, sd, var)
sum(US)
mean(as.matrix(US))
sd(as.matrix(US))
var(as.matrix(US))

#7. сумму, среднее, дисперсию, среднеквадратическое отклонение каждого столбца 
for(elem in 1:ncol(US)) {
  print(colnames(US)[elem])
  print(sum(US[,elem]))
  print(mean(US[,elem]))
  print(sd(US[,elem]))
}
#8. сумму, среднее, дисперсию, среднеквадратическое  отклонение каждой строки
for (elem in row.names(US))
{
  print(elem)
  print(sum(US[elem,]))
  print(mean(as.matrix(US[elem,])))
  print(sd(US[elem,]))
}

#9. вывести с 10 по 14 элементы для каждого столбца (обращение по индексам строк, for)
for (elem in colnames(US))
{
  print(elem)
  print(US[[elem]][10:14])
}

#10. сколько в датасете элементов меньше 10 (sum, условие)
sum(US < 10)

#11. сколько в каждом столбце элементов меньше 10 (sum, условие)
apply(US<10 ,2, sum)

#12. какие штаты содержат в названии “Miss” (which, rownames, grepl)
rownames(US)[grepl("Miss", rownames(US))]
which(grepl("Miss", rownames(US)))

#13. вывести на консоль криминальную статистику для штатов, содержащих в названии “New”
summary(US[which(grepl("New",rownames(US))),])

#14. записать файл .csv, содержащий 1-20 строки и 1 и 3 столбцы 
write.csv(US[seq(1:20),c(1,3)],"task1.csv")

#15. записать файл .xlsx, содержащий листы M и N со статистикой для штатов, начинающихся на букву M и N 
M <- which(grepl("^M", rownames(US)))
N <- which(grepl("^N", rownames(US)))
dataset <- list('M' = summary(US[M,]),'N' = summary(US[N,]))
openxlsx::write.xlsx(dataset, file="task1.xlsx")
#16. прочитать записанные файлы
csv <-read.csv('task1.csv')
xlsx_M <- openxlsx::read.xlsx("task1.xlsx", sheet = "M")
xlsx_N <- openxlsx::read.xlsx("task1.xlsx", sheet = "N")

