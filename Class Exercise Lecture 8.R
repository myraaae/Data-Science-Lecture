#--Intro------------------------------------------------------------------------
install.packages("dplyr")
library(dplyr)
library(readr)

student_data <- read.csv("C:/Users/airah/Downloads/student_data.csv")

View(student_data) #table
head(student_data) #first six row
tail(student_data) #last six row

#--Filter-----------------------------------------------------------------------
student_fail <- student_data %>% filter (final_exam_mark< 40)
View(student_fail)

mydata <- student_data %>% filter(final_exam_mark > 40) %>% arrange(final_exam_mark)
View (mydata)

mydata1 <- student_data %>% filter(final_exam_mark > 40) %>% arrange(desc(final_exam_mark))
View(mydata1)

#select
mydata2 <- student_data %>% select(student_id, coursework_mark, final_exam_mark)
View(mydata2)

#not select
mydata3 <- student_data[ , c("student_id", "coursework_mark", "final_exam_mark")]
View(mydata3)

glimpse(mydata3)

#mutate
mydata4 = student_data %>% mutate(Total_Mark =(coursework_mark + final_exam_mark/200*100))
View(mydata4)

#not select
mydata5 <- cbind(student_data, Total_Mark = (student_data $coursework_mark + student_data$final_exam_mark/200*100))
View(mydata5)

#--Descriptive Analytics--------------------------------------------------------

data <- iris
head(data)
tail(data)
str(data) #check structure

min(data$Sepal.Length) #produces 4.3
max(data$Sepal.Length) #produces 7.9
summary(data) #check summary
#can also produce sd, var, mean, median. can use boxplot to visualise

#quantiles
A <- c(170.2, 181.5, 188.9, 163.9, 166.4, 163.7, 160.4, 175.8, 181.5)
quantile(A)
sort(A)
IQR(A) #interquartile

# Histogram for sepal length!!
hist(iris$Sepal.Length,
     main = "Histogram of Sepal Length by Uyah",
     xlab = "Sepal Length (cm)",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")
#it's normally distributed

# Boxplot for species!!
boxplot(Sepal.Length ~ Species,
        data = iris,
        main = "Boxplot of Sepal Length by Species (made by Uy)",
        xlab = "Species",
        ylab = "Sepal Length (cm)",
        col = c("lightgreen", "lightpink", "mediumpurple1"))
#virginica is an outlier

# Scatter Plot!!
plot(iris$Sepal.Length, iris$Petal.Length,
     main = "Sepal Length vs Petal Length",
     xlab = "Sepal Length (cm)",
     ylab = "Petal Length (cm)",
     col = as.numeric(iris$Species),
     pch = 19)
legend("topleft",
       legend = levels(iris$Species),
       col = 1:3,
       pch = 19)
#see if it's correlated, can use mean or median to replace missing value

#--Outliers---------------------------------------------------------------------
players <- read.csv("C:/Users/airah/Downloads/players.csv")
View(players)
is.na(players)

median_age <- median(players$Age, na.rm = TRUE)

players$Age[players$Age<18 | players$Age>38] <- median_age
View(players)

data<-c(30,24,26,28,29,28,27,26,32,34,13,15,14,31,29,28,24,25,30,34,35,27,30,34,44,48)
boxplot(data, main = "Boxplot")

# low extreme = Q1 - 1.5 * IQR
# upper extreme = Q3 + 1.5 * IQR

first_q<-quantile(data,0.25) #produces 26
third_q<-quantile(data,0.75) #produces 31.75
iqr<-IQR(data) #produces 5.75
le<-first_q- 1.5 * iqr #produces 17.375
ue<-third_q + 1.5 * iqr #this produces 40.375

# Handling Outliers!!!
#drop vals
data_new<-data
data_new <- data_new[!data_new<le]
data_new <- data_new[!data_new>ue]
data_new

#replace vals with mean
data_new1 <- data
avg <- round(mean(data_new1))
data_new1[data_new1<le] <- avg
data_new1[data_new1>ue] <- avg
data_new1

#replace values that are <le and >ue: replace with le and ue
data_new2 <- data
data_new2[data_new2<le] <- le
data_new2[data_new2>ue] <- ue
data_new2
