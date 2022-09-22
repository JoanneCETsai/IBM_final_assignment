oliveOils=read.table(file='/Users/chiaentsai/Desktop/Data Mining/lab/lab0/olive-train.dat', header =FALSE, sep="")
oliveOils

typeof(oliveOils)

class(oliveOils)
oliveOils[1:10, ]

colnames(oliveOils)<-c("region","area","Palmitic","Palmitoleic","tearic","Oleic","Linoleic","Linolenic","Arachidic","Eicosenoic")
dim(oliveOils)
nrow(oliveOils)

oliveOils$region

as.factor(oliveOils$region+oliveOils$area)

class(oliveOils$area)

is.factor(oliveOils$area)
oliveOils

gender <- c("boy", "girl", "boy", "boy", "girl")
gender<-factor(gender)
gender
oliveOils$region<-factor(oliveOils$region)
class(oliveOils$region)
oliveOils
oliveOils$area<-factor(oliveOils$area)
class(oliveOils$area)

print(oliveOils$region)
is.factor(oliveOils$region)

levels(oliveOils$area)

sum(is.na(oliveOils$region))

a<-c(TRUE,FALSE,FALSE)
sum(a)
computemean<-function(a){
  result<-0
  number<-length(a)
  for(i in a){
    result<-result+i
  }
  return(result/number)
}
x<-c(1,2,3,4,5)
computemean(x)

computemean(oliveOils$Palmitic)
mean(oliveOils$Palmitic)

computemean(oliveOils$Palmitoleic)
computemean(oliveOils[,4])

install.packages("ggplot2")
library("ggplot2")
install.packages("tidyverse")

taData<-data.frame(
  name=c("Tao","Annie","tao"),
  role=c("full","half","full")
)
 taData[!duplicated(tolower(taData$name)),]

