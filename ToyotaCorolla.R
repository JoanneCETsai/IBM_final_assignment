# 2 Plotting in R
x<-seq(-10,20,0.5)
y<-0.1*x^3 - x^2 -6*x

par(mfrow = c(2, 1))

plot(x, y, type = "l", 
     main = "x-y", 
     xlab = "x axis",
     ylab = "y axis",
     xlim = c(-10, 20)
)

plot(x, y, type = "p", 
     main = "x-y", 
     xlab = "x axis",
     ylab = "y axis",
     xlim = c(-10, 20)
)


# 3 Linear Regression

corollas<-read.table(file = "/Users/chiaentsai/Desktop/Data Mining/lab/lab1/ToyotaCorolla.csv",
                     sep = ",",
                     header = TRUE)

# this is more convenient
# corollas<-read.csv(file = "/Users/chiaentsai/Desktop/Data Mining/lab/lab1/ToyotaCorolla.csv")

library("tibble")
corollas<-as_tibble(corollas)
corollas2<-corollas[, c("Price", "Age_08_04", "KM", "Fuel_Type", 
                        "HP", "Met_Color", "Doors", "Quarterly_Tax", "Weight")]
is.factor(corollas2$Fuel_Type)
is.factor(corollas2$Met_Color)

corollas2$Fuel_Type<-as.factor(corollas2$Fuel_Type)
corollas2$Met_Color<-as.factor(corollas2$Met_Color)

unique(corollas2$Fuel_Type)

corollasLM = lm(formula = Price~., data = corollas2)

summary(corollasLM)

library("ggplot2")
ggplot(data = corollas2, aes(x = Age_08_04, y = Price))+
        geom_point()+
        stat_smooth(method = "lm",
                    color = "red")

lev<-hatvalues(corollasLM)
plot(lev, type ="h")

studRes <- rstudent(corollasLM)
plot(studRes, type ="h")
