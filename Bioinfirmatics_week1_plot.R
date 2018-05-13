library(ggplot2)
rawdata <- read.csv("Week_1_Plotdata.csv", header = TRUE)
ggplot(rawdata, aes(x=Subject, y=a))+geom_point()

##instalacja RESHAPE2
install.packages("reshape2")
library(reshape2)
library(ggplot2)


melted = melt(rawdata, id.vars ="Subject", measure.vars = c("a","c","d","e","f","g","j","k"))
print(melted)


myPlot <- ggplot(melted, aes(x=variable, y=value, col=Subject, group=Subject)) + 
  geom_point() + geom_line()+ggtitle("Mój tytuł") +
  xlab("Sample")+ ylab("obserwacja")
print(myPlot)


ggsave(filename = "Myplot.pdf", plot=myPlot)
