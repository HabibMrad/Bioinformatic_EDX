# R Test Script
# Lidia Lipinska
# March, 29th 2018
# The purpose of this script is to learn how to work in R


count <- 0
count

# wertory
primes <- c(1,2,3,4)
Names <- c("Jan", "Zbigniew", "Rysiu", "Dzordz")
Truth <- c(TRUE, FALSE, TRUE)

# data frame
organism <- c("czlowiek", "szympans", "drozdze")
chromosomes <- c(23,24,16)
multicellular <- c(TRUE, TRUE, FALSE)

OgranismTable <- data.frame(organism, chromosomes, multicellular)
OgranismTable

count <- count+1
count


#praca z wektorami
primes[1]

OgranismTable$organism
OgranismTable$organism[2]

# BASIC R: READING AND WRITING DATA

write.table(OgranismTable, file="MyData.csv", row.names = FALSE, 
            na="", col.names = FALSE, sep = ",")
getwd
getwd()
setwd("/home/lidka/ZabawyzR")
getwd()
write.table(OgranismTable, file="MyData2.csv", row.names = TRUE, 
            na="", col.names = TRUE, sep = ",")

#New Data Frame
NewDataFrame <- read.csv("MyData.csv", header = TRUE, sep=",")
NewDataFrame


#Manipulacja danymi

count <- count*count
count<- count-1
count <- count/5


#FOR-LOOPS and FUNCTIONs
if(OgranismTable$chromosomes[1] > 20) count=count+1
if(OgranismTable$chromosomes[2] > 20) count=count+1
if(OgranismTable$chromosomes[3] > 20) count=count+1

count <- 0
for (val in OgranismTable$chromosomes) {
  if(val > 20) count = count+1
}
print(count)

##
count<-0
for (i in 1:100) {
  if(i>20) count=count+1
}
print(count)

##WYKRESY

barplot(OgranismTable$chromosomes)
install.packages("ggplot2")


### WEEK 2
# Lidia Lipinska
# 2018-04-02

library(seqinr)
