### WEEK 2
# Lidia Lipinska
# 2018-04-02
getwd()
setwd("/home/lidka/ZabawyzR")

library(seqinr)
cox1 <- read.fasta(file="cox1.fasta", seqtype = "AA")
cox1

length(cox1)

cox1[2]
cox1[0]
cox1[4]

seq1 <- cox1[1]
seq1


## Ładowanie sekwencji z genbanku

install.packages("ape")
library(ape)
AB003468 <- read.GenBank("AB003468", as.character = "TRUE")
AB003468
write.dna(AB003468, file ="AB003468.fasta", format = "fasta", append = FALSE, nbcol = 6, colsep = " ", colw = 10)


##Retrieving  (pobieranie sekwencji z genbank)

install.packages("rentrez")
library(rentrez)
entrez_search(db="nucleotide", term="human superoxide dismutase")


# Lidia LIpinska  
## FASTA ANALYSIS
# 2018-04-07

CloningVector <- AB003468[[1]]
count <- count(CloningVector,1)
count
count(CloningVector,2)
count(CloningVector,3)

## proporcja par GC to AT
GC <- GC(CloningVector)
GC


## Calculating GC content by "windows" or short sequence, tuzakłądamy, że okno ma 200 długości
GCwindow <-seq(1,length(CloningVector)-200, by= 200)
#funkcja seq zeby znalezc diury w tych 200
GCwindow

## ile mamy chunksóœ
n<- length(GCwindow)
n
Chunks<- numeric(n)
n
Chunks


##FOR loop to compute GC per chunk

for (i in 1:n) {
  chunk<- CloningVector[GCwindow[i]:(GCwindow[i]+199)]
  chunkGC<-GC(chunk)
  print(chunkGC)
}

### COS dziwnego
for (i in 1:n) {
  chunk<- CloningVector[GCwindow[i]:(GCwindow[i]+199)]
  chunkGC<-GC(chunk)
  print(chunkGC)
  Chunks[i] <-chunkGC
}

plot(GCwindow,Chunks, type="b", xlab = "Nucleotide start position", ylab = "GC content")

# type="b" oznacza wykres z fajnymi połączonymi kropeczkami

## ille ikienko:

for (i in 1:n) {
  chunk<- CloningVector[GCwindow[i]:(GCwindow[i]+49)]
  chunkGC<-GC(chunk)
  print(chunkGC)
  Chunks[i] <-chunkGC
}

plot(GCwindow,Chunks, type="b", xlab = "Nucleotide start position", ylab = "GC content")

##okno 400
for (i in 1:n) {
  chunk<- CloningVector[GCwindow[i]:(GCwindow[i]+99)]
  chunkGC<-GC(chunk)
  print(chunkGC)
  Chunks[i] <-chunkGC
}

plot(GCwindow,Chunks, type="b", xlab = "Nucleotide start position", ylab = "GC content")


## Custom functions in R:A custom function in R is a way to define a piece of code that you can then invoke – much like you can
#invoke any other R function. (Funkcja niestandardowa)


## GC Custom-window plot function
#windowsice wielkość okna; inputseq funkcja do analizy
#For(l in 1:n){}
slidingwindowGCplot<- function(windowsize, inputseq)
{
  GCwindow<-seq(1,length(inputseq)-windowsize,by=windowsize)
  # Find the length of the GCwindow
  n<- length(GCwindow)
  #MAke a vector the same length but "blank" for us to fill
  Chunks <- numeric(n)
  for(i in 1:n) {
    chunk <- inputseq[GCwindow[i]:(GCwindow[i]+windowsize-1)]
    chunkGC<-GC(chunk)
    print(chunkGC)
    Chunks[i]<-chunkGC
    
  }
  plot(GCwindow, Chunks,type = "b", xlab = "Nucleotide start position", ylab = "GC content")
}

slidingwindowGCplot(174,CloningVector)
plot(GCwindow, Chunks,type = "b", xlab = "Nucleotide start position", ylab = "GC content")

plot(GCwindow, Chunks,type="b", xlab = "Nucleotide start position", 
     ylab = "GC content", main=paste("GC Plot with windowsize", windowsize))


slidingwindowGCplot<- function(windowsize, inputseq)
{
  GCwindow<-seq(1,length(inputseq)-windowsize,by=windowsize)
  # Find the length of the GCwindow
  n<- length(GCwindow)
  #MAke a vector the same length but "blank" for us to fill
  Chunks <- numeric(n)
  for(i in 1:n) {
    chunk <- inputseq[GCwindow[i]:(GCwindow[i]+windowsize-1)]
    chunkGC<-GC(chunk)
    print(chunkGC)
    Chunks[i]<-chunkGC
    
  }
  plot(GCwindow, Chunks,type="b", xlab = "Nucleotide start position",  ylab = "GC content", main=paste("GC Plot with windowsize", windowsize))
}



##    PROTEIN SEQUENCE STATISTICS

install.packages("Peptides")
library(Peptides)
aaComp(cox1[1])

aaComp(cox1)
aIndex(cox1)

charge(seq, pH=7,pKscale = "Lehninger")
charge(seq, pH = 7, pKscale = "Lehninger")
charge(cox1)

charge(seq="FLPVLAG",pH=7,pKscale = "EMBOSS")
hydrophobicity(seq)
hydrophobicity(cox1[1])
