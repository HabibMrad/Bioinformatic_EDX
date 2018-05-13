### WEEK 3
# Lidia Lipinska
# 2018-05-08
# Dopasowywanie par zasad w R

getwd()
setwd("/home/lidka/ZabawyzR")
 #Fetch latest Bioconductor packages
source("https://bioconductor.org/biocLite.R")
biocLite("Biostrings")
library(Biostrings)
library(seqinr)
prokaryotes <- read.fasta(file="prok.fasta", seqtype="DNA")
prokaryotes


# tworzenie sekwencji pojedynczych

seq1 <- as.character(prokaryotes[[1]])
seq1
seq1 = paste(seq1, collapse="")
seq1

seq2 <- as.character(prokaryotes[[2]])
seq2 = paste(seq2, collapse = "")
seq2

## wisienka na torcie - porownujemy sekencje
pairwiseAlignment(pattern = seq2, subject = seq1)
pairwiseAlignment(pattern = seq1, subject = seq2)


#zapisywanie wyniku dopasowania sekwencji:
pairalign <- pairwiseAlignment(pattern = seq1, subject = seq2)
summary(pairalign)


#konwersja naszego wyniku do formatu FASTA

pairalignString = BStringSet(c(toString(subject(pairalign)), toString(pattern(pairalign))))
pairalign
pairalignString

writeXStringSet(pairalignString, "aligned.txt", format = "FASTA")


#Pairwise Sequences: Dot Plots
coxgenes <- read.fasta(file = "cox1multi.fasta", seqtype = "AA")
cox1 <- as.character(coxgenes[[1]])
cox1
cox1 = paste(cox1, collapse = "")
cox1
cox2 <- as.character(coxgenes[[2]])
cox2 = paste(cox2, collapse = "")
cox2




pairalign_cox <- pairwiseAlignment(pattern = cox1, subject = cox2)
summary(pairalign_cox)

## wykres
dotPlot(cox1,cox2,main="Human vs Mouse Cox1 Dotplot")

dotPlot(cox1,cox2,wsize = 3,wstep=3, nmatch=3,
        main="Human vs Mouse Cox1 Dotplot\nwsize = 3, wstep = 3, nmatch = 3")

##kolejny Dotplot, tym razem dla pierwszych 100 aminokwasóœ

dotPlot(cox1[1:100],cox2[1:100],wsize = 3,wstep=3, nmatch=3,
        main="Human vs Mouse Cox1 first 100 AA Dotplot\nwsize = 3, wstep = 3, nmatch = 3")


## Lidia Lipińska
## 2018-05-09
## Multiple Seqense Alignment (MSA)

source("https://bioconductor.org/biocLite.R")
biocLite("Biostrings")
n
biocLite("msa")
library(msa)
n
coxAA <- readAAStringSet("cox1multi.fasta")
coxAA
prokDNA <- readDNAStringSet("prok.fasta")
prokDNA

coxAligned <- msa(coxAA)
coxAligned
prokALigned <- msa(prokDNA)
prokALigned

print(prokALigned, show="complete")
print(coxAligned, show="complete")


## Multiple Sequence Alignments ALgorithms

msa(prokDNA, "ClustalW")
msa(prokDNA, "ClustalOmega")
msa(prokDNA, "Muscle")

## Exporting MSAs


