---
title: "reqspec"
author: "Elias Arildskov, Maiken Bak Poulsen and Camilla Kudahl"
---


## Functions:
- simSeq: simulates mutations based on branch length. Made function together. Maiken and Camilla tests and write documentation.
- simBranchFixed: help function (to simDNAseq) that simulates branchlengths from a population of fixed size. Made by Camilla and Maiken
- simBranchVar: help function (to simDNAseq) that simulates branchlengths from an exponentially growing population size. Made by Camilla and Maiken
- simBranchSudExpansion: help function (to simDNAseq) that simulates branchlengths from a suddenly expanded population size (aka. bottleneck). Made by Camilla and Maiken
- simDNAseq: simulates DNA sequences based on branchlengths (belonging to 1 of our 3 classes) using the simSeq (help-)function. Made by Camilla and Maiken
- Various functions that provides different properties of data. For example: site frequency spectrum (SFS), SNP matrix, total number of mutations (S_total), different ways of estimating mutation rate. Made by Elias.
- If time: visualization of ancestral tree. Made by everybody


## Classes
- Population with fixed size "fixedPop"
- Population with variable (exponentially growing) size "varPop"
- Population with variable (sudden expansion aka. bottleneck) size "sudExpansionPop"

Elias changes/specify print function for the different classes.

## Vignettes:

We make 2 vignettes. One for the functions that simulates data and one for the functions that analyzes data.
- Everybody is responsible for the vignette. Each person adds documentation for what they are working with. Elias creates the vignette.


## Variables:
Here we write what we call the different variables to ensure that they are consistent.

- n: the sample size
- SNP: the SNP matrix
- SFS: the site frequence spectrum
- branchLen: the vector of branchlengths in the following order (T_2,...,T_n)
- seqLen: the length of DNA sequences
- mutRate: the mutation rate 
- expRate: the rate of the exponentially growing population
- expansionTime: parameter determining the time of expansion
- proportion: parameter determining the fraction of the population size before the expansion

