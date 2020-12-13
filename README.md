[![Build Status](https://travis-ci.com/Dscientist20/Project.svg?branch=main)](https://travis-ci.com/Dscientist20/Project)
# Project_Algorithms
# Topic : Ruptures detection Algorithms

### Group 8

#### Data Science, Evry Paris-Saclay University

### December 12, 2020
> [Introduction](#qs)

> [The 2 algorithms at fixed data length](#com)

> [Time complexity](#time)

<a id="qs"></a>

##Introduction : 

The Ruptures Detection is a regression problem of Analysis Statistic, in an aim to identify when the distribution change, we make an estimation.   

# Project
ruptures detection Algorithms
# Package installation
You first need to install the devtools package, it can be done easily from Rstudio. We install the package from Github (remove the # sign):

#devtools::install_github("tungle92/Project_Algorithms")
library(Algo)

## Optimal Partitioning : 

Roughly speaking, it computes the cost of all subsequences of a given signal. The number of computed costs is of the order O(Kn2), where K is the number of change points and n the number of samples. This has to be multiplied by the computational cost of computing the approximation error on one sub-sequence. Consequently, piecewise constant models are significantly faster than linear or autoregressive models.

## Pruned Exact Linear Times (PELT) : 

Because the enumeration of all possible partitions impossible, the algorithm relies on a pruning rule. Many indexes are discarded, greatly reducing the computational cost while retaining the ability to find the optimal segmentation. The implementation follows [BKFE12]. In addition, under certain conditions on the change point repartition, the computational complexity is linear on average.
