# Project_Algorithms
## Topic : Ruptures detection Algorithms

### Group 8

#### Data Science, Evry Paris-Saclay University

##### December 12, 2020
> [Introduction](#qs)

> [The 2 algorithms](#com)

> [Package installation](#pac)

> [Time complexity Comparaison](#time)

> [References](#ref)

<a id="qs"></a>

# Introduction : 

The `Changepoint` is a R package developed for the detection of changepoints as a project of the **algorithmic M2 courses in Data Science master's program at Evry Paris-Saclay University**. This package includes two algorithms: **Optimal Partitioning (OP)** and **Pruned Exact Linear Times (PELT)** implemented in R and Rcpp.

Details on the detection of changepoints can be found on [its wikipedia page](https://fr.wikipedia.org/wiki/Détection_de_ruptures). This image provides a graphical representation of its mechanisms.
### Example of changes in mean.

![](README_files/D_moyenne.PNG)

<a id="pac"></a>

# Package installation :
You first need to install the devtools package, it can be done easily from Rstudio. We install the package from Github :

devtools::install_github("tungle92/Project_Algorithms")

library(Changepoints)

<a id="com"></a>

# The 2 algorithms

### Optimal Partitioning (OP) : 

This algorithm computes the cost of all subsequences of a given data. The number of computed costs is of the order O(n2). This has to be multiplied by the computational cost of computing the approximation error on one sub-sequence.

### Pruned Exact Linear Times (PELT) : 

The algorithm relies on a pruning rule. Many indexes are discarded, greatly reducing the computational cost while retaining the ability to find the optimal segmentation. In addition, under certain conditions of the changepoint repartition,  on average, the computational complexity is linear.


### A first simple test :

We take x as simple vector,and the penalty parameter `beta` equals 0.1.
We've implemeted 2 algorithms :

-   `OP`
-   `PELT`

They all have two arguments: the initial vector data `x` and `beta`.

```{r}
graph_cp <- function(x, cps){
  plot(x)
  abline(v=cps+0.1, col='red')
}
n=100
m = sample(n/10)
v <- sample(m)
w = sample(m)
x = rep(v,w*n/sum(w))+runif(length(rep(v,w*n/sum(w))))

cps = OP(x, beta = 0.1)
cps1 = PELT(x, beta = 0.1)
par(mfrow=c(1,2))
graph_cp(x, cps)
graph_cp(x, cps1)
```
 
We obtain the following result given `x` and `beta` above

![](README_files/Firsexample.PNG)

<a id="time"></a>

# Time complexity Comparison :

### a) Simulation function :
```{r}
one.simu <- function(n, type = "sample", func = "OP", beta = 1)
{
  m = sample(n/10)
  if(type == "sample"){v <- sample(m)}else{v <- m:1}
  w = sample(m)
  x=rep(v,w*n/sum(w))+runif(length(rep(v,w*n/sum(w))))
  f=get(func)
  t <- system.time(f(x, beta))[[1]]
  return(t)
}
one.simu(100, func = 'OP', beta =1)
```
```{r}
one.simu(100, func = 'OP')
```
  ##### [1] 0.12

```{r}
one.simu(100, func = 'PELT')
```
  ##### [1] 0.02


### b) OP Time complexity graph:

```{r}
nbSimus <- 10
vector_n <- seq(from = 100, to = 1000, length.out = nbSimus)
nbRep <- 2
res_cp <- data.frame(matrix(0, nbSimus, nbRep + 1))
colnames(res_cp) <- c("n", paste0("Rep",1:nbRep))

j <- 1
for(i in vector_n)
{
  res_cp[j,] <- c(i, replicate(nbRep, one.simu(i, func = 'OP')))  
  j <- j + 1
}

res <- rowMeans(res_cp[,-1])
plot(vector_n, res, type = 'b', xlab = "data length", ylab = "mean time in seconds")
```
![](README_files/graph1.PNG)


### c) PELT Time complexity graph: 

```{r}
nbSimus <- 10
vector_n <- seq(from = 100, to = 1000, length.out = nbSimus)
nbRep <- 2
res_cp <- data.frame(matrix(0, nbSimus, nbRep + 1))
colnames(res_cp) <- c("n", paste0("Rep",1:nbRep))

j <- 1
for(i in vector_n)
{
  res_cp[j,] <- c(i, replicate(nbRep, one.simu(i, func = 'PELT')))  
  j <- j + 1
}

res <- rowMeans(res_cp[,-1])
plot(vector_n, res, type = 'b', xlab = "data length", ylab = "mean time in seconds")
```

![](README_files/graph2.PNG)

### Some comparisons:
```{r}
nbSimus <- 100
n <- 1000
time1 <- 0; time2 <- 0; time3 <- 0; time4 <- 0

for(i in 1:nbSimus){time1 <- time1 + one.simu(n, func = "OP")}
for(i in 1:nbSimus){time2 <- time2 + one.simu(n, func = "PELT")}
for(i in 1:nbSimus){time3 <- time3 + one.simu(n, func = "OP_rcpp")}
for(i in 1:nbSimus){time4 <- time4 + one.simu(n, func = "PELT_rcpp")}
```
Rcpp is faster than R
```{r}
time1/time3
```
[1] 4.476015
```{r}
time2/time4
```
[1] 40.26923

PELT is faster than OP
```{r}
time1/time2
```
[1] 46.52149

```{r}
time3/time4
```
[1] 418.5385

<a id="ref"></a>
# References
Killick, R., Fearnhead, P. and Eckley, I.A., *Optimal detection of changepoints with a linear computational cost*. Journal of the American Statistical Association, 107(500), 1590-1598.
