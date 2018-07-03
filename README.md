Network-base regularization
================

-   [Install](#install)
-   [Overview](#overview)

> Elastic-Net models with additional regularization based on network centrality metrics

Install
-------

Bioconductor is necessary for the installation of this package.

``` r
source("https://bioconductor.org/biocLite.R")
biocLite('averissimo/network.cox', dependencies=TRUE, build_vignettes=FALSE)
```

Overview
--------

This package extends the `glmnet` r-package with network-based regularization based on features relations. This network can be calculated from the data itself or using external networks to enrich the model.

It adds two new functions called `network.glmnet` and `network.cv.glmnet` that extend both model inference and model selection via cross-validation with network-based regularization.

There are 3 methods available to use data-dependant methods to generate the netork:

1.  Correlation matrix with cutoff;
2.  Covariance matrix with cutoff;
3.  Sparse bayesian networks using `sparsebn` package.

Alternatively, the network can be passed as a adjancency matrix or an already calculate metric for each node.

The example below, shows random datasets being generated and `network.glmnet` new function being called.

``` r
library(network.cox)
# Gaussian
x <- matrix(rnorm(100*20),100,20)
y <- rnorm(100)
fit1 <- network.glmnet(x,y, 'correlation')
plot(fit1)
```

![](/ssd_home/averissimo/work/rpackages/network.cox/README_files/figure-markdown_github/unnamed-chunk-6-1.png)

The result can be used with all functions available to glmnet objects, such as `predict`, `coef` or plot

``` r
predicted <- predict(fit1, newx=x[1:10,],s=c(0.01,0.005))
```

    ## [INFO] Observed vs. Predicted
    ## 
    ##           Observed  lambda_0.01 lambda_0.005
    ##  [1,] -1.410558566  0.074584354   0.07584066
    ##  [2,]  0.255234485  0.007575141   0.02247735
    ##  [3,]  0.455632366  0.456942649   0.48661460
    ##  [4,]  1.022373484 -0.107534364  -0.14260392
    ##  [5,] -0.483326213 -0.651332900  -0.70926358
    ##  [6,] -0.001113666  0.378602959   0.39333629
    ##  [7,]  1.084494649 -0.219241463  -0.21245116
    ##  [8,] -0.386501633 -0.206891922  -0.21774425
    ##  [9,]  0.867004022  0.670178978   0.73440152
    ## [10,]  0.074823164  0.736168984   0.78142602

It also extends the new methods to the cross validation function with `network.cv.glmnet`

``` r
plot(network.cv.glmnet(x,y, 'covariance'))
```

![](/ssd_home/averissimo/work/rpackages/network.cox/README_files/figure-markdown_github/unnamed-chunk-9-1.png)
