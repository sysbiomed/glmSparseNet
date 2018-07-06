glmSparseNet
================

-   [Overview](#overview)
-   [Citation](#citation)
-   [Instalation](#instalation)
-   [Running](#running)
    -   [Network-based penalization](#network-based-penalization)
    -   [Survival Example using RNASeq data](#survival-example-using-rnaseq-data)
-   [Visualization tools](#visualization-tools)
    -   [Survival curves with `draw.kaplan`](#survival-curves-with-draw.kaplan)
    -   [Heatmap with results from Hallmarks of cancer](#heatmap-with-results-from-hallmarks-of-cancer)

> Elastic-Net models with additional regularization based on network centrality metrics

Overview
--------

`glmSparseNet` is a R package that generalizes sparse regression models when the features have a graph structure (e.g. genes), by including network-based regularizers. `glmSparseNet` uses the glmnet package, by including centrality measures of the network as penality factors. The current version implements regularization based on node degree, i.e. the strength and/or number of its associated edges, either by promoting hubs in the solution (glmDegree) or orphan genes (glmOrphan) in the solution. All the glmnet distribution families are supported, namely "gaussian", "poisson", "binomial", "multinomial", "cox", and "mgaussian".

It adds two new main functions called `network.glmnet` and `network.cv.glmnet` that extend both model inference and model selection via cross-validation with network-based regularization.

Below, we provide one example for survival analysis of .... using transcriptomic data of....RNA-seq tumor data.??? More information and RMD files are available in the folder ...???....where more extensive and complete examples are provided for logistic regressoin and... in ....

Citation
--------

Veríssimo, A., Oliveira, A.L., Sagot, M.-F., & Vinga, S. (2016). DegreeCox – a network-based regularization method for survival analysis. BMC Bioinformatics. 17(16): 449. <https://doi.org/10.1186/s12859-016-1310-4>

A more detailed description of the extensions here developed will be released soon in a manuscript (under preparation).

This package was developed by André Veríssimo, Eunice Carrasquinha, Marta B. Lopes and Susana Vinga under the project SOUND, funded from the European Union Horizon 2020 research and innovation program under grant agreement No. 633974.

Instalation
-----------

Bioconductor is necessary for the installation of this package.

``` r
source("https://bioconductor.org/biocLite.R")
biocLite('averissimo/loose.rock')
biocLite('glmSparseNet', siteRepos = 'https://sels.tecnico.ulisboa.pt/r-repos/')
```

Running
-------

To run the following examples, the next libraries are also needed:

``` r
library(futile.logger)
library(dplyr)
library(ggplot2)
library(reshape2)
library(MultiAssayExperiment)
library(survival)
library(glmnet)
library(loose.rock)
library(glmSparseNet)
```

### Network-based penalization

!!!!! explicar bem qual é o input e opcoes da package

This package extends the `glmnet` r-package with network-based regularization based on features relations. This network can be calculated from the data itself or using external networks to enrich the model.

There are 3 methods available to use data-dependant methods to generate the network:

1.  Correlation matrix with cutoff;
2.  Covariance matrix with cutoff; <!-- 1. Sparse bayesian networks using `sparsebn` package. -->

Alternatively, the network can be passed as an adjancency matrix or an already calculate metric for each node.

### Survival Example using RNASeq data

TODO:

-   integrar no exemplo de todas as funções core
-   exemplo com rede aleatória

We use an example data from TCGA Adrenocortical Carcinoma project with '92' patients and a reduced RNASeq data. See `MultiAssayExperiment::miniACC` for more information and details of the data.

There is some pre-processing needed to remove patients with invalid follow-up date or death date:

``` r
# load data
xdata <- miniACC

# build valid data with days of last follow up or to event
event.ix <- which(!is.na(xdata$days_to_death))
cens.ix  <- which(!is.na(xdata$days_to_last_followup))
surv_event_time <- array(NA, nrow(xdata@colData))
surv_event_time[event.ix] <- xdata$days_to_death[event.ix]
surv_event_time[cens.ix]  <- xdata$days_to_last_followup[cens.ix]

# Keep only valid individuals
#
# they are valid if they have:
#  - either a follow_up time or event time
#  - a valid vital_status (i.e. not missing)
#  - folloup_time or event_time > 0
valid.ix <- as.vector(!is.na(surv_event_time) & !is.na(xdata$vital_status) & surv_event_time > 0)
ydata <- data.frame(time      = surv_event_time[valid.ix], 
                    status    = xdata$vital_status[valid.ix], 
                    row.names = xdata$patientID[valid.ix])
```

Fitting the survival model using a correlation network with cutoff at 0.6.

``` r
# build response object for glmnet
fit3 <- network.cv.glmnet(xdata, ydata, family = 'cox', 
                          network = 'correlation', 
                          experiment.name = 'RNASeq2GeneNorm', 
                          alpha = .7,
                          nlambda = 1000,
                          network.options = network.options.default(cutoff = .6, 
                                                                    min.degree = 0.2,
                                                                    trans.fun = degree.heuristic))
plot(fit3)
```

![](README_files/figure-markdown_github/fit.surv-1.png)

Visualization tools
-------------------

### Survival curves with `draw.kaplan`

This function generates Kaplan-Meier survival model based on the coefficients of a Cox model. It separates two groups based on relative risk and display both the curves and results of Log-rank test.

``` r
xdata.reduced <- reduce.by.experiment(xdata, 'RNASeq2GeneNorm')
ydata.km <- ydata[rownames(xdata.reduced@colData),]
best.model.coef <- coef(fit3, s = 'lambda.min')[,1]
draw.kaplan(best.model.coef, t(assay(xdata[['RNASeq2GeneNorm']])), ydata.km, ylim = c(0,1))
```

    ## $pvalue
    ## [1] 2.334101e-09
    ## 
    ## $plot

![](README_files/figure-markdown_github/draw.kaplan-1.png)

    ## 
    ## $km
    ## Call: survfit(formula = survival::Surv(time, status) ~ group, data = prognostic.index.df)
    ## 
    ##            n events median 0.95LCL 0.95UCL
    ## Low risk  40      2     NA      NA      NA
    ## High risk 39     26   1105     579    2102

### Heatmap with results from Hallmarks of cancer

Search the non-zero coefficients in the results and query for known hallmarks of cancer.

``` r
hallmarks(names(best.model.coef)[best.model.coef > 0])$heatmap
```

![](README_files/figure-markdown_github/hallmarks-1.png)
