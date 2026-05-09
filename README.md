
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![R-CMD-check](https://github.com/Nicolas-Schmidt/preDose2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Nicolas-Schmidt/preDose2/actions/workflows/R-CMD-check.yaml)

# preDose <img align="right" src = "man/figures/logo_new.png" width="135px">

# preDose: An R-package for Robust External Evaluation of popPKPD Models.

preDose is a free and open source package that automatize the process of
*external evaluation process* using an independent dataset from which
the original popPKPD model was developed.

Currently, the user can use the package choose to use the package based
on the [mapbayr](https://github.com/FelicienLL/mapbayr) package

You can perform an external evaluation for a single model from :

- a population PKPD model (coded in
  [mrgsolve](https://github.com/metrumresearchgroup/mrgsolve),
- a data set with concentrations (NONMEM format)

## Installation

You can install the development version of preDose from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("Martin-Umpierrez/preDose")
```

## Example

This document presents a case of study to illustrates the external
evaluation of two population pharmacokinetic model of Tacrolimus:

- **Han et al. (2011)**  
  *Prediction of the tacrolimus population pharmacokinetic parameters
  according to CYP3A5 genotype and clinical factors using NONMEM in
  adult kidney transplant recipients.*  
  European Journal of Clinical Pharmacology, 69(1), 53–63.

- **Zuo et al. (2013)**  
  *Effects of CYP3A4 and CYP3A5 polymorphisms on tacrolimus
  pharmacokinetics in Chinese adult renal transplant recipients: a
  population pharmacokinetic analysis.* Pharmacogenet Genomics. 2013
  May;23(5):251-61. doi: 10.1097/FPC.0b013e32835fcbb6. PMID: 23459029.

The external evaluation was performed using data from an internal study
conducted between 2022 and 2024 by Umpierrez et al.

``` r
library(preDose)
## basic example code
```

#### 1) Properly code you model

##### 1.1) Code your model in mrgsolve format.

Models can be defined in two different ways:

- **Inline model code**, which can be stored and accessed globally
  within the project.
- **External `.cpp` files**, which can be read and compiled directly by
  *mrgsolve*.

Additionally, the package provides several pre-coded models that can be
accessed using the `list_models()` function, allowing users to readily
explore and apply available population pharmacokinetic models.

``` r
Han_etal_test<-
  '$PROB
# One Comparment Model with first order absorption- Ka is FIXED
$GLOBAL
#define CP (CENT/iV)
$CMT  @annotated
EV   : Extravascular compartment
CENT : Central compartment#two compt model with first order absorption

$PARAM @annotated 
CL  :  24.13 : Clearance for CYP3A5*3*3
V  :  716 : central volume
KA  : 4.5 : absorption rate constant
ETA1 : 0 : IIVCl (L/h)
ETA2 : 0 : IIVV (L)

$PARAM @annotated @covariate
POD    : 0   : COV POST OPERATIVE DAY
HCT      : 0  : COV HCH
WT      : 0  : COV WT
CYP3A5      : 0  : Polimorfismo CYP3A5
OCC     : -99  : Occasion, shall be passed by dataset imported

$ODE
dxdt_EV = -iKA*EV;
dxdt_CENT = iKA*EV  - iCL*CP;

$MAIN
##CYP3A5 effect on Cl##

double HM = 1.186 ;  ####Rapid Metabolizer ####
double IM = 1.13 ;  ####Intermediate Metabolizer ####
double PM = 1 ;  ####Poor Metabolizer####

if(CYP3A5==1) double CL_EFFECT = HM ;
if(CYP3A5==2) CL_EFFECT = IM ;
if(CYP3A5==3) CL_EFFECT = PM ;

double CL_HCT1 = 1.3458 ; ##Effect of HCT< 33
double CL_HCT2 = 1.124 ;  ##Effect of HCT >33

if(HCT< 33) double CL_HCT = CL_HCT1 ;
if(HCT >= 33) CL_HCT = CL_HCT2 ;

double CL_POD = - 0.00762 ;

double iCL =  CL *exp(ETA(1) + ETA1)* pow(POD, CL_POD) * CL_EFFECT * CL_HCT ;  
double iV =  V *exp(ETA(2) + ETA2) * exp (0.355*WT/59.025) ;  
double iKA =  KA ;    


$OMEGA @name IIV 
0.248 
0.237

$SIGMA  @name SIGMA @annotated
ADD : 0 : ADD residual error
PROP : 0.16 : Proportional residual error


$TABLE
double IPRED = CENT/iV;
double DV = IPRED * (1 + PROP) ;

$CAPTURE @annotated
CP : Plasma concentration (mass/volume)
iCL :  Clearance
iV : :Central Volume
iKA : KA: absorption rate constant
EVID : EVENT ID
DV : PREDICCION
OCC: OCCASION

               '
```

#### 2) Import your external data

Import the data set in NM-TRAN-formatted datasets Ensure that the
dataset structure aligns with the required format for proper processing.

``` r
data("tacrolimus_pk1_kidney", package = "preDose")  # Cargar dataset desde el paquete
head(tacrolimus_pk1_kidney)  # Ver primeras filas
#> # A tibble: 6 × 30
#>      ID   OCC    DD   AMT  TIME   POD    DV  EVID   CMT   MDV    II    SS Creatinine   SCR  eGFR ClCrea   AGE   SEX    WT   HCT CYP3A5
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>      <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
#> 1     1     1   6    3000   168     7   0       1     1     1    12     1       5.3   469.  9.16   9.80    49     0  48.4  29.9      3
#> 2     1     1   6       0   168     7   9.4     0     2     0     0     0       5.3   469.  9.16   9.80    49     0  48.4  29.9      3
#> 3     1     2   6.5  3250   264    11   0       1     1     1    12     1       4.17  369. 12.1   12.5     49     0  48.4  28.7      3
#> 4     1     2   6.5     0   264    11   8.4     0     2     0     0     0       4.17  369. 12.1   12.5     49     0  48.4  28.7      3
#> 5     1     3   7.5  3750   360    15   0       1     1     1    12     1       3.56  315. 14.5   15.1     49     0  50.2  26.9      3
#> 6     1     3   7.5     0   360    15   8.4     0     2     0     0     0       3.56  315. 14.5   15.1     49     0  50.2  26.9      3
#> # ℹ 9 more variables: EXPRESSION <dbl>, PDN_DOSE <dbl>, PDNXWT <dbl>, Heigth <dbl>, Height..m. <dbl>, BSA <dbl>, BMIcalc <dbl>, LBW <dbl>,
#> #   DMELITU <dbl>
```

#### 3) External model evaluation with `exeval_ppk()`

External model evaluation is performed using the `exeval_ppk()`
function.  
This function automates the full evaluation workflow, including:

- Calculation of individual parameters
- Model updating based on individual estimates
- Simulation of predicted concentrations
- Comparison between observed and predicted data

##### Detailed workflow

A step-by-step description of the external evaluation procedure  
(i.e., individual parameter estimation, model updating, and
simulation)  
is provided in the corresponding **vignette**.

Please refer to the vignette for a detailed and reproducible explanation
of each step.

``` r
res.1 <- exeval_ppk(model = Han_etal_test,
                    drug_name = "Tacrolimus",
                    model_name = "Han_etal_2011",
                    tool = "mapbayr",
                    data = subset(tacrolimus_pk1_kidney, ID <11),
                    evaluation_type= "Progressive",
                    assessment = 'Bayesian_forecasting')  # Cargar dataset desde el paquete
print(res.1)
#> ===================================
#> Data summary
#>           argument value
#> 1          Num IDs    10
#> 2     Observations    30
#> 3 Max Num Occasion     6
#> ===================================
#> 
#> ===================================
#> Evaluation information
#>     argument                value
#> 5  Drug Name           Tacrolimus
#> 6 Model Name        Han_etal_2011
#> 7 Evaluation          Progressive
#> 8 Assessment Bayesian_forecasting
#> ===================================
#> 
#> ===================================
#> Evaluation metrics
#> # A tibble: 5 × 8
#>     OCC rBIAS rBIAS_lower rBIAS_upper MAIPE rRMSE  IF20  IF30
#>   <dbl> <dbl>       <dbl>       <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     2 14.5        -7.96       37.0   31.5  38.5  23.1  69.2
#> 2     3  5.86      -27.5        39.2   40.5  44.6  10    20  
#> 3     4 17.0        -8.49       42.4   35.1  43.9  38.5  53.8
#> 4     5 -8.59      -21.0         3.87  16.7  18.6  70    90  
#> 5     6 -8.72      -28.5        11.1   23.8  27.7  50    60  
#> ===================================
```

#### 4) Make Some Important Plots to compare metrics

Several plot types are available to visualize model performance using
the `metrics_plot()` function. \##### 4.1) Bias BarPlot

``` r

plot1 = metrics_plot(res.1,
             type = "bias_barplot")

print(plot1)
```

<img src="man/figures/README-plot1-1.png" width="100%" /> \##### 4.2)
Bias boxplot

``` r

plot2 = metrics_plot(res.1,
             type = "bias_boxplot")

print(plot2)
```

<img src="man/figures/README-plot2-1.png" width="100%" />

##### 4.3) Relative Error Distribution by OCC

``` r

plot3 = metrics_plot(res.1,
             type = 'error_plot')

print(plot3)
```

<img src="man/figures/README-plot3-1.png" width="100%" />

#### 5) Import Models and assess the predicitve performance

##### 5.1) New Models and Estimations

``` r
source("inst/model_examples/ZuoX_etal_2013.R")

res.2 <- exeval_ppk(model = ZuoX_etalfull_noCYP3A4,
                    drug_name = "Tacrolimus",
                    model_name = "Zuo_etal",
                    tool = "mapbayr",
                    data = subset(tacrolimus_pk1_kidney, ID <11),
                    evaluation_type= "Progressive",
                    assessment = 'Bayesian_forecasting')  # Cargar dataset desde el paquete
print(res.2)
#> ===================================
#> Data summary
#>           argument value
#> 1          Num IDs    10
#> 2     Observations    30
#> 3 Max Num Occasion     6
#> ===================================
#> 
#> ===================================
#> Evaluation information
#>     argument                value
#> 5  Drug Name           Tacrolimus
#> 6 Model Name             Zuo_etal
#> 7 Evaluation          Progressive
#> 8 Assessment Bayesian_forecasting
#> ===================================
#> 
#> ===================================
#> Evaluation metrics
#> # A tibble: 5 × 8
#>     OCC  rBIAS rBIAS_lower rBIAS_upper MAIPE rRMSE  IF20  IF30
#>   <dbl>  <dbl>       <dbl>       <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     2 102.         67.4        136.  102.  116.    0    7.69
#> 2     3  74.6        13.0        136.   80.9 111.   20   40   
#> 3     4  41.9        -2.32        86.2  58.5  81.9  23.1 38.5 
#> 4     5  20.7        -4.55        45.9  29.6  39.4  50   60   
#> 5     6   1.53      -19.5         22.6  25.9  28.0  40   70   
#> ===================================
```

##### 5.2) Compare Models

###### 5.2.1) By Plotting:

1.  Use `combine_metrics()` to generate a summary of all evaluation
    metrics across the tested models.  
2.  Visualize and compare model performance using the `plot_combined()`
    function.

``` r

###### Generate a summary of metrics for all tested models
model_list <- list(list(model_name="Han_etal", metrics_list=res.1$metrics),
                   list(model_name="Zuo_etal", metrics_list=res.2$metrics))

#### Use combine_metrics() function with the summary created
combined_results<- combine_metrics(model_list)

#### Make the Plot! 
plot_comparrison <- plot_combined(combined_results,
                                  'bias_barplot')

print(plot_comparrison)
```

<img src="man/figures/README-plot_comparisson-1.png" width="100%" />

###### 5.2.2) Select models according to a specific evaluation metric and threshold using select_best_models() function

The `select_best_models()` function selects the best models from a
dataframe of combined metrics based on a specified ranking metric.  
It requires a dataframe containing model evaluation metrics and the name
of the metric to use for ranking.  
Optionally, you can specify a particular occasion to focus on and the
number of top models to select.

``` r

Best_fit <- select_best_models(combined_results, metric = "rBIAS",
                               top_n = 1)

print(Best_fit)
#> # A tibble: 5 × 9
#>     OCC rBIAS rBIAS_lower rBIAS_upper MAIPE rRMSE  IF20  IF30 Model   
#>   <dbl> <dbl>       <dbl>       <dbl> <dbl> <dbl> <dbl> <dbl> <chr>   
#> 1     2 14.5        -7.96       37.0   31.5  38.5  23.1  69.2 Han_etal
#> 2     3  5.86      -27.5        39.2   40.5  44.6  10    20   Han_etal
#> 3     4 17.0        -8.49       42.4   35.1  43.9  38.5  53.8 Han_etal
#> 4     5 -8.59      -21.0         3.87  16.7  18.6  70    90   Han_etal
#> 5     6 -8.72      -28.5        11.1   23.8  27.7  50    60   Han_etal
```
