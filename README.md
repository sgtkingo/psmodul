# psmodul

_Package of usefull functions for PS - Probability & Statistics course from students and teachers on VSB-TU Ostrava.
|| Balíček užitečných funkcí od studentů a učitelů VŠB-TU pro předmět PS - Pravděpodonost & Statistika._

## PackageSettings.R - Nastavení package

> Obsahuje funkce pro konfiguraci package.

- Seznam funkcí:

```
PSM.activateAllLibs <-function()
```

## DiscreteProbabilityDistribution.R - Rozdělení diskrétní pravděpodobnosti

> Obsahuje funkce pro výpočet pravděpodobnosti v oblasti Rozdělení diskrétní pravděpodobnosti jako jsou: **_Hypergeometric Distribution, Binomial Distribution, Negative Binomial Distribution a Poisson Distribution_**.

- Seznam funkcí:

```
DPD.hypergeo_disc <- function(x, N, M, n, logic = "<=", draw_plot = FALSE)
DPD.binom_disc <- function(x, n, PI, logic = "<=", draw_plot = FALSE)
DPD.nbinom_disc <- function(x, k, PI, logic = "<=", draw_plot = FALSE)
DPD.pois_disc <- function(x, t, LAMBDA, logic = "<=", draw_plot = FALSE)
```

## DiscreteRandomVariable.R - Diskrétní náhodná veličina

> Obsahuje funkce pro práci s diskérntí náhodnou veličinou: **_převod mezi různými formami reprezentace DNV, číselné charakteristiky (střední hodnota, rozptyl, směrodatná odchylka), výpočet pravděpodobnosti, transformace náhodné veličiny_**.

- Seznam funkcí­:

```
DRV.prob_to_dist <- function (x, p, calculate = FALSE)
DRV.dist_to_prob <- function (x, Fx, calculate = FALSE)
DRV.calculate_metric <- function(x, p)
DRV.get_mean <- function (x, p)
DRV.get_variance <- function(x, p, EX)
DRV.get_standard_deviation <- function(DX)
DRV.get_probability <- function(x, Fx, probabilityType, a, b = 0)
DRV.transform_probability <- function(x, p, y)
```
## ContinuousProbabilityDistribution.R - Rozdělení spojité pravděpodobnosti

> Obsahuje funkce pro výpočet pravděpodobnosti v oblasti Rozdělení spojité pravděpodobnosti jako jsou: **_Exponencional Distribution, Weibull Distribution, Normaly Distribution_**.

- Seznam funkcí:

```
CPD.exp_cont <- function(x, LAMBDA, logic = "<=", draw_plot = FALSE)
CPD.weib_cont <- function(x, LAMBDA, BETA, logic = "<=", draw_plot = FALSE)
CPD.norm_cont <- function(x, mu, sigma, logic = "<=", draw_plot = FALSE)
```

## ContinuousRandomVariable.R - Spojitá náhodná veličina

> Obsahuje funkce pro práci se spojitou náhodnou veličinou: **_převod distribuční funkce na hustotu pravděpodobnosti, číselné charakteristiky (střední hodnota, rozptyl, směrodatná odchylka), výpočet pravděpodobnosti, transformace náhodné veličiny_**.

- Seznam funkcí­:

```
CRV.get_constant <- function(fx, min, max)
CRV.dist_to_dens <- function(Fx)
CRV.calculate_metric <- function(fx, min, max)
CRV.get_mean <- function (fx, min, max)
CRV.get_variance <- function(fx, min, max, EX)
CRV.get_standard_deviation <- function(DX)
CRV.get_probability <- function(fx, min, max, probabilityType, a, b = 0)
CRV.transform <- function(fx, min, max, y)
```

## ExplorationDataAnalysis.R - Explorační datová analýza

> Obsahuje funkce pro explorační datovou analýzu: **_práci se soubory, ověřenéní N datasetu, statistické charakteristiky, boxploty a odlehlá pozorování+hranice_**.

- Seznam funkcí­:

```
EDA.setWorkingDirectoryToSource <-function(subdir="")
EDA.readExcel <-function(file="", sheet="", colNames=NULL)
EDA.isNorm <- function(skewness, kurtosis)
EDA.getStats <- function(data, colName)
EDA.getStatsWithGroupBy <- function(data, colName, groupColName)
EDA.getBordersAndOutValues_AsBoxPlot <- function(data, title="No title", y_name="Y", x_name="", color="lightblue", size=1.0)
EDA.getBordersAndOutValues_AsBoxPlot_ByGroup <- function(data, group, title="No title", y_name="Y", x_name="", color="lightblue")
```
