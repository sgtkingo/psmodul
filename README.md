# psmodul

_Package of usefull functions for PS - Probability & Statistics course from students and teachers on VSB-TU Ostrava.
|| Balíček užitečných funkcí od studentů a učitelů VŠB-TU pro předmět PS - Pravděpodonost & Statistika._

## DiscreteProbabilityDistribution.R - Rozdělení diskrétní pravděpodobnosti

> Obsahuje funkce pro výpočet pravděpodobnosti v oblasti Rozdělení diskrétní pravděpodobnosti jako jsou: **_Hypergeometric Distribution, Binomial Distribution, Negative Binomial Distribution a Poisson Distribution_**.

- Seznam funkcí:

```
hypergeo_disc <- function(x, N, M, n, logic = "<=")
binom_disc <- function(x, n, PI, logic = "<=")
nbinom_disc <- function(x, k, PI, logic = "<=")
pois_disc <- function(x, t, LAMBDA, logic = "<=")
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
