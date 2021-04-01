# psmodul
*Package of usefull functions for PS - Probability & Statistics course from students and teachers on VSB-TU Ostrava.
 || Balíček užitečných funkcí od studentů a učitelů VŠB-TU pro předmět PS - Pravděpodonost & Statistika.*

## DiscreteProbabilityDistribution.R - Rozdělení diskrétní pravděpodobnosti
  >Obsahuje funkce pro výpočet pravděpodobnosti v oblasti Rozdělení diskrétní pravděpodobnosti jako jsou: ***Hypergeometric Distribution, Binomial Distribution, Negative Binomial Distribution a Poisson Distribution***.
  - Seznam funkcí:
```
hypergeo_disc <- function(x, N, M, n, logic = "<=")
binom_disc <- function(x, n, PI, logic = "<=")
nbinom_disc <- function(x, k, PI, logic = "<=")
pois_disc <- function(x, t, LAMBDA, logic = "<=")
```

## DiscreteRandomVariable.R - Diskrétní náhodná veličina
  >Obsahuje funkce pro práci s diskérntí náhodnou veličinou: ***převod mezi různými formami reprezentace DNV, číselné charakteristiky (střední hodnota, rozptyl, směrodatná odchylka), výpočet pravděpodobnosti, transformace náhodné veličiny***.
  - Seznam funkcí­:
```
prob_to_dist <- function (x, p, calculate = FALSE)
dist_to_prob <- function (x, Fx, calculate = FALSE)
calculate_metric <- function(x, p)
get_mean <- function (x, p)
get_variance <- function(x, p, EX)
get_standard_deviation <- function(DX)
get_probability <- function(x, Fx, probabilityType, a, b = 0)
transform_probability <- function(x, p, y)
```
