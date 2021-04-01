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
