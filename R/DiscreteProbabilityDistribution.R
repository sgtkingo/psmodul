#Seznam logickych operandu
logicOperands <- c("=","<=","<",">",">=")
#' Funkce pro vypocet hodnoty Hypergeometric Distribution
#' @author Konecny Jiri (kon0327)
#'
#' @param x = hodnota NV
#' @param N = pocet vsech prvku
#' @param M = pocet "spravnych" prvku
#' @param n = pocet vyberu prvku
#' @param logic = "<=" P(X <= x), ">=" P(X >= x), "<" P(X < x), ">" P(X > x), "=" P(X = x)
#' @param draw_plot = vykreslit graf?
#' @return Vraci \code{result}:double jako hodnotu pravdepodobnosti
#' @examples
#' hypergeo_disc(4,20,12,4,"=") - 20 piv(svetle, tmave), 12 svetlych, vybirame 4, chceme 4 svetle
#' hypergeo_disc(3,100,30,20) - 100 prvku(bile, cerne), 30 bilych, vybirame 20, chceme MAX 3 bile
#' hypergeo_disc(5,100,30,20,">=") - 100 prvku(bile, cerne), 30 bilych, vybirame 20, chceme alespon 5 bilych
DPD.hypergeo_disc <- function(x, N, M, n, logic = "<=", draw_plot = FALSE){
  result = 0.0
  reverse_logic = TRUE
  type = 'p'

  if (!(logic %in% logicOperands)){
    cat(sprintf("\t (!)ERROR: Unknown logic operand! (%s) \n", logic))
    return (0.0)
  }

  switch (logic,
          "=" = { type = 'd' },
          "<=" = { reverse_logic = TRUE },
          "<" = { reverse_logic = TRUE; x = x + 1; },
          ">" = { reverse_logic = FALSE },
          ">=" = { reverse_logic = FALSE; x = x - 1; }
  )
  switch (type,
          'd'= { result = dhyper(x, M, N-M, n)},
          'p'= { result = phyper(x, M, N-M, n, lower.tail = reverse_logic)},
  )
  if (draw_plot){
    plot(x, result, type='l', ylab = "Fx")
    grid()
  }

  return (result)
}

#' Funkce pro vypocet hodnoty Binomial Distribution
#' @author Konecny Jiri (kon0327)
#'
#' @param x = hodnota NV
#' @param n = pocet pokusu
#' @param PI = pravdepodobnost uspechu pokusu
#' @param logic = "<=" P(X <= x), ">=" P(X >= x), "<" P(X < x), ">" P(X > x), "=" P(X = x)
#' @param draw_plot = vykreslit graf?
#' @return Vraci \code{result}:double jako hodnotu pravdepodobnosti
#' @examples
#' binom_disc(4,10,1/6) - Hazime 10x kostkou, sance ze padnou nejvysse 4 sestky, sance na 1 sestku: 1/6
#' binom_disc(3,10,1/6,"=") - Hazime 10x kostkou, sance ze padnou presne 3 sestky, sance na 1 sestku: 1/6
#' binom_disc(5,10,1/6, ">") - Hazime 10x kostkou, sance ze padne 5 sestkek a vice, sance na 1 sestku: 1/6
DPD.binom_disc <- function(x, n, PI, logic = "<=", draw_plot = FALSE){
  result = 0.0
  reverse_logic = TRUE
  type = 'p'

  if (!(logic %in% logicOperands)){
    cat(sprintf("\t (!)ERROR: Unknown logic operand! (%s) \n", logic))
    return (0.0)
  }

  switch (logic,
          "=" = { type = 'd' },
          "<=" = { reverse_logic = TRUE },
          "<" = { reverse_logic = TRUE; x = x + 1; },
          ">" = { reverse_logic = FALSE },
          ">=" = { reverse_logic = FALSE; x = x - 1; }
  )
  switch (type,
          'd'= { result = dbinom(x, n, PI)},
          'p'= { result = pbinom(x, n, PI, lower.tail = reverse_logic)},
  )
  if (draw_plot){
    plot(x, result, type='l', ylab = "Fx")
    grid()
  }

  return (result)
}

#' Funkce pro vypocet hodnoty Negative Binomial Distribution
#' @author Konecny Jiri (kon0327)
#'
#' @param x = hodnota NV
#' @param k = pocet pokusu k dosazeni uspechu
#' @param PI = pravdepodobnost uspechu pokusu
#' @param logic = "<=" P(X <= x), ">=" P(X >= x), "<" P(X < x), ">" P(X > x), "=" P(X = x)
#' @param draw_plot = vykreslit graf?
#' @return Vraci \code{result}:double jako hodnotu pravdepodobnosti
#' @examples
#' nbinom_disc(x=120, k=100, PI=0.9, ">=") - Pravdepodobnost ze vyroste 120 seminek, kdyz vime ze 100 jich vyroste s 90% uspesnosti
DPD.nbinom_disc <- function(x, k, PI, logic = "<=", draw_plot = FALSE){
  result = 0.0
  reverse_logic = TRUE
  type = 'p'

  #uprava pro R definici
  x = x - k

  if (!(logic %in% logicOperands)){
    cat(sprintf("\t (!)ERROR: Unknown logic operand! (%s) \n", logic))
    return (0.0)
  }

  switch (logic,
          "=" = { type = 'd' },
          "<=" = { reverse_logic = TRUE },
          "<" = { reverse_logic = TRUE; x = x + 1; },
          ">" = { reverse_logic = FALSE },
          ">=" = { reverse_logic = FALSE; x = x - 1; }
  )
  switch (type,
          'd'= { result = dnbinom(x, k, PI)},
          'p'= { result = pnbinom(x, k, PI, lower.tail = reverse_logic)},
  )
  if (draw_plot){
    plot(x, result, type='l', ylab = "Fx")
    grid()
  }

  return (result)
}



#' Funkce pro vypocet hodnoty Poisson Distribution
#' @author Konecny Jiri (kon0327)
#'
#' @param x = hodnota NV
#' @param t = pocet casovych useku
#' @param LAMBDA = vyskytovost / casovy usek
#' @param logic = "<=" P(X <= x), ">=" P(X >= x), "<" P(X < x), ">" P(X > x), "=" P(X = x)
#' @param draw_plot = vykreslit graf?
#' @return Vraci \code{result}:double jako hodnotu pravdepodobnosti
#' @examples
#' pois_disc(x=1, t=2, LAMBDA=2, "<=")
DPD.pois_disc <- function(x, t, LAMBDA, logic = "<=", draw_plot = FALSE){
  result = 0.0
  reverse_logic = TRUE
  type = 'p'

  if (!(logic %in% logicOperands)){
    cat(sprintf("\t (!)ERROR: Unknown logic operand! (%s) \n", logic))
    return (0.0)
  }

  switch (logic,
          "=" = { type = 'd' },
          "<=" = { reverse_logic = TRUE },
          "<" = { reverse_logic = TRUE; x = x + 1; },
          ">" = { reverse_logic = FALSE },
          ">=" = { reverse_logic = FALSE; x = x - 1; }
  )
  switch (type,
          'd'= { result = dpois(x, t*LAMBDA)},
          'p'= { result = ppois(x, t*LAMBDA, lower.tail = reverse_logic)},
  )
  if (draw_plot){
    plot(x, result, type='l', ylab = "Fx")
    grid()
  }

  return (result)
}


