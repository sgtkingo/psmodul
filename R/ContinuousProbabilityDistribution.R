#Seznam logickych operandu
logicOperands <- c("=","<=","<",">",">=")

#' Funkce pro vypocet hodnoty Exponencional Distribution
#' @author Konecny Jiri (kon0327)
#'
#' @param x = hodnota NV
#' @param LAMBDA = vyskytovost / casovy usek
#' @param logic = "<=" P(X <= x), ">=" P(X >= x), "<" P(X < x), ">" P(X > x), "=" P(X = x)
#' @param q = % qvantil
#' @param draw_plot = vykreslit graf?
#' @return Vraci \code{result}:double jako hodnotu pravdepodobnosti
#' @examples
#' CPD.exp_cont(x=1, LAMBDA=2, "<=")
CPD.exp_cont <- function(x, LAMBDA, logic = "<=", q = 0.0, draw_plot = FALSE){
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
          "<" = { reverse_logic = TRUE },
          ">" = { reverse_logic = FALSE },
          ">=" = { reverse_logic = FALSE }
  )
  if( q > 0 ){
    type = 'q'
  }
  switch (type,
          'd'= { result = dexp(x, LAMBDA)},
          'p'= { result = pexp(x, LAMBDA, lower.tail = reverse_logic)},
          'q'= { result = qexp(q, LAMBDA)}
  )
  if (draw_plot){
    plot(x, result, type='l', ylab = "Fx")
    grid()
  }

  return (result)
}

#' Funkce pro vypocet hodnoty Weibull Distribution
#' @author Konecny Jiri (kon0327)
#'
#' @param x = hodnota NV
#' @param LAMBDA = vyskytovost / casovy usek
#' @param BETA = obdob�
#' @param logic = "<=" P(X <= x), ">=" P(X >= x), "<" P(X < x), ">" P(X > x), "=" P(X = x)
#' @param q = % qvantil
#' @param draw_plot = vykreslit graf?
#' @return Vraci \code{result}:double jako hodnotu pravdepodobnosti
#' @examples
#' CPD.weib_cont(x=1, LAMBDA=2, BETA=1.5, "<=")
CPD.weib_cont <- function(x, LAMBDA, BETA, logic = "<=", q = 0.0, draw_plot = FALSE){
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
          "<" = { reverse_logic = TRUE },
          ">" = { reverse_logic = FALSE },
          ">=" = { reverse_logic = FALSE }
  )
  if( q > 0 ){
    type = 'q'
  }
  switch (type,
          'd'= { result = dweibull(x, shape=BETA, scale = 1/LAMBDA)},
          'p'= { result = pweibull(x, shape=BETA, scale = 1/LAMBDA, lower.tail = reverse_logic)},
          'q'= { result = qweibull(q, shape=BETA, scale = 1/LAMBDA)}
  )
  if (draw_plot){
    plot(x, result, type='l', ylab = "Fx")
    grid()
  }

  return (result)
}


#' Funkce pro vypocet hodnoty Normal Distribution
#' @author Konecny Jiri (kon0327)
#'
#' @param x = hodnota NV
#' @param mu = st�edn� hodnota
#' @param sigma = st�edn� odchylka
#' @param logic = "<=" P(X <= x), ">=" P(X >= x), "<" P(X < x), ">" P(X > x), "=" P(X = x)
#' @param q = % qvantil
#' @param draw_plot = vykreslit graf?
#' @return Vraci \code{result}:double jako hodnotu pravdepodobnosti
#' @examples
#' CPD.norm_cont(x=1, mu=1, sigma=0.5, "<=")
CPD.norm_cont <- function(x, mu, sigma, logic = "<=", q = 0.0, draw_plot = FALSE){
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
          "<" = { reverse_logic = TRUE },
          ">" = { reverse_logic = FALSE },
          ">=" = { reverse_logic = FALSE }
  )
  if( q > 0 ){
    type = 'q'
  }
  switch (type,
          'd'= { result = dnorm(x, mean = mu, sd = sigma)},
          'p'= { result = pnorm(x,  mean = mu, sd = sigma, lower.tail = reverse_logic)},
          'q'= { result = qnorm(q, mean = mu, sd = sigma)}
  )
  if (draw_plot){
    plot(x, result, type='l', ylab = "Fx")
    grid()
  }

  return (result)
}

