

#Prevedeni pravdepodobnostni funkce na distribucni
#' @param x = hodnoty x
#' @param p = pravdepodobnostni funkce
#' @param calculate = priznak pro rozhodnuti o vypoctu EX, DX, sigma
#' @return Vraci \code{Fx}: popis distribucni funkce
prob_to_dist <- function (x, p, calculate = FALSE){
  sum = 0
  row <- sprintf("%f        x <  %d",sum, x[[1]])[1]
  print(eval(row))
  Fx <- c(0)
  for (i in 2:(length(x))) {
    sum = sum + p[[i-1]]
    row <- sprintf("%f    %d < x <= %d",sum, x[[i-1]], x[[i]])[1]
    print(eval(row))
    Fx <- c(Fx, sum)
  }
  sum = sum + p[[i]]
  Fx <- c(Fx, sum)
  row <- sprintf("%f    %d < x",sum, x[[length(x)]])[1]
  print(eval(row))

  plot(x, cumsum(p), type="s", ylim=c(0,1))

  if(calculate){
    print("---------------------------")
    calculate_metric(x, p);
  }

  x <- c(x, Inf)
  names(Fx) <- x
  return(Fx)
}

#Prevedeni distribucni funkce na pravdepodobnostni
#' @param x = horni limity pro distribucni funkci
#' @param Fx = distribucni funkce
#' @param calculate =  priznak pro rozhodnuti o vypoctu EX, DX, sigma
#' @return Vraci \code{p}: popis pravdepodobnostni funkce
dist_to_prob <- function (x, Fx, calculate = FALSE){
  xAxis = c()
  yAxis = c()
  p <- c()

  print("x    P(X = x)")

  for (i in 1:(length(x))) {
    Px = Fx[[i+1]]- Fx[[i]]
    p <- c(p, Px)
    row <- sprintf("%d    %f",x[[i]], Px)[1]
    print(eval(row))
    xAxis[[i]] <- x[[i]]
    yAxis[[i]] <- Px
  }

  plot(xAxis, yAxis, ylim=c(0,1))

  if(calculate){
    print("---------------------------")
    calculate_metric(x, p);
  }

  names(p) <- x
  return(p)
}

#Vypocet stredni hodnoty, rozptylu a smerodatne odchylky
#' @param x = hodnoty x
#' @param p = pravdepodobnostni funkce
calculate_metric <- function(x, p){
  EX = get_mean(x,p)
  DX = get_variance(x,p,EX)
  STDDEV = get_standard_deviation(DX)
  cat("EX: ", EX,"\n")
  cat("DX: ", DX,"\n")
  cat("SIGMA: ", STDDEV,"\n")
}

#Vypocet stredni hodnoty
#' @param x = hodnoty x
#' @param p = pravdepodobnostni funkce
#' @return Vraci \code{EX}: støední hodnota
get_mean <- function (x, p){
  EX = sum(x*p)
  return(EX)
}

#Vypocet rozptylu
#' @param x = hodnoty x
#' @param p = pravdepodobnostni funkce
#' @param EX = stredni hodnoty
#' @return Vraci \code{DX}: rozptyl
get_variance <- function(x, p, EX){
  EX2 = sum(x*x*p)
  DX = EX2 - EX^2
  return(DX)
}

#Vypocet smerodatne odchylky
#' @param DX = rozptyl
#' @return Vraci \code{sigmaX}: sigma
get_standard_deviation <- function(DX){
  sigma.X = sqrt(DX)
  return(sigma.X)
}


probabilityEnum <- function() {
  list(LOWER = "LOWER", HIGHER = "HIGHER", BETWEEN = "BETWEEN", EQUAL="EQUAL")
}
probability <- probabilityEnum()

#Vypocet pravdepodobnosti
#' @param x = hodnoty x
#' @param Fx = distribucni funkce
#' @param probabilityType = hodnoty x
#' @param a = mezni hodnota (pripadnì dolni mez)
#' @param b = horni mez (pro probablityType$BETWEEN)
#' @return Vraci pravdepodobnostni hodnotu
get_probability <- function(x, Fx, probablityType, a, b = 0){
  F <- function(a){
    i <- 1
    while (x[[i]] < a) {
      i = i+1
    }
    return(list(value = Fx[[i]], index=i))
  }
  switch (probablityType,
          "LOWER" = {return(F(a)$value)},         # P(X < a)
          "HIGHER" = {return(1-F(a)$value)},      # P(X >= a)
          "BETWEEN" = {                           # P(a <= X < b)
            return(F(b)$value-F(a)$value)
           },
          "EQUAL" = {                             # P(X = a)
            z =F(a)
            return(Fx[[z$index+1]]-z$value)
           },
  )
}

#Prevedeni dle nahodne veliciny Y
#' @param x = hodnoty x
#' @param p = pravdepodobnostni funkce
#' @param y = tvar nahodne veliciny Y
#' @return Vraci \code{p}: prevedena pravdepodobnostni funkce
transform_probability <- function(x, p, y){
  for(i in 1:length(x)){
    x[[i]] = eval(y,envir = list(x=x[[i]]))
  }
  names(p) <- x
  return(p)
}
