

#Prevedeni pravdepodobnostni funkce na distribucni
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
calculate_metric <- function(x, p){
  EX = get_mean(x,p)
  DX = get_variance(x,p,EX)
  STDDEV = get_standard_deviation(DX)
  cat("EX: ", EX,"\n")
  cat("DX: ", DX,"\n")
  cat("SIGMA: ", STDDEV,"\n")
}

#Vypocet stredni hodnoty
get_mean <- function (x, p){
  EX = sum(x*p)
  return(EX)
}

#Vypocet rozptylu
get_variance <- function(x, p, EX){
  EX2 = sum(x*x*p)
  DX = EX2 - EX^2
  return(DX)
}

#Vypocet smerodatne odchylky
get_standard_deviation <- function(DX){
  sigma.X = sqrt(DX)
  sigma.X
}


probabilityEnum <- function() {
  list(LOWER = "LOWER", HIGHER = "HIGHER", BETWEEN = "BETWEEN", EQUAL="EQUAL")
}
probability <- probabilityEnum()

#Vypocet pravdepodobnosti
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
transform_probability <- function(x, p, y){
  for(i in 1:length(x)){
    x[[i]] = eval(y,envir = list(x=x[[i]]))
  }
  names(p) <- x
  return(p)
}
