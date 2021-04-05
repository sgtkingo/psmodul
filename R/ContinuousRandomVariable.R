#Vypocet konstanty c
#' @param fx = hustota pravdepodobnosti
#' @param min = dolni mez hustoty pravdepodobnosti
#' @param max = horni mez hustoty pravdepodobnosti
#' @return Vraci hodnotu konstanty c
CRV.get_constant <- function(fx, min, max){
  i <- integrate(fx, min, max)$value
  return(1/i)
}

#Prevedeni distribucni funkce na hustotu pravdepodobnosti
#' @param Fx = distribucni funkce
#' @return Vraci hustotu pravdepodobnosti
CRV.dist_to_dens <- function(Fx){
  fx <- data.frame("exp" ="0" , "interval"= "otherwise")
  derivation <- D(parse(text=Fx$exp),'x')
  interval <- paste("<",Fx$min,",",Fx$max,">")
  inner <- data.frame("exp"=deparse(derivation), "interval" = toString(interval))
  fx <- rbind(fx, inner)
  return(fx)
}

#Vypocet stredni hodnoty, rozptylu a smerodatne odchylky
#' @param fx = hustota pravdepodobnosti
#' @param min = dolni mez hustoty pravdepodobnosti
#' @param max = horni mez hustoty pravdepodobnosti
CRV.calculate_metric <- function(fx, min, max){
  EX = CRV.get_mean(fx, min, max)
  DX = CRV.get_variance(fx, min, max, EX)
  STDDEV = CRV.get_standard_deviation(DX)
  cat("EX: ", EX,"\n")
  cat("DX: ", DX,"\n")
  cat("SIGMA: ", STDDEV,"\n")
}

#Vypocet stredni hodnoty
#' @param fx = hustota pravdepodobnosti
#' @param min = dolni mez hustoty pravdepodobnosti
#' @param max = horni mez hustoty pravdepodobnosti
#' @return Vraci \code{EX}: støední hodnota
CRV.get_mean <- function (fx, min, max){
  f <- function(x){return(x*fx(x))}
  EX = integrate(f,min, max)$value
  return(EX)
}

#Vypocet rozptylu
#' @param fx = hustota pravdepodobnosti
#' @param min = dolni mez hustoty pravdepodobnosti
#' @param max = horni mez hustoty pravdepodobnosti
#' @param EX = stredni hodnoty
#' @return Vraci \code{DX}: rozptyl
CRV.get_variance <- function(fx, min, max, EX){
  f <- function(x){return(x*x*fx(x))}
  EX2 = integrate(f,min, max)$value
  DX = EX2 - EX*EX
  return(DX)
}

#Vypocet smerodatne odchylky
#' @param DX = rozptyl
#' @return Vraci \code{sigmaX}: sigma
CRV.get_standard_deviation <- function(DX){
  sigma.X = sqrt(DX)
  return(sigma.X)
}

probability <- list(LOWER = "LOWER", HIGHER = "HIGHER", BETWEEN = "BETWEEN", EQUAL="EQUAL")

#Vypocet pravdepodobnosti
#' @param fx = hustota pravdepodobnosti
#' @param min = dolni mez hustoty pravdepodobnosti
#' @param max = horni mez hustoty pravdepodobnosti
#' @param probabilityType = hodnoty x
#' @param a = mezni hodnota (pripadnì dolni mez)
#' @param b = horni mez (pro probablityType$BETWEEN)
#' @return Vraci pravdepodobnostni hodnotu
CRV.get_probability <- function(fx, min, max, probabilityType, a, b = 0){
  if (length(probabilityType) == 0){
    stop("Unknown logic operand!")
    return (-1)
  }

  a[a < min] = min
  a[a > max] = max
  b[b > max] = max

  switch (probabilityType,
          "LOWER" = {return(integrate(fx,min,a)$value)},
          "HIGHER" = {
            return(1 - integrate(fx,min,a)$value)
          },
          "BETWEEN" = {
            return(integrate(fx,a,b)$value)
          },
          "EQUAL" = {return(0)}
  )
}

#Prevedeni dle nahodne veliciny Y
#' @param fx = hustota pravdepodobnosti
#' @param min = dolni mez hustoty pravdepodobnosti
#' @param max = horni mez hustoty pravdepodobnosti
#' @param y = tvar nahodne veliciny Y
#' @return Vraci stredni hodnotu a rozptyl
CRV.transform <- function(fx, min, max, y){
  rv <- list(EX=0, DX=0)
  fy <- function(x){return(eval(y))}
  EX = CRV.get_mean(fx, min, max)
  rv$EX = fy(EX)
  DX = CRV.get_variance(fx, min, max, EX)
  c <- eval(D(y,'x'))
  rv$DX = c * c * DX
  return(rv)
}
