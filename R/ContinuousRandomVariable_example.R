
#PRIKLAD 1: vypocet konstanty c
fx <- function(x){return(4- 3*x)}
CRV.get_constant(fx,0,2/3)

#PRIKLAD 2: prevedeni distribucni funkce na hustotu pravdepodobnosti
Fx <- list(exp="sin(x)", min=0, max=pi/2)
CRV.dist_to_dens(Fx)

#PRIKLAD 3: vypocet stredni hodnoty, rozptylu a smerodatne odchylky
fx <- function(x){
  return(2 - (3/2) * x)
}
CRV.calculate_metric(fx, 0, 2/3)

#PRIKLAD 4: vypocet pravdepodobnosti
CRV.get_probability(fx, 0, 2/3, probability$HIGHER, 1/3)
CRV.get_probability(fx, 0, 2/3, probability$BETWEEN, -1/3, 1/3)

#PRIKLAD 5: prevedeni dle nahodne veliciny Y
CRV.transform(fx,0,2/3,y = expression(2-4*x))
