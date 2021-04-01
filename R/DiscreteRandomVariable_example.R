
#PRIKLAD 1: prevedeni pravdepodobnostni funkce na distribucni
x = c(0,1,2,3)
p = c(0.2,0.2,0.3,0.3)
Fx <- prob_to_dist(x, p, calculate = TRUE)

#PRIKLAD 2: prevedeni distribucni funkce na pravdepodobnostni
x = c(0,1,2,3)
Fx = c(0,0.2,0.4,0.7,1)
p <- dist_to_prob(x, Fx, calculate = TRUE)

#PRIKLAD 3: prevedeni pravdepodobnosti dle nahodne veliciny Y
x = c(0,1,2,3)
p = c(0.2,0.2,0.3,0.3)
p <- transform_probability(x, p, y=expression(x-x^2))
p

#PRIKLAD 4: vypocet pravdepodobnosti
get_probability(x, Fx, probability$LOWER, 1)
get_probability(x, Fx, probability$HIGHER, 1)
get_probability(x, Fx, probability$BETWEEN, 0,2)
get_probability(x, Fx, probability$EQUAL, 2)

