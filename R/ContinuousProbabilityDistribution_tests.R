testCase_isEqual <- function(value1, value2){
  result = FALSE
  if( value1 == value2 ){
    result = TRUE
    cat(sprintf("TEST CASE - IS EQUAL: OK (%.3f == %.3f)", value1, value2))
  }
  else cat(sprintf("TEST CASE - IS NOT EQUAL: FAIL! (%.3f != %.3f)", value1, value2))

  return(result)
}

#Test exp dist

#TEST 1
# Hustota pravdìpodobnosti f(x)
lambda = 2
x = 1

val1 = dexp( x, lambda )
val2 = CPD.exp_cont(x, lambda, logic = "=")
testCase_isEqual(val1,val2)

# vykreslíme si Hustotu pravdìpodobnosti
x = seq(from = 0, to = 6, by = 0.01)
f_x = dexp(x, lambda)
plot(x, f_x, type='l')
grid()

plt = CPD.exp_cont(x, lambda, logic = "=", draw_plot = TRUE)

# Distribuèní funkce F(x) = P(X < x)
lambda = 2
x = 1
val1 = pexp( x, lambda )
val2 = CPD.exp_cont(x, lambda, logic = "<")
testCase_isEqual(val1,val2)

# vykreslíme si Distribuèní funkci
x = seq(from = 0, to = 6, by = 0.01)
F_x = pexp(x, lambda)
plot(x, F_x, type = 'l')
grid()

plt = CPD.exp_cont(x, lambda, logic = "<", draw_plot = TRUE)

# Distribuèní funkce F(x) = P(X > x)
lambda = 2
x = 1
val1 = 1 - pexp( x, lambda )
val2 = CPD.exp_cont(x, lambda, logic = ">")
testCase_isEqual(val1,val2)



#Test weib dist

#TEST 1
# Hustota pravdìpodobnosti f(x)
# Hustota pravdìpodobnosti f(x)
lambda = 2 # ekvivalent 1/lambda u exp. rozdìlení
beta = 1  # beta = 1 -> exponenciální rozdìlení
x = 5

val1 = dweibull(x,shape=beta, scale=1/lambda)
val2 = CPD.weib_cont(x, LAMBDA = lambda, BETA = beta, logic="=")
testCase_isEqual(val1,val2)

# vykreslíme si Hustotu pravdìpodobnosti
x = seq(from = 0, to = 6, by = 0.01)
f_x = dweibull(x,shape=beta, scale=1/lambda)
plot(x, f_x, type='l')
grid()

plt = CPD.weib_cont(x, LAMBDA = lambda, BETA = beta, logic="=", draw_plot = TRUE)

# Distribuèní funkce F(x) = P(X < x)
lambda = 3 # ekvivalent 1/lambda u exp. rozdìlení
beta = 2  # beta = 1 -> exponenciální rozdìlení
x = 5

val1 = pweibull(x,shape=beta, scale=1/lambda)
val2 = CPD.weib_cont(x, LAMBDA = lambda, BETA = beta)
testCase_isEqual(val1,val2)

# vykreslíme si Distribuèní funkci
x = seq(from = 0, to = 6, by = 0.01)
F_x = pweibull(x,shape=beta, scale=1/lambda)
plot(x, F_x, type = 'l')
grid()

plt = CPD.weib_cont(x, LAMBDA = lambda, BETA = beta, draw_plot = TRUE)

# Distribuèní funkce F(x) = P(X > x)
lambda = 3 # ekvivalent 1/lambda u exp. rozdìlení
beta = 2  # beta = 1 -> exponenciální rozdìlení
x = 5

val1 = 1 - pweibull(x,shape=beta, scale=1/lambda)
val2 =  CPD.weib_cont(x, LAMBDA = lambda, BETA = beta, logic=">")
testCase_isEqual(val1,val2)


#Test norm dist

# Hustota pravdìpodobnosti f(x)
mu = 2
sigma = 3
x = 4

val1 = dnorm(x, mean=mu, sd=sigma)
val2 =  CPD.norm_cont(x, mu, sigma, logic="=")
testCase_isEqual(val1,val2)

# vykreslíme si Hustotu pravdìpodobnosti
x = seq(from = -5, to = 10, by = 0.01)
f_x = dnorm(x, mean=mu, sd=sigma)
plot(x, f_x, type='l')
grid()

plt = CPD.norm_cont(x, mu, sigma, logic="=", draw_plot = TRUE)

# Distribuèní funkce F(x) = P(X < x)
mu = 2
sigma = 3
x = 4

val1 = pnorm(x, mean=mu, sd=sigma)
val2 =  CPD.norm_cont(x, mu, sigma)
testCase_isEqual(val1,val2)

# vykreslíme si Distribuèní funkci
x = seq(from = -5, to = 10, by = 0.01)
F_x = pnorm(x, mean=mu, sd=sigma)
plot(x, F_x, type = 'l')
grid()

plt =  CPD.norm_cont(x, mu, sigma, draw_plot = TRUE)

# Distribuèní funkce F(x) = P(X > x)
mu = 2
sigma = 3
x = 4

val1 = 1 - pnorm(x, mean=mu, sd=sigma)
val2 =  CPD.norm_cont(x, mu, sigma, logic=">")
testCase_isEqual(val1,val2)
