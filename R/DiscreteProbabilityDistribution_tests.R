testCase_isEqual <- function(value1, value2){
  result = FALSE
  if( value1 == value2 ){
    result = TRUE
    cat(sprintf("TEST CASE - IS EQUAL: OK (%.3f == %.3f)", value1, value2))
  }
  else cat(sprintf("TEST CASE - IS NOT EQUAL: FAIL! (%.3f != %.3f)", value1, value2))

  return(result)
}

#Test hypergeo_disc

#TEST 1
val1 = dhyper( 4, m=12, n=8, 4 )
val2 = DPD.hypergeo_disc(x=4, N=20, M=12, n=4, "=")
testCase_isEqual(val1,val2)

#TEST 2
val1 = phyper(3,30,70,20)
val2 = DPD.hypergeo_disc(x=3, N=100, M=30, n=20,'<=')
testCase_isEqual(val1,val2)

#TEST 2
val1 = phyper(3,30,70,20, lower.tail = FALSE)
val2 = DPD.hypergeo_disc(x=3, N=100, M=30, n=20,'>')
testCase_isEqual(val1,val2)

#Test binom_disc

#TEST 3
val1 = pbinom(q=4, size = 10, prob=1/6)
val2 = DPD.binom_disc(x=4, n=10, PI=1/6)
testCase_isEqual(val1,val2)

#TEST 4
val1 = dbinom(x=3, size = 10, prob=1/6)
val2 = DPD.binom_disc(x=3, n=10, PI=1/6, "=")
testCase_isEqual(val1,val2)

#TEST 5
val1 = pbinom(q=149, size = 1000, prob=0.2, lower.tail = FALSE)
val2 = DPD.binom_disc(x=150, n=1000, PI=0.2, ">=")
testCase_isEqual(val1,val2)

#Test nbinom_disc

#TEST 6
val1 = pnbinom(3, 1, 0.1)
val2 = DPD.nbinom_disc(x=4, k=1, PI=0.1, "<=")
testCase_isEqual(val1,val2)

#TEST 7
val1 = pnbinom(19, 100, 0.9, lower.tail = FALSE)
val2 = DPD.nbinom_disc(x=120, k=100, PI=0.9, ">=")
testCase_isEqual(val1,val2)

#Test pois_disc
val1 = ppois(1, 4)
val2 = DPD.pois_disc(x=1, t=2, LAMBDA=2, "<=")
testCase_isEqual(val1,val2)

