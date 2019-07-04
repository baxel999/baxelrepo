#libreria baxel

box.muller <- function (n)
{
  u1 <- runif(n)
  u2 <- runif(n)
  R    = sqrt (-2 * log(u1)) 
  teta = 2 * pi * u2
  x    = R * cos(teta)  # ~ N(0,1)
  y    = R * sin(teta)  # ~N(0,1)
  lista <- list(N1=x, N2=y, r=R, angoli=teta)
  return (lista)
}

marsaglia <- function (n)
{
  u1 <- runif(n)
  u2 <- runif(n)
  w1   = 2 * u1 - 1
  w2   = 2 * u2 - 1
  S    = w1^2 + w2^2
  ind  = which(S<=1)
  S = S[ind]
  w1 = w1[ind]
  w2 = w2[ind]
  Z1    = w1 * sqrt(-2/(S) * log (S))   # ~ N(0,1)
  Z2    = w2 * sqrt(-2/(S) * log (S))   # ~ N(0,1)
  m     = cbind(Z1,Z2,S)
  ind <- which(m[,3] <= 1)
  lista <- list(N1 = m[, 1], N2=m[,2], S = S)
  return (lista)
}