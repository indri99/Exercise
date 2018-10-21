========================================== S E T ==========================================
#no.1
(Q <- c(sort(sample(-100:100))))

#no.2
#odd number
i = Q%%2 == 1
P <- Q[i]
P
#even number
i = Q%%2 == 0
P <- Q[i]
P

#no.3
S <- c(1,5,1,0,2,0,1,8)
R <- setdiff(Q, S)
R

#no.4
union(P, R)

#no.5
setdiff(Q, union(P, R))

#no.6
setdiff(Q, P)
setdiff(Q, R)
intersect(setdiff(Q, P), setdiff(Q, R))
===================================== F U N C T I O N =====================================
#no.1
f <- function(x,y){
    result <- sqrt(x) + y^2
    return(result)
}
f(4,2)

#no.2
g <- function(a,b){
  result <- a*b*(a^2 + b/3)
  return(result)
}
g(1,3)

#no.3
h <- function(x,y){
  result <- sqrt(f(x,y)+3+g(x,y))
  return(result)
}
h(1,3)

#no.4
f1 <- function(x){
  result <- x^3 + x + 1
  return(result)
}
f2 <- function(x){
  result <- sqrt(x) - 1
  return(result)
}
f <- function(x){
  result <- f1(f2(x))
  return(result)
}
f(4)

#no.5
f1 <- function(x){
  return(1/x)
}
f2 <- function(x){
  return(2/x)
}
f3 <- function(x){
  return(3/x)
}
f4 <- function(x){
  return(4/x)
}
f5 <- function(x){
  return(5/x)
}
x <- c(-25,25)
plot(x, f1(x), ylab="y", type="l", col="1")
line(x, f2(x), ylab="y", type="l", col="2")
line(x, f3(x), ylab="y", type="l", col="3")
line(x, f4(x), ylab="y", type="l", col="4")
line(x, f5(x), ylab="y", type="l", col="5")

##no.1
f <- function(x){
  fx <- (1 - cos(x)) / x
  return(fx)
}
library(Ryacas)
x <- Sym("x")
Limit(f(x),x,0)

##no.2
f <- function(h){
  fh <- (2 * ((-3 + h)^2) - 18)/h
  return(fh)
}
library(Ryacas)
h <- Sym("h")
Limit(f(h),h,0)

##no.3
f <- function(t){
  ft <- (t - (sqrt(3*t + 4)))/(4 - t)
  return(ft)
}
library(Ryacas)
t <- Sym("t")
Limit(f(t),t,4)
=============================== D I F F E R E N T A T I O N ===============================
#no.1  
library(Ryacas)
x <- Sym("x")
Simplify(deriv(sqrt(x)*(x+1)))

#no.2  
library(Ryacas)
x <- Sym("x")
Simplify(deriv((2*x^2-3)/sqrt(x)))

#no.3  
library(Ryacas)
x <- Sym("x")
Simplify(deriv((x-1)/(x+1)))

#no.4  
integrand <- function(x){
  return((2 * x + (x + 1))/(2 * root(x, 2)))
}

integrand <- function(x){
  return((8 * x^2 + (-2 * x^2 + 3))/(2 * (root(x, 2) * x)))
}

integrand <- function(x){
  return(2/(x^2 + 2 * x + 1))
}
================================== I N T E G R A T I O N ==================================
#no.1
integrand <- function(x){
  return(2*x^3)
}
integrate(f=integrand, lower=0, upper=3)

#no.2
integrand <- function(x){
  return(1-(5*x^4))
}
integrate(f=integrand, lower=-1, upper=2)

#no.3
integrand <- function(x){
  return(x^4-(3*x^2)+5)
}
integrate(f=integrand, lower=-2, upper=2)

#no.4
integrand <- function(x){
  return(x^2+(1/2*sqrt(x)))
}
integrate(f=integrand, lower=1, upper=4)

#no.5
integrand <- function(x){
  return((2-(3*x))^2)
}
integrate(f=integrand, lower=0, upper=2)
=========================================================================================
17523111
17523116
17523119