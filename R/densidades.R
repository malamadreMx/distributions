duniformeContinua <- function(x,a,b){
      ifelse(x>a & x<b,1/(b-a),0)
}

dexponencial <- function(x,b,...){
      ifelse(x>0,exp(-x/b)/b,0)
}

dgamma <- function(x,r,lambda){
      ifelse(x>0,lambda**r/(base::gamma(r))*x**(r-1)*exp(-lambda*x),0)
}

dweibull <- function(x,alpha,beta){
      ifelse(x>0,alpha*beta**(-alpha)*x**(alpha-1)*exp(-(x/beta)**alpha),0)
}

dnormal <- function(x,mu,sigma){
      1/sqrt(2*pi*sigma**2)*exp(-(x-mu)**2/(2*sigma**2))
}

dbeta <- function(x,a,b){
      ifelse(x>0 & x<1,x**(a-1)*(1-x)**(b-1)/base::beta(a,b),0)
}

dlognormal <- function(x,mu,sigma){
      ifelse(x>0,1/(sqrt(2*pi)*sigma*x)*exp(-(log(x)-mu)**2/(2*sigma**2)),0)
}

dbernoulli <- function(x,p,...){
      ifelse(x==1,p,1-p)
}

duniformeDiscreta <- function(x,a,b){
      ifelse(x>=a & x<=b,1/m,0)
}

dbinomial <- function(x,n,p){
      choose(n,x)*p**x*(1-p)**(n-x)
}

dgeometrica <- function(x,p,...){
      (1-p)**x*p
}

dbinomialNegativa <- function(x,r,p){
      choose(x+r-1,x)*(1-p)**r*p**x
}

dpoisson <- function(x,lambda,...){
      lambda**x*exp(-lambda)/factorial(x)
}
