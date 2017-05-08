random <- function(n,m=2**31,a=65539,c=0,z0=round((as.numeric(Sys.time())*1000)%%1000)){
      randomGen(n+30,m,a,c,z0)[30:(n+29)]
}

uniformeContinua <- function(n,a,b){
      a+(b-a)*random(n)
}

exponencial <- function(n,b,...){
      -b*log(random(n))
}
erlang <- function(n,r,lambda){
      apply(matrix(exponencial(n*r,1/lambda),ncol = r),1,sum)
}

gamma <- function(n,r,lambda){
      if(r%%1==0){
            erlang(n,r,lambda)
      }
      else{
            if(r > 0 & r < 1){
                  b = (exp(1)+r)/exp(1)
                  u1 = random(100000)
                  u2 = random(100000)
                  x=ifelse(u1 > 1/b,
                         ifelse(u2 < (-log(b*(1-u1)/r))**(r-1),-log(b*(1-u1)/r),NA),
                         ifelse(exp(-(b*u1)**(1/r)),(b*u1)**(1/r),NA)
                         )
                  x=x[!is.na(x)]
                  x[1:n]
            }
            else
            {
                  a=1/sqrt(2*r-1)
                  b=r-log(4)
                  q=r+1/a
                  theta=4.5
                  d=1+log(theta)
                  u1 = random(100000)
                  u2 = random(100000)
                  v = a*log(u1/(1-u1))
                  y = r*exp(v)
                  z=u1**2*u2
                  w=b+q*v-y
                  x=ifelse(w+d-theta*z>=0,y,ifelse(w>= log(z),y,NA))
                  x=x[!is.na(x)]
                  x[1:n]
            }
      }
}

weibull <- function(n,alpha,beta){
      beta*(-log(1-random(n)))**(1/alpha)
}

normal <- function(n,mu,sigma){
      u = random(n)
      u1 = u[1:(n/2)]
      u2 = u[(n/2+1):n]
      x1 = (-2*log(u1))**(1/2)*cos(2*pi*u2)
      x2 = (-2*log(u1))**(1/2)*sin(2*pi*u2)
      x1p = mu + sigma*x1
      x2p = mu + sigma*x2
      c(x1p,x2p)
      }

beta <- function(n,a,b){
     x1 = gamma(n,a,1)
     x2 = gamma(n,b,1)
     x1/(x1+x2)
}

lognormal <- function(n,mu,sigma){
      exp(normal(n,mu,sigma))
}

bernoulli <- function(n,p,...){
      ifelse(random(n)<p,1,0)
}


uniformeDiscreta <- function(n,a,b){
      round(uniformeContinua(n,a-1,b)+.5)
}

binomial <- function(N,n,p){
      apply(matrix(bernoulli(N*n,p),ncol = n),1,sum)
}

geometrica <- function(n,p,...){
      ceiling(log(random(n))/log(1-p)-1)
}

binomialNegativa <- function(n,r,p){
      apply(matrix(geometrica(n*r,1-p),ncol=r),1,sum)
}

poisson <- function(n,lambda,...){
      i=1
      x=vector(length = n)
      A=1
      k=0
      N = max(10*n,2*n*lambda)
      U = random(N)
      j=1
      while(i<=n){
            u=U[j]
            j=j+1
            A = A*u
            if(A < exp(-lambda)){
                  x[i]=k
                  i = i+1
                  A=1
                  k=0
            }else{
                  k=k+1
            }
      }
      x
}









