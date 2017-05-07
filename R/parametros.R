PuniformeDiscreta<- function(a,b){
      mean<-(a+b)/2
      var<-((b-a+1)**2-1)/12
      c(mean,var)
}
Pbernoulli<- function(p,...){
      mean<-p
      var<-p*(1-p)
      c(mean,var)
}
Pbinomial<- function(n,p){
      mean<-n*p
      var<-n*p*(1-p)
      c(mean,var)
}
Pgeometrica<- function(p,...){
      mean<-(1-p)/p
      var<-(1-p)/p**2
      c(mean,var)
}
PbinomialNegativa<- function(r,p){
      mean<-(r*p)/(1-p)
      var<-(r*p)/(1-p)**2
      c(mean,var)
}
Ppoisson<- function(l,...){
      mean<-l
      var<-l
      c(mean,var)
}
PuniformeContinua<- function(a,b){
      mean<-(a+b)/2
      var<-(b-a)**2/12
      c(mean,var)
}
Pexponencial<- function(l,...){
      mean<-l
      var<-l**2
      c(mean,var)
}
Pgamma<- function(r,l){
      mean<-r/l
      var<-r/l**2
      c(mean,var)
}
Pbeta<- function(a,b){
      mean<-a/(a+b)
      var<-(a*b)/((a+b)**2*(a+b+1))
      c(mean,var)
}
Pnormal<- function(m,s){
      mean<-m
      var<-s**2
      c(mean,var)
}
Plognormal<- function(m,s){
      mean<-exp(m+s**2/2)
      var<-(exp(s**2)-1)*exp(2*m+s**2)
      c(mean,var)
}
Pweibull<- function(k,l){
      mean<- l* base::gamma(1+1/k)
      var<-l**2*( base::gamma(1+2/k)-( base::gamma(1+1/k))**2)
      c(mean,var)
}
