#include <iostream>
#include <vector>
#include <math.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<double> randomGen(int n, double m, double a, double c, double z0){
	std::vector <double> z(n),u(n);
    z[0]=z0;
    for(int i = 0; i<n;i++){
        double k = floor((a*z[i]+c)/m);
        z[i+1]= a*z[i]+c - m*k;
        u[i] = z[i+1]/m;
    }
	return u;
}
