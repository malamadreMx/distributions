// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// randomGen
std::vector<double> randomGen(int n, double m, double a, double c, double z0);
RcppExport SEXP distributions_randomGen(SEXP nSEXP, SEXP mSEXP, SEXP aSEXP, SEXP cSEXP, SEXP z0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type c(cSEXP);
    Rcpp::traits::input_parameter< double >::type z0(z0SEXP);
    rcpp_result_gen = Rcpp::wrap(randomGen(n, m, a, c, z0));
    return rcpp_result_gen;
END_RCPP
}
