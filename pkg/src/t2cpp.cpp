#include <Rcpp.h>
#include <cmath>

RcppExport SEXP t2cpp(SEXP xx, SEXP yy, SEXP tt, SEXP params) {
BEGIN_RCPP
  //SEXP t2cpp(NumericVector x, NumericVector y, NumericVector t, int interval, int keep, int type) {
  //using namespace Rcpp;
  Rcpp::NumericVector x(xx);
  Rcpp::NumericVector y(yy);
  Rcpp::NumericVector t(tt);
  Rcpp::List rparam(params);

  int interval = Rcpp::as<int>(rparam["interval"]);
  int type = Rcpp::as<int>(rparam["type"]);
  int keep =  Rcpp::as<int>(rparam["keep"]);
    
  double sumx = 0;
  double sumy = 0;
  int m = 0;
  int n = x.size();
  int tlower = interval - keep;
  int tupper = interval + keep;
  // type 1 - use all points that are reachable, regardless of the order; 2 - strict, only consecutive steps that macht the time interval; 3 - more flexible version of consecutive version, i.e. if no points fall within the time interval, take next possible
  if (type == 1) {
    for (int i = 1; i < n; i++) {
      for (int j = 0; j < (n-1); j++) {
	int tdiff = t[i] - t[j];
	if (tdiff >= tlower && tdiff <= tupper) {
	  double tempx = x[j] - x[i];
	  double tempy = y[j] - y[i];
	  sumx += pow(tempx, 2);
	  sumy += pow(tempy, 2);
	  m++;
	}
      }
    }
  } else if (type == 2) {
    int i = 0;
    while (i < n) {
      for (int j = i; j < n; j++) {
	int tdiff = t[j] - t[i];
	if (tdiff >= tlower && tdiff <= tupper) {
	  double tempx = x[i-1] - x[i];
	  double tempy = y[i-1] - y[i];
	  sumx += pow(tempx, 2);
	  sumy += pow(tempy, 2);
	  m++;
	  i = j;
	  break;
	} else if ( tdiff > tupper) {
	  i = j;
	  break;
	}
      }
      i++;
    }
  } else if (type == 3) {
    int tdiff = 0;
    int cumdiff = 0;
    for (int i = 1; i < n; i++) {
      tdiff = t[i] - t[i - 1];
      cumdiff += tdiff;
      if (cumdiff >= interval) {
	double tempx = x[i-1] - x[i];
	double tempy = y[i-1] - y[i];
	sumx += pow(tempx, 2);
	sumy += pow(tempy, 2);
	m++;
	cumdiff = 0;
      }
    }
  }
  return Rcpp::NumericVector::create(1/(double)m * (sumx + sumy), n, m);
END_RCPP
}
