#ifndef T2CPP_H
#define T2CPP_H

#include <Rcpp.h>

RcppExport SEXP t2cpp2(SEXP xx, SEXP yy, SEXP tt, SEXP interval);
RcppExport SEXP t2cpp(SEXP x, SEXP y, SEXP t, int interval, int keep, int type);
RcppExport SEXP t2sub(SEXP x, SEXP y, SEXP t, int interval, int keep, int type);

#endif
