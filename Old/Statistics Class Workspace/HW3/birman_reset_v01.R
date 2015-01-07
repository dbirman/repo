rm(list=ls())
library(ggplot2)
library(reshape)
pp = function(x) {round(x, digits=3)}
p = function(x) {round(x, digits=2)}
## APA Format Printing
## Author: Dan Birman
## 10/13/2014

apaprint = function(stat_obj, coefs=c(), stars=F) {
  so = class(stat_obj)
  if (length(so) > 1) {so = so[1]}
  switch(so,
         "htest" = .phtest(stat_obj,stars),
         "lm" = .plm(stat_obj,coefs,stars),
         "glm" = .pglm(stat_obj,coefs,stars))
}

.pglm = function(s,coefs,stars) {
  #Warning this defaults 
  if (length(coefs)==0) {coefs = dimnames(summary(s)$coefficients)[[1]][2]}
  out_s = .lin_mod_coefs(s,coefs,stars)
  out_s
}

.plm = function(s,coefs,stars) {
  sm = summary(s)
  out_s = .lin_mod_coefs(s,coefs,stars)
  if (length(coefs)>=1) {paste(out_s,", ",sep="")}
  #Everything standard
  f = sm$fstatistic
  p = pf(f[1],f[2],f[3],lower.tail=F)
  r2 = sm$r.squared
  if (length(coefs)>0) {
    return(sprintf("%s$;  F_{(%.0f, %.0f)} = %.2f, %s, adj. R^2 = %.2f$",out_s,f[2],f[3],f[1],.pformat(p,stars),r2))
  }
  return(sprintf("$F_{(%.0f, %.0f)} = %.2f, %s, adj. R^2 = %.2f$",f[2],f[3],f[1],.pformat(p,stars),r2))
}

.lin_mod_coefs = function(s,coefs,stars) {
  sm = summary(s)
  co_s = c()
  if (length(coefs) >= 1) {
    for (i in 1:length(coefs)) {
      cur = coefs[i]
      dat = sm$coefficients[cur,]
      p = dat[4]
      ci = confint(s,cur)
      if (i > 1) {co_s = paste(", ",co_s,sep="")}
      co_s = paste(sprintf("$b_{%s} = %.2f, [%.2f %.2f], %s$",cur,dat[1],ci[1],ci[2],.pformat(p,stars)),co_s,sep = "")
    }
  }
  if (length(co_s)==0) {co_s = ""}
  co_s
}

.pformat = function(pv,stars) {
  if (stars==T) {
    if (pv >= .05) {return(sprintf("p = %.3f",pv))}
    if (pv >= .01) {return(sprintf("p = %.3f *",pv))}
    if (pv >= .001) {return(sprintf("p = %.3f **",pv))}
    return("p < 0.001 ***")
  }
  if (pv >= .05) {return(sprintf("p = %.3f",pv))}
  if (pv >= .01) {return(sprintf("p = %.3f",pv))}
  if (pv >= .001) {return(sprintf("p = %.3f",pv))}
  return("p < 0.001")
}

.phtest = function(s,stars) {
  switch(s["method"]$method,
         "One Sample t-test" = .pttest(s,stars),
         "Welch Two Sample t-test" = .pttest(s,stars),
         "Pearson's Chi-squared test" = .pchisqp(s,stars),
         "Pearson's Chi-squared test with Yates' continuity correction" = .pchisqp(s,stars),
         "Chi-squared test for given probabilities" = .pchisqp(s,stars),
         "One-way analysis of means (not assuming equal variances)" = .panova(s,stars),
         "Pearson's product-moment correlation" = .pcorr(s,stars)
  )
}

.panova = function(s,stars) {
  .basicprint(s,"F",stars)
}

.pchisqp = function(s,stars) {
  sprintf("$\\chi^2$(%.0f, N = %.0f) = %.2f, %s",s$parameter,s$parameter+1,s$statistic,.pformat(s$p.value,stars))
}

.pcorr = function(s,stars) {
  sprintf("*r* (%.0f) = %.2f, %s",s$parameter,s$estimate,.pformat(s$p.value,stars))
}

.pttest = function(s,stars) {
  .basicprint(s,"t",stars)
}

.basicprint = function(s,st,stars) {
  sprintf("*%s* (%.0f) = %.2f, %s",st,s$parameter,s$statistic,.pformat(s$p.value,stars))
}