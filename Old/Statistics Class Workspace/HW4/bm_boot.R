bm.bootstrapmed<-function(x,med,y,iterations=1000,alpha=.05) {
  ## Bootstrapping mediation based on Preacher & Hayes (2004)
  # Version 2.0
  # Requires bm.med()
  as.data.frame(cbind(x,med,y))->vars;
  length(x)->N;
  bootab<-vector()
  for (i in 1:iterations) {
    sample(c(1:N),N,replace=T)->sampnums;
    lm(vars[sampnums,2]~vars[sampnums,1])$coefficients[2]->itera;
    lm(vars[sampnums,3]~vars[sampnums,2]+vars[sampnums,1])$coefficients[2]->iterb;
    (append(bootab,itera*iterb))->bootab
  }
  hist(bootab,main=paste("Bootsrapped a*b, with",iterations,"iterations"),col="red");
  bm.med(x,med,y)[1,5]->ab
  # Bias correction after Stine (1989)
  sum(bootab<=ab)/iterations->prob
  qnorm(prob)->Z0
  round(pnorm(2*Z0+qnorm(alpha/2)),3)->bcl
  round(pnorm(2*Z0+qnorm(1-alpha/2)),3)->bcu
  print("Bootstrap results:",quote=F)
  print(round(c("Mean(ab*)"=mean(bootab),"p(ab*<ab)"=prob),3))
  print("Uncorrected:",quote=F)
  print(round(quantile(bootab,c(alpha/2,1-alpha/2)),3))
  print("Bias Corrected:",quote=F)
  print(round(quantile(bootab,c(bcl,bcu)),3))
}

mediation_bootstrap = function(x, med, y, iterations = 1000){
  
  # setup some parameters
  N = length(x)
  df = as.data.frame(cbind(x, med, y))
  boot_ab = vector(length=iterations) # set up empty vector for storage
  
  # now go through a loop where we'll randomly sample, and get a a*b value
  for (i in 1:iterations){
    ind_boot = sample(c(1:N), N, replace=TRUE) # random indices
    df_boot = df[ind_boot,]
    
    iter_a = lm(df_boot$med ~ df_boot$x)$coefficients[2] # coeff of x
    iter_b = lm(df_boot$y ~ df_boot$med + df_boot$x)$coefficients[2] # coeff of mediator
    
    boot_ab[i] = iter_a * iter_b
  }
  
  # create plot
  hist(boot_ab,main=paste("Bootstrapped a*b, with",iterations,"iterations"),col="red");
  abline(v=0, col='black', lty=2, lwd=2)
  abline(v=c(quantile(boot_ab,c(.025,.975))), col='blue', lty=3)
  
  # Print results
  print("Bootstrap results:",quote=F);
  print(c(ab=mean(boot_ab)));
  print(quantile(boot_ab,c(.025,.975)))
  
  return(boot_ab)
}