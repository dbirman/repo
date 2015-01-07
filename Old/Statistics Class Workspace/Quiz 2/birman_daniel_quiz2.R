sigma = 18
h_0 = 0
h_1 = 8
d = 8
n = 36
alpha = 0.025

power.t.test(n=36,delta=8,sd=18,sig.level=.025,type="paired",alternative="one.sided")

se = sigma/sqrt(n)
x = seq(-5,15,.01)
d0 = dnorm(x,mean=h_0,sd=se)
d1 = dnorm(x,mean=h_1,sd=se)
dat = data.frame(null=d0,alt=d1,x=x)
val_0 = qnorm(alpha,mean=h_0,sd=se,lower.tail=F)
ggplot() + 
  geom_line(data=dat,aes(x,null,color="red")) +
  geom_line(data=dat,aes(x,alt,color="green")) +
  xlab("Values") + ylab("Density") +
  geom_vline(xintercept=val_0,color="blue") +
  geom_ribbon(aes(x=dat$x[dat$x>val_0],ymin=0,ymax=dat$null[dat$x>val_0]),alpha=0.2,fill="darkred") +
  geom_ribbon(aes(x=dat$x[dat$x>val_0],ymin=0,ymax=dat$alt[dat$x>val_0]),alpha=0.2,fill="51") +
  scale_color_manual(name="Hypothesis",values=c("red"="red","green"="green"),labels=c("Alternative","Null"))

