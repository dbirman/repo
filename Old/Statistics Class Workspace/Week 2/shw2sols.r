# HW-2, Logistic regression
sink("rhw2sols.r")

d0 = read.csv("http://web.stanford.edu/class/psych252/_downloads/hw2data.csv")

# Question 3 ans.
pdf('hw2.histo1.pdf')	# Save plots in this pdf file
par(mfrow=c(3,2))		# sets up grid for plots as 3 rows by 2 cols, i.e., 6 plots per page

g1 = hist(Pasthapp, prob=T)
lines(density(sort(Pasthapp), adjust=2))			# plots line for density function; sort() gets rid of NA's
g2 = hist(Futurehapp, prob=T)
lines(density(sort(Futurehapp), adjust=2))			# plots line for density function; sort() gets rid of NA's
g3 = hist(Responsible, prob=T)
lines(density(sort(Responsible), adjust=2))			# plots line for density function; sort() gets rid of NA's
g4 = hist(FTP, prob=T)
lines(density(sort(FTP), adjust=2))					# plots line for density function; sort() gets rid of NA's
g5 = hist(complain, prob=T)
lines(density(sort(complain), adjust=2))			# plots line for density function; sort() gets rid of NA's
graphics.off()			# need to turn off graphics device to regain normal use of R console

# Question 4 ans.
rs0 = chisq.test(table(d0$Type, d0$complain))
print(rs0)

# Questions 5 & 6 ans.
d0.f = d0[d0$Type==1,]
d0.b = d0[d0$Type==2,]
d0.v = d0[d0$Type==3,]
x1 = min(d0$Pasthapp); x2 = max(d0$Pasthapp)
y1 = min(d0$Futurehapp); y2 = max(d0$Futurehapp)

pdf('hw2.pastfutureplots1.pdf')
par(mfrow=c(2,2))

rs0 = lm(d0$Futurehapp ~ d0$Pasthapp, data = d0)
g0 = plot(d0$Pasthapp, d0$Futurehapp, type='p', xlim=c(x1,x2), ylim=c(y1,y2), xlab='phapp', ylab='fhapp', main='All Groups', pch=1)
abline(rs0, lty=1)
lines(lowess(d0$Pasthapp, d0$Futurehapp), lty = 2)
legend(x1,y2, c('regress', 'lowess'), lty = 1:2, cex=.7)	# put legend at x=x1, y=y2

rs0 = lm(d0$Futurehapp ~ d0$Pasthapp, data=d0.f)
g0 = plot(d0.f$Pasthapp, d0.f$Futurehapp, type='p', xlim=c(x1,x2), ylim=c(y1,y2), xlab='phapp', ylab='fhapp', main='Free', pch=1)
abline(rs0, lty=1)
lines(lowess(d0.f$Pasthapp, d0.f$Futurehapp), lty = 2)
legend(x1,y2, c('regress', 'lowess'), lty = 1:2, cex=.7)	# put legend at x=x1, y=y2

rs0 = lm(d0$Futurehapp ~ d0$Pasthapp, data=d0.b)
g0 = plot(d0.b$Pasthapp, d0.b$Futurehapp, type='p', xlim=c(x1,x2), ylim=c(y1,y2), xlab='phapp', ylab='fhapp', main='Bias', pch=1)
abline(rs0, lty=1)
lines(lowess(d0.b$Pasthapp, d0.b$Futurehapp), lty = 2)
legend(x1,y2, c('regress', 'lowess'), lty = 1:2, cex=.7)	# put legend at x=x1, y=y2

rs0 = lm(d0$Futurehapp ~ d0$Pasthapp, data=d0.v)
g0 = plot(d0.v$Pasthapp, d0.v$Futurehapp, type='p', xlim=c(x1,x2), ylim=c(y1,y2), xlab='phapp', ylab='fhapp', main='Varied', pch=1)
abline(rs0, lty=1)
lines(lowess(d0.v$Pasthapp, d0.v$Futurehapp), lty = 2)
legend(x1,y2, c('regress', 'lowess'), lty = 1:2, cex=.7)	# put legend at x=x1, y=y2
graphics.off()

# Alternative way of doing # 5 and # 6; use a for() loop to cycle through different levels of Type
d0 = read.csv('hw2data.csv')
d0$Type = factor(d0$Type)	# many functions operate only on factors, hence convert the vector, d0$Type, into a factor
pdf('hw2.pastfutplots2.pdf')
par(mfrow=c(2,2))

rs1 = list(length = 3)	# in anticipation of the for() loop, we need a placeholder for the output of the for() loop

for (i in levels(d0$Type)) {
	rs1[[i]] = cor.test(~ Pasthapp + Futurehapp, d0, Type==i, na.action=na.omit)
	with(d0[Type==i,], plot(Pasthapp, Futurehapp, main = i))
	plot(d0[Type==i, c(2,4)], type = 'p', main = i)
	lines(lowess(d0[Type==i, c(2,4)]), lty = 2)
	}
print(rs1)

graphics.off()


# Questions 7 & 8
d0$Type = factor(d0$Type, labels = c("free", "bias", "varied"))	# Recode 'Type', (1,2,3), into a factor.  

rs1 = lm(d0$Futurehapp ~ d0$Type, data=d0, na.action=na.omit)
contrasts(d0$Type, 2) = "contr.treatment"
	# General linear model: DV, 'phapp' is quantitative; IV's may be quant or categ factor
	# contr.treatment is the default set of contrasts, i.e., '2' vs. '1', and '3' vs. '1'; 
	# the argument, "2", specifies the number of contrasts
print(summary(rs1))
# To test 'homogeneity of variance', use var.test(), with 2 samples, or bartlett.test()
rs1a = bartlett.test(d0$Futurehapp ~ d0$Type, data=d0, na.action=na.omit)
print(rs1a)			
cat('\n')

# Try the log transform to stabilise variance
d0$lgfhap = log(d0$Futurehapp +.5)
rs1b = lm(d0$lgfhap ~ d0$Type, data=d0, na.action=na.omit)
contrasts(d0$Type, 2) = "contr.treatment"
print(summary(rs1b))
# To test 'homogeneity of variance', use var.test(), with 2 samples, or bartlett.test()
rs1ba = bartlett.test(d0$lgfhap ~ d0$Type, data=d0, na.action=na.omit)
print(rs1ba)			
cat('\n')


rs2 = lm(d0$Futurehapp ~ d0$Type + d0$Pasthapp, data=d0, na.action=na.omit)
print(summary(rs2))
rs3 = lm(d0$Futurehapp ~ d0$Type + d0$Pasthapp + d0$Responsible, data=d0, na.action=na.omit)
print(summary(rs3))
rs4 = lm(d0$Futurehapp ~ d0$Type + d0$Pasthapp + d0$Responsible + d0$FTP, data=d0, na.action=na.omit)
print(summary(rs4))
rs14 = anova(rs1,rs2,rs3,rs4)
print(rs14)

# lm() is designed to test if a quant var, Y, depends significantly on X1, X2, etc., which
# may be quant or categ.  However, Complain, C, is 0 or 1; it has the Binomial distrn, not the familiar
# Normal distrn. "C depends on R" means that "Prob(C = 1) depends on R". 
# To see if this relation is sig, use glm(), not lm().

rs1 = glm(complain ~ Responsible, data = d0, family = binomial, na.action = na.omit)
print(summary(rs1))

respcoef = coef(rs1)["Responsible"]	
CI95 = confint(rs1, parm = "Responsible")
cat("\n Effect of Responsible on Complain: ", respcoef, "\n 95% CI: ", CI95,"\n")

# Plot estimate of Prob(Complain = 1) against Responsible; save plot in *.pdf

pdf("hw2.logistic.pdf")
# par(mfrow=c(2,2))
xr = seq(0, 20, .5)		# possible values of Responsible
yc = predict(rs1, data.frame(Responsible = xr), type = 'response')	# predicted P(Complain) for given levels of Responsible
# plot(c(0, 20), c(0, 1), type = 'n', main = 'P(Complain) vs Responsible', xlab='Responsible',ylab='P(Complain)')		# type='n' sets up axes without plotting anything 
plot(d0$Responsible, d0$complain, type = 'p', main = 'P(Complain) vs Responsible', xlab='Responsible',ylab='P(Complain)')
lines(xr, yc)			# plots predicted curve
graphics.off()

sink(file=NULL, append=FALSE)