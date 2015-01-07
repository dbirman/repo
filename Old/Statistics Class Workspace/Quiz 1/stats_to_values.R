# Set up a data.frame

st = data.frame(xbar=3.0,ybar=3.8,SSx=134.4,SSy=401.1,SPxy=55.2,n=100)

# Compute

# if you don't know SPxy

# st$SPxy = sum[(x-xbar)*(y-bar)] expanded:
# st$SPxy = sum(x*y) - xbar*sum(y) - ybar*sum(x) + n*xbar*ybar
st$SPxy = st$sum_xy - st$xbar*st$sum_y - st$ybar*st$sum_x+st$n*st$xbar*st$ybar

# Pearon's r

r = st$SPxy / sqrt(st$SSy * st$SSx)
t_r = r * sqrt((st$n-2)/(1-r^2))
p_r = pt(t_r,st$n-2,lower.tail=F)

# Eqn: y = ax + b

b = st$SPxy / st$SSx # Slope
a = st$ybar - b*st$xbar # Intercept

var_y = st$SSy/(st$n-1)
s_y = sqrt(var_y)
var_x = st$SSx/(st$n-1)
s_x = sqrt(var_x)

se_y = s_y / sqrt(st$n)
se_x = s_x / sqrt(st$n)

SE of an estimate (of correlation)
se_est = sqrt((1-r^2) * st$SSy / (st$n-2))

95ci_y = c(st$ybar - se_y * 1.96,st$ybar + se_y*1.96)
90ci_y = c(st$ybar - se_y * 1.64,st$ybar + se_y*1.64)
  
# Predict using equation
  
# solve for y
x = 5
yv = a + b*x


# Set up a table:

T = as.table(matrix(),nrow=?,ncol=?,byrows=?)
mT = addmargins(T)

# Pooled Standard Deviation

df = n1+n2 - 2
pvar = (n1*var1 + n2*var2)/(n1+n2-2)

# Binomial Distribution 90% CI for variance

SS / var ~= chisq(df), so find P(chisq(df) < c1) = .05 and P(chisq(df) > .95). Use:
  qchisq(.05,df) to do this

# Cohen's D (test the difference of two populations)

(M1 - M2) / Pooled-SD

Pooled-SD = (var1 + var2) / 2

# Bivariate Distribution Table

Make sure you check that the % sizes are WITHIN the groups?

# Binomial distribution

n = number
p = prob
q = (1-prob)

Mean = np
variance = npq

Approximate MOE is np +- 1/sqrt(n)

Test for significance, find Z, first do continuity correction, then do: mean - h_0 / sqrt(npq)

Multiply by 2 for both ends if you use pbinom

# Calculate Sum of Squares from std and df

SS = sd ^ 2 * n-1 (variance * df)
