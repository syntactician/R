```r
z = c(1,-1.5,.75)    # coefficients of the polynomial
(a = polyroot(z)[1]) # = 1+0.57735i,  print one root which is 1 + i 1/sqrt(3)
arg = Arg(a)/(2*pi)  # arg in cycles/pt  
1/arg                # = 12,  the period
```

```R
set.seed(8675309)    # Jenny, I got your number
ar2 = arima.sim(list(order=c(2,0,0), ar=c(1.5,-.75)), n = 144)
plot(ar2, axes=FALSE, xlab="Time")
axis(2); axis(1, at=seq(0,144,by=12)); box()  # work the plot machine
abline(v=seq(0,144,by=12), lty=2)
```

```{r}
ACF = ARMAacf(ar=c(1.5,-.75), ma=0, 50)
plot(ACF, type="h", xlab="lag")
abline(h=0)
```
