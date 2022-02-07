library(stats)
## set random number seed
#set.seed(123)
## length of time series
TT=1000
## setup plot area
par(mfrow = c(1, 2))
# initialize for running Number of Similations
Nsamples=1
sum=NULL
average=NULL
sum_acf=NULL
average_acf=NULL
single_acf=NULL
acf_time=NULL
x=NULL
alfa=0.6
for (i in 1:TT){
    sum[i]=0
    average[i]=0
    x[i]=0
    if(i==TT){
        break
    }else{
        sum_acf[i]=0
        average_acf[i]=0
        #single_acf[t]=0
    }
}
sum_acf_=data.frame(sum_acf)
#
for (i in 1:TT-1){
    x[i]=i-1
}
x_plot=x[1:TT-1]
x_plot_=data.frame(x_plot)
#
for (i in 1:Nsamples){
    ## initialize {x_t} and {w_t}
    xx=ww=rnorm(n = TT, mean = 0, sd = 0.5)
    sum[1]=sum[1]+xx[1]
    ylim=c(-5,5)
    ## compute values 2 thru TT
    for (t in 2:TT) {
        xx[t]=alfa*xx[t - 1] + ww[t]
        sum[t]=sum[t]+xx[t]
    }
    print(i)
    if(i==1){
        plot(xx, ylab = expression(italic(x[t])), type="l", col="gray", ylim=ylim)    
    }else{   
        lines(xx, ylab = expression(italic(x[t])), col="gray", ylim=ylim)
    }
    data_acf=acf(xx, plot=FALSE,lag.max=(TT-1))
#    single_acf=data.frame(x_plot_,data_acf$acf)
#    if(i==1){
#        plot(single_acf,ylim=c(-0.2,1), type="l")
#    }else{
#        lines(single_acf,ylim=c(-0.2,1), type="l")
#    }
    sum_acf_=sum_acf_+data_acf$acf
}
#
average[1:TT]=sum[1:TT]/Nsamples
average_acf=sum_acf_/Nsamples
#print(average_acf)
## plot line
# plot.ts(xx, ylab = expression(italic(x[t])))
#lines(average,col="red")
## plot ACF
#acf(average, plot = TRUE)
average_acf_=data.frame(x_plot_,average_acf)
plot(average_acf_,ylim=c(-0.2,1), type="l",col="red")
#acf(average,plot=TRUE,lag.max=(TT-1),col="red")
#lines(average_acf_,ylim=c(-0.2,1))



