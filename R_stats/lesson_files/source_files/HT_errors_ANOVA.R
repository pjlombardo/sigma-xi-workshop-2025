pvalue_sim<-function(null_mu=100, n=30, sigma=20){
    pop<-rnorm(3000,100,sigma)
    pvals<-numeric(2000)
    for(i in 1:2000){
        ts<-sample(pop,n)
        pvals[i]<-t.test(ts,mu = null_mu)$p.value
    }
    
    pvals
}

hist_with_alpha<-function(pvals,alpha = 0.05, breaks = 0:10/10){
    
    ggplot()+
        geom_histogram(
            aes(x = pvals),
            breaks = breaks,
            color='gray3',fill='dodgerblue',alpha = .3
        ) + geom_vline(xintercept = alpha, color='red',
                       linewidth=1.1)
    # 
    # hist(pvals, freq=F,
    #      breaks = breaks, 
    #      col=rgb(0,0,1,.5))
    # lines(c(alpha,alpha),c(0,20),col='red',
    #       lwd = 2)
}

typeI_rate<-function(n, sigma,alpha){
    # get pvals with null_mu = 100
    pvals<-pvalue_sim(null_mu =100,
                      n = n,
                      sigma = sigma)
    # return the % that fall below alpha (leading to us incorrectly rejecting H0!)
    sum(pvals< alpha)/length(pvals)
}

typeII_rate<-function(null_mu, n, sigma, alpha){
    # get pvals with null_mu = 100
    pvals<-pvalue_sim(null_mu =null_mu,
                      n = n,
                      sigma = sigma)
    # return the % that are above alpha (leading to us incorrectly rejecting H0!)
    sum(pvals > alpha)/length(pvals)
}

two_sample_get_p<-function(i){
    t.test(rnorm(30),
           rnorm(30),
           var.equal = T)$p.value
}

