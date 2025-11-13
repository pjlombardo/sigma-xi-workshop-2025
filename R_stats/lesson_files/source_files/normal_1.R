

# Visual for the normal distribution
norm_vis<-function(a, b = NA, mu, sigma, tail = "lower"){
    xvals<-seq(mu -3.5*sigma, 
               mu +3.5*sigma,
               length.out = 1000)
    y_top <- max(dnorm(xvals,mu,sigma))
    df<-data.frame(x = xvals,
                   y= dnorm(xvals,mu,sigma))
    if(tail=="upper"){
        ggplot(data = df,
               aes(x = x, y=y))+
            geom_hline(yintercept = 0)+
            geom_ribbon(data = subset(df,x>a),
                        aes(x=x,ymax =y),ymin =0,
                        color='blue',
                        alpha = .3,
                        fill='dodgerblue')+
            geom_line(color='blue')+
            annotate(geom="text",size =5,x = mu+2*sigma, y = .85*y_top,
                     label=paste("P(X>=",a,")=\n",round(1-pnorm(a,mu,sigma),3),sep=""),
                     color='blue')+
            theme_bw()
    }else if(tail=="middle"){
        ggplot(data = df,
               aes(x = x, y=y))+
            geom_hline(yintercept = 0)+
            geom_ribbon(data = subset(df, (x>a & x<b)),
                        aes(x=x,ymax =y),ymin =0,
                        color='blue',
                        alpha = .3,
                        fill='dodgerblue')+
            geom_line(color='blue')+
            annotate(geom="text",size =5,x = mu+2*sigma, y = .85*y_top,
                     label=paste("P(",a,"<= X <=",b,")=\n",
                                 round(pnorm(b,mu,sigma)-pnorm(a,mu,sigma),3),sep=""),
                     color='blue')+
            theme_bw()
    } else {
        ggplot(data = df,
               aes(x = x, y=y))+
            geom_hline(yintercept = 0)+
            geom_ribbon(data = subset(df,x<a),
                        aes(x=x,ymax =y),ymin =0,
                        color='blue',
                        alpha = .3,
                        fill='dodgerblue')+
            geom_line(color='blue')+
            annotate(geom="text",size =5,x = mu+2*sigma, y = .85*y_top,
                     label=paste("P(X<=",a,")=\n",round(pnorm(a,mu,sigma),3),sep=""),
                     color='blue')+
            theme_bw()
    }
}

cplot<-ggplot(data = data.frame(x = bank$savings),
              aes(x = x))+
    geom_histogram(aes(
        # have to add a special `y` aesthetic to make this
        # a "density" histogram (i.e. on the same scale as
        # our normal distribution
        y = after_stat(density),
        fill="Histogram of\nSavings Accounts"
    ),
    color='white', bins = 15
    )+
    stat_function(aes(color="Normal R.V. Model\nmu = 2100\nsigma = 150"),
                  fun = dnorm,
                  geom="line",
                  args = list(mean = 2100,
                              sd = 150)) + 
    scale_color_manual("",values = c("blue"))+
    scale_fill_manual("",values = c("gray"))+
    labs(x = "Savings Account Values",
         title = "Comparing Account Data to a 'Good' Normal Probability Model")+
    theme_bw()

cplot_bad <-ggplot(data = data.frame(x = bank$savings),
              aes(x = x))+
    geom_histogram(aes(
        # have to add a special `y` aesthetic to make this
        # a "density" histogram (i.e. on the same scale as
        # our normal distribution
        y = after_stat(density),
        fill="Histogram of\nSavings Accounts"
    ),
    color='white', bins = 15
    )+
    stat_function(aes(color="Normal R.V. Model\nmu = 2200\nsigma = 200"),
                  fun = dnorm,
                  geom="line",
                  args = list(mean = 2200,
                              sd = 200)) + 
    scale_color_manual("",values = c("red"))+
    scale_fill_manual("",values = c("gray"))+
    labs(x = "Savings Account Values",
         title = "Comparing Account Data to a 'Bad' Normal Probability Model")+
    theme_bw()
