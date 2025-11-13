set.seed(8732)
jelly_jar<-read.csv('data/jelly_jar.csv', header = T,
                    stringsAsFactors = T)
samp_dist_pct<-sapply(1:5000, function(x){
    sum(sample(jelly_jar$color,200)=="green")/200
})
people<-data.frame(
    heights = round(c(rnorm(12234,68.3,2.6),
                      rnorm(14231,63.2,2.2)),1),
    gender = c(rep("male",12234),
               rep("female",14231))
)
samp_dist_means<-sapply(1:5000, function(x){
    mean(sample(people$heights,50))
})

compare_sampdist_norm<-function(vector, our_mean, our_stdev, bins = 20){
    ggplot(data = data.frame(
        samp_means = vector),
        aes(x = samp_means)
    )+
        geom_histogram(aes(y=after_stat(density)),
                       color='white',bins = bins,
                       alpha = .5)+
        stat_function(fun = dnorm,args = list(mean = our_mean,
                                              sd = our_stdev),
                      geom="line", color='blue',linewidth=1)+theme_bw()
}