flip_tibble<-function(df){
    temp<-t(df)
    if (dim(temp)[2]==1){
        colnames(temp)<-"values"
    } else {
        tidyr::pivot_longer(df,-1) %>%
        tidyr::pivot_wider(names_from = 1, values_from = value) -> temp
    }
    return(temp)
}

discrete_pmf_plot<-function(distr_df){
    space_vals<-distr_df$space
    ggplot(data = distr_df,
           aes(x = space,
               y = pmf)) + 
        geom_col(color='gray21',fill="dodgerblue",alpha = .7)+
        geom_hline(yintercept =0, color='black')+
        scale_x_continuous( breaks = space_vals)+
        labs(x='Space',y="Probabilities",
             title="Distribution Plot")+
        theme_bw()
}

compare_barplot<-function(distr_df, sim1=NA){
    space_vals<-distr_df$space
    probs<-numeric(length(distr_df$space))
    j<-1
    for (i in distr_df$space){
        probs[j]<- sum(sim1==i)/length(sim1)
        j<-j+1
    }
    # print(probs)
    new_df<-data.frame(
        space = distr_df$space,
        pmf = distr_df$pmf,
        probs = probs
    )
    
    sim1[is.na(sim1)]<-0
    
    ggplot(data = new_df)+
        geom_col(aes(x = space, y = pmf),
                 fill='gray')+
        geom_col(aes(x = space, y = probs),
                 width =.15, fill='blue')+
        ylab("Probabilities")+
        scale_x_continuous(breaks = space_vals)+
        ggtitle("Comparing probabilities to\nsimulated relative frequencies")+
        theme_bw()
    
}

empirical_pct<-function(vec){
    m = mean(vec)
    s = sd(vec)

    tot<-sum(vec < m+2*s) - sum(vec < m-2*s)
    return(tot/length(vec))
}