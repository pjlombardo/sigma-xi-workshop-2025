# Comparing success rates between normal 
# and t-dist approaches

pop<-rnorm(10000,100,10)
pop2<-ITcosts$x
mu<-mean(pop2)

samples<-lapply(1:5000,
                function(x){
                    s1<-sample(pop2, 3)
                    c(mean(s1),sd(s1))
                }) %>% unlist() %>% matrix(., nrow = 5000, byrow=T)

for (n in c(4:100)){
    temp<-lapply(1:5000,
                 function(x){
                     s1<-sample(pop2, n)
                     c(mean(s1),sd(s1))
                 }) %>% unlist() %>% matrix(., nrow = 5000, byrow=T)
    samples<-rbind(samples,temp)
}

df<-as.data.frame(samples)
colnames(df)<-c("mean","sd")
df$sample_size<-c(
    rep(3:100,each=5000)
)

df %>% mutate(
    MEnormal = 2*sd/sqrt(sample_size),
    ME_t = qt(1-.025,df = sample_size -1)*sd/sqrt(sample_size),
    InCI_normal = abs(mean - mu)<MEnormal,
    InCI_t = abs(mean-mu)<ME_t
) -> df2
get_pct<-function(logic){
    sum(logic==TRUE)/5000
}

df2 %>% group_by(sample_size) %>%
    summarise(normal_success_rate = get_pct(InCI_normal),
              t_success_rate = get_pct(InCI_t)) %>%
    pivot_longer(cols =-sample_size,
                 names_to = "method",
                 values_to = "success_rate") -> plot_df
write.csv(plot_df,"data/t-simulation-non-normal.csv",row.names=F)
