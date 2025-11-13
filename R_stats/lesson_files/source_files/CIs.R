set.seed(100)


EC_homestate<- data.frame(
    state = ifelse(rbinom(10000,1,.57)==1,"MA","Other")
)

wald_success_rate <-function(multiplier){
    p<-sum(EC_homestate$state=="MA")/10000
    some_samples<-sapply(1:5000, function(x){
        sum(sample(EC_homestate$state,35)=="MA")/35
    })
    
    SEs<- sqrt(some_samples*(1-some_samples)/30)
    
    return(sum(abs(some_samples - p)<multiplier*SEs)/length(some_samples))
}



get_wald<-function(n,p){
    x<-rbinom(1,n,p)
    CI<-BinomCI(x = x, n = n, method = "wald")[2:3]
    (CI[1]<p)&(p<CI[2])
}

get_AC<-function(n,p){
    x<-rbinom(1,n,p)
    CI<-BinomCI(x = x, n = n, method = "agresti-coull")[2:3]
    (CI[1]<p)&(p<CI[2])
}

get_wilson<-function(n,p){
    x<-rbinom(1,n,p)
    CI<-BinomCI(x = x, n = n, method = "wilson")[2:3]
    (CI[1]<p)&(p<CI[2])
}

wald_CI_success<-function(n,p){
    res<-sapply(1:3000, function(x){
        get_wald(n,p)
    })
    sum(res)/3000
}

agresti_coull_CI_success<-function(n,p){
    res<-sapply(1:3000, function(x){
        get_AC(n,p)
    })
    sum(res)/3000
}

wilson_CI_success<-function(n,p){
    res<-sapply(1:3000, function(x){
        get_wilson(n,p)
    })
    sum(res)/3000
}

MM_sample<-c("otherColor","otherColor","otherColor","blue","otherColor",
             "otherColor","otherColor","otherColor","otherColor","otherColor",
             "otherColor","blue","otherColor","blue","otherColor",
             "otherColor","otherColor","otherColor","otherColor","otherColor")

goldfish_sample<-c("alive","dead","alive","alive","alive",
                   "alive","alive","dead","alive","dead",
                   "alive","alive","alive","alive","alive")

