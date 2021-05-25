library(tidytext)
library(tidyverse)
# data ----
txt <- tibble(text = c( "A A B C D A B A C D A B D C A A A A C B","C B D A B A C A A A A C D A C B D A B A" ))

TriG <- txt %>% 
        unnest_tokens(word, text, token = "ngrams", n=3) %>% 
        separate(word, c("x", "y", "z")) %>% 
        unite(xy,x,y, sep = " " )

TriG <- TriG %>% split(TriG$xy)

# Counts c(x,y,z) ----
TriG.count <- lapply(TriG, function(df) df %>% group_by(xy,z) %>% count() %>% rename(c.xyz = n))

# MLE ----
MLE <- function(df.count){ 
        sum.c <- sum(df.count$c.xyz)                    # ojo
        df.count %>% mutate(ML = c.xyz/sum.c)
}

TriG.MLE <- lapply(TriG.count, MLE)

# Add-k smoothing ----

add.k <- function(df.count){
        V <- 4  # a,b,c,d
        m <- 1  # m = k*V
        sum.c <- sum(df.count$c.xyz)                    # ojo
        df.count %>% mutate( addk = ( c.xyz + m*(1/V) ) / (sum.c + m) )
}

TriG.addk <- lapply(TriG.count, add.k)

# Good-Turing smoothing ----
TriG.c.rev <- lapply(TriG.count, function(df){ df %>% mutate(c.rev=c.xyz)})                             # inicializar                

TriG.Nc <- lapply(TriG.count, function(df) df %>% group_by(c.xyz) %>% count() %>% rename(Nc = n))        # freq of freq
Nc.fit.list <- lapply(TriG.Nc, function(df){ lm1 <- lm(Nc ~ c.xyz, data = df) } )                        # Nc lm list to E(...) . Estudiar cual seria el mejor ajuste

c.rev <- function( df.count, Ncs ) {
        m <- max( df.count[,"c.xyz"] )
        n <- nrow( df.count[,"c.xyz"] ) 
        for( i in 1:n ){
                if( df.count[,"c.xyz"][i,] < m ){
                        C <- df.count[,"c.xyz"][i,]
                        Nc <- predict(Ncs[[df.count[,"xy"][[1]][i]]], 
                                      newdata = data.frame(c.xyz = df.count[,"c.xyz"][[1]][i]) )
                        Nc1 <- predict(Ncs[[df.count[,"xy"][[1]][i]]], 
                                      newdata = data.frame(c.xyz = (df.count[,"c.xyz"][i] + 1) ) )
                        df.count[,"c.rev"][i,] <- (C+1)*(Nc1/Nc)
                }
        }
        df.count
}
a <- lapply(TriG.c.rev, c.rev, Ncs = Nc.fit.list)

Nc <- sapply(TriG.c.rev["a a"], 
             function(x, Ncs){ 
                        Nc <- predict(Ncs, x)
             }, 
             Ncs = Nc.fit.list$`a a`)



# mejor ----
TriG.c.rev <- lapply(TriG.c.rev, 
                     function(x){
                             m <- max( x[,"c.xyz"] )
                             n <- nrow( x[,"c.xyz"] ) 
                             for( i in 1:n ){
                                     if( x[,"c.xyz"][i,] < m ){
                                             x[,"c.rev"][i,] <- 13
                                     }
                             }
                             x        
})


























