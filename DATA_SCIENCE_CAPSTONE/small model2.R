library(tidytext)
library(tidyverse)
# data ----
#txt <- data.frame( text = c( "he is he is he is going abroad is going to study in the field" ) )
txt <- data.frame( text = c("JOHN READ MOBY DICK", "MARY READ A DIFFERENT BOOK", "SHE READ A BOOK BY CHER"))
TriG <- txt %>% 
        unnest_tokens( word, text, token = "ngrams", n=2 ) %>% 
        separate( word, c("y", "z" ) ) 
TriG <- TriG %>% split( TriG$y )

# Counts c(x,y,z) ----
TriG.count <- lapply( TriG, function(df) df %>% group_by( y, z ) %>% count() %>% rename( c.yz = n ) )
