library(tidyverse)

df_3 <- expand.grid(c("a","b","c"),c("a","b","c"),c("a","b","c"))

# 3-grams model ----
lista_3_gram <- df_3 %>% split(df_3$Var1)
lista_3_gram <- lapply(lista_3_gram, function(x) x %>% split(.[]$Var2) )
lista_3_gram <- lapply( X = lista_3_gram, FUN = lapply, "[", c("Var3") )

# 2-gram model special ----

df_2_spec <- df_3 %>% select("Var1","Var3") %>% mutate(n = 10)

lista_2_gram_spec <- df_2_spec %>% split(df_3$Var1)
borrar <- lapply(lista_2_gram_spec, function(x) x %>% group_by(Var3) %>% summarize(sum(n)) %>% arrange("sum(n)") )     

# nivel_1 <- df_2_spec %>% 
#                 group_by(Var1,Var3) %>%
#                 summarize(Count = n()) %>% 
#                 split(.[]$Var1) 
# 
# new_list <- lapply(nivel_1, "[", c("Var3", "Count"))

  

