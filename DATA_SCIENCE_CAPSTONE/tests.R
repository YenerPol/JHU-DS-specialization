library(tidyverse)
data <- read_table2("./data/test.txt") %>% select(-X6)
# ---- counting ----
color_c <- data %>% group_by(lepisto, color) %>% count() %>% rename(columna = color, P = n)
alas_c <- data %>% group_by(lepisto, alas) %>% count() %>% rename(columna = alas, P = n)
tamano_c <- data %>% group_by(lepisto, tamano) %>% count() %>% rename(columna = tamano, P = n)
velocidad_c <- data %>% group_by(lepisto, velocidad) %>% count() %>% rename(columna = velocidad, P = n)

# ---- empty list - counts-MLE-laplace ----

lvls <- sapply(data, unique) #mucho mejor wey, que te paso

list_count <- list(data.frame(matrix(0, nrow = 2,ncol = length(lvls$color))),
                   data.frame(matrix(0, nrow = 2,ncol = length(lvls$alas))),
                   data.frame(matrix(0, nrow = 2,ncol = length(lvls$tamano))),
                   data.frame(matrix(0, nrow = 2,ncol = length(lvls$velocidad)))
                )        

names(list_count) <- c("color","alas","tamano","velocidad")

colnames(list_count$color) <- lvls$color
rownames(list_count$color) <- lvls$lepisto
colnames(list_count$alas) <- lvls$alas
rownames(list_count$alas) <- lvls$lepisto
colnames(list_count$tamano) <- lvls$tamano
rownames(list_count$tamano) <- lvls$lepisto
colnames(list_count$velocidad) <- lvls$velocidad
rownames(list_count$velocidad) <- lvls$lepisto

# ---- llenar function ----
llenar <- function(col = "", datos, list){
        for (i in 1:nrow(datos)) {
                list[[col]][datos$columna[i]][datos$lepisto[i], ] <- datos$P[i] 
        }
        list
}

list_count <- llenar(col = "color", color_c, list_count)
list_count <- llenar(col = "alas", alas_c, list_count)
list_count <- llenar(col = "tamano", tamano_c, list_count)
list_count <- llenar(col = "velocidad", velocidad_c, list_count)
# ---- prob_func funcion recive list_count and return MLE.  Can make smoothing. ----
prob_func <- function(list1 = list_count, smoothing = "MLE", kin){
        outcome_list <- list1
        if(smoothing == "MLE" | smoothing == "LAP"){ 
                k <- if(smoothing == "MLE"){0}else{kin}
                i <- 0
                end_i <- length(list1)
                for (i in 1:end_i) { # recorre lista
                       B <- ncol(list1[[i]])
                       nA <- rowSums(list1[[i]])
                       list1[[i]] <- ( list1[[i]] + k )/( nA + k * B)
                }
        }else if(smoothing == "laplace"){
                outcome_list <- "laplace"
        }else{
                outcome_list <- "otro"
        }
        list1
}

list_MLE <- prob_func(list1 = list_count, smoothing = "MLE")
list_laplace <- prob_func(list1 = list_count, smoothing = "LAP", kin = 0.5)

data %>% group_by(.[["lepisto"]]) %>% count() %>% mutate(n/10)

0.3 * list_laplace[["color"]]["+","amarillo"] * 
        list_laplace[["alas"]]["+","no"] * 
        list_laplace[["tamano"]]["+","peque"] *
        list_laplace[["velocidad"]]["+","alta"]

0.7 * list_laplace[["color"]]["-","amarillo"] * 
        list_laplace[["alas"]]["-","no"] * 
        list_laplace[["tamano"]]["-","peque"] *
        list_laplace[["velocidad"]]["-","alta"]
