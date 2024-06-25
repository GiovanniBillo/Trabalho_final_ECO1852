### funcoes auxiliarias ###
# por importar dados talvez è melhor ter algumas funcoes auxiliarias, jà que o procedimento è repetitivo
primeira_diferenca <- function(serie){
  for (t in 2:nrow(serie)) {
    serie$serie[t] <- (serie$serie_old[t] - serie$serie_old[t-1])
  }
  return(serie)
}


variacao_percentual = function(serie){
  for (t in 2:nrow(serie)) {
    serie$new[t] <- (serie$old[t]/serie$old[t-1] - 1) * 100
  }
  return(serie)
}

get_series_safe <- function(index, start_date, end_date) {
  tryCatch({
    serie <- get_series(index, start_date = start_date, end_date = end_date, as = "data.frame")
    return(serie)
  }, error = function(e) {
    warning("Error fetching data for index ", index, ": ", conditionMessage(e))
    # Create a data frame with NA values
    return(data.frame(date = seq.Date(from = as.Date(start_date), by = "month", length.out = 149), old = rep(NA, 149)))
  })
}

baixa_series <- function(tabela) {
  for (i in seq_along(tabela$Nome)) {
    index <- tabela$Codigo.SGS[i]
    
    serie <- get_series_safe(index, start_date = "2011-10-01", end_date = "2024-03-31")
    
    if (ncol(serie) != 2) {
      warning("Skipping index ", index, " due to unexpected number of columns")
      next  # Skip to the next iteration
    }
    
    colnames(serie)[2] <- "old"
    serie$new <- rep(NA, nrow(serie))
    
    if (tabela$Transformacao[i] == "Variacao percentual") {
      serie <- variacao_percentual(serie)
      # browser()
      serie <- serie[-1, ]
    } else if (tabela$Transformacao[i] == "Primeira diferenca") {
      serie <- primeira_diferenca(serie)
      serie <- serie[-1, ]
    }
    
    colnames(serie)[3] <- tabela$Nome[i]
    serie <- serie %>% select(date, tabela$Nome[i])
    assign(tabela$Nome[i], serie, envir = .GlobalEnv)
  }
}

baixa_series(tabela)
