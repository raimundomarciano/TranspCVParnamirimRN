# Nivel 1: Unidade Gestora (1 result x 10 Elementos) ----
for (l0 in parnaLinksL0){
  parnaLinksL1_temp <- getLinks(l0)
  parnaLinksL1 <- c(parnaLinksL1, parnaLinksL1_temp)
}

# Nivel 2: Gestao (1 result x 1 UG x 10 Elementos) ----
for (l1 in parnaLinksL1){
  parnaLinksL2_temp <- getLinks(l1)
  parnaLinksL2 <- c(parnaLinksL2, parnaLinksL2_temp)
}

# Nivel 3: Favorecido (variavel) ----
for (l2 in parnaLinksL2){
  parnaLinksL3_temp <- getLinks(l2)
  parnaLinksL3 <- c(parnaLinksL3, parnaLinksL3_temp)
}

# Nivel 4: Documentos (Variavel) ----
pageTemp <- NULL
contagem <- 0

for(l3 in 1:length(parnaLinksL3)){
  #for(l3 in 50:60){
  contagem = contagem +1 
  print(c('else', contagem))
  urlL4 <- paste0("http://201.59.185.194:8080/transparencia/",parnaLinksL3[l3])
  parnaL4 <- GET(urlL4)
  # parnaL4 <- GET(url_diarias)
  # urlL4 <- url_diarias
  parnaL4 <- read_html(parnaL4)
  parnaTableListL4 <- parnaL4 %>% html_nodes("table.lbl")
  parnaTableListL4v2 <- parnaTableListL4 %>% html_nodes("a") %>% 
    html_attr("href")
  checkpaginacao <- str_detect(parnaTableListL4v2, "[']Page.+")
  #print(any(checkpaginacao))
  if (any(checkpaginacao)){
    pageTemp <- paginada(urlL4, parnaTableListL4)
    colnames(pageTemp) <- c("links")
    parnaLinksL4 <- rbind(parnaLinksL4, pageTemp)
    
  } else {
    pageTemp <- parnaTableListL4 %>% html_nodes("tr") %>% html_attr("onclick")
    pageTemp <- str_extract(pageTemp, pattern = "[\'].+[\']")
    pageTemp <- gsub("\'", "", pageTemp)
    pageTemp <- pageTemp[!is.na(pageTemp)]
    pageTemp <- as.data.frame(pageTemp)
    colnames(pageTemp) <- c("links")
    parnaLinksL4 <- rbind(parnaLinksL4, pageTemp) 
  }
}

# Nivel 5: Nota de Pagamento (1 x Documentos) ----
dadosL5 <- NULL
contagem <- 0
for (l4 in 1:nrow(parnaLinksL4)){
  contagem = contagem + 1
  print(c("linha", contagem))
  url <- paste0("http://201.59.185.194:8080/transparencia/",
                parnaLinksL4[l4,1])
  parnaL5 <- GET(url) %>% read_html() 
  
  elemDespesa = parnaL5 %>% html_nodes("table.resumo") %>% html_table() %>%
    as.data.frame() %>% select("X1") 
  elemDespesa <- str_extract(elemDespesa$X1, pattern = "Despesa:.+") %>%
    gsub("Despesa: ", "", .)
  elemDespesa <- elemDespesa[!is.na(elemDespesa)] 
  
  parnaTableListL5 <- parnaL5 %>% html_nodes("table.lbl") %>%
    html_table(fill = TRUE)
  parnaTableListL5v2parte1 <- as.data.frame(parnaTableListL5[[1]])
  parnaTableListL5v2parte2 <- as.data.frame(parnaTableListL5[[2]])
  
  a = data.frame(fase = parnaTableListL5v2parte1[2,2], 
                 documento = parnaTableListL5v2parte1[3,2], 
                 data = parnaTableListL5v2parte1[4,2], 
                 unidadeGestora = parnaTableListL5v2parte1[5,2], 
                 gestao = parnaTableListL5v2parte1[6,2], 
                 favorecido = parnaTableListL5v2parte1[7,2], 
                 valor = parnaTableListL5v2parte1[8,2],
                 observacaoDoc = parnaTableListL5v2parte2[2,2],
                 numeroProcesso = parnaTableListL5v2parte2[3,2],
                 elementoDespesa = elemDespesa)
  
  dadosL5 <- rbind(dadosL5, a)  
}

dadosL5 <- lapply(dadosL5, stringr::str_squish)
dadosL5 <- as.data.frame(dadosL5)
dadosL5$real <- str_replace(dadosL5$valor, "[.]", "")
dadosL5$real <- str_replace(dadosL5$real, "[,]", ".")
dadosL5$real <- as.numeric(dadosL5$real)

View(dadosL5)
sum(dadosL5$real)
dados5ajustado = unique(dadosL5)
sum(dados5ajustado$real) - 18854285.53
# valor esperado: 18.854.285,53
conferencia = dados5ajustado %>% group_by(elementoDespesa) %>%
  summarise(sum(real))
View(conferencia)

diarias = dadosL5 %>% filter(elementoDespesa == "DIÁRIAS - CIVIL")
View(diarias)