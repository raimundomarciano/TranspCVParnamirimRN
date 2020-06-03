library(httr)
library(rvest)
library(stringr)
library(dplyr)

source("funcoes.R")


scrapeCamara <- function(ano){
  
  url_inicial <- construtor(ano)
  
  parnaLinksL0 <- NULL
  parnaLinksL1 <- NULL
  parnaLinksL2 <- NULL
  parnaLinksL3 <- NULL
  parnaLinksL4 <- NULL
  
# Nivel 0: Elemento da Despesa ----
# 2019 (10 results) 
  parnaLinksL0 <- parnaL0(url_inicial)
  
  # Nivel 1: Unidade Gestora ----
  # 2019: (1 result x 10 Elementos) 
  for (l0 in parnaLinksL0){
    parnaLinksL1_temp <- getLinks(l0)
    parnaLinksL1 <- c(parnaLinksL1, parnaLinksL1_temp)
  }
  
  # Nivel 2: Gestao  ----
  # 2019: (1 result x 1 UG x 10 Elementos) 
  for (l1 in parnaLinksL1){
    parnaLinksL2_temp <- getLinks(l1)
    parnaLinksL2 <- c(parnaLinksL2, parnaLinksL2_temp)
  }
  
  # Nivel 3: Favorecido (variavel) ----
  pageTemp <- NULL
  contagem <- 0
  
  for (l2 in parnaLinksL2){
    
    urlL3 <- paste0("http://201.59.185.194:8080/transparencia/", l2)
    parnaL3 <- GET(urlL3)
    parnaL3 <- read_html(parnaL3)
    parnaTableListL3 <- parnaL3 %>% html_nodes("table.lbl")
    parnaTableListL3v2 <- parnaTableListL3 %>% html_nodes("a") %>% 
      html_attr("href")
    checkpaginacao <- str_detect(parnaTableListL3v2, "[']Page.+")
    #print(any(checkpaginacao))
    if (any(checkpaginacao)){
      pageTemp <- paginada(urlL3, parnaTableListL3)
      colnames(pageTemp) <- c("links")
      parnaLinksL3 <- rbind(parnaLinksL3, pageTemp)
    } else {
      pageTemp <- parnaTableListL3 %>% html_nodes("tr") %>% html_attr("onclick")
      pageTemp <- str_extract(pageTemp, pattern = "[\'].+[\']")
      pageTemp <- gsub("\'", "", pageTemp)
      pageTemp <- pageTemp[!is.na(pageTemp)]
      pageTemp <- as.data.frame(pageTemp)
      colnames(pageTemp) <- c("links")
      parnaLinksL3 <- rbind(parnaLinksL3, pageTemp) 
    }
  }
  
  # Nivel 4: Documentos (Variavel) ----
  pageTemp <- NULL
  contagem <- 0
  
  for(l3 in 1:nrow(parnaLinksL3)){
    #for(l3 in 50:60){
    contagem = contagem +1 
    print(c('else', contagem))
    urlL4 <- paste0("http://201.59.185.194:8080/transparencia/",parnaLinksL3[l3,1])
    parnaL4 <- GET(urlL4)
    parnaL4 <- read_html(parnaL4)
    parnaTableListL4 <- parnaL4 %>% html_nodes("table.lbl")
    parnaTableListL4v2 <- parnaTableListL4 %>% html_nodes("a") %>% 
      html_attr("href")
    checkpaginacao <- str_detect(parnaTableListL4v2, "[']Page.+")
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
    
    parnaTableListL5 <- parnaL5 %>% html_nodes("table.lbl")
    
    erro <- tryCatch(html_table(parnaTableListL5, fill = TRUE), error = function(e) print(e))
    erro <- unlist(erro$message)
    erro <- str_detect(erro, pattern = "muito grande ou NA") 
    if (length(erro) != 0){next} else{
      
      parnaTableListL5 <- parnaTableListL5 %>% html_table(fill = TRUE)
      
      parnaTableListL5v2parte1 <- as.data.frame(parnaTableListL5[[1]])
      parnaTableListL5v2parte2 <- as.data.frame(parnaTableListL5[[2]])
      
      dataL5 <- data.frame(fase = parnaTableListL5v2parte1[2,2], 
                           documento = parnaTableListL5v2parte1[3,2], 
                           data = parnaTableListL5v2parte1[4,2], 
                           unidadeGestora = parnaTableListL5v2parte1[5,2], 
                           gestao = parnaTableListL5v2parte1[6,2], 
                           favorecido = parnaTableListL5v2parte1[7,2], 
                           valor = parnaTableListL5v2parte1[8,2],
                           observacaoDoc = parnaTableListL5v2parte2[2,2],
                           numeroProcesso = parnaTableListL5v2parte2[3,2],
                           elementoDespesa = elemDespesa,
                           link = url)
      
      dadosL5 <- rbind(dadosL5, dataL5)  }
  }
  
  dadosL5 <- lapply(dadosL5, stringr::str_squish)
  dadosL5 <- as.data.frame(dadosL5)
  dadosL5$real <- str_replace(dadosL5$valor, "[.]", "")
  dadosL5$real <- str_replace(dadosL5$real, "[,]", ".")
  dadosL5$real <- as.numeric(dadosL5$real)
  
  dadosL5
}

gastos2019 <- scrapeCamara(2019)

conferidor(construtor(2019), gastos2019)

gastos2017 <- scrapeCamara(2017)
conferidor(construtor(2017), gastos2017)


gastos2017_copy <- gastos2017 %>% filter(elementoDespesa ==  "VENCIMENTOS E VANTAGENS FIXAS - PESSOAL CIVIL") %>%
  group_by(favorecido) %>% summarise(sum(real))
