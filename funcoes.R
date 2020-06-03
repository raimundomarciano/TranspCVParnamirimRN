# Funcoes ----

construtor <- function(ano = 2019){
  link = paste0("http://201.59.185.194:8080/transparencia/despesasdet.aspx?perfil=1&nvl=1&exercicio=",
  ano, "&pos=0&mes=12&fase=0")
  link
}

parnaL0 <- function(url){
  parnaL0html <- GET(url)
  parnaL0html <- read_html(parnaL0html)
  parnaTableL0 <- parnaL0html %>% html_nodes("table.lbl")
  parnaLinksL0 <- parnaTableL0 %>% html_nodes("tr") %>% html_attr("onclick")
  parnaLinksL0 <- str_extract(parnaLinksL0, pattern = "[\'].+[\']")
  parnaLinksL0 <- gsub("\'", "", parnaLinksL0)
  parnaLinksL0 <- parnaLinksL0[!is.na(parnaLinksL0)]
  parnaLinksL0
}

getLinks <- function(url){
  parnaLX <- GET(paste0("http://201.59.185.194:8080/transparencia/",url))
  parnaLX <- read_html(parnaLX)
  parnaTableLX <- parnaLX %>% html_nodes("table.lbl")
  parnaLinksLX_temp <- parnaTableLX %>% html_nodes("tr") %>% html_attr("onclick")
  parnaLinksLX_temp <- str_extract(parnaLinksLX_temp, pattern = "[\'].+[\']")
  parnaLinksLX_temp <- gsub("\'", "", parnaLinksLX_temp)
  parnaLinksLX_temp <- parnaLinksLX_temp[!is.na(parnaLinksLX_temp)]
}

paginada <- function(url, tabela){
  
  temp <- NULL
  links <- NULL
  holder <- NULL
  
  lista <- tabela %>% html_table(fill = TRUE) %>% as.data.frame()
  lista = c(lista[17,])
  lista = as.integer(lista)
  lista = max(lista, na.rm = T)
  
  links <- layer4tablep1(tabela)
  print("1")
  
  for (i in 2:lista){
    #holder <- temp
    temp <- layer4tablepX(url, i) 
    #if (identical(holder, temp)){
    #  break
    #} else {
    links <- rbind(links, temp)
    print(i) }
  #}
  links
}

layer4tablep1 <- function(tabela){
  linksP1 <- data.frame()
  linksTemp <- tabela %>% html_nodes("tr") %>% html_attr("onclick")
  linksTemp <- str_extract(linksTemp, pattern = "[\'].+[\']")
  linksTemp <- gsub("\'", "", linksTemp)
  linksTemp <- linksTemp[!is.na(linksTemp)]
  linksP1 <- as.data.frame(linksTemp)
  colnames(linksP1) <- c("links")
  linksP1
}

layer4tablepX <- function(url, i){
  pageX <- getPage(url, i)
  tabelapX <- pageX %>% read_html %>% html_nodes("table.lbl")
  linksTemp <- tabelapX %>% html_nodes("tr") %>% html_attr("onclick")
  linksTemp <- str_extract(linksTemp, pattern = "[\'].+[\']")
  linksTemp <- gsub("\'", "", linksTemp)
  linksTemp <- linksTemp[!is.na(linksTemp)]
  linksPX <- as.data.frame(linksTemp)
  colnames(linksPX) <- c("links")
  linksPX
}

getPage <- function(url, pagina){
  pagina <- paste0("Page$",pagina)
  session <- html_session(url)
  pgform<-html_form(session)[[1]]
  page<-rvest:::request_POST(session,url,
                             body=list(
                               `__VIEWSTATE`=pgform$fields$`__VIEWSTATE`$value,
                               `__EVENTTARGET`='ctl00$ContentPlaceHolder1$gvDet',
                               `__EVENTARGUMENT`=pagina,
                               `__VIEWSTATEGENERATOR`=pgform$fields$`__VIEWSTATEGENERATOR`$value,
                               `__VIEWSTATEENCRYPTED`=pgform$fields$`__VIEWSTATEENCRYPTED`$value,
                               `__EVENTVALIDATION`=pgform$fields$`__EVENTVALIDATION`$value
                             ),
                             encode="form"
  )
  page}

conferidor <- function(url, planGastos){
  html <- GET(url)
  html <- read_html(html)
  parnaTableL0 <- html %>% html_nodes("table.lbl") %>%
    html_table() 
  parnaTableL0 <- as.data.frame(parnaTableL0)
  colnames(parnaTableL0) <- c("grupoDespesas", "elementoDespesa", 
                              "exercCorrente", "restosPagar")
  parnaTableL0$exercCorrente <- str_replace_all(parnaTableL0$exercCorrente, "[.]", "")
  parnaTableL0$exercCorrente <- str_replace_all(parnaTableL0$exercCorrente, "[,]", ".")
  parnaTableL0$exercCorrente <- as.numeric(parnaTableL0$exercCorrente)
  parnaTableL0$restosPagar <- str_replace_all(parnaTableL0$restosPagar, "[.]", "")
  parnaTableL0$restosPagar <- str_replace_all(parnaTableL0$restosPagar, "[,]", ".")
  parnaTableL0$restosPagar <- as.numeric(parnaTableL0$restosPagar)
  parnaTableL0 <- parnaTableL0 %>% mutate(valorEsperado = 
                                            restosPagar + exercCorrente)
  
  scraped <- planGastos %>% select(elementoDespesa, real)
  #scraped$real <- str_replace_all(scraped$real, "[.]", "")
  #scraped$real <- str_replace_all(scraped$real, "[,]", ".")
  #scraped$real <- as.numeric(scraped$real)
  scraped <- scraped %>% group_by(elementoDespesa) %>% 
    summarise(valorCapturado = sum(real)) %>% ungroup()
  
  parnaTableL0 <- merge(parnaTableL0, scraped)
  
  parnaTableL0 <- parnaTableL0 %>% mutate(diagnostico = valorEsperado - valorCapturado)
  parnaTableL0 <- parnaTableL0 %>% select(elementoDespesa, valorEsperado,
                                          valorCapturado, diagnostico)
  parnaTableL0
}

#url_diarias <- "http://201.59.185.194:8080/transparencia/despesasdet.aspx?exercicio=2019&mes=12&pos=0&fase=0&perfil=1&nvl=4&flt=c%2by5bUIKMNRcQa2cP4gSidnWd%2bggPAuNX%2bYG9HqcgogWqhLEMCwkt8mf66Nxskl0APuL5X73WN2j7VcdKvAdDdosYgD%2fcO91exTznNzFYZYIkrx3KD0QWzVZjfADqEad&dt=20200529230111"