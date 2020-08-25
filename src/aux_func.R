# scripts auxiliares para tratar janelas de plantio do ZARC e GS

cores_zarc <- c("20" = "#262168",
                "30" = "#1eaa28",
                "40" = "#e28d19",
                "50" = "#ff4a21",
                "0"  = "#d9d9d9")

# funcao para abrir decendios do ZARC
# conforme gerados pelo MICURA.
# é preciso baixar planilha do Micura para processar usando essa função
abre_zarc_micura <- function(arquivo) {
  
  tabua_risco <-  readxl::read_xls(arquivo)
  
  zarc <- tabua_risco %>%
    select(UF, Geocodigo, D1:D36) %>%
    rename(uf = UF, geocodigo = Geocodigo) %>%
    gather(decendio, freq, -uf, -geocodigo) %>%
    mutate(
      decendio = parse_number(decendio),
      freq = as.numeric(freq),
      freq = if_else(freq == 0, 100, freq)
    ) %>%
    filter(
      !is.na(geocodigo),
      #freq > 0
    )
    
  return(zarc)
}

# extrai o risco mínimo de cada município e n. decendios aptos
# zarc é a tabela do ZARC, conforme gerada pela função abre_zarc_micura
risco_minimo_munic <- function(zarc) {
  risco <- zarc %>%
    group_by(uf, geocodigo) %>%
    summarise(
      risco_min = min(freq),
      decendios_20 = sum(freq == 20),
      decendios_30 = sum(freq <= 30),
      decendios_40 = sum(freq <= 40),
    ) %>%
    ungroup()
  
  return(risco)
}

# função original passada pelo Alan
# Alterações para permitir escolher freq do risco
# dados do Alan eram apenas 0 e 1, para todos decendios
# eu tenho apenas dados para decendios aptos
encontra_janelas_aptas = function(decendio, frequencia, risco = 20) {
  janelas = with(rle(frequencia <= risco), {
    #ok <- values <= risco
    ends_idx <- cumsum(lengths)[values]
    starts_idx <- ends_idx - lengths[values] + 1
    cbind(decendio[starts_idx], decendio[ends_idx])
  })
  
  njanelas = length(janelas[,1])
  
  if (njanelas > 1 &&
      janelas[1,1] == 1 && janelas[njanelas, 2] == 36) {
    janelas[njanelas,2] = janelas[1,2]
    janelas = janelas[2:njanelas,]
  }
  
  return(janelas)
  
}

# Obtém maior janela para municípios com janelas múltiplas
acha_maior_janela <- function(janelas) {
  if (length(janelas) == 2) {
    return(janelas)
  }
  
  comprimento <- apply(janelas, 1, diff) + 1
  jan_idx <- which(comprimento == max(comprimento))
  
  if (length(jan_idx > 1)) {
    return(janelas[jan_idx[1],])
  } else {
    return(janelas[jan_idx,])
  }

}


#### Velharia - mantendo por hora, não sei se irei precisa ####

# definir pasta onde o nextcloud armazena os dados
# para poder rodar mesmo código de outro computador
cloud_local <- '/home/daniel/Nextcloud/dados/'



# plota riscos do zarc junto com a janela do GS para a UF/Região
# permite plotar uma janela customizada, mostrando qual seria um ajuste possível
plota_zarc_GS <- function(zarc, uf, regiao_gs, cultura,
                          dec_ini = 0, dec_fim = 0) {
  
  cores_zarc <- c("80" = "#262168",
                  "70" = "#1eaa28",
                  "60" = "#e28d19",
                  "50" = "#ff4a21",
                  "0"  = "#d9d9d9")
  
  dados_gs <- dec_gs %>%
    filter(UF == uf,
           gs_reg == regiao_gs)
  
  if (nrow(dados_gs) == 0) {
    return(cat(paste('Não existem dados para uf', uf, 'região', regiao_gs)))
  }
  
  dados_zarc <- zarc %>%
    filter(geocodigo %in% dados_gs$geocodigo)
  
  if (nrow(dados_zarc) == 0) {
    return(cat(paste('Não existe janela ZARC para uf', uf, 'cultura', cultura)))
  }
  
  # ordenando municípios no heatmap pela
  # quantia de descendios aptos
  total_apto <- dados_zarc %>%
    group_by(geocodigo) %>%
    summarise(apto = sum(as.numeric(freq)),
              apto80 = sum(as.numeric(freq == 80)),
              apto70 = sum(as.numeric(freq == 70)),
              apto60 = sum(as.numeric(freq == 60)))
  
  dados_zarc <- dados_zarc %>%
    mutate(geocodigo = factor(geocodigo,
                              levels = total_apto$geocodigo[order(total_apto$apto80,
                                                                  total_apto$apto70,
                                                                  total_apto$apto60,
                                                                  total_apto$apto, decreasing = TRUE)]))
  
  
  # para customizar janela, é preciso definir tanto inicial quanto final
  if (dec_ini == 0) {
    janela <- janela_GS(dados_gs)
    dec_ini <- janela$ini
    dec_fim <- janela$fim
    poly_fill <- 'black'
    poly_alpha <- 0.4
    poly_color <- 'black'
    poly_linetype <- 1
  } else{
    poly_fill <- NA
    poly_alpha <- 0.4
    poly_color <- 'red'
    poly_linetype <- 2
  }
  
  poli_janela_gs <- poligono_janela(dec_ini, dec_fim, length(unique(dados_gs$geocodigo)))
    
  graf <- ggplot(dados_zarc) +
    geom_tile(aes(x = decendio, y = geocodigo, fill = as.factor(freq))) +
    scale_fill_manual(values = cores_zarc) +
    geom_polygon(data = poli_janela_gs, mapping = aes(y = geocodigo, x = decendio, group = grupo),
              fill = poly_fill, alpha = poly_alpha, color = poly_color, linetype = poly_linetype) +
    labs(x = 'Decêndio', y = 'Municípios', fill = 'Frequência de sucesso') +
    ggtitle('Janelas de plantio do ZARC vs GS',
            subtitle = paste0('Cultura: ', cultura, "; UF: ", uf, '; Região GS: ', regiao_gs)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = 'bottom'
    )
}

# obter limites das janela do GS
janela_GS <- function(dados_gs) {
  munic <- dados_gs$geocodigo[1]
  janela <- dados_gs %>%
    filter(geocodigo == munic)
  
  # caso janela 'vire o ano', decendio 36 e 1 são TRUE
  if (janela$plantio[janela$decendio == 1] & janela$plantio[janela$decendio == 36]) {
    dec <- janela$decendio[janela$plantio == TRUE]
    quebra <- which(diff(dec) > 1) # posicao da quebra da janela
    
    dec_ini <- dec[quebra + 1]
    dec_fim <- dec[quebra]
    
  } else {
    dec_ini <- min(dados_gs$decendio[dados_gs$plantio == TRUE])
    dec_fim <- max(dados_gs$decendio[dados_gs$plantio == TRUE])
  }
  
  return(list(ini = dec_ini, fim = dec_fim))
}

# data.frame do polígono da janela
# em função dos decendios de inicio, fim e número de municípios
poligono_janela <- function(dec_ini, dec_fim, n_municipios) {

  # caso janela 'vire o ano', decendio 36 e 1 são TRUE
  if (dec_ini > dec_fim) {
    limites <- data.frame(geocodigo = c(0.5, 0.5, n_municipios + 0.5,
                                        n_municipios +0.5,
                                        0.5, 0.5, n_municipios +0.5,
                                        n_municipios + 0.5),
                          decendio = c(0.5, dec_fim + 0.5,
                                       dec_fim + 0.5,
                                       0.5,
                                       dec_ini - 0.5,
                                       36.5, 36.5,
                                       dec_ini - 0.5),
                          grupo = c(1, 1, 1, 1, 2, 2, 2, 2)
                          )
  } else {
    limites <- data.frame(geocodigo = c(1, 1, n_municipios,n_municipios),
                          decendio = c(dec_ini - 0.5,
                                       dec_fim + 0.5,
                                       dec_fim + 0.5,
                                       dec_ini -0.5),
                          grupo = c(1, 1, 1, 1)
    )
  }
}


# funcao para abrir decendios do ZARC
# conforme enviados ao MAPA.
# Inclui risco de 50%
# nome da pasta e arquivo não está padronizado
# por isso é preciso apontar o nome do arquivo completo
# pegar o arquivo TXT dentro da pasta de tábua de risco
abre_zarc <- function(arquivo) {
  
  tabua_risco <- read_csv2(arquivo)
  
  zarc <- tabua_risco %>%
    select(uf, geocodigo, dec1:dec36) %>%
    gather(decendio, risco, -uf, -geocodigo) %>%
    mutate(decendio = parse_number(decendio)) %>%
    filter(!is.na(geocodigo))
  
  # tabela traz risco do decendio, com valores de 20, 30, 40 ou 50
  # precisa converter em prob. sucesso
  # atenção - erro na tabela. Zero (0) não quer dizer 0 risco!!
  zarc <- zarc %>%
    mutate(freq = ifelse(risco > 0, 100 - risco, 0))
  
}



# tabula quantos decendios temos em cada faixa de risco
# considerando toda a região do GS
# dec_min e dec_max: alterar os limites da janela do GS para avaliar o risco
conta_risco_zarc_GS <- function(zarc, uf, regiao_gs,
                                dec_ini = 0, dec_fim = 0,
                                verbose = TRUE) {
  dados_gs <- dec_gs %>%
    filter(UF == uf,
           gs_reg == regiao_gs)
  
  if (nrow(dados_gs) == 0) {
    return(cat(paste('Não existem dados para uf', uf, 'região', regiao_gs)))
  }
  
  limites_janela <- janela_GS(dados_gs)
  
  if (dec_ini == 0) {
    dec_ini <- limites_janela$ini
  }
  
  if (dec_fim == 0) {
    dec_fim <- limites_janela$fim
  }
  
  if (verbose) {
    cat(paste("dec_ini:", dec_ini, "dec_fim:", dec_fim, "\n"))
  }
  
  dados_zarc <- zarc %>%
    filter(geocodigo %in% dados_gs$geocodigo)
  
  if (nrow(dados_zarc) == 0) {
    return(cat(paste('Não existe janela ZARC para uf', uf, 'cultura', cultura)))
  }
  
  if (dec_ini < dec_fim) {
    # janela está dentro do ano - fácil
    dados_zarc %>%
      filter(decendio >= dec_ini,
             decendio <= dec_fim) %>%
      group_by(freq = as.factor(freq)) %>%
      summarise(numero = n()) %>%
      mutate(porcento = numero / sum(numero)) 
  } else {
    # janela do GS vira o ano
    dados_zarc %>%
      filter(decendio >= dec_ini |
             decendio <= dec_fim) %>%
      group_by(freq = as.factor(freq)) %>%
      summarise(numero = n()) %>%
      mutate(porcento = numero / sum(numero)) 
  }
}
