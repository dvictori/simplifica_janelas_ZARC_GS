# Cruza mapas
# ideia é fazer classificação com duas variáveis:
# 1) onde se pode plantar
# 2) quando se pode plantar
# usar ZARC milho (20 e 30%), sorgo (20 e 30%) e mamona (20 e 30%)

library(tidyverse)
library(sf)
library(mapview)
source('src/aux_func.R')

resumo_zarc <- readRDS('results/resumo_zarc.RDS') %>%
  filter(cultura != 'milheto') %>%
  mutate(
    tolerancia = 
      case_when(
        cultura == 'milho' ~ 'sem',
        cultura == 'sorgo' ~ 'média',
        cultura == 'mamona' ~ 'alta'
      )
  )

resumo_zarc$tolerancia <- factor(resumo_zarc$tolerancia,
                                 levels = c('sem', 'média', 'alta'))

janelas_zarc <- readRDS('results/janelas_zarc.RDS') %>%
  filter(cultura != 'milheto') %>%
  mutate(
    tolerancia = 
      case_when(
        cultura == 'milho' ~ 'sem',
        cultura == 'sorgo' ~ 'média',
        cultura == 'mamona' ~ 'alta'
      )
  )

janelas_zarc$tolerancia <- factor(janelas_zarc$tolerancia,
                                  levels = c('sem', 'média', 'alta'))

resumo_zarc_gs <- resumo_zarc %>%
  filter(uf %in% c('MA', 'PI', 'CE', 'RN', 'PB',
                   'PE', 'AL', 'SE', 'BA', 'MG')
  )

# brasil <- read_sf('../../../../geodb/IBGE/bc250_2019-10-29.gpkg',
#                   layer = 'lml_municipio_a') %>%
#   mutate(cod_uf = substr(geocodigo, 1, 2))

brasil <- read_sf('../../../../geodb/IBGE/bcim_2016_18_09_2018.gpkg',
                  layer = 'lim_municipio_a') %>%
  select(nome, geocodigo) %>%
  mutate(cod_uf = substr(geocodigo, 1, 2))

# uf <- read_sf('../../../../geodb/IBGE/bc250_2019-10-29.gpkg',
#               layer = 'lml_unidade_federacao_a')

uf <- read_sf('../../../../geodb/IBGE/bcim_2016_18_09_2018.gpkg',
              layer = 'lim_unidade_federacao_a')

sigla_uf <- uf %>%
  st_drop_geometry() %>%
  select(sigla, 
         cod_uf = geocodigo)

brasil_gs <- brasil %>%
  left_join(sigla_uf) %>%
  filter(
    sigla %in% c('MA', 'PI', 'CE', 'RN', 'PB',
                 'PE', 'AL', 'SE', 'BA', 'MG')
  )

#### Identificando tipo de cultura adequado ###

# considerando apto municípos com pelo menos 2 decendios
# contando em perídos de <1 mes, 1 a 2 meses e > 2 meses
aptos <- resumo_zarc_gs %>%
  filter(n >= 1,
         freq %in% c(20, 30)) %>%
  mutate(meses = cut(dec_acum,
                      breaks =  c(0, 3, 6, 36),
                      labels = c('até 1 mes', '1 a 2 meses', '> 2 meses'))
  )

brasil_gs %>%
  inner_join(aptos) %>%
  ggplot() +
  geom_sf(aes(fill = meses), col = NA) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
  facet_wrap(~ freq + cultura) +
  labs(fill = 'ZARC: Meses aptos para plantio - solo 2')

# Maior interesse é no milho (20%), depois sorgo (20%).
# Por último fecha mapa com mamona (20 e 30%)

aptos_2 <- aptos %>%
  filter(cultura == 'milho' & freq == 20 |
           cultura == 'sorgo' & freq == 20 |
           cultura == 'mamona')

brasil_gs %>%
  inner_join(aptos_2) %>%
  ggplot() +
  geom_sf(aes(fill = meses), col = NA) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
  facet_wrap(~ cultura + freq) +
  theme(legend.position = 'bottom') +
  labs(fill = 'ZARC: Meses aptos para plantio - solo 2')

ggsave('figs/meses_aptos_para_plantio.png')

brasil_gs %>%
  inner_join(aptos_2) %>%
  ggplot() +
  geom_sf(aes(fill = meses), col = NA) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
  facet_wrap(~ tolerancia + freq) +
  theme(legend.position = 'bottom') +
  labs(fill = 'ZARC: Meses aptos para plantio - solo 2')

ggsave('figs/meses_aptos_para_plantio_sem_indicar_cultura.png')

aptos_wide <- aptos_2 %>%
  pivot_wider(id_cols = c(uf, geocodigo),
              names_from = c(tolerancia,  freq),
              values_from = meses)

# convertendo NA para 'não apto'
# Primeiro adiciona level 'Não apto' e depois troca valor
addNaoApta <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "Não apto")))
  return(x)
}

aptos_wide <- as.data.frame(lapply(aptos_wide, addNaoApta))

aptos_wide$sem_20[is.na(aptos_wide$sem_20)] <- 'Não apto'
aptos_wide$média_20[is.na(aptos_wide$média_20)] <- 'Não apto'
aptos_wide$alta_20[is.na(aptos_wide$alta_20)] <- 'Não apto'
aptos_wide$alta_30[is.na(aptos_wide$alta_30)] <- 'Não apto'

aptos_wide$classe <- interaction(aptos_wide$sem_20,
                                 aptos_wide$média_20,
                                 aptos_wide$alta_20,
                                 aptos_wide$alta_30,
                                 drop = TRUE)

# Ao todo temos 36 combinações. Preciso realizar alguns agrupamentos
aptos_wide$classe <- NA

# reduzindo para onde pode milho, sorgo ou mamona
# somente 4 classes
aptos_wide$classe[is.na(aptos_wide$classe) &
                    aptos_wide$sem_20 != 'Não apto'] <- 'sem tolerância'
aptos_wide$classe[is.na(aptos_wide$classe) &
                     aptos_wide$média_20 != 'Não apto'] <- 'média tolerância'
aptos_wide$classe[is.na(aptos_wide$classe) &
                     aptos_wide$alta_20 != 'Não apto'] <- 'alta tolerância (20)'
aptos_wide$classe[is.na(aptos_wide$classe) &
                     aptos_wide$alta_30 != 'Não apto'] <- 'alta tolerância (30)'

brasil_gs %>%
  inner_join(aptos_wide) %>%
  ggplot() +
  geom_sf(aes(fill = classe), col = NA) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0))
ggsave('figs/aptidao_ordenada_por_tolerancia_a_seca.png')

#### resumindo época de plantio ####

janelas <- janelas_zarc %>%
  select(-ini_40, -fim_40) %>%
  filter(cultura %in% c('milho', 'sorgo', 'mamona'))

# época milho
janela_milho <- janelas %>%
  filter(cultura == 'milho') %>%
  mutate(ini_trimestre = cut(ini_20,
                         breaks = c(0, 9, 18, 27, 36),
                         labels = c('1 tri', '2 tri', '3 tri', '4 tri')),
         fim_trimestre = cut(fim_20,
                             breaks = c(0, 9, 18, 27, 36),
                             labels = c('1 tri', '2 tri', '3 tri', '4 tri'))) %>%
  select(uf, geocodigo, cultura, ini_trimestre, fim_trimestre) %>%
  mutate(tolerancia = 'sem tolerância')

janela_sorgo <- janelas %>%
  filter(cultura == 'sorgo') %>%
  mutate(ini_trimestre = cut(ini_20,
                         breaks = c(0, 9, 18, 27, 36),
                         labels = c('1 tri', '2 tri', '3 tri', '4 tri')),
         fim_trimestre = cut(fim_20,
                             breaks = c(0, 9, 18, 27, 36),
                             labels = c('1 tri', '2 tri', '3 tri', '4 tri'))) %>%
  select(uf, geocodigo, cultura, ini_trimestre, fim_trimestre) %>%
  mutate(tolerancia = 'média tolerância')

janela_mamona20 <- janelas %>%
  filter(cultura == 'mamona') %>%
  mutate(ini_trimestre = cut(ini_20,
                         breaks = c(0, 9, 18, 27, 36),
                         labels = c('1 tri', '2 tri', '3 tri', '4 tri')),
         fim_trimestre = cut(fim_20,
                             breaks = c(0, 9, 18, 27, 36),
                             labels = c('1 tri', '2 tri', '3 tri', '4 tri')),
         cultura = 'mamona_20') %>%
  select(uf, geocodigo, cultura, ini_trimestre, fim_trimestre) %>%
  mutate(tolerancia = 'alta tolerância (20)')

janela_mamona30 <- janelas %>%
  filter(cultura == 'mamona') %>%
  mutate(ini_trimestre = cut(ini_30,
                         breaks = c(0, 9, 18, 27, 36),
                         labels = c('1 tri', '2 tri', '3 tri', '4 tri')),
         fim_trimestre = cut(fim_30,
                             breaks = c(0, 9, 18, 27, 36),
                             labels = c('1 tri', '2 tri', '3 tri', '4 tri')),
         cultura = 'mamona_30') %>%
  select(uf, geocodigo, cultura, ini_trimestre, fim_trimestre) %>%
  mutate(tolerancia = 'alta tolerância (30)')

janelas_trimestre <- rbind(janela_milho,
                           janela_sorgo,
                           janela_mamona20,
                           janela_mamona30)

janelas_trimestre$tolerancia <- factor(janelas_trimestre$tolerancia,
                                       levels = c('sem tolerância', 'média tolerância',
                                                  'alta tolerância (20)', 'alta tolerância (30)'))

brasil_gs %>%
  inner_join(janelas_trimestre) %>%
  ggplot() +
  geom_sf(aes(fill = ini_trimestre), col = NA) +
  facet_wrap(~tolerancia) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0))
ggsave('figs/inicio_trimestre_plantio.png')

brasil_gs %>%
  inner_join(janelas_trimestre) %>%
  ggplot() +
  geom_sf(aes(fill = fim_trimestre), col = NA) +
  facet_wrap(~tolerancia) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0))
ggsave('figs/final_trimestre_plantio.png')

#### Juntando local indicado e épocas início e fim ####

aptos_simples <- aptos_wide %>%
  select(uf, geocodigo, tolerancia = classe) %>%
  left_join(janelas_trimestre)

# 24 classes
aptos_simples$classe <- interaction(aptos_simples$tolerancia,
                                    aptos_simples$ini_trimestre,
                                    aptos_simples$fim_trimestre,
                                    drop = TRUE)

# 2 tem apenas 3 ou 4 municípios
table(aptos_simples$classe)

# também existem municípios isolados que podem ser unidos
brasil_gs %>%
  inner_join(aptos_simples) %>%
  ggplot() +
  geom_sf(aes(fill = classe), col = NA) +
  #facet_wrap(~cultura) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0))

ggsave('figs/zonas_originais.png')

zonas <- brasil_gs %>%
  inner_join(aptos_simples)

st_write(zonas, 'results/zonas_orig.shp', append = FALSE)

# Não está gerando com cores certas
# trabalhar no QGis
#mapview::mapview(zonas, zcol = 'classe')

#### correções manuais ####
# classe 'sem tolerância.4 tri.2 tri' tem apenas 3 municípios no MA
# juntando na classe 'sem tolerância.1 tri.2 tri'

zonas[zonas$classe == 'sem tolerância.4 tri.2 tri','ini_trimestre'] <- '1 tri'

# 'alta tolerância (30).4 tri.4 tri'
# 4 municípios na BA, cercados por alta tolerância (30) 1tri 1tri
# e um at(30).1tri.1tri no meio.
# passado p/ 1tri. Ou seria uma região mais alta? Municípo do centro que está errado?
zonas[zonas$classe == 'alta tolerância (30).4 tri.4 tri','ini_trimestre'] <- '1 tri'
zonas[zonas$classe == 'alta tolerância (30).4 tri.4 tri','fim_trimestre'] <- '1 tri'

# 'alta tolerância (20).4 tri.1 tri': 11 municípios
# e alguns são corrigidos depois
# encurtando janela
zonas[zonas$classe == 'alta tolerância (20).4 tri.1 tri','ini_trimestre'] <- '1 tri'

# filtrando municípios isolados

# MA
# Igarapé do Meio - termina no 1tri
zonas[zonas$geocodigo == 2105153,'fim_trimestre'] <- '2 tri'

# PI
# Gilbués - termina no 4tri
zonas[zonas$geocodigo == 2204402,'fim_trimestre'] <- '1 tri'
# Dirceu Arcoverde - inicia no 1tri
zonas[zonas$geocodigo == 2203354,'ini_trimestre'] <- '4 tri'

### BA
# Luis Ed. Magalhães - Isolado, fim 1.tri. Encurtando p/ 4.tri
zonas[zonas$geocodigo == 2919553,'fim_trimestre'] <- '4 tri'

# Barra - Isolado, inicio em 1tri cercado de ini 4tri
zonas[zonas$geocodigo == 2902708,'ini_trimestre'] <- '4 tri'

# Serra Dourada - MT.1tri.1tri cercado de 4tri.4tri
zonas[zonas$geocodigo == 2930303,'ini_trimestre'] <- '4 tri'
zonas[zonas$geocodigo == 2930303,'fim_trimestre'] <- '4 tri'

# Cairu final 3tri cercado de fim 2.tri
zonas[zonas$geocodigo == 2905404,'fim_trimestre'] <- '2 tri'

# Barra do Choça. Final 1tri cercado de final 2tri. Ampliando
zonas[zonas$geocodigo == 2902906,'fim_trimestre'] <- '2 tri'

# Bom Jesus da Serra - Final 2.tri passando p/ 1.tri
zonas[zonas$geocodigo == 2903953,'fim_trimestre'] <- '1 tri'

# Matina 4tri.1tri --> juntando com os vizinhos
zonas[zonas$geocodigo == 2921054,'ini_trimestre'] <- '4 tri'
zonas[zonas$geocodigo == 2921054,'fim_trimestre'] <- '4 tri'

# Igaporã 1tri.1tri --> juntando com vizinhos
zonas[zonas$geocodigo == 2913408,'ini_trimestre'] <- '4 tri'
zonas[zonas$geocodigo == 2913408,'fim_trimestre'] <- '4 tri'

# Manuel Vitorino AT20 cercado de AT30. Passando p/ AT30
# e encurtando final
zonas[zonas$geocodigo == 2920403,'tolerancia'] <- 'alta tolerância (30)'
zonas[zonas$geocodigo == 2920403,'fim_trimestre'] <- '1 tri'

# Poções, Planalto, Barra do Choça, Caatiba, Itambé
# Grupo isolado de alta tol 20 na divisa com média tol.
# 'relaxando' restrição
zonas[zonas$geocodigo %in% c(2925105, 2925006,
                             2902906, 2904803,
                             2915809), 'tolerancia'] <- 'média tolerância'

# Remanso - Divisa. Única com AT20. Passando p/ AT30
zonas[zonas$geocodigo == 2926004,'tolerancia'] <- 'alta tolerância (30)'

# Camaçari, Lauro de Freitas, Simões Filho,
# Salvador, Itaparica, Vera Cruz, Salinas da Margarida
# Grupo de 1tri.3tri isolado no meio de 1tri.2tri ou 2tri.3tri
# reduzindo janela p/ início em 2tri
zonas[zonas$geocodigo %in% c(2905701, 2919207,
                             2930709, 2927408,
                             2916104, 2933208,
                             2927309), 'ini_trimestre'] <- '2 tri'

# 15 municípios alta tolerância (30).4tri.1tri. 
# passando para alta tolerância (30).1tri.1tri
zonas[zonas$classe == 'alta tolerância (30).4 tri.1 tri', 'ini_trimestre'] <- '1 tri'

# MG
# Apenas 3 municípios com Alta Tolerância (20)
# Gameleiras, Monte Azul e Espinosa
# passando para média tolerância
zonas[zonas$geocodigo %in% c(3127339, 3139250, 3124302),'tolerancia'] <- 'média tolerância'
zonas[zonas$geocodigo %in% c(3127339, 3139250, 3124302),'ini_trimestre'] <- '4 tri'
zonas[zonas$geocodigo %in% c(3127339, 3139250, 3124302),'fim_trimestre'] <- '4 tri'

### AL
# Barra de Sto Antônio ST.2tri.3tri - Cercado de sem toler.2tri.2tri - encurtando
zonas[zonas$geocodigo == 2700508,'fim_trimestre'] <- '2 tri'
# Piaçabuçu - igual Barra de Sto. Ant. 
zonas[zonas$geocodigo == 2706802,'fim_trimestre'] <- '2 tri'

# Pariconha - único no estado p/ alta toler (30) - não tem AT(20) em AL
# Passando p/ média tolerância
zonas[zonas$geocodigo == 2706422,'tolerancia'] <- 'média tolerância'

# Passo de Camaragibe & São Miguel dos Milagres
# Únicos no estado com 1tri.3tri., cercados de 2tri.2tri ou 1tri.2tri
# encurtando p/ 2tri.2tri
zonas[zonas$geocodigo %in% c(2706505, 2708709),'ini_trimestre'] <- '2 tri'
zonas[zonas$geocodigo %in% c(2706505, 2708709),'fim_trimestre'] <- '2 tri'

### SE
# Pacatuba e Brejo Grande - Liga com Piaçabuçu (AL)
# 2 isolados com final 3tri. Encurtando p/ 2tri
zonas[zonas$geocodigo %in% c(2804904, 2800704),'fim_trimestre'] <- '2 tri'

### PE
# Cabrobó - Alta tolerância (20) cercado por AT(30) e média tol a norte
# clara divisa
zonas[zonas$geocodigo == 2603009,'tolerancia'] <- 'alta tolerância (30)'

# Exu - Sem tolerância, cercado de média tolerância no estado.
# Mas sem tolerancia ao norte (divisa CE)
zonas[zonas$geocodigo == 2605301,'tolerancia'] <- 'média tolerância'

# PB
# Salgadinho - encurta final de 2tri p/ 1tri
zonas[zonas$geocodigo == 2513000,'fim_trimestre'] <- '1 tri'
# Taperoá - Atrasa início do 1tri p/ 2tri
zonas[zonas$geocodigo == 2516508,'ini_trimestre'] <- '2 tri'


# finalizando

# tapando buracao
buraco <- brasil_gs %>%
  filter(! geocodigo %in% zonas$geocodigo) %>%
  mutate(uf = sigla,
         tolerancia = 'alta tolerância (30)',
         cultura = 'mamona_30',
         ini_trimestre = '1 tri',
         fim_trimestre = '1 tri',
         classe = NA)

zonas <- rbind(zonas, buraco)

zonas$classe <- interaction(zonas$tolerancia,
                            zonas$ini_trimestre,
                            zonas$fim_trimestre,
                            drop = TRUE)

ggplot(zonas) +
  geom_sf(aes(fill = classe), col = NA) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0))

ggsave('figs/zonas_simplificadas.png')

st_write(zonas, 'results/zonas_simplificadas.shp', append = FALSE)

