# Cruza mapas
# ideia é fazer classificação com duas variáveis:
# 1) onde se pode plantar
# 2) quando se pode plantar
# usar ZARC milho (20 e 30%), sorgo (20 e 30%) e mamona (20 e 30%)

library(tidyverse)
library(sf)
library(mapview)
source('src/aux_func.R')

resumo_zarc <- readRDS('results/resumo_zarc.RDS')
janelas_zarc <- readRDS('results/janelas_zarc.RDS')

resumo_zarc_gs <- resumo_zarc %>%
  filter(uf %in% c('MA', 'PI', 'CE', 'RN', 'PB',
                   'PE', 'AL', 'SE', 'BA', 'MG')
  )

# brasil <- read_sf('../../../../geodb/IBGE/bc250_2019-10-29.gpkg',
#                   layer = 'lml_municipio_a') %>%
#   mutate(cod_uf = substr(geocodigo, 1, 2))

brasil <- read_sf('../../../../geodb/IBGE/bcim_2016_18_09_2018.gpkg',
                  layer = 'lim_municipio_a') %>%
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

# considerando apto municípos com pelo menos 2 decendios
# contando em perídos de <1 mes, 1 a 2 meses e > 2 meses
aptos <- resumo_zarc_gs %>%
  filter(cultura %in% c('milho', 'sorgo', 'mamona'),
         n >= 1,
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
  facet_wrap(~ freq + cultura) +
  theme(legend.position = 'bottom') +
  labs(fill = 'ZARC: Meses aptos para plantio - solo 2')

ggsave('figs/meses_aptos_para_plantio.png')

aptos_wide <- aptos_2 %>%
  pivot_wider(id_cols = c(uf, geocodigo),
              names_from = c(cultura, freq),
              values_from = meses)

# convertendo NA para 'não apto'
# Primeiro adiciona level 'Não apto' e depois troca valor
addNaoApta <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "Não apto")))
  return(x)
}

aptos_wide <- as.data.frame(lapply(aptos_wide, addNaoApta))

aptos_wide$milho_20[is.na(aptos_wide$milho_20)] <- 'Não apto'
aptos_wide$sorgo_20[is.na(aptos_wide$sorgo_20)] <- 'Não apto'
aptos_wide$mamona_20[is.na(aptos_wide$mamona_20)] <- 'Não apto'
aptos_wide$mamona_30[is.na(aptos_wide$mamona_30)] <- 'Não apto'

aptos_wide$classe <- interaction(aptos_wide$milho_20,
                                 aptos_wide$sorgo_20,
                                 aptos_wide$mamona_20,
                                 aptos_wide$mamona_30,
                                 drop = TRUE)

# Ao todo temos 36 combinações. Preciso realizar alguns agrupamentos
aptos_wide$classe <- NA

# reduzindo para onde pode milho, sorgo ou mamona
# somente 4 classes
aptos_wide$classe[is.na(aptos_wide$classe) &
                    aptos_wide$milho_20 != 'Não apto'] <- 'milho'
aptos_wide$classe[is.na(aptos_wide$classe) &
                     aptos_wide$sorgo_20 != 'Não apto'] <- 'sorgo'
aptos_wide$classe[is.na(aptos_wide$classe) &
                     aptos_wide$mamona_20 != 'Não apto'] <- 'mamona_20'
aptos_wide$classe[is.na(aptos_wide$classe) &
                     aptos_wide$mamona_30 != 'Não apto'] <- 'mamona_30'

brasil_gs %>%
  inner_join(aptos_wide) %>%
  ggplot() +
  geom_sf(aes(fill = classe), col = NA) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0))
ggsave('figs/aptidao_ordenada_milho_sorgo_mamona.png')

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
  select(uf, geocodigo, cultura, ini_trimestre, fim_trimestre)

janela_sorgo <- janelas %>%
  filter(cultura == 'sorgo') %>%
  mutate(ini_trimestre = cut(ini_20,
                         breaks = c(0, 9, 18, 27, 36),
                         labels = c('1 tri', '2 tri', '3 tri', '4 tri')),
         fim_trimestre = cut(fim_20,
                             breaks = c(0, 9, 18, 27, 36),
                             labels = c('1 tri', '2 tri', '3 tri', '4 tri'))) %>%
  select(uf, geocodigo, cultura, ini_trimestre, fim_trimestre)

janela_mamona20 <- janelas %>%
  filter(cultura == 'mamona') %>%
  mutate(ini_trimestre = cut(ini_20,
                         breaks = c(0, 9, 18, 27, 36),
                         labels = c('1 tri', '2 tri', '3 tri', '4 tri')),
         fim_trimestre = cut(fim_20,
                             breaks = c(0, 9, 18, 27, 36),
                             labels = c('1 tri', '2 tri', '3 tri', '4 tri')),
         cultura = 'mamona_20') %>%
  select(uf, geocodigo, cultura, ini_trimestre, fim_trimestre)

janela_mamona30 <- janelas %>%
  filter(cultura == 'mamona') %>%
  mutate(ini_trimestre = cut(ini_30,
                         breaks = c(0, 9, 18, 27, 36),
                         labels = c('1 tri', '2 tri', '3 tri', '4 tri')),
         fim_trimestre = cut(fim_30,
                             breaks = c(0, 9, 18, 27, 36),
                             labels = c('1 tri', '2 tri', '3 tri', '4 tri')),
         cultura = 'mamona_30') %>%
  select(uf, geocodigo, cultura, ini_trimestre, fim_trimestre)


janelas_trimestre <- rbind(janela_milho,
                           janela_sorgo,
                           janela_mamona20,
                           janela_mamona30)

brasil_gs %>%
  inner_join(janelas_trimestre) %>%
  ggplot() +
  geom_sf(aes(fill = ini_trimestre), col = NA) +
  facet_wrap(~cultura) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0))
ggsave('figs/inicio_trimestre_plantio.png')

brasil_gs %>%
  inner_join(janelas_trimestre) %>%
  ggplot() +
  geom_sf(aes(fill = fim_trimestre), col = NA) +
  facet_wrap(~cultura) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0))
ggsave('figs/final_trimestre_plantio.png')

#### Juntando local indicado e épocas início e fim ####

aptos_simples <- aptos_wide %>%
  select(uf, geocodigo, cultura = classe) %>%
  left_join(janelas_trimestre)

# 24 classes
aptos_simples$classe <- interaction(aptos_simples$cultura,
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
# classe 'milho.4 tri.2 tri' tem apenas 3 municípios no MA
# juntando na classe 'milho.1 tri.2 tri'

zonas[zonas$classe == 'milho.4 tri.2 tri','ini_trimestre'] <- '1 tri'

# 'mamona_30.4 tri.4 tri': 4 municípios cercados por mamona 30 1tri 1tri
zonas[zonas$classe == 'mamona_30.4 tri.4 tri','ini_trimestre'] <- '1 tri'
zonas[zonas$classe == 'mamona_30.4 tri.4 tri','fim_trimestre'] <- '1 tri'

# 'mamona_20.4 tri.1 tri': 11 municípios
# e alguns são corrigidos depois
zonas[zonas$classe == 'mamona_20.4 tri.1 tri','ini_trimestre'] <- '1 tri'

# filtrando municípios isolados
# MA
zonas[zonas$geocodigo == 2112902,'fim_trimestre'] <- '2 tri'
zonas[zonas$geocodigo == 2105153,'fim_trimestre'] <- '2 tri'


# PI
zonas[zonas$geocodigo == 2204402,'fim_trimestre'] <- '1 tri'
zonas[zonas$geocodigo == 2203354,'ini_trimestre'] <- '4 tri'

# BA
zonas[zonas$geocodigo == 2919553,'fim_trimestre'] <- '4 tri'
zonas[zonas$geocodigo == 2902708,'ini_trimestre'] <- '4 tri'
zonas[zonas$geocodigo == 2930303,'ini_trimestre'] <- '4 tri'
zonas[zonas$geocodigo == 2930303,'fim_trimestre'] <- '4 tri'
zonas[zonas$geocodigo == 2905404,'fim_trimestre'] <- '2 tri'
zonas[zonas$geocodigo == 2902906,'fim_trimestre'] <- '2 tri'
zonas[zonas$geocodigo == 2903953,'fim_trimestre'] <- '1 tri'

zonas[zonas$geocodigo == 2921054,'ini_trimestre'] <- '4 tri'
zonas[zonas$geocodigo == 2921054,'fim_trimestre'] <- '4 tri'
zonas[zonas$geocodigo == 2913408,'ini_trimestre'] <- '4 tri'
zonas[zonas$geocodigo == 2913408,'fim_trimestre'] <- '4 tri'

zonas[zonas$geocodigo == 2920403,'cultura'] <- 'mamona_30'
zonas[zonas$geocodigo == 2920403,'fim_trimestre'] <- '1 tri'

zonas[zonas$geocodigo %in% c(2925105, 2925006,
                             2902906, 2904803,
                             2915809), 'cultura'] <- 'sorgo'

zonas[zonas$geocodigo == 2926004,'cultura'] <- 'mamona_30'

zonas[zonas$geocodigo %in% c(2905701, 2919207,
                             2930709, 2927408,
                             2916104, 2933208,
                             2927309), 'ini_trimestre'] <- '2 tri'

# 15 municípios mamona_30.4tri.1tri. 
# passando para mamona_30.1tri.1tri
zonas[zonas$classe == 'mamona_30.4 tri.1 tri', 'ini_trimestre'] <- '1 tri'

# MG
# Apenas 3 municípios com Mamona
# passando para Sorgo
zonas[zonas$geocodigo %in% c(3127339, 3139250, 3124302),'cultura'] <- 'sorgo'
zonas[zonas$geocodigo %in% c(3127339, 3139250, 3124302),'ini_trimestre'] <- '4 tri'
zonas[zonas$geocodigo %in% c(3127339, 3139250, 3124302),'fim_trimestre'] <- '4 tri'

# AL
zonas[zonas$geocodigo == 2700508,'fim_trimestre'] <- '2 tri'
zonas[zonas$geocodigo == 2706802,'fim_trimestre'] <- '2 tri'
zonas[zonas$geocodigo == 2706422,'cultura'] <- 'sorgo'
zonas[zonas$geocodigo %in% c(2706505, 2708709),'ini_trimestre'] <- '2 tri'
zonas[zonas$geocodigo %in% c(2706505, 2708709),'fim_trimestre'] <- '2 tri'

# PE
zonas[zonas$geocodigo == 2603009,'cultura'] <- 'mamona_30'
zonas[zonas$geocodigo == 2605301,'cultura'] <- 'sorgo'

# PB
zonas[zonas$geocodigo == 2513000,'fim_trimestre'] <- '1 tri'
zonas[zonas$geocodigo == 2516508,'ini_trimestre'] <- '2 tri'

# SE
zonas[zonas$geocodigo %in% c(2804904, 2800704),'fim_trimestre'] <- '2 tri'

# finalizando

# tapando buracao
buraco <- brasil_gs %>%
  filter(! geocodigo %in% zonas$geocodigo) %>%
  mutate(uf = sigla,
         cultura = 'mamona_30',
         ini_trimestre = '1 tri',
         fim_trimestre = '1 tri',
         classe = NA)

zonas <- rbind(zonas, buraco)

zonas$classe <- interaction(zonas$cultura,
                            zonas$ini_trimestre,
                            zonas$fim_trimestre,
                            drop = TRUE)

ggplot(zonas) +
  geom_sf(aes(fill = classe), col = NA) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0))

ggsave('figs/zonas_simplificadas.png')

st_write(zonas, 'results/zonas_simplificadas.shp', append = FALSE)

# #### Publicando mapa ####
# 
# zonas_limpo <- zonas %>%
#   mutate(classe = as.integer(classe)) %>%
#   select(-geometriaaproximada, -anodereferencia, -sigla,
#          -cultura, -ini_trimestre, -fim_trimestre) 
# 
# # uf_gs <- uf %>%
# #   filter(sigla %in% c('MA', 'PI', 'CE', 'RN', 'PB',
# #                       'PE', 'AL', 'SE', 'BA', 'MG'))
# 
# mapview(zonas_limpo, zcol = 'classe',
#         col.regions = rainbow(18))
