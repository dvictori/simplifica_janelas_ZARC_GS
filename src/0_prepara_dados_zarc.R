# Abre tabelas do ZARC para as culturas selecionadas
# e extrai métricas de interesse
# decendios aptos para diferentes faixas de risco
# início e final de janela para diferentes faixas de risco

library(tidyverse)
library(sf)

source('src/aux_func.R')

brasil <- read_sf('../../../../geodb/IBGE/bc250_2019-10-29.gpkg',
                  layer = 'lml_municipio_a') %>%
  mutate(cod_uf = substr(geocodigo, 1, 2))

uf <- read_sf('../../../../geodb/IBGE/bc250_2019-10-29.gpkg',
              layer = 'lml_unidade_federacao_a')

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

#### Milho ####

zarc_milho <- abre_zarc_micura('dados/ZARC_milho.xls')

resumo_milho <- zarc_milho %>%
  filter(freq < 100) %>%
  count(uf, geocodigo, freq)

resumo_milho$dec_acum <- resumo_milho %>%
  group_by(uf, geocodigo) %>%
  summarise(dec_acum = cumsum(n)) %>%
  pull(dec_acum)

janelas_milho <- zarc_milho %>%
  group_by(uf, geocodigo) %>%
  summarise(ini_20 = encontra_janelas_aptas(decendio, freq, risco = 20) %>%
              acha_maior_janela() %>%
              .[1],
            fim_20 = encontra_janelas_aptas(decendio, freq, risco = 20) %>%
              acha_maior_janela() %>%
              .[2],
            ini_30 = encontra_janelas_aptas(decendio, freq, risco = 30) %>%
              acha_maior_janela() %>%
              .[1],
            fim_30 = encontra_janelas_aptas(decendio, freq, risco = 30) %>%
              acha_maior_janela() %>%
              .[2],
            ini_40 = encontra_janelas_aptas(decendio, freq, risco = 40) %>%
              acha_maior_janela() %>%
              .[1],
            fim_40 = encontra_janelas_aptas(decendio, freq, risco = 40) %>%
              acha_maior_janela() %>%
              .[2]
            ) 

tudo_na <- apply(janelas_milho[3:8], 1, function(x) sum(is.na(x)))

janelas_milho<- janelas_milho[tudo_na < 6,]

#### Sorgo ####

zarc_sorgo <- abre_zarc_micura('dados/ZARC_sorgo_granifero.xls') %>%
  mutate(freq = as.integer(freq))

resumo_sorgo <- zarc_sorgo %>%
  filter(freq < 100) %>%
  count(uf, geocodigo, freq)

resumo_sorgo$dec_acum <- resumo_sorgo %>%
  group_by(uf, geocodigo) %>%
  summarise(dec_acum = cumsum(n)) %>%
  pull(dec_acum)

janelas_sorgo <- zarc_sorgo %>%
  group_by(uf, geocodigo) %>%
  summarise(ini_20 = encontra_janelas_aptas(decendio, freq, risco = 20) %>%
              acha_maior_janela() %>%
              .[1],
            fim_20 = encontra_janelas_aptas(decendio, freq, risco = 20) %>%
              acha_maior_janela() %>%
              .[2],
            ini_30 = encontra_janelas_aptas(decendio, freq, risco = 30) %>%
              acha_maior_janela() %>%
              .[1],
            fim_30 = encontra_janelas_aptas(decendio, freq, risco = 30) %>%
              acha_maior_janela() %>%
              .[2],
            ini_40 = encontra_janelas_aptas(decendio, freq, risco = 40) %>%
              acha_maior_janela() %>%
              .[1],
            fim_40 = encontra_janelas_aptas(decendio, freq, risco = 40) %>%
              acha_maior_janela() %>%
              .[2]
            ) 

tudo_na <- apply(janelas_sorgo[3:8], 1, function(x) sum(is.na(x)))

janelas_sorgo <- janelas_sorgo[tudo_na < 6,]

#### Milheto ####

zarc_milheto <- abre_zarc_micura('dados/ZARC_milheto.xls') %>%
  mutate(freq = as.integer(freq))

resumo_milheto <- zarc_milheto %>%
  filter(freq < 100) %>%
  count(uf, geocodigo, freq)

resumo_milheto$dec_acum <- resumo_milheto %>%
  group_by(uf, geocodigo) %>%
  summarise(dec_acum = cumsum(n)) %>%
  pull(dec_acum)

janelas_milheto <- zarc_milheto %>%
  group_by(uf, geocodigo) %>%
  summarise(ini_20 = encontra_janelas_aptas(decendio, freq, risco = 20) %>%
              acha_maior_janela() %>%
              .[1],
            fim_20 = encontra_janelas_aptas(decendio, freq, risco = 20) %>%
              acha_maior_janela() %>%
              .[2],
            ini_30 = encontra_janelas_aptas(decendio, freq, risco = 30) %>%
              acha_maior_janela() %>%
              .[1],
            fim_30 = encontra_janelas_aptas(decendio, freq, risco = 30) %>%
              acha_maior_janela() %>%
              .[2],
            ini_40 = encontra_janelas_aptas(decendio, freq, risco = 40) %>%
              acha_maior_janela() %>%
              .[1],
            fim_40 = encontra_janelas_aptas(decendio, freq, risco = 40) %>%
              acha_maior_janela() %>%
              .[2]
            ) 
tudo_na <- apply(janelas_milheto[3:8], 1, function(x) sum(is.na(x)))

janelas_milheto <- janelas_milheto[tudo_na < 6,]

#### Mamona ####

zarc_mamona <- abre_zarc_micura('dados/ZARC_mamona.xls') %>%
  mutate(freq = as.integer(freq))

resumo_mamona<- zarc_mamona %>%
  filter(freq < 100) %>%
  count(uf, geocodigo, freq)

resumo_mamona$dec_acum <- resumo_mamona %>%
  group_by(uf, geocodigo) %>%
  summarise(dec_acum = cumsum(n)) %>%
  pull(dec_acum)

janelas_mamona <- zarc_mamona %>%
  group_by(uf, geocodigo) %>%
  summarise(ini_20 = encontra_janelas_aptas(decendio, freq, risco = 20) %>%
              acha_maior_janela() %>%
              .[1],
            fim_20 = encontra_janelas_aptas(decendio, freq, risco = 20) %>%
              acha_maior_janela() %>%
              .[2],
            ini_30 = encontra_janelas_aptas(decendio, freq, risco = 30) %>%
              acha_maior_janela() %>%
              .[1],
            fim_30 = encontra_janelas_aptas(decendio, freq, risco = 30) %>%
              acha_maior_janela() %>%
              .[2],
            ini_40 = encontra_janelas_aptas(decendio, freq, risco = 40) %>%
              acha_maior_janela() %>%
              .[1],
            fim_40 = encontra_janelas_aptas(decendio, freq, risco = 40) %>%
              acha_maior_janela() %>%
              .[2]
            ) 

tudo_na <- apply(janelas_mamona[3:8], 1, function(x) sum(is.na(x)))

janelas_mamona <- janelas_mamona[tudo_na < 6,]

#### Juntando ####

resumo_zarc <- resumo_milho %>%
  mutate(cultura = 'milho') %>%
  rbind(resumo_sorgo %>%
          mutate(cultura = 'sorgo')) %>%
  rbind(resumo_milheto %>%
          mutate(cultura = 'milheto')) %>%
  rbind(resumo_mamona %>%
          mutate(cultura = 'mamona'))

janelas_zarc <- janelas_milho %>%
  mutate(cultura = 'milho') %>%
  rbind(janelas_sorgo %>%
          mutate(cultura = 'sorgo')) %>%
  rbind(janelas_milheto %>%
          mutate(cultura = 'milheto')) %>%
  rbind(janelas_mamona %>%
          mutate(cultura = 'mamona'))

saveRDS(resumo_zarc, 'results/resumo_zarc.RDS')
saveRDS(janelas_zarc, 'results/janelas_zarc.RDS')


#### Velharia daqui p/ frente ####

brasil_gs %>%
  inner_join(zarc_milho %>%
              filter(decendio == 9,
                     freq < 100)) %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(freq)), col = NA) +
  scale_fill_manual(values = cores_zarc) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0))



brasil_gs %>%
  filter(sigla == 'PI') %>%
  left_join(resumo_milho) %>%
  ggplot() +
  geom_sf(aes(fill = dec_acum), col = NA) +
  #scale_fill_manual(values = cores_zarc) +
  facet_wrap(~freq)







resumo_zarc <- resumo_milho %>%
  mutate(cultura = 'milho') %>%
  rbind(resumo_sorgo %>%
          mutate(cultura = 'sorgo')) %>%
  rbind(resumo_milheto %>%
          mutate(cultura = 'milheto')) %>%
  rbind(resumo_mamona %>%
          mutate(cultura = 'mamona'))

brasil_gs %>%
  left_join(resumo_zarc %>%
              filter(freq == 30)) %>%
  ggplot() +
  geom_sf(aes(fill = n), col = NA) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
  facet_wrap(~cultura)

resumo_zarc_wide <- resumo_zarc %>%
  #filter(freq == 20,
  #       n > 1) %>%
  pivot_wider(id_cols = c(uf, geocodigo), names_from = cultura, values_from = freq,
              values_fn = min)

resumo_zarc_2 <- resumo_zarc %>%
  group_by(uf, geocodigo, cultura) %>%
  summarise(freq = min(freq, na.rm = TRUE))

brasil_gs %>%
  inner_join(resumo_zarc_2) %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(freq)), col = NA) +
  scale_fill_manual(values = cores_zarc) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
  facet_wrap(~cultura) +
  labs(fill = 'Risco ZARC - solo 2')

zonas <- resumo_zarc_2 %>%
  ungroup () %>%
  mutate(cf = as.factor(paste(cultura, freq, sep ='_'))) %>%
  group_by(uf, geocodigo) %>%
  summarise(zona = do.call(interaction, as.list(cf)))

zonas %>% ungroup() %>% count(zona)
