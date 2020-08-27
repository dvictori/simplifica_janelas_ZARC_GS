# Gera alguns mapas para tentar identificar melhor
# os parâmetros para gerar o agrupamento de regiões
# Não nomear as culturas para não gerar expectatíva de que serão as recomendadas
# usando 'não tolerante', 'média tolerância', 'alta tolerância'

library(tidyverse)
library(sf)

source('src/aux_func.R')

resumo_zarc <- readRDS('results/resumo_zarc.RDS') %>%
  filter(cultura != 'milheto') %>%
  mutate(
    tolerancia = 
      case_when(
        cultura == 'milho' ~ 'baixa',
        cultura == 'sorgo' ~ 'média',
        cultura == 'mamona' ~ 'alta'
      )
  )

resumo_zarc$tolerancia <- factor(resumo_zarc$tolerancia,
                                 levels = c('baixa', 'média', 'alta'))

janelas_zarc <- readRDS('results/janelas_zarc.RDS') %>%
  filter(cultura != 'milheto') %>%
  mutate(
    tolerancia = 
      case_when(
        cultura == 'milho' ~ 'baixa',
        cultura == 'sorgo' ~ 'média',
        cultura == 'mamona' ~ 'alta'
      )
  )

janelas_zarc$tolerancia <- factor(janelas_zarc$tolerancia,
                                  levels = c('baixa', 'média', 'alta'))

janelas_zarc_long <- janelas_zarc %>%
  pivot_longer(ini_20:fim_40, values_to = 'decendio') %>%
  separate(
    name,
    into = c('período', 'risco'),
    sep = '_'
  ) %>%
  mutate(período = factor(período, levels = c('ini', 'fim')))


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

brasil_gs %>%
  inner_join(resumo_zarc %>%
               group_by(uf, geocodigo, cultura) %>%
               summarise(freq = min(freq, na.rm = TRUE))) %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(freq)), col = NA) +
  scale_fill_manual(values = cores_zarc) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
  facet_wrap(~cultura) +
  theme(legend.position = 'bottom') +
  labs(fill = 'ZARC: Risco mínimo - solo 2')

ggsave('figs/ZARC_risco_minimo_milho_sorgo_mamona.png')

brasil_gs %>%
  inner_join(resumo_zarc %>%
               group_by(uf, geocodigo, tolerancia) %>%
               summarise(freq = min(freq, na.rm = TRUE))) %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(freq)), col = NA) +
  scale_fill_manual(values = cores_zarc) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
  facet_wrap(~tolerancia) +
  theme(legend.position = 'bottom') +
  labs(fill = 'ZARC: Risco mínimo - solo 2')

ggsave('figs/ZARC_risco_minimo_classe_tolerancia.png')

#### Milho ####

brasil_gs %>%
  #filter(sigla == 'PI') %>%
  inner_join(resumo_zarc %>%
              filter(cultura == 'milho')) %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(dec_acum)), col = NA) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
  #scale_fill_manual(values = cores_zarc) +
  facet_wrap(~freq) +
  labs(title = 'ZARC cult. anual baixa tolerância', fill = 'Decêndios aptos')

ggsave('figs/milho_decendios_aptos.png')

brasil_gs %>%
  #filter(sigla == 'MG') %>%
  inner_join(janelas_zarc_long %>%
               filter(cultura == 'milho')) %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(decendio)), col = NA) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
  facet_wrap(~período + risco) +
  labs(title = 'ZARC cult. anual baixa tolerância', fill = 'decêndio')

ggsave('figs/milho_inicio_final_janela.png')

#### Milheto ####

# brasil_gs %>%
#   inner_join(resumo_zarc %>%
#                filter(cultura == 'milheto')) %>%
#   ggplot() +
#   geom_sf(aes(fill = as.factor(dec_acum)), col = NA) +
#   geom_sf(data = uf, fill = NA) +
#   coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
#   #scale_fill_manual(values = cores_zarc) +
#   facet_wrap(~freq) +
#   labs(title = 'ZARC milheto', fill = 'Decêndios aptos')
# 
# ggsave('figs/milheto_decendios_aptos.png')
# 
# brasil_gs %>%
#   inner_join(janelas_zarc_long %>%
#                filter(cultura == 'milheto')) %>%
#   ggplot() +
#   geom_sf(aes(fill = as.factor(decendio)), col = NA) +
#   geom_sf(data = uf, fill = NA) +
#   coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
#   facet_wrap(~período + risco) +
#   labs(title = 'ZARC milheto', fill = 'decêndio')
# 
# ggsave('figs/milheto_inicio_final_janela.png')

#### Sorgo ####

brasil_gs %>%
  inner_join(resumo_zarc %>%
               filter(cultura == 'sorgo')) %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(dec_acum)), col = NA) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
  #scale_fill_manual(values = cores_zarc) +
  facet_wrap(~freq) +
  labs(title = 'ZARC cult. anual média tolerância', fill = 'Decêndios aptos')

ggsave('figs/sorgo_decendios_aptos.png')

brasil_gs %>%
  inner_join(janelas_zarc_long %>%
               filter(cultura == 'sorgo')) %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(decendio)), col = NA) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
  facet_wrap(~período + risco) +
  labs(title = 'ZARC cult. anual média tolerância', fill = 'decêndio')

ggsave('figs/sorgo_inicio_final_janela.png')

#### Mamona ####

brasil_gs %>%
  inner_join(resumo_zarc %>%
               filter(cultura == 'mamona')) %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(dec_acum)), col = NA) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
  #scale_fill_manual(values = cores_zarc) +
  facet_wrap(~freq) +
  labs(title = 'ZARC cult. anual alta tolerância', fill = 'Decêndios aptos')

ggsave('figs/mamona_decendios_aptos.png')

brasil_gs %>%
  inner_join(janelas_zarc_long %>%
               filter(cultura == 'mamona')) %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(decendio)), col = NA) +
  geom_sf(data = uf, fill = NA) +
  coord_sf(xlim = c(-52, -35), ylim = c(-25, 0)) +
  facet_wrap(~período + risco) +
  labs(title = 'ZARC cult. anual alta tolerância', fill = 'decêndio')

ggsave('figs/mamona_inicio_final_janela.png')
