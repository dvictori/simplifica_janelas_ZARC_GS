# scratch
# local para testes

# munic com janela que vira o ano
# para risco 30
# ajenla dupla para risco 20
a <- zarc_milho %>% filter(geocodigo == 2903201)

# quebra para janela dupla
janela_zarc(a)

janela_zarc(a, risco = 30)

b <- rle(a$freq)


