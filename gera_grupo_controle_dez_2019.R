## Geração de grupo controle para Tá de Pé, com obras do TCU

library(DeclareDesign)
library(estimatr)
library(tidyverse)
library(arm)

setwd("C:/Users/mczfe/Transparencia Brasil/Projetos/Ta de Pe/R/1_blockrand/1_blockrand")
load(file="data_rand_tadepe.RData") ## carrega dat3, data.frame objeto da aleatorização
nrow(dat3) # 8491 linhas


set.seed(725548) # do random.org, em anexo no email q te mando


# se fosse ser aleatorizaçao simples
block <- dat3 %>%
  group_by(uf, situacao, dados) %>% # 
  mutate(treatment = rbinom(n(), 1, prob=.88)) %>%
  ungroup()

# MAs faremos block randomization, que é mais eficiente

# recategoriza fonte dos dados, tá de pé ou outros (aka, TCU)
block$dados2 = block$dados
block$dados2[block$dados2!='tadepe'] = 'Outros'
block$dados2[block$dados2=='tadepe'] = 'TDP'

# seleciona dados que entrarão no block
# blocos serão por: dados, mediana do percenrual realizado, mediana do low_income_families2010,
# mediana do transferS_fundeb2016, mediana do ideb_final_2017, mediana do avgPartyCouncilofPT
# UF entrará como efeito fixo na análise posterior.

datblock <- block %>%
  dplyr::select(id, uf, dados2, percentual_realizado, low_income_families2010, 
                transfers_fundeb2016, ideb_final_2017, avgPartyCouncilorPT) %>%
  mutate(
    percentual_realizado_med = 
      as.numeric(percentual_realizado>median(percentual_realizado, na.rm=T)),
    low_income_families2010_med = 
      as.numeric(low_income_families2010>median(low_income_families2010, na.rm=T)),
    transfers_fundeb2016_med = 
      as.numeric(transfers_fundeb2016>median(transfers_fundeb2016, na.rm=T)),
    ideb_final_2017_med = 
      as.numeric(ideb_final_2017>median(ideb_final_2017, na.rm=T)),
    avgPartyCouncilorPT_med = 
      as.numeric(avgPartyCouncilorPT>median(avgPartyCouncilorPT, na.rm=T))
  ) %>%
  dplyr::select(id, dados2, percentual_realizado_med, low_income_families2010_med,
                transfers_fundeb2016_med, ideb_final_2017_med,
                avgPartyCouncilorPT_med) %>%
  mutate(blockvar = paste(dados2, percentual_realizado_med, low_income_families2010_med,
                          transfers_fundeb2016_med, ideb_final_2017_med,
                          avgPartyCouncilorPT_med,
                          sep='_'))
# DeclareDesign

# estou definindo o ate em termos da invlogit (inverso da logística)
inv_logit_ate <- .2

# isso significa, em um modelo de probabilidade linear, do seguinte ate
interpretable_ate <- arm::invlogit(.3 + inv_logit_ate) - invlogit(.3) # 0.04801681

# declara populacao
population <- declare_population(datblock) 
df1 <- population()

# potential outcomes em termos da logísitca
# Supondo (a priori) que 30% das obras são concluídas,
# e que o ate na logística é .2 (ver acima, ou seja, .05 no MPL), temos:
ate <- inv_logit_ate
potential_outcomes <- declare_potential_outcomes(Y ~ rbinom(n = N, size = 1,
                                                            prob = invlogit(0.3 + ate*Z)))
df1 <- potential_outcomes(df1)

estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) 

decl_exp <- declare_ra(blocks = df1$blockvar, 
                       prob_each = c(.88,.12),
                       conditions = c('Treatment', 'Control'))

df1$Z <- block_ra(blocks = df1$blockvar, 
                       prob_each = c(.88,.12),
                       conditions = c('Treatment', 'Control'))

# obter inverse probability weigths
df1$IPW <- 1/obtain_condition_probabilities(decl_exp, df1$Z)

assignment <-  declare_assignment( assignment_variable = Z)

df1 <- assignment(df1)

## validando, vejo que é um pouco menos eficiente que com block. mas ok.

reveal <- declare_reveal(outcome_variables = Y)
df1 <- reveal(df1)


estimated_effect <- declare_estimator(
  Y ~ Z + dados2 + percentual_realizado_med +
    transfers_fundeb2016_med + 
    low_income_families2010_med +
    ideb_final_2017_med +
    avgPartyCouncilorPT_med,
  model = lm_robust, # Modelo de Probabilidade Linear
  estimand = c("ATE"), 
  label = "Treatment Effect"
)

our_design <- population + 
  potential_outcomes + 
  estimand + 
  assignment + 
  reveal + 
  estimated_effect 

our_design %>% 
  draw_estimands()

our_design %>% 
  draw_estimates()

my_diagnosands <- declare_diagnosands(select = c( power, coverage, type_s_rate, mean_estimate))

n_sims <- 1000

diagnosis1 <- our_design %>% 
  diagnose_design(sims = n_sims, diagnosands = my_diagnosands)

# poder do teste ficou bom, > 80%
diagnosis1 %>% 
  get_diagnosands()


## gerando dados para Jéssica
load("obras_amostra1.RData")
arquivo_jessica <- obras_amostra1 %>%
  left_join(datblock, by = "id")

glimpse(arquivo_jessica)

save(arquivo_jessica, file="grupo_controle_dez_2019.RData")


