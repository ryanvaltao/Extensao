# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# A ALTERAÇÃO DO SCRIPT ESQUELETO - ETAPA 1 - DEVERÁ SER FEITA DENTRO DA BRANCH SINASC

# Tarefa 1. Leitura do banco de dados do SINASC 2015  com 3017668 linhas e 61 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sinasc

dados_sinasc = read.csv("SINASC_2015.csv", header = TRUE, sep = ";")
head(dados_sinasc)
str(dados_sinasc)
View(dados_sinasc)

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

dados_sinasc_1 = dados_sinasc[, c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)]
names(dados_sinasc_1)

# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF

dados_sinasc_2 = dados_sinasc_1[substr(dados_sinasc_1$CODMUNRES, 1, 2) == "52",]

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11409     15: 143657    16: 15750      17: 25110
# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026     
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122 

# Exportar o arquivo com o nome dados_sinasc_2.csv

write.csv(dados_sinasc_2, "dados_sinasc_2", row.names = FALSE)

# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

str(dados_sinasc_2)
dados_sinasc_2 = read.csv("dados_sinasc_2", header = TRUE, sep = ",")

freq_LOCNASC = table(dados_sinasc_2$LOCNASC)
freq_ESTCIVMAE = table(dados_sinasc_2$ESTCIVMAE)
freq_GESTACAO = table(dados_sinasc_2$GESTACAO)
freq_GRAVIDEZ = table(dados_sinasc_2$GRAVIDEZ)
freq_PARTO = table(dados_sinasc_2$PARTO)
freq_SEXO = table(dados_sinasc_2$SEXO)
freq_RACACOR = table(dados_sinasc_2$RACACOR)
freq_IDANOMAL = table(dados_sinasc_2$IDANOMAL)
freq_ESCMAE2010 = table(dados_sinasc_2$ESCMAE2010)
freq_RACACORMAE = table(dados_sinasc_2$RACACORMAE)
freq_TPAPRESENT = table(dados_sinasc_2$TPAPRESENT)
freq_TPROBSON = table(dados_sinasc_2$TPROBSON)
freq_PARIDADE = table(dados_sinasc_2$PARIDADE)
freq_KOTELCHUCK = table(dados_sinasc_2$KOTELCHUCK)

# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável

dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == 9] = NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] = NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] = NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] = NA
dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == 9] = NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] = NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] = NA
dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == 9] = NA
dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == 9] = NA
dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == 9] = NA
dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] = NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] = NA
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] = NA
dados_sinasc_2$CONSPRENAT[dados_sinasc_2$CONSPRENAT == 99] = NA


table(dados_sinasc_2$KOTELCHUCK, useNA = 'ifany')
table(dados_sinasc_2$TPROBSON, useNA = 'ifany')
table(dados_sinasc_2$SEXO, useNA = 'ifany')
table(dados_sinasc_2$PARTO, useNA = 'ifany')
table(dados_sinasc_2$GRAVIDEZ, useNA = 'ifany')
table(dados_sinasc_2$GESTACAO, useNA = 'ifany')
table(dados_sinasc_2$ESTCIVMAE, useNA = 'ifany')
table(dados_sinasc_2$IDANOMAL, useNA = 'ifany')
table(dados_sinasc_2$ESCMAE2010, useNA = 'ifany')
table(dados_sinasc_2$TPAPRESENT, useNA = 'ifany')
table(dados_sinasc_2$APGAR5, useNA = 'ifany')
table(dados_sinasc_2$PESO, useNA = 'ifany')
table(dados_sinasc_2$CONSPRENAT, useNA = 'ifany')


# Tarefa 6. Atribuir legendas para as categorias das variáveis investigadas na etapa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK,
                                   levels = c(1,2,3,4,5),
                                   labels = c('Não realizou pré-natal','Inadequado','Intermediário','Adequado','Mais que adequado'))

dados_sinasc_2$TPROBSON = factor(dados_sinasc_2$TPROBSON,
                                 levels = c(1,2,3,4,5,6,7,8,9,10),
                                 labels = c('Grupo 1','Grupo 2','Grupo 3','Grupo 4','Grupo 5','Grupo 6','Grupo 7','Grupo 8','Grupo 9','Grupo 10'))

dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO,
                             levels = c('1','2'),
                             labels = c('Masculino','Feminino'))

dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC,
                                levels = c(1,2,3,4,5),
                                labels = c('Hospital','Outros estabelecimentos de saúde','Domicílio','Outros','Aldeia indígena'))

dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE,
                                  levels = c(1,2,3,4,5),
                                  labels = c('Solteira','Casada','Viúva','Separada judicialmente/divorciada','União estável'))

dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO,
                                 levels = c(1,2,3,4,5,6),
                                 labels = c('Menos de 22 semanas','22 a 27 semanas','28 a 31 semanas','32 a 36 semanas','37 a 41 semanas','42 semanas e mais'))

dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ,
                                 levels = c(1,2,3),
                                 labels = c('Única','Dupla','Tripla ou mais'))

dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO,
                              levels = c(1,2),
                              labels = c('Vaginal','Cesário'))

dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR,
                                levels = c(1,2,3,4,5),
                                labels = c('Branca','Preta','Amarela','Parda','Indígena'))

dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL,
                                 levels = c(1,2),
                                 labels = c('Sim','Não'))

dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010,
                                   levels = c(0,1,2,3,4,5),
                                   labels = c('Sem escolaridade','Fundamental 1','Fundamental 2','Médio','Superior incompleto','Superior completo'))

dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE,
                                levels = c(1,2,3,4,5),
                                labels = c('Branca','Preta','Amarela','Parda','Indígena'))

dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT,
                                levels = c(1,2,3),
                                labels = c('Cefálico','Pélvica ou podálica','Transversa'))

dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE,
                                 levels = c(0,1),
                                 labels = c('Nulípara','Multípara'))

                                
# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator

dados_sinasc_2$F_PESO = NA
dados_sinasc_2$F_PESO[dados_sinasc_2$PESO < 2500] = "Baixo peso"
dados_sinasc_2$F_PESO[dados_sinasc_2$PESO >= 2500 & dados_sinasc_2$PESO < 4000] = "Peso normal"
dados_sinasc_2$F_PESO[dados_sinasc_2$PESO >= 4000] = "Macrossomia"

dados_sinasc_2$F_IDADE = NA
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE < 15] = "<15"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 19] = "15-19"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 20 & dados_sinasc_2$IDADEMAE <= 24] = "20-24"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 25 & dados_sinasc_2$IDADEMAE <= 29] = "25-29"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 30 & dados_sinasc_2$IDADEMAE <= 34] = "30-34"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 35 & dados_sinasc_2$IDADEMAE <= 39] = "35-39"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 40 & dados_sinasc_2$IDADEMAE <= 44] = "40-44"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 45 & dados_sinasc_2$IDADEMAE <= 49] = "45-49"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 50] = "50+"

dados_sinasc_2$F_APGAR5 = NA
dados_sinasc_2$F_APGAR5[dados_sinasc_2$APGAR5 < 7] = "Baixo"
dados_sinasc_2$F_APGAR5[dados_sinasc_2$APGAR5 >= 7] =  "Normal"

dados_sinasc_2$F_PESO = factor(dados_sinasc_2$F_PESO,
                                levels = c("Baixo peso", "Peso normal", "Macrossomia"))

dados_sinasc_2$F_IDADE = factor(dados_sinasc_2$F_IDADE,
                                 levels = c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"))

dados_sinasc_2$F_APGAR5 = factor(dados_sinasc_2$F_APGAR5,
                                  levels = c("Baixo", "Normal"))

dados_sinasc_2$PERIG = NA
dados_sinasc_2$PERIG[dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES] = "Não"
dados_sinasc_2$PERIG[dados_sinasc_2$CODMUNNASC != dados_sinasc_2$CODMUNRES] = "Sim"
dados_sinasc_2$PERIG = factor(dados_sinasc_2$PERIG,
                               levels = c("Não", "Sim"))


dados_sinasc_2$ESTCIV = NA
dados_sinasc_2$ESTCIV[dados_sinasc_2$ESTCIVMAE == "Solteira" | dados_sinasc_2$ESTCIVMAE == "Viúva" | dados_sinasc_2$ESTCIVMAE == "Separada judicialmente/divorciada"] = "Sem companheiro"
dados_sinasc_2$ESTCIV[dados_sinasc_2$ESTCIVMAE == "Casada" | dados_sinasc_2$ESTCIVMAE == "União estável"] = "Com companheiro"
dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV,
                                levels = c("Sem companheiro", "Com companheiro"))

# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.

#leitura da tabela_pig

tabela_pig = read.csv("Tabela_PIG_Brasil.csv", header = TRUE, sep = ';')

#garantindo que SEXO esta como fator

tabela_pig$SEXO = factor(tabela_pig$SEXO, levels = c("Masculino", "Feminino"))

#merge SEMAGESTAC e SEXO

dados_sinasc_2 = merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC","SEXO"), all.x = TRUE)

#agora criando a nova variavel 

dados_sinasc_2$F_PIG = ifelse(dados_sinasc_2$GRAVIDEZ != "Única", NA,
                              ifelse(is.na(dados_sinasc_2$SEMAGESTAC) | is.na(dados_sinasc_2$SEXO) | is.na(dados_sinasc_2$PESO) | is.na(dados_sinasc_2$PESO_P10) | is.na(dados_sinasc_2$PESO_P90),
                                     NA,
                                     ifelse(dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
                                            ifelse(dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90, "AIG", "GIG"))))

dados_sinasc_2$F_PIG = factor(dados_sinasc_2$F_PIG, levels = c("PIG", "AIG", "GIG"))

#conferindo

table(dados_sinasc_2$F_PIG, useNA = "ifany")

#Tarefas 9 e 10 (reformulada) - Crie um banco de dados contendo as 103 variáveis listadas no arquivo
#“Variáveis - Projeto - Tarefas 9 e 10 da Etapa 1.pdf”
#O banco final deverá possuir:
#  • 103 colunas, correspondentes às variáveis especificadas;
#  • n + 1 linhas, onde:
#  • n corresponde ao número de municípios distintos da UF em análise
#  • a primeira linha corresponde aos valores agregados para a UF como um todo;
#  • as demais linhas correspondem aos municípios da UF.
# As variáveis devem ser construídas a partir dos microdados do SINASC, respeitando os nomes e a ordem especificados.

#separando apenas os registros de Goiás no dados_sinasc

dados_sinasc_go = dados_sinasc[substr(as.character(dados_sinasc$CODMUNRES), 1, 2) == "52", ]

#municipios presente em Goias

municipios_go = sort(unique(dados_sinasc_2$CODMUNRES))

#criando o banco de dados sinasc final (Data frame)

SINASC_GO = data.frame()



#loop para gerar a linha do estado e as linhas dos municipios

for(i in 0:length(municipios_go)) {
  
  if(i == 0) {
    banco_completo = dados_sinasc_go
    banco_reduzido = dados_sinasc_2
    nivel = "UF"
    cod = "52"
  } else {
    banco_completo = dados_sinasc_go[as.character(dados_sinasc_go$CODMUNRES) == as.character(municipios_go[i]), ]
    banco_reduzido = dados_sinasc_2[as.character(dados_sinasc_2$CODMUNRES) == as.character(municipios_go[i]), ]
    nivel = "MUNICIPIO"
    cod = as.character(municipios_go[i])
  }
  
  #criando uma linha com os resultados
  
  linha = data.frame(
    ANO = 2015,
    NIVEL = nivel,
    CODMUNRES = cod,
    
    #totais
    TN = nrow(banco_reduzido),
    TNRC = sum(complete.cases(banco_completo)),
    TNRCR = sum(complete.cases(banco_reduzido)),
    
    #idade da mae
    TGI_15 = sum(banco_reduzido$IDADEMAE < 15, na.rm = TRUE),
    TGI_15_19 = sum(banco_reduzido$IDADEMAE >= 15 & banco_reduzido$IDADEMAE <= 19, na.rm = TRUE),
    TGI_20_24 = sum(banco_reduzido$IDADEMAE >= 20 & banco_reduzido$IDADEMAE <= 24, na.rm = TRUE),
    TGI_25_29 = sum(banco_reduzido$IDADEMAE >= 25 & banco_reduzido$IDADEMAE <= 29, na.rm = TRUE),
    TGI_30_34 = sum(banco_reduzido$IDADEMAE >= 30 & banco_reduzido$IDADEMAE <= 34, na.rm = TRUE),
    TGI_35_39 = sum(banco_reduzido$IDADEMAE >= 35 & banco_reduzido$IDADEMAE <= 39, na.rm = TRUE),
    TGI_40_44 = sum(banco_reduzido$IDADEMAE >= 40 & banco_reduzido$IDADEMAE <= 44, na.rm = TRUE),
    TGI_45_49 = sum(banco_reduzido$IDADEMAE >= 45 & banco_reduzido$IDADEMAE <= 49, na.rm = TRUE),
    TGI_50 = sum(banco_reduzido$IDADEMAE >= 50, na.rm = TRUE),
    TGIF = sum(banco_reduzido$IDADEMAE >= 15 & banco_reduzido$IDADEMAE <= 49, na.rm = TRUE),
    
    #medidas da idade da mae
    IM_P25 = quantile(banco_reduzido$IDADEMAE, 0.25, na.rm = TRUE),
    IM_P50 = quantile(banco_reduzido$IDADEMAE, 0.50, na.rm = TRUE),
    IM_P75 = quantile(banco_reduzido$IDADEMAE, 0.75, na.rm = TRUE),
    IM_MD = mean(banco_reduzido$IDADEMAE, na.rm = TRUE),
    IM_DP = sd(banco_reduzido$IDADEMAE, na.rm = TRUE),
    
    #escolaridade da mae
    EM_S = sum(banco_reduzido$ESCMAE2010 == "Sem escolaridade", na.rm = TRUE),
    EM_FI = sum(banco_reduzido$ESCMAE2010 == "Fundamental 1", na.rm = TRUE),
    EM_FII = sum(banco_reduzido$ESCMAE2010 == "Fundamental 2", na.rm = TRUE),
    EM_M = sum(banco_reduzido$ESCMAE2010 == "Médio", na.rm = TRUE),
    EM_SI = sum(banco_reduzido$ESCMAE2010 == "Superior incompleto", na.rm = TRUE),
    EM_SC = sum(banco_reduzido$ESCMAE2010 == "Superior completo", na.rm = TRUE),
    
    #raça/cor da mae
    TGRC_B = sum(banco_reduzido$RACACORMAE == "Branca", na.rm = TRUE),
    TGRC_PT = sum(banco_reduzido$RACACORMAE == "Preta", na.rm = TRUE),
    TGRC_A = sum(banco_reduzido$RACACORMAE == "Amarela", na.rm = TRUE),
    TGRC_PD = sum(banco_reduzido$RACACORMAE == "Parda", na.rm = TRUE),
    TGRC_I = sum(banco_reduzido$RACACORMAE == "Indígena", na.rm = TRUE),
    
    #estado civil
    TGSC = sum(banco_reduzido$ESTCIV == "Sem companheiro", na.rm = TRUE),
    TGCC = sum(banco_reduzido$ESTCIV == "Com companheiro", na.rm = TRUE),
    
    #paridade
    TGPRI = sum(banco_reduzido$PARIDADE == "Nulípara", na.rm = TRUE),
    TGNPRI = sum(banco_reduzido$PARIDADE == "Multípara", na.rm = TRUE),
    
    #gravidez
    TGU = sum(banco_reduzido$GRAVIDEZ == "Única", na.rm = TRUE),
    TGG = sum(banco_reduzido$GRAVIDEZ == "Dupla" | banco_reduzido$GRAVIDEZ == "Tripla ou mais", na.rm = TRUE),
    
    #semanas de gestaçao
    TGD_22 = sum(banco_reduzido$SEMAGESTAC < 22, na.rm = TRUE),
    TGD_22_27 = sum(banco_reduzido$SEMAGESTAC >= 22 & banco_reduzido$SEMAGESTAC <= 27, na.rm = TRUE),
    TGD_28_31 = sum(banco_reduzido$SEMAGESTAC >= 28 & banco_reduzido$SEMAGESTAC <= 31, na.rm = TRUE),
    TGD_32_36 = sum(banco_reduzido$SEMAGESTAC >= 32 & banco_reduzido$SEMAGESTAC <= 36, na.rm = TRUE),
    TGD_37_41 = sum(banco_reduzido$SEMAGESTAC >= 37 & banco_reduzido$SEMAGESTAC <= 41, na.rm = TRUE),
    TGD_42 = sum(banco_reduzido$SEMAGESTAC >= 42, na.rm = TRUE),
    
    #pre termo, termo e pos termo
    TGD_PRT = sum(banco_reduzido$SEMAGESTAC < 37, na.rm = TRUE),
    TGD_AT = sum(banco_reduzido$SEMAGESTAC >= 37 & banco_reduzido$SEMAGESTAC <= 41, na.rm = TRUE),
    TGD_PST = sum(banco_reduzido$SEMAGESTAC >= 42, na.rm = TRUE),
    
    #medidas da gestaçao
    DG_P25 = quantile(banco_reduzido$SEMAGESTAC, 0.25, na.rm = TRUE),
    DG_P50 = quantile(banco_reduzido$SEMAGESTAC, 0.50, na.rm = TRUE),
    DG_P75 = quantile(banco_reduzido$SEMAGESTAC, 0.75, na.rm = TRUE),
    DG_MD = mean(banco_reduzido$SEMAGESTAC, na.rm = TRUE),
    DG_DP = sd(banco_reduzido$SEMAGESTAC, na.rm = TRUE),
    
    #Kotelchuck
    TKC_NR = sum(banco_reduzido$KOTELCHUCK == "Não realizou pré-natal", na.rm = TRUE),
    TKC_ID = sum(banco_reduzido$KOTELCHUCK == "Inadequado", na.rm = TRUE),
    TKC_IT = sum(banco_reduzido$KOTELCHUCK == "Intermediário", na.rm = TRUE),
    TKC_AD = sum(banco_reduzido$KOTELCHUCK == "Adequado", na.rm = TRUE),
    TKC_MAD = sum(banco_reduzido$KOTELCHUCK == "Mais que adequado", na.rm = TRUE),
    
    #peregrinaçao
    TGPRG_S = sum(banco_reduzido$PERIG == "Sim", na.rm = TRUE),
    TGPRG_N = sum(banco_reduzido$PERIG == "Não", na.rm = TRUE),
    
    #tipo de parto
    TPV = sum(banco_reduzido$PARTO == "Vaginal", na.rm = TRUE),
    TPC = sum(banco_reduzido$PARTO == "Cesário", na.rm = TRUE),
    
    #apresentaçao fetal
    TRAP_C = sum(banco_reduzido$TPAPRESENT == "Cefálico", na.rm = TRUE),
    TRAP_P = sum(banco_reduzido$TPAPRESENT == "Pélvica ou podálica", na.rm = TRUE),
    TRAP_T = sum(banco_reduzido$TPAPRESENT == "Transversa", na.rm = TRUE),
    
    #grupos de Robson
    TGROB_1 = sum(banco_reduzido$TPROBSON == "Grupo 1", na.rm = TRUE),
    TGROB_2 = sum(banco_reduzido$TPROBSON == "Grupo 2", na.rm = TRUE),
    TGROB_3 = sum(banco_reduzido$TPROBSON == "Grupo 3", na.rm = TRUE),
    TGROB_4 = sum(banco_reduzido$TPROBSON == "Grupo 4", na.rm = TRUE),
    TGROB_5 = sum(banco_reduzido$TPROBSON == "Grupo 5", na.rm = TRUE),
    TGROB_6 = sum(banco_reduzido$TPROBSON == "Grupo 6", na.rm = TRUE),
    TGROB_7 = sum(banco_reduzido$TPROBSON == "Grupo 7", na.rm = TRUE),
    TGROB_8 = sum(banco_reduzido$TPROBSON == "Grupo 8", na.rm = TRUE),
    TGROB_9 = sum(banco_reduzido$TPROBSON == "Grupo 9", na.rm = TRUE),
    TGROB_10 = sum(banco_reduzido$TPROBSON == "Grupo 10", na.rm = TRUE),
    
    #local de nascimento
    TNLOC_H = sum(banco_reduzido$LOCNASC == "Hospital", na.rm = TRUE),
    TNLOC_ES = sum(banco_reduzido$LOCNASC == "Outros estabelecimentos de saúde", na.rm = TRUE),
    TNLOC_D = sum(banco_reduzido$LOCNASC == "Domicílio", na.rm = TRUE),
    TNLOC_O = sum(banco_reduzido$LOCNASC == "Outros", na.rm = TRUE),
    TNLOC_AI = sum(banco_reduzido$LOCNASC == "Aldeia indígena", na.rm = TRUE),
    
    #sexo do recem nascido
    TRS_M = sum(banco_reduzido$SEXO == "Masculino", na.rm = TRUE),
    TRS_F = sum(banco_reduzido$SEXO == "Feminino", na.rm = TRUE),
    
    #raça/cor do recem nascido
    TRRC_B = sum(banco_reduzido$RACACOR == "Branca", na.rm = TRUE),
    TRRC_PT = sum(banco_reduzido$RACACOR == "Preta", na.rm = TRUE),
    TRRC_A = sum(banco_reduzido$RACACOR == "Amarela", na.rm = TRUE),
    TRRC_PD = sum(banco_reduzido$RACACOR == "Parda", na.rm = TRUE),
    TRRC_I = sum(banco_reduzido$RACACOR == "Indígena", na.rm = TRUE),
    
    #peso ao nascer
    TRP_BP = sum(banco_reduzido$F_PESO == "Baixo peso", na.rm = TRUE),
    TRP_N = sum(banco_reduzido$F_PESO == "Peso normal", na.rm = TRUE),
    TRP_M = sum(banco_reduzido$F_PESO == "Macrossomia", na.rm = TRUE),
    
    #medidas do peso
    PESO_P25 = quantile(banco_reduzido$PESO, 0.25, na.rm = TRUE),
    PESO_P50 = quantile(banco_reduzido$PESO, 0.50, na.rm = TRUE),
    PESO_P75 = quantile(banco_reduzido$PESO, 0.75, na.rm = TRUE),
    PESO_MD = mean(banco_reduzido$PESO, na.rm = TRUE),
    PESO_DP = sd(banco_reduzido$PESO, na.rm = TRUE),
    
    #PIG/AIG/GIG
    TRPIG_P = sum(banco_reduzido$F_PIG == "PIG", na.rm = TRUE),
    TRPIG_A = sum(banco_reduzido$F_PIG == "AIG", na.rm = TRUE),
    TRPIG_G = sum(banco_reduzido$F_PIG == "GIG", na.rm = TRUE),
    
    #apgar5 categorizado
    TRAPG5_B = sum(banco_reduzido$F_APGAR5 == "Baixo", na.rm = TRUE),
    TRAPG5_N = sum(banco_reduzido$F_APGAR5 == "Normal", na.rm = TRUE),
    
    #media e desvio padrao do apgar5
    APG5_MD = mean(as.numeric(as.character(banco_reduzido$APGAR5)), na.rm = TRUE),
    APG5_DP = sd(as.numeric(as.character(banco_reduzido$APGAR5)), na.rm = TRUE),
    
    #anomalias
    TRAC = sum(banco_reduzido$IDANOMAL == "Sim", na.rm = TRUE),
    TRSAC = sum(banco_reduzido$IDANOMAL == "Não", na.rm = TRUE)
  )
  
  #linha do banco final
  SINASC_GO = rbind(SINASC_GO, linha)
}

#exportando o arquivo

write.csv(SINASC_GO, "SINASC_GO.csv", row.names = FALSE)



# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv



# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"



##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 2

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1216475 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão (a informar)
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, CODMUNNATU, IDADE,  SEXO,  RACACOR,  ESTCIV, ESC2010, 
# CODMUNRES,  LOCOCOR, CODMUNOCOR, TPMORTEOCO,  OBITOGRAV, OBITOPUERP, CAUSABAS, CAUSABAS_O, TPOBITOCOR, MORTEPARTO



#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 3

# Tarefa 1. Acesso aos bancos de dados e obtenção da informação



#####################################################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO, BASEADO NAS ANÁLISES DE SINASC, SIM, IBGE, SNIS,...
######################################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 4

# Cada aluno gerar um dataframe de uma única linha (referente ao seu estado) com as variáveis na ordem indicada pela professora



############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

