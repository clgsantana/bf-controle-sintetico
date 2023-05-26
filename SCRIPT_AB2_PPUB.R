# AB2 da matéria de Políticas Públicas
# Alunos: Cayo Luca e Jamylle Lyra
# Análise dos impactos do programa Bolsa Família para o estado de Alagoas.
# A metodologia aplicada será a de Controle Sintético.

# 1. Preparar o ambiente instalando o pacote Synth, o qual utilizaremos para 
# realizar os procedimentos de controle sintético, e o pacote Readxl, o qual
# utilizaremos para importar os dados da base de dados em xlsx.

install.packages('Synth')
install.packages('readxl')
library('Synth')
library('readxl')

# Para importar a base de dados, utilizamos as seguintes linhas de comando para
# tal. É importante frisar que o comando dataprep() a ser utilizado posterior-
# mente necessita de que os dados sejam da classe data.frame, e para isso usamos
# o comando as.data.frame():

dataset_dados <- read_excel('C:/local_armazenado_no_computador
                            /AB2 Políticas Públicas - Cayo e Jamylle/data.xlsx')
dataset_dados <- as.data.frame(dataset_dados)
is.data.frame(dataset_dados)
class(dataset_dados)


# 2. Análise do impacto do Programa Bolsa Família na Renda per capita.

# Organizamos os dados utilizando o comando dataprep() da biblioteca Synth, que
# transformará em matriz os dados do data.frame e determinará os preditores, a
# linha temporal, a variável dependente e as unidades tratada e de controle.

dataprep_controle.out <- dataprep(foo = dataset_dados,
           predictors = c('indiv_pobres', 'domic_pobres', 'ind_gini', 
                          'ind_analfabetos', 'ocup_5a9', 'ocup_10a14',
                          'ocup_15a19', 'taxa_mortalidade'),
           predictors.op = "mean",
           time.predictors.prior = (2002:2003),
           special.predictors = list(
             list('tran_pc_bf',       seq(2013,2014,2), 'mean'),
             list('renda_pc',         seq(2002,2014,2), 'mean'),
             list('indiv_pobres',     seq(2002,2014,2), 'mean'),
             list('domic_pobres',     seq(2002,2014,2), 'mean'),
             list('ind_gini',         seq(2002,2014,2), 'mean'),
             list('ind_analfabetos',  seq(2002,2014,2), 'mean'),
             list('ocup_5a9',         seq(2002,2014,2), 'mean'),
             list('ocup_10a14',       seq(2002,2014,2), 'mean'),
             list('ocup_15a19',       seq(2002,2014,2), 'mean'),
             list('taxa_mortalidade', seq(2002,2014,2), 'mean')
             ),
           dependent = "renda_pc",
           unit.variable = "num_regiao",
           unit.names.variable = "nome_regiao",
           time.variable = "ano",
           treatment.identifier = 3,
           controls.identifier = c(2,4:28),
           time.optimize.ssr = (2004:2014),
           time.plot = 2002:2014)

 
# Verificaremos as variáveis do X1 a partir do seguinte comando, e juntaremos os
# dados das variáveis ocup_5a9, ocup_10a14 e ocup_15a19 na variável ocup_5a9,
# para melhor utilização e visualização dos dados posteriormente:
dataprep_controle.out$X1
dataprep_controle.out$Z1

dataprep_controle.out$X1["ocup_10a14",] <-
  dataprep_controle.out$X1["ocup_10a14",] + dataprep_controle.out$X1["ocup_15a19",]

# Com o seguinte comando, excluiremos as colunas sobressalentes advindas da uti-
# lização do comando anterior:
dataprep_controle.out$X1 <-
  as.matrix(dataprep_controle.out$X1[-which(rownames(dataprep_controle.out$X1)=="ocup_15a19"),])

# Faremos o mesmo procedimento para com as variáveis do X0:
dataprep_controle.out$X0
dataprep_controle.out$Z0

dataprep_controle.out$X0["ocup_10a14",] <-
  dataprep_controle.out$X0["ocup_10a14",]  + dataprep_controle.out$X0["ocup_15a19",]

dataprep_controle.out$X0 <-
  dataprep_controle.out$X0[-which(rownames(dataprep_controle.out$X0)=="ocup_15a19"),]

lowest  <- which(rownames(dataprep_controle.out$X0)=="ocup_5a9")
highest <- which(rownames(dataprep_controle.out$X0)=="ocup_10a14")

dataprep_controle.out$X1[lowest:highest,] <-
  (100 * dataprep_controle.out$X1[lowest:highest,]) /
  sum(dataprep_controle.out$X1[lowest:highest,])

dataprep_controle.out$X0[lowest:highest,] <-
  100 * scale(dataprep_controle.out$X0[lowest:highest,],
  center=FALSE,
  scale=colSums(dataprep_controle.out$X0[lowest:highest,]))

# Após rodar os comandos e preparar os dados para utilização no comando synth():
synth.out <- synth(data.prep.obj = dataprep_controle.out)

synth.tables <- synth.tab(dataprep.res = dataprep_controle.out,
                          synth.res = synth.out)
print(synth.tables)
synth.tables$tab.pred[1:7,]

# Criando o gráfico do tratado e do controle
path.plot(synth.res = synth.out,
          dataprep.res = dataprep_controle.out,
          Main = "Renda per capita de Alagoas",
          Ylab = " ",
          Xlab = "Ano",
          tr.intake = 2004,
          Legend = c("Alagoas","Alagoas sintética"),
          Legend.position = "bottomright")

# Gráfico da diferença entre tratado e controle
gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep_controle.out,
          Main = "Diferença entre controle e tratado",
          Ylab = " ",
          Xlab = "Ano")

# Realizamos um teste-placebo, utilizando o controle sintético para a unidade de
# tratamento que melhor se adequa a Alagoas - que, neste caso, é o estado do Ma-
# ranhão. O procedimento utilizado é similar ao utilizado para Alagoas.
# Inicialmente, realizamos o controle sintético tendo Maranhão como o unidade de
# tratamento em relação às demais unidades federativas, excetuando Alagoas:
dataprep_controle.out <- dataprep(foo = dataset_dados,
         predictors = c('indiv_pobres', 'domic_pobres', 'ind_gini', 
                        'ind_analfabetos', 'ocup_5a9', 'ocup_10a14',
                        'ocup_15a19', 'taxa_mortalidade'),
         predictors.op = "mean",
         time.predictors.prior = (2002:2003),
         special.predictors = list(
                           list('tran_pc_bf',       seq(2013,2014,2), 'mean'),
                           list('renda_pc',         seq(2002,2014,2), 'mean'),
                           list('indiv_pobres',     seq(2002,2014,2), 'mean'),
                           list('domic_pobres',     seq(2002,2014,2), 'mean'),
                           list('ind_gini',         seq(2002,2014,2), 'mean'),
                           list('ind_analfabetos',  seq(2002,2014,2), 'mean'),
                           list('ocup_5a9',         seq(2002,2014,2), 'mean'),
                           list('ocup_10a14',       seq(2002,2014,2), 'mean'),
                           list('ocup_15a19',       seq(2002,2014,2), 'mean'),
                           list('taxa_mortalidade', seq(2002,2014,2), 'mean')
                              ),
                              dependent = "renda_pc",
                              unit.variable = "num_regiao",
                              unit.names.variable = "nome_regiao",
                              time.variable = "ano",
                              treatment.identifier = 11,
                              controls.identifier = c(2:10,12:28),
                              time.optimize.ssr = (2004:2014),
                              time.plot = 2002:2014)

dataprep_controle.out$X1["ocup_10a14",] <-
  dataprep_controle.out$X1["ocup_10a14",] +
  dataprep_controle.out$X1["ocup_15a19",]

dataprep_controle.out$X1 <-
  as.matrix(dataprep_controle.out$X1[-which(
    rownames(dataprep_controle.out$X1)=="ocup_15a19"),])


dataprep_controle.out$X0["ocup_10a14",] <-
  dataprep_controle.out$X0["ocup_10a14",]  +
  dataprep_controle.out$X0["ocup_15a19",]

dataprep_controle.out$X0 <-
  dataprep_controle.out$X0[-which(
    rownames(dataprep_controle.out$X0)=="ocup_15a19"),]

lowest  <- which(rownames(dataprep_controle.out$X0)=="ocup_5a9")
highest <- which(rownames(dataprep_controle.out$X0)=="ocup_10a14")

dataprep_controle.out$X1[lowest:highest,] <-
  (100 * dataprep_controle.out$X1[lowest:highest,]) /
  sum(dataprep_controle.out$X1[lowest:highest,])

dataprep_controle.out$X0[lowest:highest,] <-
  100 * scale(dataprep_controle.out$X0[lowest:highest,],
        center=FALSE,
        scale=colSums(dataprep_controle.out$X0[lowest:highest,]))

synth.out <- synth(data.prep.obj = dataprep_controle.out)

path.plot(synth.res = synth.out,
          dataprep.res = dataprep_controle.out,
          tr.intake = 2004,
          Main = "Renda per capita do Maranhão",
          Ylab = " ",
          Xlab = "Ano",
          Legend = c("Maranhão","Maranhão sintético"),
          Legend.position = "bottomright")

# Logo então, nos preparamos para realizar o teste das diferenças. Para isso,
# preparamos uma matriz que posteriormente virá a conter os dados das diferenças
# entre as unidades tratadas e unidades de controle.
store <- matrix(NA,length(2002:2014),28)
colnames(store) <- unique(dataset_dados$nome_regiao)

# Rodando o teste Placebo para todo o grupo de controle, utilizando um loop para
# captar as diferenças entre eles. Vale ressaltar que o loop precisará de um 
# certo tempo para agregar os dados:
for(iter in 2:28)
{
dataprep_controle.out <- dataprep(foo = dataset_dados,
                predictors = c('indiv_pobres', 'domic_pobres', 'ind_gini',
                               'ind_analfabetos', 'ocup_5a9', 'ocup_10a14',
                               'ocup_15a19', 'taxa_mortalidade'),
                predictors.op = "mean",
                time.predictors.prior = (2002:2003),
                special.predictors = list(
                          list('tran_pc_bf',       seq(2013,2014), 'mean'),
                          list('renda_pc',         seq(2002,2014,2), 'mean'),
                          list('indiv_pobres',     seq(2002,2014,2), 'mean'),
                          list('domic_pobres',     seq(2002,2014,2), 'mean'),
                          list('ind_gini',         seq(2002,2014,2), 'mean'),
                          list('ind_analfabetos',  seq(2002,2014,2), 'mean'),
                          list('ocup_5a9',         seq(2002,2014,2), 'mean'),
                          list('ocup_10a14',       seq(2002,2014,2), 'mean'),
                          list('ocup_15a19',       seq(2002,2014,2), 'mean'),
                          list('taxa_mortalidade', seq(2002,2014,2), 'mean')
                                ),
                dependent = "renda_pc",
                unit.variable = "num_regiao",
                unit.names.variable = "nome_regiao",
                time.variable = "ano",
                treatment.identifier = iter,
                controls.identifier = c(2:28)[-iter+1],
                time.optimize.ssr = (2004:2014),
                time.plot = 2002:2014)


dataprep_controle.out$X1["ocup_10a14",] <-
    dataprep_controle.out$X1["ocup_10a14",] +
    dataprep_controle.out$X1["ocup_15a19",]

dataprep_controle.out$X1 <-
    as.matrix(dataprep_controle.out$X1[-which(
      rownames(dataprep_controle.out$X1)=="ocup_15a19"),])


dataprep_controle.out$X0["ocup_10a14",] <-
    dataprep_controle.out$X0["ocup_10a14",]  +
    dataprep_controle.out$X0["ocup_15a19",]

dataprep_controle.out$X0 <-
    dataprep_controle.out$X0[-which(
      rownames(dataprep_controle.out$X0)=="ocup_15a19"),]


lowest  <- which(rownames(dataprep_controle.out$X0)=="ocup_5a9")
highest <- which(rownames(dataprep_controle.out$X0)=="ocup_10a14")

dataprep_controle.out$X1[lowest:highest,] <-
    (100 * dataprep_controle.out$X1[lowest:highest,]) /
    sum(dataprep_controle.out$X1[lowest:highest,])

dataprep_controle.out$X0[lowest:highest,] <-
    100 * scale(dataprep_controle.out$X0[lowest:highest,],
          center=FALSE,
          scale=colSums(dataprep_controle.out$X0[lowest:highest,]))

synth.out <- synth(data.prep.obj = dataprep_controle.out)

# Salvaremos as diferenças na matriz anteriormente realizada, para uso posterior
store[,iter-1] <- dataprep_controle.out$Y1plot -
    (dataprep_controle.out$Y0plot %*% synth.out$solution.w)
}

# Logo então, plotaremos o gráfico de diferenças entre as unidades de tratamento
# e controle da seguinte forma:
data <- store
rownames(data) <- 2002:2014
gap.start     <- 1
gap.end       <- nrow(data)
years         <- 2002:2014
gap.end.pre  <- which(rownames(data)=="2004")
mse        <- apply(data[gap.start:gap.end.pre,]^2,2,mean)
al.mse     <- as.numeric(mse[26])
data <- data[,mse<5*al.mse]

plot(years, data[gap.start:gap.end, which(colnames(data)=="alagoas")],
     xlab="Ano",
     ylim=c(-150,150),
     xlim=c(2002,2014),ylab="Diferença na Renda per capita",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")

for (i in 1:ncol(data)) { lines(years,data[gap.start:gap.end,i],col="gray") }

lines(years, data[gap.start:gap.end, which(colnames(data)=="alagoas")],
      lwd=2,
      col="blue")
abline(v=2004,lty="dotted",lwd=2)
abline(h=0,lty="dashed",lwd=2)
legend("bottomright",legend=c("Alagoas","Regiões de controle"),
       lty=c(1,1),col=c("black","gray"),lwd=c(2,1),cex=.8)
