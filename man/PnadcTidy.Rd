\name{PnadcTidy}
\alias{PnadcTidy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transforma microdados baixados da PnadC em um tidy dataset.

}
\description{
O pacote PnadcTidy transforma microdados baixados da PnadC em um tidy dataset, isto é, um banco relacional do tipo definido por Wickham (2014).




Para usá-lo é preciso antes baixar os microdados e também o arquivo de Input para o SAS, disponíveis em:
ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/.


Também é necessário declarar quais variáveis serão utilizadas (não é necessário declarar as variáveis de peso e do deseho amostral).
}
\usage{
PnadcTidy(inputSAS,arquivoPnad, variaveis)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputSAS}{
usado para especificar o nome do arquivo Input do SAS (deve terminar em ".txt").
}

  \item{arquivoPnad}{
usado para especificar o nome do arquivo de microdados da PnadC (deve terminar em ".txt").
}

  \item{variaveis}{
usado para selecionar as variáveis da PnadC que serão mantidas na base.
}
}

\details{
O pacote PnadcTidy permite que os microdados da PNAD Contínua sejam transformados em um banco de dados relacional do tipo tidy dataset.




Ele é baseado no pacote "PNADcIBGE"", de Douglas Braga. Inclusive, utiliza parte da programação desse pacote para a extração de informações do arquivo de input do SAS, na função inputPnadC.


Diferentemente do PNADcIBGE, o PnadcTidy retorna um banco relacional, que é mais fácil de ser manuseado utilizando, por exemplo, os pacotes dplyr e ggplot2 - ambos incluídos no pacote tidyverse.
Embora isso signifique que este banco não contenha a declaração do desenho amostral, isso também representa uma vantagem para aquelas pessoas com menos familiaridade com os conceitos relacionados ao desenho amostral, como Coeficiente de Variação.


A desvantagem é que o banco de dados resultante exige que sempre se declare a variável de peso amostral (V1028, para as pesquisas trimestrais ou V1032 para as pesquisas anuais).




Por exemplo, após rodar o comando PnadcTidy para os microdados de determinado trimestre, e salvar o banco com o nome PNAD1, se alguém quiser obter a frequência de pessoas segundo Sexo (V2007) e Condição na ocupação (V4002), terá que instalar o pacote questionr e, em seguida, rodar as seguintes linhas de comando:




tabela <- wtd.table(PNAD1$V2007, PNAD1$VD4002, weights = PNAD1$V1028)

tabela




Observe que é o comando especifica o peso amostral utilizando o parâmetro "weights".


}
\value{
Retorna uma base de dados arrumada (tidy dataset) que contém apenas as variáveis selecionadas e as variáveis de peso e do desenho amostral.
}
\references{
Wickham, H. (2014). Tidy data. Journal of Statistical Software, 59(10):1–23. Disponível em: <https://www.jstatsoft.org/article/view/v059i10>.

Braga, D. Análise de microdados da PNAD Contínua com os pacotes PNADcINGE e survey. Disponível em: <https://rpubs.com/BragaDouglas/335574>.
}
\author{
Gustavo Monteiro
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Depois de baixar os microdados do 4º trimestre de 2019
# e o arquivo de input, salvá-los em uma pasta local e
# definir essa pasta como a pasta de trabalho
# usando o comando setwd(caminho/da/pasta):

PNADc04T2019 <- PnadcTidy(inputSAS="Input_PNADC_trimestral.txt",
                                arquivoPnad="PNADC_042019.txt",
                                variaveis=c("VD4009", "V2009", "V2007" ,"UF", "VD4016"))

# Retorna um tidy dataset, alocado no objeto PNADc04T2019.
# Ele conterá as variáveis do desenho amostral
# e também as variáveis VD4009, V2009, V2007 UF e VD4016.

***

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
