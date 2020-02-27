PnadcTidy <- function(inputSAS, arquivoPnad, variaveis) {

        # carrega os pacotes utilizados
        library(dplyr)
        library(readr)

        # fun??o inputPnadC: lendo arquivo de input do sas (baseado no pacote PNADcIBGE)
        inputPnadC <- function(inputSAS) {
                suppressWarnings(suppressMessages(
                        readr::read_table2(inputSAS, col_names = F) %>% # l? a tabela
                                subset(substr(X1, 1, 1) == "@") %>%  # seleciona linhas em que X1 come?a com @
                                dplyr:: mutate(X3 = as.integer(chartr("$", " ", X3))) %>% # tira o $ do X3 e transforma e num?rico
                                dplyr::select(X3, X2)))

        }

        input <- inputPnadC(inputSAS)



        # fun??o lendoPnadC: usando o input do sas para ler e fazer um banco
        lendoPnadC <- function(arquivoPnad) {
                dadosPnad <- read_fwf(file = arquivoPnad, fwf_widths(input$X3))
                colnames(dadosPnad) <- input$X2
                dadosPnad
        }

        PNADC4T19 <- lendoPnadC(arquivoPnad)



        # cria vetor com variaveis de desenho amostral de acordo com pnad anual ou trimestral
        if ("V1027" %in% names(PNADC4T19)) {
                varsDesenho <- c("UPA", "Estrato", "posest", "V1028", "V1027", "V1029")
        } else {
                varsDesenho <- c("UPA", "Estrato", "posest", "V1032",  "V1031", "V1030")
        }



        # transforma as variaveis de desenho amostral e as selecionadas em numericas
        arrumaVars <- function(variaveis) {
                PNAD1 <-PNADC4T19%>%
                        dplyr::select(varsDesenho, variaveis)

                for (i in seq_along(varsDesenho)) {
                        PNAD1 <-PNAD1 %>%
                                dplyr::mutate(!!sym(varsDesenho[i]) := as.numeric(!!sym(varsDesenho[i])))
                }

                for (i in seq_along(variaveis)) {
                        PNAD1 <-PNAD1 %>%
                                dplyr::mutate(!!sym(variaveis[i]) := as.numeric(!!sym(variaveis[i])))
                }
                PNAD1

        }

        PNAD1 <- arrumaVars(variaveis)
        return(PNAD1)

}
