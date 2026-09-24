library(readxl)
library(openxlsx)
library(dplyr)
library(xlsx)
library(rio)
library(tidyr)
library(writexl)

#fiz o R ler a planilha
FormigasdoMarcio <- read_excel("formigasMarcioAtualizada.xlsx")

#chamei a planilha e a função 'paste' criou uma coluna nova usando duas colunas. dai eu preciso chamar a planilha e depois as colunas que eu quero juntar
FormigasdoMarcio$ScientificName <- paste(FormigasdoMarcio$genus, FormigasdoMarcio$specificEpithet)

#isso foi pra olhar para uma coluna específica da planilha, no caso a que eu tinha acabado de criar
table(FormigasdoMarcio$ScientificName)

#isso foi pra salvar a planilha com as alterações. podia ter alterado o nome do arquivo, mas não quis
write.xlsx(FormigasdoMarcio, "formigasMarcioAtualizada.xlsx")

#fiz o R ler a planilha
confirmacaoPontosGABI.xlsx <- read_excel("confirmacaoPontosGABI.xlsx")

#tirei os pontos na coluna Scientific Name com 'gsub("\\.", " ",  confirmacaoPontosGABI.xlsx$`Scientific Name`)'
confirmacaoPontosGABI.xlsx$`Scientific Name`<- gsub("\\.", " ",  confirmacaoPontosGABI.xlsx$`Scientific Name`)

#chequei se tava sem os pontos
head(confirmacaoPontosGABI.xlsx$`Scientific Name`)

#salvei a planilha atualizada
write.xlsx(confirmacaoPontosGABI.xlsx, "confimacaoPontosGABI.xlsx")

#agora, vou usar uma fórmula do chatgpt

#isso foi pra garantir que individualCount seja numérico
FormigasMarcioAtualizada.xlsx$individualCount <- as.numeric(FormigasMarcioAtualizada.xlsx$individualCount)

#criando um índice repetindo cada linha conforme o individualCount
indice <- rep( seq_len(nrow(FormigasMarcioAtualizada.xlsx)), times = FormigasMarcioAtualizada.xlsx$individualCount)

#agora, vou criar a planilha com os registros expandidos
FormigasFlorestadaTijuca <- FormigasMarcioAtualizada.xlsx[indice, ]

#Reorganizar os números das linhas- nao entendi muito bem 
rownames(FormigasFlorestadaTijuca) <- NULL

nrow(FormigasMarcioAtualizada.xlsx)

sum(FormigasMarcioAtualizada.xlsx$individualCount, na.rm = TRUE)

nrow(FormigasFlorestadaTijuca)

View(FormigasFlorestadaTijuca)

#agora, vou salvar tudo isso
write.xlsx(FormigasFlorestadaTijuca, "FormigasFlorestadaTijuca") #como já existia, deu erro, então eu substituí o . por _ e mandei ele criar, não entendi muito bem mas ok

write_xlsx(FormigasFlorestadaTijuca, "FormigasFlorestadaTijuca.xlsx")

#agora, voltando pra planilha GABI vou atualizar os nomes que mudaram 

confirmacaoPontosGABI.xlsx$`Scientific Name` <- ifelse(
  confirmacaoPontosGABI.xlsx$`Scientific Name` == "cryptopone holmgreni",
  "Wadeura holmgreni",
  
  ifelse(
    confirmacaoPontosGABI.xlsx$`Scientific Name` == "Alfaria minuta",
    "Gnamptogenys minuta",
    
    ifelse(
      confirmacaoPontosGABI.xlsx$`Scientific Name` == "Gnamptogenys rastrata",
      "Poneracantha rastrata",
      
      ifelse(
        confirmacaoPontosGABI.xlsx$`Scientific Name` == "Heteroponera microps",
        "Bazboltonia microps",
        
        ifelse(
          confirmacaoPontosGABI.xlsx$`Scientific Name` == "Labidus mars",
          "Neivamyrmex mars",
          
          ifelse(
            confirmacaoPontosGABI.xlsx$`Scientific Name` == "gnamptogenys striatula",
            "holcoponera striatula",
            
            ifelse(
              confirmacaoPontosGABI.xlsx$`Scientific Name` == "gnamptogenys minuta",
              "alfaria minuta",
              
              ifelse(
                confirmacaoPontosGABI.xlsx$`Scientific Name` == "gnamptogenys rastrata",
                "poneracantha rastrata",
                
                ifelse(
                  confirmacaoPontosGABI.xlsx$`Scientific Name` == "gnamptogenys piei",
                  "alfaria piei",
                  
                  ifelse(
                    confirmacaoPontosGABI.xlsx$`Scientific Name` == "gnamptogenys moelleri",
                    "holcoponera moelleri",
                    
                    ifelse(
                      confirmacaoPontosGABI.xlsx$`Scientific Name` == "acromyrmex subterraneus molestans",
                      "acromyrmex molestans",
                      
                      ifelse(
                        confirmacaoPontosGABI.xlsx$`Scientific Name` == "acromyrmex subterraneus brunneus",
                        "acromyrmex brunneus",
                        
                        confirmacaoPontosGABI.xlsx$`Scientific Name`
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)


unique(confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` %in% c(
    "cryptopone holmgreni",
    "Alfaria minuta",
    "Gnamptogenys rastrata",
    "Heteroponera microps",
    "Labidus mars",
    "gnamptogenys striatula",
    "gnamptogenys minuta",
    "gnamptogenys rastrata",
    "gnamptogenys piei",
    "gnamptogenys moelleri",
    "acromyrmex subterraneus molestans",
    "acromyrmex subterraneus brunneus"
  )
])

table(confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` %in% c(
    "Alfaria minuta",
    "Gnamptogenys minuta",
    "alfaria minuta",
    "gnamptogenys minuta"
  )
])



#agora q deu tudo errado, to tentando de novo


unique(confirmacaoPontosGABI.xlsx$`Scientific Name`[
  grepl(
    "cryptopone holmgreni|gnamptogenys rastrata|heteroponera microps|labidus mars|gnamptogenys striatula|gnamptogenys minuta|gnamptogenys piei|gnamptogenys moelleri|acromyrmex subterraneus molestans|acromyrmex subterraneus brunneus",
    confirmacaoPontosGABI.xlsx$`Scientific Name`,
    ignore.case = TRUE
  )
])

confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` == "gnamptogenys minuta"
] <- "alfaria minuta"

unique(confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` == "gnamptogenys minuta"
])

table(confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` == "alfaria minuta"
])


confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` == "cryptopone holmgreni"
] <- "Wadeura holmgreni"

confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` == "Gnamptogenys rastrata"
] <- "Poneracantha rastrata"

confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` == "Heteroponera microps"
] <- "Bazboltonia microps"

confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` == "Labidus mars"
] <- "neivamyrmex mars"

confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` == "gnamptogenys striatula"
] <- "holcoponera striatula"



confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` == "gnamptogenys rastrata"
] <- "poneracantha rastrata"

confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` == "gnamptogenys piei"
] <- "alfaria piei"

confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` == "gnamptogenys moelleri"
] <- "holcoponera moelleri"

confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` == "acromyrmex subterraneus molestans"
] <- "acromyrmex molestans"

confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` == "acromyrmex subterraneus brunneus"
] <- "acromyrmex brunneus"

table(confirmacaoPontosGABI.xlsx$`Scientific Name`)


confirmacaoPontosGABI.xlsx$`Scientific Name`[
  confirmacaoPontosGABI.xlsx$`Scientific Name` %in% c(
    "cryptopone holmgreni",
    "Gnamptogenys rastrata",
    "Heteroponera microps",
    "Labidus mars",
    "gnamptogenys striatula",
    "gnamptogenys minuta",
    "gnamptogenys rastrata",
    "gnamptogenys piei",
    "gnamptogenys moelleri",
    "acromyrmex subterraneus molestans",
    "acromyrmex subterraneus brunneus"
  )
]


write_xlsx(
  confirmacaoPontosGABI.xlsx,
  "confirmacaoPontosGABIcorrigida.xlsx"
)

confirmacaoPontosGABIcorrigida.xlsx<- read_excel("confirmacaoPontosGABIcorrigida.xlsx")

GABIeTijuca <- bind_rows(confirmacaoPontosGABIcorrigida.xlsx, FormigasFlorestadaTijuca)


nrow(confirmacaoPontosGABIcorrigida.xlsx)
nrow(FormigasFlorestadaTijuca)
nrow(GABIeTijuca)

View(GABIeTijuca)


finaldados <- rbind(confirmacaoPontosGABIcorrigida.xlsx, FormigasFlorestadaTijuca)

FormigasFlorestadaTijucaVERDADEIRA <- read_excel("FormigasFlorestadaTijuca.xlsx")
pontosGabiVERDADEIRA <- read_excel("confirmacaoPontosGabiCorrigida.xlsx")

dadosJoin <- full_join( # JUNTANDO AS DUAS PLANILHAS
  FormigasFlorestadaTijucaVERDADEIRA,
  pontosGabiVERDADEIRA,
  by = c(
    "Scientific Name",
    "Latitude" = "Latitude",
    "Longitude" = "Longitude"
  )
)

write_xlsx( # SALVANDO A PLANILHA NO FORMATO DE PLANILHA
  dadosJoin,
  "dadosJoin.xlsx"
)
