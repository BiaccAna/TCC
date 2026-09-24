library(readxl)
library(openxlsx)
library(dplyr)
library(xlsx)
library(rio)
library(tidyr)
library(writexl)

#chamei a planilha 
GABI <- read_excel("ResultadoFinalCamadasUnidas.xlsx")
GABI$cob_1 <- as.factor(GABI$cob_1) # Transformando cob_1 em uma variável fatorial para funcionar no case when
# Por serem valores numéricos, o R identificou a coluna como sendo numérica, mas esses números se referiam a 
# categorias, não a valores contínuos (como idade).

EspecialistaFloresta <- read_excel("EspecalistaFloresta.xlsx")
EspecialistaAreaAberta <- read_excel("EspecialistaAreaAberta.xlsx")
Generalista <- read_excel("Generalista.xlsx")
SemClassificacao <- read_excel("SemClassificacao.xlsx")


#tirando os pontos da coluna valid_species_name

#GABI$valid_species_name <- gsub("\\.", " ", GABI$valid_species_name)

#mudando o nome das formigas

GABI <- GABI |> 
 
  dplyr::mutate( 
    field_3 = dplyr::case_when(
      field_3 %in% "Cryptopone holmgreni" ~ "Wadeura holmgreni",
      field_3 %in% "Gnamptogenys rastrata" ~ "Poneracantha rastrata",
      field_3 %in% "Heteroponera microps" ~ "Bazboltonia microps",
      field_3 %in% "Labidus mars" ~ "Neivamyrmex mars",
      field_3 %in% "Gnamptogenys striatula" ~ "Holcoponera striatula",
      field_3 %in% "Gnamptogenys minuta" ~ "Alfaria minuta",
      field_3 %in% "Gnamptogenys rastrata" ~ "Poneracantha rastrata",
      field_3 %in% "Gnamptogenys piei" ~ "Alfaria piei",
      field_3 %in% "Gnamptogenys moelleri" ~ "Holcoponera moelleri",
      field_3 %in% "Acromyrmex subterraneus molestans" ~ "Acromyrmex molestans",
      field_3 %in% "Acromyrmex subterraneus brunneus" ~ "Acromyrmex brunneus",
      field_3 %in% "Gnamptogenys mediatrix" ~ "Poneracantha mediatrix",
      field_3 %in% "Gnamptogenys menozzii" ~ "Poneracantha menozzii",
      field_3 %in% "Gnamptogenys reichenspergeri" ~ "Typhlomyrmex reichenspergeri",
      TRUE ~ field_3
    )
  ) |> 
  
  #dizendo se está em UC ou não
  
  dplyr::mutate(
    EM_UC = if_else(
      is.na(uc_id),
      "Não",
      "Sim"
    )
  ) |>
  
  dplyr::mutate( 
    cob_1 = dplyr::case_when(
      cob_1 %in% "3" ~ "Formação Florestal",
      cob_1 %in% "11" ~ "Campo Alagado e Área Pantanosa",
      cob_1 %in% "12" ~ "Formação Campestre",
      cob_1 %in% "15" ~ "Pastagem",
      cob_1 %in% "21" ~ "Mosaico de usos",
      cob_1 %in% "24" ~ "Área Urbanizada",
      cob_1 %in% "29" ~ "Afloramento Rochoso",
      cob_1 %in% "33" ~ "Rio, Lago e Oceano",
      cob_1 %in% "49" ~ "Restinga Arbórea",
      TRUE ~ cob_1
    )
  ) |> 
  
  dplyr::mutate(
    classificacaoFormiga = dplyr::case_when( # quando o ScientificName nos 'dados' estiver no EspecialistaAreaAberta$ ScientificName, é preenchido, na coluna ClassificacaoFormiga, 'Especialista de area Aberta' 
      field_3 %in% EspecialistaAreaAberta$`Scientific Name` ~ "Especialista de Área Aberta",
      field_3 %in% EspecialistaFloresta$`Scientific Name` ~"Especialista de Floresta",
      field_3 %in% Generalista$`Scientific Name`~"Generalista",
      field_3 %in% SemClassificacao$`Scientific Name`~"Sem Classificação",
      TRUE ~ NA_character_
    )
  )

write_xlsx(GABI, "PlanilhaFinal.xlsx")

