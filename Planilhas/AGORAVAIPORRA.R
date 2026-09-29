library(readxl)
library(openxlsx)
library(dplyr)
library(xlsx)
library(rio)
library(tidyr)
library(writexl)
library(stringr)
library(tibble)
library(forcats)
library(scales)
library(ggplot2)
library(iNEXT)




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

DADOS <- read_excel("PlanilhaFinal.xlsx")

#CRIANDO A COLUNA GENERO
DADOS <- DADOS %>%
  mutate(Genero = word(scientificName, 1))

#CRIANDO A COLUNA EPITETO 

DADOS <- DADOS %>%
  mutate(Epiteto = word(scientificName, 2))

#adicionando a coluna Subfamilia

DADOS <- DADOS %>%
dplyr::mutate (
  Subfamilia = dplyr::case_when (
    Genero %in% c("Acanthognathus", "Acromyrmex", "Apterostigma", "Atta", "Basiceros",
                  "Cardiocondyla", "Carebara", "Cephalotes", "Crematogaster", "Cyphomyrmex",
                  "Eurhopalothrix", "Hylomyrma", "Lachnomyrmex", "Megalomyrmex", "Monomorium",
                  "Mycetarotes", "Mycetomoellerius", "Mycetophylax", "Mycocepurus",
                  "Myrmicocrypta", "Nesomyrmex", "Octostruma", "Oxyepoecus",
                  "Paratrachymyrmex", "Pheidole", "Pogonomyrmex", "Procryptocerus", "Rogeria",
                  "Sericomyrmex", "Solenopsis", "Strumigenys", "Wasmannia") ~ "Myrmicinae",
  
  Genero %in% c("Anochetus", "Centromyrmex", "Dinoponera", "Hypoponera", "Leptogenys",
                "Mayaponera", "Neoponera", "Odontomachus", "Pachycondyla", "Pseudoponera",
                "Rasopone", "Thaumatomyrmex", "Wadeura") ~ "Ponerinae",
  
  Genero %in% c("Acanthoponera", "Alfaria", "Ectatomma", "Gnamptogenys", "Heteroponera",
                "Holcoponera", "Poneracantha", "Typhlomyrmex") ~ "Ectatomminae",
  
  Genero %in% c("Acanthostichus", "Cylindromyrmex", "Eciton", "Labidus", "Neivamyrmex",
                "Neocerapachys", "Nomamyrmex") ~ "Dorylinae",
  
  Genero %in% c("Acropyga", "Brachymyrmex", "Camponotus", "Myrmelachista",
                "Nylanderia") ~ "Formicinae",
  
  Genero %in% c("Azteca", "Dolichoderus", "Dorymyrmex", "Linepithema",
                "Tapinoma") ~ "Dolichoderinae",
  
  Genero %in% c("Discothyrea", "Proceratium") ~ "Proceratiinae",
  
  Genero %in% c("Fulakora", "Prionopelta") ~ "Amblyoponinae",
  
  Genero == "Pseudomyrmex" ~ "Pseudomyrmecinae",
  
  TRUE ~ NA_character_
  )
)

#CONFERINDO 

nrow(DADOS)                       # deve continuar 3118
sum(is.na(DADOS$Subfamilia))      # deve dar 0
count(DADOS, Subfamilia, sort = TRUE)
  
#SALVANDO A NOVA PLANILHA

write_xlsx(DADOS, "DADOSFINAL.xlsx")

#fazendo os gráficos

theme_set(theme_classic(base_size = 32))

g1 <- ggplot(regioes, aes(x = valor, y = regiao_curta)) +
  geom_col(fill = "#114F11", width = 0.7) +
  geom_text(aes(label = number(valor, big.mark = ".")), hjust = -0.15, size = 9) +
  facet_wrap(~ medida, scales = "free_x",
             labeller = as_labeller(c(Registros = "Registros", Especies = "Espécies"))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 32) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.spacing.x = unit(2.5, "lines"),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 10))

g1


cobertura <- DADOS %>%
  mutate(cobertura = fct_lump_prop(coberturaSolo, prop = 0.01, other_level = "Outras classes")) %>%
  count(cobertura) %>%
  mutate(pct = n / sum(n),
         cobertura = fct_reorder(cobertura, pct))

g2 <- ggplot(cobertura, aes(x = pct, y = cobertura)) +
  geom_col(fill = "#366899", width = 0.7) +
  geom_text(aes(label = percent(pct, accuracy = 0.1, decimal.mark = ",")),
            hjust = -0.15, size = 7) +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  labs(x = "Porcentagem dos registros", y = NULL)

g2

niveis_hab <- c(
  "Especialista de Floresta",
  "Generalista",
  "Especialista de Área Aberta",
  "Sem Classificação"
)

habitat <- DADOS %>%
  filter(!is.na(regiao_curta)) %>%
  group_by(regiao_curta) %>%
  mutate(n_reg = n()) %>%
  ungroup() %>%
  mutate(
    rotulo = fct_reorder(
      paste0(
        regiao_curta,
        "\n(n = ",
        number(n_reg, big.mark = "."),
        ")"
      ),
      n_reg
    ),
    classificacaoFormiga = factor(
      classificacaoFormiga,
      levels = niveis_hab
    )
  )

niveis_hab <- c(
  "Especialista de Floresta",
  "Generalista",
  "Especialista de Área Aberta",
  "Sem Classificação"
)

habitat <- DADOS %>%
  filter(!is.na(regiao_curta)) %>%
  group_by(regiao_curta) %>%
  mutate(n_reg = n()) %>%
  ungroup() %>%
  mutate(
    rotulo = fct_reorder(
      paste0(
        regiao_curta,
        "\n(n = ",
        number(n_reg, big.mark = "."),
        ")"
      ),
      n_reg
    ),
    classificacaoFormiga = factor(
      classificacaoFormiga,
      levels = niveis_hab
    )
  )

g3 <- ggplot(
  habitat,
  aes(
    y = rotulo,
    fill = classificacaoFormiga
  )
) +
  geom_bar(
    position = "fill",
    width = 0.7
  ) +
  scale_x_continuous(
    labels = percent,
    breaks = seq(0, 1, 0.1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(
    values = c(
      "Especialista de Floresta" = "#2E7D32",
      "Generalista" = "#F9A825",
      "Especialista de Área Aberta" = "#8D6E63",
      "Sem Classificação" = "grey70"
    )
  ) +
  labs(
    x = "Porcentagem dos registros",
    y = NULL,
    fill = NULL
  ) +
  theme_classic(
    base_size = 18,
    base_family = "Times New Roman"
  ) +
  theme(
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    
    legend.key.size = unit(0.7, "cm"),
    legend.spacing.x = unit(0.4, "cm"),
    
    plot.margin = margin(10, 20, 10, 20)
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

g3

