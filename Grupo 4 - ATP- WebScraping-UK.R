library(rvest)
library(car)
library(dplyr)
library(stringr)
library(ggplot2)
library(knitr)
library(gridExtra)
library(caTools)
library(nnet)
library(caret)
library(pROC)
library(rpart.plot)
library(randomForest)

#Primeiro passo: criar o link de todos os anos em que existem arquivos de resultados (DE 1915 a 2023) e extrair o ficheiro html
base_link = "https://www.atptour.com/en/scores/results-archive"

all_tournament_links = c()

country = "Great Britain"
country_cities = c("London", "Liverpool", "Manchester", "Wolverhampton", "Birmingham", "Leeds", "Sheffield", "Bristol", "Leicester", "Bradford", "Coventry", "Kingston", "Plymouth", "Stoke-on-trent", "Derby", "Nottingham", "Southampton", "Portsmouth")

for (year in 1915:2023){
  Sys.sleep(runif(1, min = 0.66, max = 1.3))
  year_links = paste0(base_link,"?year=",year, sep="")
  year_pages = read_html(year_links)
  
  tournament_url_xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "button-border", " " ))]'
  tournament_links <- year_pages %>% html_nodes(xpath = tournament_url_xpath) %>% html_attr("href") %>% paste("https://www.atptour.com", ., sep="")
  
  
  #O código anterior devolve-me todos os torneios que já aconteceram,
  #mas também os que estão a decorrer no momento ( e eu não estou interessado nesses).
  #O código que se segue serve para filtrar os links dos torneios que não estão a decorrer.
  tournament_links = tournament_links[!grepl("/current/", tournament_links)]
  
  all_tournament_links = c(all_tournament_links, tournament_links)
  #".button-border" serve para fazermos scraping da página de resultados do torneio,
  #ao invés de nos levar para a página principal do torneio (que é igual para todos os anos).
  #Ao realizarmos o scraping desta forma, estamos a excluir desde já os torneios que não têm
  #histórico de resultados (e que por isso também não nos são úteis).
}


all_tournament_links

#Segundo passo: dos links de todos os torneios de todos os anos, extrair a sua localização e filtrar apenas pelo país UK

# Create empty vectors to store data
all_tournament_location <- character(0)
all_tournament_date <- character(0)
all_tournament_name <- character(0)
all_tournament_ground <- character(0)
all_tournament_prize_money <- character(0)
all_tournament_rounds <- character(0)

for (i in 1:length(all_tournament_links)) {
  Sys.sleep(runif(1, min = 0.66, max = 1.3))
  tournament_links <- read_html(all_tournament_links[i])
  
  # Corrected the code to extract tournament_location
  tournament_location <- tournament_links %>% 
    html_nodes("#lastEventsPlayedStandAloneNoSlider .tourney-location") %>% 
    html_text2() %>% 
    trimws()
  
  all_tournament_location <- c(all_tournament_location,
                               ifelse(length(tournament_location) == 0, "NA", tournament_location))
  
  # Corrected the code to extract tournament_date
  tournament_date <- tournament_links %>% 
    html_nodes(".tourney-dates") %>% 
    html_text2() %>% 
    trimws()
  
  all_tournament_date <- c(all_tournament_date, 
                           ifelse(length(tournament_date) == 0, "NA", tournament_date))
  
  # Corrected the code to extract tournament_name
  tournament_name <- tournament_links %>% 
    html_nodes("#lastEventsPlayedStandAloneNoSlider .tourney-title") %>% 
    html_text2() %>% 
    trimws()
  
  all_tournament_name <- c(all_tournament_name, 
                           ifelse(length(tournament_name) == 0, "NA", tournament_name))
  
  # Corrected the code to extract tournament_ground
  tournament_ground <- tournament_links %>% 
    html_nodes(".tourney-details:nth-child(2) .item-value") %>% 
    html_text2() %>% 
    trimws()
  
  all_tournament_ground <- c(all_tournament_ground, 
                             ifelse(length(tournament_ground) == 0, "NA", tournament_ground))
  
  # Corrected the code to extract tournament_prize_money
  tournament_prize_money <- tournament_links %>% 
    html_nodes(".prize-money .item-value") %>% 
    html_text2() %>% 
    trimws()
  
  all_tournament_prize_money <- c(all_tournament_prize_money, 
                                  ifelse(length(tournament_prize_money) == 0, "NA", tournament_prize_money))
  
  # Corrected the code to extract tournament_rounds
  tournament_rounds <- tournament_links %>% 
    html_nodes(".tourney-details:nth-child(1) .item-details") %>% 
    html_text2() %>% 
    trimws()
  
  all_tournament_rounds <- c(all_tournament_rounds,
                             ifelse(length(tournament_rounds) == 0, "NA", tournament_rounds))
  
  
  
}

all_tournament_location

#Algumas das localizações estão mal identificadas no site (exemplo, "London" ao invés de "London, Great Britain")
#Os seguintes comandos servem para corrigir estes erros para todas as cidades de UK

for(city in country_cities){
  pattern = paste0("^", city, ",?$")
  replacement = paste0(city, ", Great Britain")
  all_tournament_location = gsub(pattern, replacement,trimws(all_tournament_location))
}

all_tournament_location

#Isolar o país de cada torneio
all_tournament_country = gsub (".*\\, ", "", all_tournament_location)
all_tournament_country
#Isolar o ano de cada torneio
all_tournament_years = gsub("\\..*", "", all_tournament_date)
all_tournament_years

df <- data.frame(
  all_tournament_location,
  all_tournament_country,
  all_tournament_date,
  all_tournament_years,
  all_tournament_name, 
  all_tournament_ground,
  all_tournament_prize_money,
  all_tournament_rounds,
  all_tournament_links
)


#Para não termos que estar sempre a extrair esta informação do site enquanto fazemos o trabalho, passamos já os dados sobre os torneios para um ficheiro csv

#write.csv2(df, paste(getwd(), "All_Tournament_Info.csv", sep="/"), row.names = FALSE)

#Criar um dataframe apenas para os torneios de Great Britain
df_UK = df[df$all_tournament_country == "Great Britain", ]

#Fazemos o mesmo para este data frame

#write.csv2(df_UK, paste(getwd(), "UK_Tournament_Info.csv", sep="/"), row.names = FALSE)

###############################################################################################
#Correr no caso não ter os dados no RStudio
#df = read.csv2('C:/Users/afare/OneDrive/Documentos/All_Tournament_Info.csv')
#df_UK = read.csv2('C:/Users/afare/OneDrive/Documentos/UK_Tournament_Info.csv')
###############################################################################################
###(PARTE DOS TORNEIOS)###
#Criar a variável UK_tournament_links, que é composta pelos links dos torneios realizados no UK. Basta relacionar os nosso dataframe de UK com a variável tournament_links.
UK_tournament_links = df_UK$all_tournament_links
UK_tournament_links

#Para extrair o link de todos os jogos de cada torneio
all_game_links <- character(0)
for (i in 1:length(df_UK$all_tournament_links)) {
  Sys.sleep(runif(1, min = 0.54, max = 1.18))
  game_xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "day-table-score", " " ))]//a'
  game_links <- tournament_links %>% 
    html_nodes(xpath = game_xpath) %>% html_attr("href") %>% 
    trimws()
  
  all_game_links = c(all_game_links, game_links)
}
#
year_links = paste0(base_link,"?year=",year, sep="")
year_pages = read_html(year_links)

tournament_url_xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "button-border", " " ))]'
tournament_links <- year_pages %>% html_nodes(xpath = tournament_url_xpath) %>% html_attr("href") %>% paste("https://www.atptour.com", ., sep="")
#
all_game_links = all_game_links[!is.na(all_game_links)]
all_game_links = gsub("stats-centre", "match-stats", all_game_links)
all_game_links = paste0("https://www.atptour.com",all_game_links)
all_game_links

all_game_id = toupper(sub(".*/([^/]+)$", "\\1", all_game_links))
all_tournament_id = sub(".*/([^/]+)/(?:[^/]+)$", "\\1", all_game_links)

###############################################################################################
###(PARTE DOS JOGOS)###
all_game_phase = c() 
all_player1_link = c()
all_player2_link = c()
all_matches_score = c()
tournaments_link = c()

for (link in UK_tournament_links) {
  Sys.sleep(runif(1, min = 0.59, max = 1.07))
  
  game_page = read_html(link)
  all_games_table = html_nodes(game_page, xpath='//table[@class="day-table"]/*')
  
  for (game in all_games_table) {
    nodes = html_nodes(game, xpath='tr/th')
    if (length(nodes) == 1) {all_game_phase_value = nodes %>% html_text2()}
    else {
      for (i in html_nodes(game, xpath='tr')) {
        players = html_nodes(i, xpath='td[@class="day-table-name"]/a')
        all_player1_link_value = players[1] %>% html_attr("href")
        if (length(players)==1) {
          all_player2_link_value=""
          all_matches_score_value=""} 
        else {
          all_player2_link_value = players[2] %>% html_attr("href")
          all_matches_score_value <- html_nodes(i, xpath='td[@class="day-table-score"]/a') %>% html_text() %>% str_replace_all('\r\n', ' ') %>% str_squish() 
        }
        all_game_phase = c(all_game_phase, all_game_phase_value) 
        all_player1_link = c(all_player1_link, all_player1_link_value)
        all_player2_link = c(all_player2_link, all_player2_link_value)
        all_matches_score = c(all_matches_score, all_matches_score_value)
        tournaments_link = c(tournaments_link, link)
      }
    }
  }
}


all_sets_number = str_count(all_matches_score, "\\s")+1
year_id = sub(".*/([^/]+)/(?:[^/]+)$", "\\1", tournaments_link)
tournament_id = sub(".*/([^/]+)/(?:[^/]+/[^/]+)$", "\\1", tournaments_link)

match_df = data.frame(tournament_id, year_id, all_game_phase, all_player1_link, all_player2_link, all_matches_score, all_sets_number)

#write.csv(match_df,"all_uk_games.csv")

##############################################################################################
###(PARTE DOS JOGADORES)###
#Correr no caso não ter os dados no RStudio
#match_df = read.csv2('C:\Users\afare\OneDrive\Documentos\all_uk_games.csv')

all_player_link = c(all_player1_link, all_player2_link) %>% unique()
all_player_link = all_player_link[all_player_link != ""]
all_player_link = paste0("https://www.atptour.com",all_player_link)
all_player_link

all_player_id = c()
all_player_name = c()
all_player_height = c()
all_player_weight = c()
all_player_born_date = c()
all_player_birth_place = c()
all_player_country_code = c()
all_player_strong_hand = c()

suppressWarnings({
  # Dentro do loop
  for (link in all_player_link) {
    Sys.sleep(runif(1, min = 0.8, max = 1.2))
    
    # Tentar acessar a página
    player_page <- tryCatch(
      read_html(link),
      error = function(e) {
        if (grepl("HTTP error 404", conditionMessage(e))) {
          warning(paste("Erro 404 ao acessar a página:", link))
        } else {
          warning(paste("Erro ao acessar a página:", link))
        }
        return(NULL) #Vai retornar NULL para indicar que ocorreu um erro
      }
    )
    
    # Verificar se a página foi carregada corretamente
    if (!is.null(player_page)) {
      
      
      # Extração de dados  
      all_player_id = c(all_player_id, sub(".*/([^/]+)/(?:[^/]+)$", "\\1", link))
      
      player_name_value <- player_page %>% html_nodes(".page-title") %>% html_text2() %>% trimws()
      all_player_name = c(all_player_name, ifelse(length(player_name_value) == 0, "", player_name_value))
      
      player_height_value <- player_page %>% html_nodes(".table-height-cm-wrapper") %>% html_text2() %>% trimws()
      all_player_height = c(all_player_height, ifelse(length(player_height_value) == 0, "", player_height_value))
      
      player_weight_value <- player_page %>% html_nodes(".table-weight-kg-wrapper") %>% html_text2() %>% trimws()
      all_player_weight = c(all_player_weight, ifelse(length(player_weight_value) == 0, "", player_weight_value))
      
      player_birthday_value <- player_page %>% html_nodes(".table-birthday") %>% html_text2() %>% trimws()
      all_player_born_date = c(all_player_born_date, ifelse(length(player_birthday_value) == 0, "", player_birthday_value))
      
      player_birthplace_value <- player_page %>% html_nodes("td:nth-child(1) .table-value") %>% html_text2() %>% trimws()
      all_player_birth_place = c(all_player_birth_place, ifelse(length(player_birthplace_value) == 0, "", player_birthplace_value))
      
      player_country_code_value <- player_page %>% html_nodes(".player-flag-code") %>% html_text2() %>% trimws()
      all_player_country_code = c(all_player_country_code, ifelse(length(player_country_code_value) == 0, "", player_country_code_value))
      
      player_hand_value <- player_page %>% html_nodes("td:nth-child(2) .table-value") %>% html_text2() %>% trimws()
      all_player_strong_hand = c(all_player_strong_hand, ifelse(length(player_hand_value) == 0, "", player_hand_value))
      
    } else {
      # Página não encontrada (erro 404), continuar para a próxima iteração
      next
    }
  }
})

#As variáveis já estão a sair limpas
#clear_player_name = gsub("\\s+|\\r","", all_player_name)
#clear_player_height = gsub("\\s+|\\r","", all_player_height)
#clear_player_weight = gsub("\\s+|\\r","", all_player_weight)
#clear_player_born_date = gsub("\\s+|\\r","", all_player_born_date)
#clear_player_birth_place = gsub("\\s+|\\r","", all_player_birth_place)
#clear_player_country_code = gsub("\\s+|\\r","", all_player_country_code)
#clear_player_strong_hand = gsub("\\s+|\\r","", all_player_strong_hand)

player_info = data.frame(
  player_id = all_player_id,
  player_name = all_player_name,
  player_height = all_player_height,
  player_weight = all_player_weight,
  player_born_date = all_player_born_date,
  player_birth_place = all_player_birth_place,
  player_country_code = all_player_country_code,
  player_strong_hand = all_player_strong_hand
)

#write.csv(player_info,"player_info.csv")


#########################################(Preparação dos dados)############################################

##############################
#Dados sobre os jogadores:
##############################

player_info_edit = data.frame(player_info)

#1 Height
class(all_player_height) #character
#Para podermos fazer análises estatísticas numéricas com a altura do jogador, teremos que remover os parênteses e "cm".
player_info_edit$player_height = as.numeric(gsub("[^0-9.]", "", player_info$player_height))
player_info_edit$player_height[player_info_edit$player_height == 0] = NA

#2 Weight 
class(all_player_weight) #character
#Aplicamos o mesmo processo com o peso do jogador
player_info_edit$player_weight = as.numeric(gsub("[^0-9.]", "", player_info_edit$player_weight))
player_info_edit$player_weight[player_info_edit$player_weight == 0] = NA

#3 Born date
#Para a nossa análise, é apenas importante saber o ano de nascimento como variável numérica
player_info_edit$player_born_date <- as.numeric(gsub("[^0-9]", "", substr(player_info_edit$player_born_date, 2, 5)))
colnames(player_info_edit)[colnames(player_info_edit) == "player_born_date"] = "player_born_year" #Mudamos o nosso para ser mais coerente com a informação

#4 Player birth place
#O local de nascimento do jogador não nos acrescenta nada para a análise a mais do que a nacionalidade, por isso decidimos removê-la
player_info_edit = subset(player_info_edit, select = -player_birth_place)

#5 Player country code
#O nosso objetivo é passar o código para o nome completo do país. Para isso iremos fazer websracping 
#De uma página que tenha essa informação, passar para um dataframe e só depois fazer a alteração.

country_names = c()
country_codes = c()
cc_site = "https://countrycode.org/"

cc_page = read_html(cc_site)

country_names = cc_page %>% html_nodes(".main-table a") %>% html_text2()
country_codes = cc_page %>% html_nodes("td:nth-child(3)") %>% html_text2()
clean_country_codes = gsub(".*\\s*/\\s*([A-Za-z]{3}).*", "\\1", country_codes)

countries_info = data.frame(country_name = country_names, country_code = clean_country_codes)


#Executamos um match para adicionarmos a coluna com o nome do país, tendo como referência a coluna do código do país em cada tabela
player_info_edit$player_country = countries_info$country_name[match(player_info_edit$player_country_code, countries_info$country_code)]

#O próximo código serve para reorganizar as colunas e eliminar as que não desejamos (neste caso a do player_country_code)
player_info_edit = player_info_edit[, c("player_id", "player_name", "player_born_year", "player_country", "player_height", "player_weight", "player_strong_hand")]

#6 Player strong hand
#A informação sobre a mão forte do jogador é relativa ao forehand e ao backhand, respetivamente
#Vamos por isso dividir esta informação em 2 colunas, a "player_strong_forehand" e "player_strong_backhand"

player_info_split = strsplit(player_info$player_strong_hand, ", ", fixed = TRUE)

#Criação das novas colunas
player_info_edit$player_strong_forehand = sapply(player_info_split, `[`, 1)
player_info_edit$player_strong_backhand = sapply(player_info_split, `[`, 2)

#Remoção da coluna original
player_info_edit = player_info_edit[, !(names(player_info_edit) %in% "player_strong_hand")]

write.csv(player_info_edit, "players_dt_preparation.csv")

##############################
#Dados sobre os torneios:
##############################
#Para preservamos a nossa informação, criamos um df igual ao df_UK para editarmos os nossos dados.
df_UK_edit = df_UK

#1 Criação da coluna tournament_id
df_UK_edit <- df_UK_edit %>% mutate(tournament_id = as.numeric(str_extract(all_tournament_links, "\\d+")))

#Usamos o próximo código para reorganizar a tabela, de forma a que o nosso tournament_id seja a primeira coluna.
df_UK_edit <- df_UK_edit %>% select(tournament_id, everything())

#2 All_Tournament_Location
#como não vamos fazer estatísticas por cidades dentro de UK, basta-nos apenas ter a coluna all_tournament_country.
df_UK_edit <- df_UK_edit %>% select(-all_tournament_location)

#3 all_tournament_date
#Alteração desta coluna, de forma a ficarmos apenas com o ano do torneio
df_UK_edit$all_tournament_date <- sub("(\\d{4}).*", "\\1", df_UK_edit$all_tournament_date)
#O padrão "(\\d{4}).*" corresponde aos quatro primeiros dígitos (ano) e qualquer coisa que venha depois. 
#A substituição \\1 mantém apenas os quatro primeiros dígitos, removendo o restante.
#Reorganização do dataframe, de forma a que a data apareça na segunda posição
df_UK_edit <- df_UK_edit %>% select(tournament_id, all_tournament_date, everything())

#4 all_tournament_prize_money
#Para nos facilitar a utilização desta variável como forma de comparação entre torneios, vamos colocar todos os valores na mesma moeda (euro)
#Assumimos que a GBP/EUR = 1.15 e DOL/EUR = 0.92 (valores aproximados, retirados do Banco de Portugal a 23/11/2023)
GBP_EUR = 1.15
USD_EUR = 0.92

# Criar uma nova coluna numérica para armazenar o prêmio transformado
df_UK_edit <- df_UK_edit %>%
  mutate(prize_money_numeric = as.numeric(gsub("[^0-9.]", "", all_tournament_prize_money)),
         prize_money_numeric = ifelse(is.na(prize_money_numeric) | prize_money_numeric == 0, NA, prize_money_numeric),
         prize_money_numeric = case_when(
           grepl("£", all_tournament_prize_money) ~ prize_money_numeric * GBP_EUR,
           grepl("\\$", all_tournament_prize_money) ~ prize_money_numeric * USD_EUR,
           TRUE ~ prize_money_numeric
         ))

# Remover a coluna original e renomear a nova coluna como "all_tournament_prize_money"
df_UK_edit <- df_UK_edit %>%
  select(-all_tournament_prize_money) %>%
  rename(all_tournament_prize_money = prize_money_numeric)

#Reorganizar o dataframe, de forma a que a coluna all_tournament_prize_money fique depois de all_tournament_ground
df_UK_edit = df_UK_edit %>% select(tournament_id, all_tournament_date,all_tournament_country,all_tournament_name, all_tournament_ground, all_tournament_prize_money, everything())

#5 Como vamos fazer a análise apenas para os torneios de jogos singles, podemos remover o resto da informação desta coluna (contudo mantemos a informação de tipo character, já que é informação )
df_UK_edit$all_tournament_rounds <- as.numeric(gsub("SGL\\s*(\\d+).*", "\\1", df_UK_edit$all_tournament_rounds))
#df_UK_edit$all_tournament_rounds <- df_UK$all_tournament_rounds

#write.csv(df_UK_edit, "df_UK.csv")
##############################
#Dados sobre os jogos:
##############################
#Para começar, criarmos um dataframe igual ao match_df para o caso de nos enganarmos com algo
match_df_edit = match_df

#1 Year ID
match_df_edit$year_id = as.numeric(match_df_edit$year_id)
#class(match_df_edit$year_id) mostra-nos que year_id é agora "numeric"

#2 Players links
#Para termos o link completo das páginas dos jogadores, temos que adicionar "https://www.atptour.com" a cada linha
match_df_edit$all_player1_link = paste0("https://www.atptour.com", match_df_edit$all_player1_link)
match_df_edit$all_player2_link = paste0("https://www.atptour.com", match_df_edit$all_player2_link)

#3 Players ID
#Queremos 2 colunas que disponham apenas a parte do link que remete para o id do jogador.
match_df_edit$player1_id <- sub(".*/players/[^/]+/([^/]+)/overview.*", "\\1", match_df_edit$all_player1_link)
match_df_edit$player2_id <- sub(".*/players/[^/]+/([^/]+)/overview.*", "\\1", match_df_edit$all_player2_link)

#Como queremos que cada coluna de id apareça antes de cada coluna de links, temos que reorganizar a tabela
match_df_edit <- match_df_edit[, c("tournament_id", "year_id", "all_game_phase", "player1_id", "all_player1_link", "player2_id", "all_player2_link", "all_matches_score", "all_sets_number")]

#########################################################(Adicionar informação dos jogadores ao match_df_edit)#########################################################

#Vamos acrescentar a informação do dataframe "player_info_edit" ao dataframe "match_df_edit"

# Renomear as colunas no dataframe player_info_edit para evitar colisões
colnames(player_info_edit) = paste0("player1_", colnames(player_info_edit))

# Juntar os dataframes com base nos IDs dos jogadores para o jogador 1
match_df_edit = merge(match_df_edit, player_info_edit, by.x = "player1_id", by.y = "player1_player_id", all.x = TRUE)

# Renomear as colunas novamente para o jogador 2
colnames(player_info_edit) = sub("player1_", "player2_", colnames(player_info_edit))

# Juntar novamente os dataframes com base nos IDs dos jogadores para o jogador 2
match_df_edit = merge(match_df_edit, player_info_edit, by.x = "player2_id", by.y = "player2_player_id", all.x = TRUE)

#Para terminar, reorganizamos a informação para facilitar a leitura dos dados
match_df = match_df_edit[, c("tournament_id", "year_id", "all_game_phase", "player1_id","player1_player_name","player1_player_born_year","player1_player_country","player1_player_height", "player1_player_weight","player1_player_strong_forehand","player1_player_strong_backhand","all_player1_link","player2_id","player2_player_name","player2_player_born_year","player2_player_country","player2_player_height", "player2_player_weight","player2_player_strong_forehand","player2_player_strong_backhand","all_player2_link", "all_matches_score", "all_sets_number")]

#########################################################(Adicionar informação dos torneios ao match_df)#########################################################
#Para começar, adicionamos todas as colunas da tabela df_UK_edit à tabela match_df, o tornament_id e year_id como valores comuns entre as linhas de ambos os dataframes 
match_df = merge(match_df, df_UK_edit, by.x = c("tournament_id", "year_id"), by.y = c("tournament_id", "all_tournament_date"), all.x = TRUE)

#Como não precisamos do link dos torneios de da sua localização (porque já sabemos que vão ser em UK), vamos removê-los do nosso match_df
match_df = match_df[, !colnames(match_df) %in% c("all_tournament_links", "all_tournament_country")]

#Reorganizamos a nossa informação para que a informação sobre os torneios apareça no início. Também já não precisamos dos ids dos jogadores, por isso vamos removê-los
match_df = match_df[, c("tournament_id", "year_id","all_tournament_name","all_tournament_ground","all_tournament_prize_money","all_tournament_rounds","all_game_phase","player1_player_name","player1_player_born_year","player1_player_country","player1_player_height", "player1_player_weight","player1_player_strong_forehand","player1_player_strong_backhand","all_player1_link","player2_player_name","player2_player_born_year","player2_player_country","player2_player_height", "player2_player_weight","player2_player_strong_forehand","player2_player_strong_backhand","all_player2_link", "all_matches_score", "all_sets_number")]

##############################################################(Divisão de data frames)##############################################################
#O nosso objetivo é prever o número de sets de um jogo de ténis. 
#Como os grandslams têm formatos (melhor de 5) diferentes dos restantes (melhor de 3), teremos que realizar as análises separadamente,
#De forma a não enviesar os nossos modelos
#Sabemos que o único grandslam que existe em UK é o Wimbledon, que tem tournament_id = 540

# Filtrar os jogos de Wimbledon
match_gs_df <- match_df[match_df$tournament_id == 540, ]

# Remover esses jogos do dataframe original
match_df <- match_df[match_df$tournament_id != 540, ]

#Antes, no match_df, tínhamos 22670 obs. Agora temos 8561 obs. no match_df e 14109 obs. no match_gs_df
#Exportação dos 2 dataframes para ficheiros csv
#write.csv(match_df, "UK_df.csv")
#write.csv(match_gs_df, "UK_GS_df.csv")

###########################################################################################################################################
##############################################################(Data cleaning)##############################################################
###########################################################################################################################################

#Para usar os cvs:

#match_gs_caminho = "C:/Users/afare/OneDrive/Documentos/UK_GS_df.csv" #alterar o caminho conforme o local do ficheiro em cada computador
#match_caminho = "C:/Users/afare/OneDrive/Documentos/UK_df.csv" #alterar o caminho conforme o local do ficheiro em cada computador
#match_gs_df = read.csv(match_gs_caminho)
#match_df = read.csv(match_caminho)

#Remover a primeira coluna de cada dataframe, que não é necessária (era a posição de cada linha no dataframe conjunto)
#match_gs_df = match_gs_df[, -1]
#match_df = match_df[, -1]
#caso estivermos a correr o código todo de uma vez, não é necessário realizar este passo, uma vez que este campo "X" não existe

#Análise das variáveis - Torneios Grandslam
summary(match_gs_df)
str(match_gs_df)

#Análise das variáveis -Torneios não Grandslam
summary(match_df)
str(match_gs_df)

############################################################################################################################################################################
colnames(match_df) <- c("Tournament_id", "Year", "Tournament_name", "Ground", "Prize", "Round", "Game_phase", "Winner_name", "Winner_born", "Winner_country", "Winner_height", "Winner_weight", "Winner_forehand", "Winner_backhand", "Winner_link", "Loser_name", "Loser_born", "Loser_country", "Loser_height", "Loser_weight", "Loser_forehand", "Loser_backhand", "Loser_link", "Score", "Sets")
colnames(match_gs_df) <- c("Tournament_id", "Year", "Tournament_name", "Ground", "Prize", "Round", "Game_phase", "Winner_name", "Winner_born", "Winner_country", "Winner_height", "Winner_weight", "Winner_forehand", "Winner_backhand", "Winner_link", "Loser_name", "Loser_born", "Loser_country", "Loser_height", "Loser_weight", "Loser_forehand", "Loser_backhand", "Loser_link", "Score", "Sets")

#Conversão para factor das variáveis das 2 dataframes
#Conveter do "Ground"
match_df$Ground = factor(match_df$Ground)
match_gs_df$Ground = factor(match_gs_df$Ground)
#Converter do "Tournament_name"
match_df$Tournament_name = factor(match_df$Tournament_name)
match_gs_df$Tournament_name = factor(match_gs_df$Tournament_name)
#Conversão de "Game_phase"
match_df$Game_phase = factor(match_df$Game_phase)
match_gs_df$Game_phase = factor(match_gs_df$Game_phase)
#conversão do Winner_name e Loser_name
match_df$Winner_name = factor(match_df$Winner_name)
match_gs_df$Winner_name = factor(match_gs_df$Winner_name)
match_df$Loser_name = factor(match_df$Loser_name)
match_gs_df$Loser_name = factor(match_gs_df$Loser_name)
#conversão de player_country 1 e 2
match_df$Winner_country = factor(match_df$Winner_country)
match_gs_df$Winner_country = factor(match_gs_df$Winner_country)
match_df$Loser_country = factor(match_df$Loser_country)
match_gs_df$Loser_country = factor(match_gs_df$Loser_country)
#Conversão do "Forehand" e "Backhand" do player 1 e 2
match_df$Winner_forehand = factor(match_df$Winner_forehand)
match_gs_df$Winner_forehand = factor(match_gs_df$Winner_forehand)
match_df$Winner_backhand = factor(match_df$Winner_backhand)
match_gs_df$Winner_backhand = factor(match_gs_df$Winner_backhand)
match_df$Loser_forehand = factor(match_df$Loser_forehand)
match_gs_df$Loser_forehand = factor(match_gs_df$Loser_forehand)
match_df$Loser_backhand = factor(match_df$Loser_backhand)
match_gs_df$Loser_backhand = factor(match_gs_df$Loser_backhand)
#Conversao do "Score"
match_df$Score = factor(match_df$Score)
match_gs_df$Score = factor(match_gs_df$Score)


#Converter empty strings "" dos player1_player_name e player2_player_name em NA
match_df <- match_df %>% mutate(Winner_name = if_else(Winner_name == "", NA, Winner_name))
match_gs_df <- match_gs_df %>% mutate(Winner_name = if_else(Winner_name == "", NA, Winner_name))
match_df <- match_df %>% mutate(Loser_name = if_else(Loser_name == "", NA, Loser_name))
match_gs_df <- match_gs_df %>% mutate(Loser_name = if_else(Loser_name == "", NA, Loser_name))

#Converter empty strings "" dos Score em NA
match_df <- match_df %>% mutate(Score = if_else(Score == "", NA, Score))
match_gs_df <- match_gs_df %>% mutate(Score = if_else(Score == "", NA, Score))

#Ver a percentagem de NA's das dataframes
NA_match <- round(colSums(is.na(match_df)) *100/nrow(match_df),2)
NA_match[NA_match >0]
NA_gs_match <- round(colSums(is.na(match_gs_df)) *100/nrow(match_gs_df),2)
NA_gs_match[NA_gs_match >0]

#Nova coluna Tournament_city
match_df$Tournament_city = NA

match_df$Tournament_city[match_df$Tournament_name %in% c("Wembley-3", "Wembley", "Surbiton","Cinch Championships", "Nitto ATP Finals", "London WCT", "London Olympics", "London / Queen's Club", "London-4", "London-3", "London-2", "London-1", "London", "ATP Finals" )] <- "London"
match_df$Tournament_city[match_df$Tournament_name %in% c("Rothesay International", "Eastbourne" )] <- "Eastbourne"
match_df$Tournament_city[match_df$Tournament_name %in% c("Newport-1", "Newport" )] <- "Newport"
match_df$Tournament_city[match_df$Tournament_name %in% c("Bristol", "Bristol WCT" )] <- "Newport"
match_df$Tournament_city[match_df$Tournament_name %in% c("Birmingham WCT", "Birmingham-2", "Birmingham" )] <- "Birmingham"
match_df$Tournament_city[match_df$Tournament_name == "Nottingham"] <- "Nottingham"
match_df$Tournament_city[match_df$Tournament_name == "Leicester"] <- "Leicester"
match_df$Tournament_city[match_df$Tournament_name == "Hoylake"] <- "Hoylake"
match_df$Tournament_city[match_df$Tournament_name == "Bournemouth"] <- "Bournemouth"
match_df$Tournament_city[match_df$Tournament_name == "Beckenham"] <- "Beckenham"
match_df$Tournament_city[match_df$Tournament_name == "Manchester"] <- "Manchester"

#Grafico Torneios por cidade
match_df$Tournament_city <- factor(match_df$Tournament_city, levels = names(sort(table(match_df$Tournament_city), decreasing = TRUE)))

ggplot(match_df, aes(y = Tournament_city)) +
  geom_bar(fill = "#FFA07A", color = "#FFA07A") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  labs(
    title = "Número de jogos por cidade do Reino Unido",
    y = "Cidade", 
    x = "Número de Jogos"
  )

#Uniformizar nomes das rondas dos torneios
match_df$Game_phase <- gsub("Quarterfinals", "Quarter-Finals", match_df$Game_phase)
match_gs_df$Game_phase <- gsub("Quarterfinals", "Quarter-Finals", match_gs_df$Game_phase)
match_df$Game_phase <- gsub("Semifinals", "Semi-Finals", match_df$Game_phase)
match_gs_df$Game_phase <- gsub("Semifinals", "Semi-Finals", match_gs_df$Game_phase)
match_df$Game_phase <- gsub("Finals", "Final", match_df$Game_phase)
match_gs_df$Game_phase <- gsub("Finals", "Final", match_gs_df$Game_phase)

#Gráfico "Numero de jogos por fase do torneio regulares"
match_df$Game_phase <- factor(match_df$Game_phase, levels = names(sort(table(match_df$Game_phase), decreasing = TRUE)))

ggplot(match_df, aes(y = Game_phase)) +
  geom_bar(fill = "#FFA07A", color = "#FFA07A") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  labs(
    title = "'Match_df': Número de jogos por fase do torneio",
    y = "Fase", 
    x = "Número de Jogos"
  )
#Gráfico "Numero de jogos por fase do torneio Grand Slam"

match_gs_df$Game_phase <- factor(match_gs_df$Game_phase, levels = names(sort(table(match_gs_df$Game_phase), decreasing = TRUE)))

ggplot(match_gs_df, aes(y = Game_phase)) +
  geom_bar(fill = "#FFA07A", color = "#FFA07A") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  labs(
    title = "'Match_gs_df': Número de jogos por fase do torneio",
    y = "Fase", 
    x = "Número de Jogos"
  )
#Estatisticas da variável "Prize"
summary(match_df$Prize)
summary(match_gs_df$Prize)

match_df$Prize = as.numeric(match_df$Prize)

ggplot(match_df, aes(x = Prize/1000)) +
  geom_boxplot(color = "black", fill = "#FFA07A", width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(
    title = "match_df: Boxplot do prémio (Prize) em milhares de Euros"
  )


match_gs_df$Prize = as.numeric(match_gs_df$Prize)

ggplot(match_gs_df, aes(x = Prize/1000)) +
  geom_boxplot(color = "black", fill = "#FFA07A", width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  labs(title = "match_gs_df: Boxplot do prémio (Prize) em milhares de Euros")


#Variável Winner_name e Loser_name
#Tratamento dos valores "Unkown"
match_df <- match_df %>% mutate(Winner_name = if_else(Winner_name == "Unknown", NA, Winner_name))
match_df <- match_df %>% mutate(Winner_name = if_else(Winner_name == "Unknown Unknown", NA, Winner_name))
match_gs_df <- match_gs_df %>% mutate(Winner_name = if_else(Winner_name == "Unknown", NA, Winner_name))
match_gs_df <- match_gs_df %>% mutate(Winner_name = if_else(Winner_name == "Unknown Unknown", NA, Winner_name))
match_df <- match_df %>% mutate(Loser_name = if_else(Loser_name == "Unknown", NA, Loser_name))
match_df <- match_df %>% mutate(Loser_name = if_else(Loser_name == "Unknown Unknown", NA, Loser_name))
match_gs_df <- match_gs_df %>% mutate(Loser_name = if_else(Loser_name == "Unknown", NA, Loser_name))
match_gs_df <- match_gs_df %>% mutate(Loser_name = if_else(Loser_name == "Unknown Unknown", NA, Loser_name))

# percentagem % de NA values da variável Winner_born e Loser_born
round(sum(is.na(match_df$Winner_born)) * 100 / length(match_df$Winner_born), 2)
round(sum(is.na(match_df$Loser_born)) * 100 / length(match_df$Loser_born), 2)
round(sum(is.na(match_gs_df$Winner_born)) * 100 / length(match_gs_df$Winner_born), 2)
round(sum(is.na(match_gs_df$Loser_born)) * 100 / length(match_gs_df$Loser_born), 2)

#Variável Winner_country e Loser_country
round(sum(is.na(match_df$Winner_country)) * 100 / length(match_df$Winner_country), 2)
round(sum(is.na(match_df$Loser_country)) * 100 / length(match_df$Loser_country), 2)
round(sum(is.na(match_gs_df$Winner_country)) * 100 / length(match_gs_df$Winner_country), 2)
round(sum(is.na(match_gs_df$Loser_country)) * 100 / length(match_gs_df$Loser_country), 2)

#Gráficos dos jogos ganhos e perdidos por britanicos

match_df <- match_df %>%
  mutate(Winner_country_grouped = if_else(is.na(Winner_country), NA,
                                          if_else(Winner_country == "United Kingdom", "United Kingdom", "Other")))

ganho_reg <- ggplot(match_df, aes(x = Winner_country_grouped)) +
  geom_bar(fill = "#FFA07A", color = "#FFA07A") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  labs(
    title = "'match_df': Jogos ganhos por britânicos",
    y = "Número de jogos", 
    x = "País"
  )
ganho_reg

match_df <- match_df %>%
  mutate(Loser_country_grouped = if_else(is.na(Loser_country), NA,
                                         if_else(Loser_country == "United Kingdom", "United Kingdom", "Other")))

perde_reg <-ggplot(match_df, aes(x = Loser_country_grouped)) +
  geom_bar(fill = "#FFA07A", color = "#FFA07A") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  labs(
    title = "'match_df': Jogos perdidos por britânicos",
    y = "Número de jogos", 
    x = "País"
  )
perde_reg

match_gs_df <- match_gs_df %>%
  mutate(Winner_country_grouped = if_else(is.na(Winner_country), NA,
                                          if_else(Winner_country == "United Kingdom", "United Kingdom", "Other")))


ganho_gs <-ggplot(match_gs_df, aes(x = Winner_country_grouped)) +
  geom_bar(fill = "#FFA07A", color = "#FFA07A") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  labs(
    title = "'match_gs_df': Jogos ganhos por britânicos",
    y = "Número de jogos", 
    x = "País"
  )
ganho_gs

match_gs_df <- match_gs_df %>%
  mutate(Loser_country_grouped = if_else(is.na(Loser_country), NA,
                                         if_else(Loser_country == "United Kingdom", "United Kingdom", "Other")))

perde_gs <- ggplot(match_gs_df, aes(x = Loser_country_grouped)) +
  geom_bar(fill = "#FFA07A", color = "#FFA07A") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  labs(
    title = "'match_gs_df': Jogos perdidos por britânicos",
    y = "Número de jogos", 
    x = "País"
  )
perde_gs
grid.arrange(ganho_reg, perde_reg, ganho_gs, perde_gs, ncol = 2)

remove(ganho_reg, perde_reg, ganho_gs, perde_gs)

# % de jogos ganhos por jogadores do UK em torneios regulares
sum(grepl("United Kingdom", match_df$Winner_country_grouped)) / length(match_df$Winner_country_grouped)
# % de jogos perdidos por jogadores do UK em torneios regulares
sum(grepl("United Kingdom", match_df$Loser_country_grouped)) / length(match_df$Loser_country_grouped)
# % de jogos ganhos por jogadores do UK em torneios Grand Slam
sum(grepl("United Kingdom", match_gs_df$Winner_country_grouped)) / length(match_gs_df$Winner_country_grouped)
# % de jogos perdidos por jogadores do UK em torneios Grand Slam
sum(grepl("United Kingdom", match_gs_df$Loser_country_grouped)) / length(match_gs_df$Loser_country_grouped)
# % de jogos ganhos por jogadores de Other em torneios regulares
sum(grepl("Other", match_df$Winner_country_grouped)) / length(match_df$Winner_country_grouped)
# % de jogos perdidos por jogadores do Other em torneios regulares
sum(grepl("Other", match_df$Loser_country_grouped)) / length(match_df$Loser_country_grouped)
# % de jogos ganhos por jogadores do Other em torneios Grand Slam
sum(grepl("Other", match_gs_df$Winner_country_grouped)) / length(match_gs_df$Winner_country_grouped)
# % de jogos perdidos por jogadores do Other em torneios Grand Slam
sum(grepl("Other", match_gs_df$Loser_country_grouped)) / length(match_gs_df$Loser_country_grouped)

#Variável Winner_height e Loser_height
summary(match_df$Winner_height)
summary(match_df$Loser_height)
summary(match_gs_df$Winner_height)
summary(match_gs_df$Loser_height)
#Jogadores com alturas (Height) erradas = 15, 71 e 511
match_df <- match_df %>% mutate(Winner_height = if_else(Winner_height == 15, NA, Winner_height))
match_df <- match_df %>% mutate(Winner_height = if_else(Winner_height == 511, NA, Winner_height))
match_df <- match_df %>% mutate(Loser_height = if_else(Loser_height == 15, NA, Loser_height))
match_df <- match_df %>% mutate(Loser_height = if_else(Loser_height == 71, NA, Loser_height))
match_df <- match_df %>% mutate(Loser_height = if_else(Loser_height == 511, NA, Loser_height))
match_gs_df <- match_gs_df %>% mutate(Winner_height = if_else(Winner_height == 15, NA, Winner_height))
match_gs_df <- match_gs_df %>% mutate(Winner_height = if_else(Winner_height == 71, NA, Winner_height))
match_gs_df <- match_gs_df %>% mutate(Winner_height = if_else(Winner_height == 511, NA, Winner_height))
match_gs_df <- match_gs_df %>% mutate(Loser_height = if_else(Loser_height == 15, NA, Loser_height))
match_gs_df <- match_gs_df %>% mutate(Loser_height = if_else(Loser_height == 71, NA, Loser_height))
match_gs_df <- match_gs_df %>% mutate(Loser_height = if_else(Loser_height == 511, NA, Loser_height))

ganha_alt<-ggplot(match_df, aes(x = Winner_height)) +
  geom_boxplot(color = "black", fill = "#FFA07A", width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(
    title = "match_df: Altura (em cm) do vencedor"
  )
perde_alt<-ggplot(match_df, aes(x = Loser_height)) +
  geom_boxplot(color = "black", fill = "#FFA07A", width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(
    title = "match_df: Altura (em cm) do perdedor"
  )

ganha_alt_gs<-ggplot(match_gs_df, aes(x = Winner_height)) +
  geom_boxplot(color = "black", fill = "#FFA07A", width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(
    title = "match_gs_df: Altura (em cm) do vencedor"
  )
perde_alt_gs<-ggplot(match_gs_df, aes(x = Loser_height)) +
  geom_boxplot(color = "black", fill = "#FFA07A", width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(
    title = "match_gs_df: Altura (em cm) do perdedor"
  )

grid.arrange(ganha_alt, perde_alt, ganha_alt_gs, perde_alt_gs, ncol = 2)

#Boxplots do Winner_weight e Loser_weight
ganha_peso<-ggplot(match_df, aes(x = Winner_weight)) +
  geom_boxplot(color = "black", fill = "#FFA07A", width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(
    title = "match_df: Peso (em kg) do vencedor"
  )
perde_peso<-ggplot(match_df, aes(x = Loser_weight)) +
  geom_boxplot(color = "black", fill = "#FFA07A", width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(
    title = "match_df: Peso (em kg) do perdedor"
  )

ganha_peso_gs<-ggplot(match_gs_df, aes(x = Winner_weight)) +
  geom_boxplot(color = "black", fill = "#FFA07A", width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(
    title = "match_gs_df: Peso (em kg) do vencedor"
  )
perde_peso_gs<-ggplot(match_gs_df, aes(x = Loser_weight)) +
  geom_boxplot(color = "black", fill = "#FFA07A", width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(
    title = "match_gs_df: Peso (em kg) do perdedor"
  )

grid.arrange(ganha_peso, perde_peso, ganha_peso_gs, perde_peso_gs, ncol = 2)

#Variável Winner_weight e Loser_weight
summary(match_df$Winner_weight)
summary(match_df$Loser_weight)
summary(match_gs_df$Winner_weight)
summary(match_gs_df$Loser_weight)

#Jogador com weight = 186, 185 e 35, são erro de base de dados, passamos a NA
match_df <- match_df %>% mutate(Winner_weight = if_else(Winner_weight == 186, NA, Winner_weight))
match_gs_df <- match_gs_df %>% mutate(Winner_weight = if_else(Winner_weight == 186, NA, Winner_weight))
match_gs_df <- match_gs_df %>% mutate(Winner_weight = if_else(Winner_weight == 185, NA, Winner_weight))
match_gs_df <- match_gs_df %>% mutate(Winner_weight = if_else(Winner_weight == 35, NA, Winner_weight))
match_df <- match_df %>% mutate(Loser_weight = if_else(Loser_weight == 186, NA, Loser_weight))
match_df <- match_df %>% mutate(Loser_weight = if_else(Loser_weight == 185, NA, Loser_weight))
match_gs_df <- match_gs_df %>% mutate(Loser_weight = if_else(Loser_weight == 186, NA, Loser_weight))
match_gs_df <- match_gs_df %>% mutate(Loser_weight = if_else(Loser_weight == 185, NA, Loser_weight))
match_gs_df <- match_gs_df %>% mutate(Loser_weight = if_else(Loser_weight == 35, NA, Loser_weight))


ganha_peso1<-ggplot(match_df, aes(x = Winner_weight)) +
  geom_histogram(fill = "#FFA07A", color = "#FFA07A") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  labs(
    title = "'match_df': Peso do vencedor",
    y = "Frequência", 
    x = "Peso"
  )

perde_peso1<-ggplot(match_df, aes(x = Loser_weight)) +
  geom_histogram(fill = "#FFA07A", color = "#FFA07A") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  labs(
    title = "'match_df': Peso do perdedor",
    y = "Frequência", 
    x = "Peso"
  )
ganha_peso_gs1<-ggplot(match_gs_df, aes(x = Winner_weight)) +
  geom_histogram(fill = "#FFA07A", color = "#FFA07A") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  labs(
    title = "'match_df': Peso do vencedor",
    y = "Frequência", 
    x = "Peso"
  )

perde_peso_gs1<-ggplot(match_gs_df, aes(x = Loser_weight)) +
  geom_histogram(fill = "#FFA07A", color = "#FFA07A") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  labs(
    title = "'match_df': Peso do perdedor",
    y = "Frequência", 
    x = "Peso"
  )
grid.arrange(ganha_peso1, perde_peso1, ganha_peso_gs1, perde_peso_gs1, ncol = 2)

#Variável Winner_forehand e Loser_forehand
#Gráficos
ggplot(match_df, aes(x = Winner_forehand)) +
  geom_bar(fill = "#FFA07A", color = "#FFA07A") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  labs(
    title = "'match_gs_df': Jogos perdidos por britânicos",
    y = "Número de jogos", 
    x = "País"
  )
#Converter os "unkowns Backhands" em NA

match_df <- match_df %>% 
  mutate(Winner_backhand = if_else(Winner_backhand == "Unknown Backhand", NA, Winner_backhand))
match_df <- match_df %>% 
  mutate(Loser_backhand = if_else(Loser_backhand == "Unknown Backhand", NA, Loser_backhand))
match_gs_df <- match_gs_df %>% 
  mutate(Winner_backhand = if_else(Winner_backhand == "Unknown Backhand", NA, Winner_backhand))
match_gs_df <- match_gs_df %>% 
  mutate(Loser_backhand = if_else(Loser_backhand == "Unknown Backhand", NA, Loser_backhand))


#Entradas com Prize = "(W/O)" (Falta de comparência), RET (retirou-se por lesão) e DEF (desistiu a meio) têm de ser removidas. 

match_df <- match_df[!grepl("(W/O)", match_df$Score),]
match_gs_df <- match_gs_df[!grepl("(W/O)", match_gs_df$Score), ]
match_df <- match_df[!grepl("DEF", match_df$Score),]
match_gs_df <- match_gs_df[!grepl("DEF", match_gs_df$Score), ]
match_df <- match_df[!grepl("RET", match_df$Score),]
match_gs_df <- match_gs_df[!grepl("RET", match_gs_df$Score), ]

#Criação da variável player1_age e player2_age que são o resultado da subtração do ano de nascimento ao ano do torneio. Não é completamente
#precisa, uma vez que 1 jogador nascido em 1 de Janeiro e outro a 31 de dezembro do mesmo ano teriam idades diferentes num torneio realizado
#a 30 de Junho e por este método terão a mesma. De qualquer forma, achamos que essa diferença não é significativa para o modelo.

match_df$Winner_age <- match_df$Year - match_df$Winner_born
match_df$Loser_age <- match_df$Year - match_df$Loser_born
match_gs_df$Winner_age <- match_gs_df$Year- match_gs_df$Winner_born
match_gs_df$Loser_age <- match_gs_df$Year - match_gs_df$Loser_born

str(match_df)
#######################
#Sets
summary(match_df$Sets)
summary(match_gs_df$Sets)

#Reparámos que o valor minimo de sets presente nos nossos dataframes é 1, o que significa que existe algo de incorreto, uma vez que um torneio
#à melhor de 3 ou 5 não pode ter apenas 1 set. Percebemos que estas linhas correspondiam a jogos em que o jogador não compareceu/desistiu (valores que já tinhamos passado para nulos)
#decidimos passar as linhas com número de sets = 1 para NA's, de forma a não enviesar os nossos resultados
match_df = match_df %>% mutate(Sets = ifelse(is.na(Score), NA, Sets))
match_gs_df = match_gs_df %>% mutate(Sets = ifelse(is.na(Score), NA, Sets))

#Para além disso, nos torneios Grand Slam observámos que existiam alguns jogos com apenas 2 sets.
gs_sets_2 = match_gs_df %>% filter(Sets == 2)

#Ao realizarmos a visualização, percebemos que existem algumas rondas (1st Round Qualifying e 2nd Round Qualifying) nos torneios de Wimbledon que era realizadas à melhor de 3.
#Decidimos passar estas linhas para o match_df em vez de as eliminar, de forma a aproveitarmos ao máximo a nossa base de dados
gs_sets_2$Tournament_city <- "London"
gs_sets_2= gs_sets_2[, c("Tournament_id","Year","Tournament_name","Ground","Prize","Round","Game_phase","Winner_name","Winner_born","Winner_country","Winner_height","Winner_weight", "Winner_forehand","Winner_backhand","Loser_link","Winner_name","Loser_born","Loser_country","Loser_height","Loser_weight", "Loser_forehand","Loser_backhand","Loser_link","Score","Sets","Tournament_city","Winner_country_grouped","Loser_country_grouped","Winner_age","Loser_age")]

match_df = bind_rows(match_df, gs_sets_2)
rm(gs_sets_2)

match_df = match_df[, !(names(match_df) %in% c("Winner_name.1", "Loser_link.1"))]

match_gs_df <- match_gs_df %>% filter(Sets >= 3)

#Ao analisarmos os números de sets dos dataframes, reparámos que existem 84 linhas que correspondem a jogos à melhor de 5, em torneios à melhor de 3. 
#Decidimos eliminá-las, para não enviesar o nosso modelo. A nossa decisão desta vez foi eliminar as linhas, devido não só ao reduzido montante de linhas, como também pela diferença existente em alguns campos (tournament_name, ground, ...), que poderiam apenas complexificar o nosso modelo desnecessáriamente
match_df <- match_df %>% filter(Sets <= 3)


#Winner_age e Loser_age
summary(match_df$Winner_age)
summary(match_gs_df$Winner_age)
summary(match_df$Loser_age)
summary(match_gs_df$Loser_age)

ganha_idade<-ggplot(match_df, aes(x = Winner_age)) +
  geom_boxplot(color = "black", fill = "#FFA07A", width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(
    title = "match_df: Idade do vencedor"
  )

perde_idade<-ggplot(match_df, aes(x = Loser_age)) +
  geom_boxplot(color = "black", fill = "#FFA07A", width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(
    title = "match_df: Idade do perdedor"
  )

ganha_idade_gs<-ggplot(match_gs_df, aes(x = Winner_age)) +
  geom_boxplot(color = "black", fill = "#FFA07A", width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(
    title = "match_gs_df: Idade do vencedor"
  )

perde_idade_gs<-ggplot(match_gs_df, aes(x = Loser_age)) +
  geom_boxplot(color = "black", fill = "#FFA07A", width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(
    title = "match_gs_df: Idade do perdedor"
  )

grid.arrange(ganha_idade, perde_idade, ganha_idade_gs, perde_idade_gs, ncol = 2)

#como a idade tem bastantes outliers, precisamos visualizar as idades para percebermos se estes valores são ou não são realistas
summary(as.factor(as.character(match_df$Winner_age))) 
summary(as.factor(as.character(match_df$Loser_age)))
summary(as.factor(as.character(match_gs_df$Winner_age)))
summary(as.factor(as.character(match_gs_df$Loser_age)))

#Ao observarmos os valores da idade dos jogadores, reparámos que existem alguns valores incorretos. 
#Até podia acontecer um jogador ter 50 e participar num torneio, mas é impossível ter uma idade negativa.
#Vamos por isso identificar os jogadores que tem a sua data de nascimento incorreta (o que torna consequentemente a sua idade também incorreta) e alterar pelo seu ano real de nascimento
match_gs_df$Winner_born[match_gs_df$Winner_name == "Ronald Mckenzie"] <- 1922
match_gs_df$Loser_born[match_gs_df$Loser_name == "Ronald Mckenzie"] <- 1922
match_gs_df$Winner_born[match_gs_df$Winner_name == "Jean-Noel Grinda"] <- 1936
match_gs_df$Loser_born[match_gs_df$Loser_name == "Jean-Noel Grinda"] <- 1936
match_gs_df <- subset(match_gs_df, Winner_age != -34)
match_gs_df <- subset(match_gs_df, Loser_age != -34)
match_df <- subset(match_df, Loser_age != -14)

#Para atualizarmos as idades
match_df$Winner_age <- match_df$Year - match_df$Winner_born
match_df$Loser_age <- match_df$Year - match_df$Loser_born
match_gs_df$Winner_age <- match_gs_df$Year- match_gs_df$Winner_born
match_gs_df$Loser_age <- match_gs_df$Year - match_gs_df$Loser_born

#write.csv(match_df, "match_df.csv")
#write.csv(match_gs_df, "match_gs_df.csv")
#################################################################################################################################################
##############################################################(Modeling)#########################################################################
#################################################################################################################################################
match_df_link = "C:/Users/afare/OneDrive/Documentos/match_df.csv"
match_gs_df_link = "C:/Users/afare/OneDrive/Documentos/match_gs_df.csv"
match_df = read.csv(match_df_link)
match_gs_df = read.csv(match_gs_df_link)

#GLM torneios regulares (melhor de 3)
data_glm = select(match_df,c("Prize","Game_phase","Winner_height","Winner_forehand","Loser_height","Loser_weight","Sets","Winner_age","Loser_age")) %>% na.omit()

data_glm <- subset(data_glm, !(Game_phase %in% c("Olympic Bronze", "3rd/4th Place Match")))
data_glm$Sets = as.factor(data_glm$Sets)

set.seed(100)

index_glm = sample(1:nrow(data_glm), round(nrow(data_glm) * 0.8))
train_glm = data_glm[index_glm,]
test_glm = data_glm[-index_glm,]

regular_glm = glm(Sets~.,data = train_glm, family = "binomial")

previsoes = predict(regular_glm, newdata = test_glm, type = "response")
previsoes_regular = ifelse(previsoes > 0.5,3,2) %>% factor()

matriz_confusao <- confusionMatrix(previsoes_regular,test_glm$Sets)

roc_previsao = roc(test_glm$Sets,previsoes)
plot(roc_previsao)
auc(roc_previsao)

################################################ DATA FRAME DE TESTES ###########################################
data_glm_temp= select(match_df,c("Prize","Game_phase","Sets","Winner_age","Loser_age")) %>% na.omit()

data_glm_temp <- subset(data_glm_temp, !(Game_phase %in% c("Olympic Bronze", "3rd/4th Place Match")))
data_glm_temp$Sets = as.factor(data_glm_temp$Sets)

set.seed(100)

indices_score_2 <- which(data_glm_temp$Sets == 2)
indices_remover <- sample(indices_score_2, length(indices_score_2) / 2)
data_glm_temp <- data_glm_temp[setdiff(seq_len(nrow(data_glm_temp)), indices_remover), , drop = FALSE]
rm(indices_score_2)
rm(indices_remover)

index_glm_temp = sample(1:nrow(data_glm_temp), round(nrow(data_glm_temp) * 0.8))
train_glm_temp = data_glm_temp[index_glm_temp,]
test_glm_temp = data_glm_temp[-index_glm_temp,]

regular_glm_temp = glm(Sets~.,data = train_glm_temp, family = "binomial")

previsoes_temp = predict(regular_glm_temp, newdata = test_glm_temp, type = "response")

previsoes_regular_temp = ifelse(previsoes_temp > 0.5,3,2) %>% factor()

matriz_confusao_temp <- confusionMatrix(previsoes_regular_temp,test_glm_temp$Sets)

roc_previsao_temp = roc(test_glm_temp$Sets,previsoes_temp)
plot(roc_previsao_temp)
auc(roc_previsao_temp)


###############################################################################################################

#GLM Torneios Grand Slam (melhor de 5)

data_gs_glm = select(match_gs_df,c("Prize","Game_phase","Winner_country","Winner_height","Winner_weight","Winner_forehand","Loser_country","Loser_height","Loser_weight","Loser_forehand","Sets","Winner_age","Loser_age")) %>% na.omit()

data_gs_glm$Sets = as.factor(data_gs_glm$Sets)

set.seed(100)

index_gs = sample(1:nrow(data_gs_glm), round(nrow(data_gs_glm) * 0.8))
train_gs_glm = data_gs_glm[index_gs,]
test_gs_glm = data_gs_glm[-index_gs,]

gs_glm = multinom(Sets ~ ., data = train_gs_glm)

previsoes_gs <- predict(gs_glm, newdata = test_gs_glm, type = "class")

matriz_confusao_gs <- confusionMatrix(previsoes_gs, test_gs_glm$Sets)

roc_previsao_gs = roc(test_gs_glm$Sets,previsoes_gs)
plot(roc_previsao_gs)
auc(roc_previsao_gs)

####################################

#Árvores de decisão

#Torneios regulares (melhor de 3)
tabela_regular = select(match_df,c("Prize","Ground","Game_phase","Winner_height","Winner_weight","Winner_forehand","Loser_height","Loser_weight","Loser_forehand","Sets","Winner_age","Loser_age")) %>% na.omit()
tabela_regular$Sets = as.factor(tabela_regular$Sets)

set.seed(100)
# Dividir os registos entre grupo de treino (80%) e teste (20%)
split_indices3 = sample.split(tabela_regular$Sets, SplitRatio = 0.8)

# Criação dos conjuntos de treino e teste
train_data3 = subset(tabela_regular, split_indices3 == TRUE)
test_data3 = subset(tabela_regular, split_indices3 == FALSE)

#Passar a variável "Sets" para factor
train_data3$Sets = as.factor(train_data3$Sets)
test_data3$Sets = as.factor(test_data3$Sets)

#equilibrar o dataset visto que a proporção de jogos terminados
#em 2 Sets é muito maior que em 3.
balanced_data3 <- downSample(train_data3, train_data3$Sets)
prop.table(table(balanced_data3$Sets)) %>% round(3)

model_tree3 <- rpart(Class ~ Game_phase + Ground + Winner_weight + Winner_height + 
                       Winner_age + Winner_forehand + Loser_weight +
                       Loser_height + Loser_forehand + Prize +
                       Loser_age,
                     data = balanced_data3,
                     #metodo class significa que vai classificar
                     method = "class")
summary(model_tree3)
varImp(model_tree3)
prp(model_tree3, main = "Decision Tree", type = 1, extra = 104, fallen.leaves = TRUE, roundint = FALSE, box.col = c("lightyellow", "lightsalmon", "lightgreen"))

predictions_tree3 <- predict(model_tree3, newdata = test_data3, type = "class")
confusionMatrix(predictions_tree3, test_data3$Sets)

predictions_tree_roc3 <- predict(model_tree3, test_data3, type = "prob")
roc_multi3 <- multiclass.roc(test_data3$Sets, predictions_tree_roc3)
auc(roc_multi3)

#Torneios Grand Slam (melhor de 5)
data_gs_tree <- match_gs_df[,c("Prize", "Game_phase", "Winner_height", "Loser_height", "Winner_weight", "Loser_weight",  "Winner_forehand", "Loser_forehand", "Winner_age", "Loser_age", "Sets")]

data_gs_tree$Winner_forehand <- factor(data_gs_tree$Winner_forehand)
data_gs_tree$Loser_forehand <- factor(data_gs_tree$Loser_forehand)
data_gs_tree$Game_phase <- factor(data_gs_tree$Game_phase)

#Converte os factors em valores numéricos

table_gs_tree<- data_gs_tree[complete.cases(data_gs_tree), ]
set.seed(100)
# Dividir os registos entre grupo de treino (80%) e teste (20%)
split_indices = sample.split(table_gs_tree$Sets, SplitRatio = 0.8)

# Criação dos conjuntos de treino e teste
train_tree_gs = subset(table_gs_tree, split_indices == TRUE)
test_tree_gs = subset(table_gs_tree, split_indices == FALSE)

#Passar a variável "Sets" para factor
train_tree_gs$Sets = as.factor(train_tree_gs$Sets)
test_tree_gs$Sets = as.factor(test_tree_gs$Sets)

#equilibrar o dataset visto que a proporção de jogos terminados
#em 3 Sets é muito maior que em 4 ou 5.
balanced_gs_tree <- downSample(train_tree_gs, train_tree_gs$Sets)
prop.table(table(balanced_gs_tree$Sets)) %>% round(3)

model_gs_tree <- rpart(Class ~ Game_phase + Winner_weight + Winner_height + 
                      Winner_age + Winner_forehand + Loser_weight +
                      Loser_height + Loser_forehand + Prize +
                      Loser_age,
                    data = balanced_gs_tree,
                    #metodo class significa que vai classificar
                    method = "class")
summary(model_gs_tree)
varImp(model_gs_tree)
prp(model_gs_tree, main = "Decision Tree", type = 1, extra = 104, fallen.leaves = TRUE, roundint = FALSE, box.col = c("lightyellow", "lightsalmon", "lightgreen"))

predictions_tree_gs <- predict(model_gs_tree, newdata = test_tree_gs, type = "class")
confusionMatrix(predictions_tree_gs, test_tree_gs$Sets)

predictions_tree_roc_gs <- predict(model_gs_tree, test_tree_gs, type = "prob")
roc_multi <- multiclass.roc(test_tree_gs$Sets, predictions_tree_roc_gs)
auc(roc_multi)

####################################

#Random Forrest

#Torneios regulares (melhor de 3)
data_forest = select(match_df,c("Prize","Game_phase","Winner_country","Winner_height","Winner_weight","Winner_forehand","Loser_country","Loser_height","Loser_weight","Loser_forehand","Sets","Winner_age","Loser_age")) %>% na.omit()
data_forest$Sets = as.factor(data_forest$Sets)

index_forest = sample(1:nrow(data_forest), round(nrow(data_forest) * 0.8))
train_forest = data_tree[index_forest,]
test_forest = data_tree[-index_forest,]

model_randforest <- randomForest(Sets ~ Prize + Game_phase + 
                                   Winner_height + Winner_forehand +
                                   Loser_height + Loser_forehand +
                                   Winner_age + Loser_age,
                                 data = train_forest,
                                 ntree = 1000,  # You can adjust the number of trees
                                 mtry = 3,     # Number of variables randomly sampled as candidates at each split
                                 importance = TRUE)  # Calculate variable importance

varImpPlot(model_randforest)

predictions_rf <- predict(model_randforest, newdata = test_forest)
confusionMatrix(predictions_rf, test_forest$Sets)

predictions_rf_roc <- predict(model_randforest, test_forest, type="prob")[,1]
plot(roc(test_forest$Sets, predictions_rf_roc))
auc(roc(test_forest$Sets, predictions_rf_roc))

#Torneios Grand Slam (melhor de 5)
data_gs_forest = select(match_gs_df,c("Prize","Game_phase","Winner_country","Winner_height","Winner_weight","Winner_forehand","Loser_country","Loser_height","Loser_weight","Loser_forehand","Sets","Winner_age","Loser_age")) %>% na.omit()
data_gs_forest$Sets = as.factor(data_gs_forest$Sets)

index_forest_gs = sample(1:nrow(data_gs_forest), round(nrow(data_gs_forest) * 0.8))
train_forest_gs = data_gs_forest[index_forest_gs,]
test_forest_gs = data_gs_forest[-index_forest_gs,]
train_forest_gs = na.omit(train_forest_gs)

model_gs_randforest = randomForest(Sets ~ Prize + Game_phase + 
                                     Loser_age + Loser_forehand,
                                   data = train_forest_gs,
                                   ntree = 1000,  # You can adjust the number of trees
                                   mtry = 3,     # Number of variables randomly sampled as candidates at each split
                                   importance = TRUE)  # Calculate variable importance

varImpPlot(model_gs_randforest)

predictions_rf_gs <- predict(model_gs_randforest, newdata = test_forest_gs)
confusionMatrix(predictions_rf_gs, reference = test_forest_gs$Sets)
