######################################  ORGANIZAÇÃO INICIAL DOS DADOS ######################################

#### Bibliotecas necessárias ####

library(sf)
library(lwgeom)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ISOweek)

#### __________________ Parte 1 - MANIPULAÇÕES DOS DADOS _____________________________ #### 
#### Parte 1.1 - Esforço - ajustes iniciais ####

##### CARREGANDO PLANILHA E VISUALIZAÇÕES

ef <- read.csv(file="Effort data humpback and fin whale2013-2020.csv", header=T)
str(ef) # Existem lat e long = 0, mas, ao conferir na tabela, estão em esforço off
table(ef$Effort)
table(ef$Platform) 

# Filtrando apenas esforço on para análise
ef$Effort[ef$Effort =="ON"] <- "on" # unificando "ON" e "on"
ef[433046:433254,5] <- "on" # corrigindo erro: on e off subsequentes trocados por "on"
ef <- ef[ef$Effort == "on",] # deixando apenas esforço on na tabela

##### ORGANIZANDO INFORMAÇÕES DE DATA

ef$Hour <- substr(as.character(ef$Date), 11, 16) # coluna só com hora
ef$Data <- as.Date(substr(as.character(ef$Date), 1, 10), format = "%d/%m/%Y") # coluna só com data
ef$Month <- format(as.Date(ef$Data), "%m")
ef$YearMonth <- paste(ef$Year, ef$Month)
ef$YearIndexMonth <- paste(ef$GPSIndex, ef$YearMonth) # coluna com GPSIndex + Ano

##### CONFERINDO DUPLICAÇÕES DE GPSINDEX

a2013 <- ef[ef$Year == "2013",]
a2014 <- ef[ef$Year == "2014",]
a2015 <- ef[ef$Year == "2015",]
a2016 <- ef[ef$Year == "2016",]
a2017 <- ef[ef$Year == "2017",]
a2018 <- ef[ef$Year == "2018",]
a2019 <- ef[ef$Year == "2019",]
a2020 <- ef[ef$Year == "2020",]


dupli13 <- a2013[duplicated(a2013$YearIndexMonth) | duplicated(a2013$YearIndexMonth, fromLast = TRUE), ] # sem duplicações
dupli14 <- a2014[duplicated(a2014$YearIndexMonth) | duplicated(a2014$YearIndexMonth, fromLast = TRUE), ] # sem duplicações
dupli15 <- a2015[duplicated(a2015$YearIndexMonth) | duplicated(a2015$YearIndexMonth, fromLast = TRUE), ] # sem duplicações
dupli16 <- a2016[duplicated(a2016$YearIndexMonth) | duplicated(a2016$YearIndexMonth, fromLast = TRUE), ] # sem duplicações
dupli17 <- a2017[duplicated(a2017$YearIndexMonth) | duplicated(a2017$YearIndexMonth, fromLast = TRUE), ] # sem duplicações
dupli18 <- a2018[duplicated(a2018$YearIndexMonth) | duplicated(a2018$YearIndexMonth, fromLast = TRUE), ] # sem duplicações
dupli19 <- a2019[duplicated(a2019$YearIndexMonth) | duplicated(a2019$YearIndexMonth, fromLast = TRUE), ] # problemático
dupli20 <- a2020[duplicated(a2020$YearIndexMonth) | duplicated(a2020$YearIndexMonth, fromLast = TRUE), ]

#### Parte 1.2 - Esforço - resolvendo erro de GPSIndex de 2019 ####

jan2019 <- a2019[a2019$Month == "01",] # filtrando mês janeiro
fev2019 <- a2019[a2019$Month == "02",] # filtrando mês fevereiro

repsjan19 <- jan2019[duplicated(jan2019$GPSIndex) | duplicated(jan2019$GPSIndex, fromLast = TRUE), ] # filtrando duplicatas em janeiro
repsfev19 <- fev2019[duplicated(fev2019$GPSIndex) | duplicated(fev2019$GPSIndex, fromLast = TRUE), ] # filtrando duplicatas em fevereiro

# Janeiro 2019

# retirando 69 linhas que possuem informações NA mas a sua duplicata tem essas infos
ef <- subset(ef, !(X %in% c("237711", "237901", "243041", "243225", "246897", "246949", "248939", "249079",
                            "249973", "260518", "260639", "274466", "274739", "284168", "289777", "289797",
                            "289818", "289835", "289936", "290262", "290300", "291733", "291922", "292080",
                            "292287", "298862", "299102", "316243", "316557", "317161", "317651", "317901",
                            "318030", "320779", "320982", "321998", "322083", "322640", "322831", "324444",
                            "324672", "324767", "326512", "326648", "326881", "330968", "330995", "339730",
                            "339856", "340385", "340519", "342387", "342696", "344358", "347727", "348258", 
                            "351537", "351845", "352085", "352212", "354365", "390998", "371119", "371660", 
                            "371866", "372072", "372237", "382404", "386327"))) 
dfjan19 <- ef[ef$YearMonth == "2019 01",] # linhas de fev 2019

dfjan19 <- dfjan19[!duplicated(dfjan19$GPSIndex), ] # pega a primeira linha das duplicadas e exclui a outra
ef_restante <- ef[!(ef$YearMonth == "2019 01"), ]
ef <- rbind(ef_restante, dfjan19)

# Fevereiro 2019

# Tirando duplicados em fev/2019
ef <- subset(ef, !(X %in% c("388909", "389124", "393388", "393471"))) # Tirando 4 linhas das duplicadas, as quais uma delas é zerada em beaufort, sightability, visibility e swell
dffev19 <- ef[ef$YearMonth == "2019 02",] # linhas de fev 2019
dffev19 <- dffev19[!duplicated(dffev19$GPSIndex), ]
ef_restante <- ef[!(ef$YearMonth == "2019 02"), ]
ef <- rbind(ef_restante, dffev19)

#### Parte 1.3 - Esforço - resolvendo erro de GPSIndex de 2020 ####

fev2020 <- a2020[a2020$Month == "02",] # filtrando mês janeiro
mar2020 <- a2020[a2020$Month == "03",] # filtrando mês fevereiro

repsfev20 <- fev2020[duplicated(fev2020$GPSIndex) | duplicated(fev2020$GPSIndex, fromLast = TRUE), ] # filtrando duplicatas em fevereiro
repsmar20 <- mar2020[duplicated(mar2020$GPSIndex) | duplicated(mar2020$GPSIndex, fromLast = TRUE), ] # filtrando duplicatas em março

# Fevereiro 2020

# Tirando duplicados em fev/2020
ef <- subset(ef, !(X %in% c("398057", "404161", "410920", "410979", 
                            "420626", "420702"))) # Tirando 4 linhas das duplicadas, as quais uma delas é zerada em beaufort, sightability, visibility e swell
dffev20 <- ef[ef$YearMonth == "2020 02",] # linhas de fev 2019
dffev20 <- dffev20[!duplicated(dffev20$GPSIndex), ]
ef_restante <- ef[!(ef$YearMonth == "2020 02"), ]
ef <- rbind(ef_restante, dffev20)

# Março 2020

# Tirando duplicados em mar/2020
ef <- subset(ef, !(X %in% c("424219", "424266"))) # Tirando 4 linhas das duplicadas, as quais uma delas é zerada em beaufort, sightability, visibility e swell
dfmar20 <- ef[ef$YearMonth == "2020 03",] # linhas de fev 2019
dfmar20 <- dfmar20[!duplicated(dfmar20$GPSIndex), ]
ef_restante <- ef[!(ef$YearMonth == "2020 03"), ]
ef <- rbind(ef_restante, dfmar20)

#### Parte 1.4 - Esforço - conferindo se duplicatas permanecem ####

a2013 <- ef[ef$Year == "2013",]
a2014 <- ef[ef$Year == "2014",]
a2015 <- ef[ef$Year == "2015",]
a2016 <- ef[ef$Year == "2016",]
a2017 <- ef[ef$Year == "2017",]
a2018 <- ef[ef$Year == "2018",]
a2019 <- ef[ef$Year == "2019",]
a2020 <- ef[ef$Year == "2020",]

dupli13 <- a2013[duplicated(a2013$YearIndexMonth) | duplicated(a2013$YearIndexMonth, fromLast = TRUE), ]
dupli14 <- a2014[duplicated(a2014$YearIndexMonth) | duplicated(a2014$YearIndexMonth, fromLast = TRUE), ]
dupli15 <- a2015[duplicated(a2015$YearIndexMonth) | duplicated(a2015$YearIndexMonth, fromLast = TRUE), ]
dupli16 <- a2016[duplicated(a2016$YearIndexMonth) | duplicated(a2016$YearIndexMonth, fromLast = TRUE), ]
dupli17 <- a2017[duplicated(a2017$YearIndexMonth) | duplicated(a2017$YearIndexMonth, fromLast = TRUE), ]
dupli18 <- a2018[duplicated(a2018$YearIndexMonth) | duplicated(a2018$YearIndexMonth, fromLast = TRUE), ]
dupli19 <- a2019[duplicated(a2019$YearIndexMonth) | duplicated(a2019$YearIndexMonth, fromLast = TRUE), ]
dupli20 <- a2020[duplicated(a2020$YearIndexMonth) | duplicated(a2020$YearIndexMonth, fromLast = TRUE), ]

# Retirando a coluna "Platform" (opcional)

ef <- ef[,-6]

write.csv(ef, "workable_ef.csv", row.names = FALSE)


# Criando planilha de esforço

load("tabfwhw.Rda")
linhas <- st_read("shp_effort_transects.shp")

periodos_eng <- c("12 Feb - 1 Mar", "7 Feb - 29 Mar", "5 Feb - 20 Feb", "12 Feb - 24 Feb","12 Feb - 6 Mar", "15 Feb - 26 Feb", "7 Jan - 9 Feb", "18 Feb - 5 Mar")

periodos_pt <- c("12 Fev - 1 Mar", "7 Fev - 29 Mar", "5 Fev - 20 Fev", "12 Fev - 24 Fev","12 Fev - 6 Mar", "15 Fev - 26 Fev", "7 Jan - 9 Fev", "18 Fev - 5 Mar")


#### Parte 1.5 - Avistagem - manipulações inciais ####

##### CARREGANDO PLANILHA E VISUALIZAÇÕES INICIAIS

av <- read.csv(file="sightings humpback and fin 2013-2020.csv", header=T)
str(av)
apply(av, 2, range)

##### ORGANIZANDO INFORMAÇÕES DE DATA

av$Hour <- substr(as.character(av$Date), 11, 16) # coluna só com hora
av$Data <- as.Date(substr(as.character(av$Date), 1, 10), format = "%d/%m/%Y") # coluna só com data
av$Month <- format(as.Date(av$Data), "%m")
av$YearMonth <- paste(av$Year, av$Month)
av$YearIndexMonth <- paste(av$GpsIndex, av$YearMonth)

# Resgatando Lat e Long pela planilha de esforço
av <- merge(av, ef[, c("YearIndexMonth", "Latitude", "Longitude")], by = "YearIndexMonth", all.x = TRUE)
table(av$Latitude) 
table(av$Longitude)
av <- subset(av, !(is.na(av$Latitude) == TRUE))

# Criação de coluna geométrica com pontos de avistagem em 4326 e com pontos de avistagem em 6932

avist <- av

geom_4326 <- st_as_sf(avist, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) # criando geometria 4326

geom_6932 <- st_transform(geom_4326, crs = 6932) # criando geometria 6932

write.csv(av, "workable_av.csv", row.names = FALSE)

st_write(geom_4326, 
         "shp_avs_4326.shp", 
         driver = "ESRI Shapefile",
         delete_layer = TRUE)

st_write(geom_6932, 
         "shp_avs_6932.shp", 
         driver = "ESRI Shapefile",
         delete_layer = TRUE)

#### __________________ Parte 2 - Criação de semanas _________________________________ ####

ef$semana_ano <- isoweek(ef$Data) # Criando coluna para informação da semana referente ao ano
ef$semana <- isoweek(ef$Data) # Criando coluna para informação da semana sequencial para análise

semanas <- ef %>%  
  distinct(Year, semana_ano) %>%
  arrange(Year, semana_ano) 


ef <- ef %>%
  mutate(semana = case_when(
    Year == 2013 & semana == 7  ~ 1,
    Year == 2013 & semana == 8  ~ 2,
    Year == 2013 & semana == 9  ~ 3,
    
    Year == 2014 & semana == 6  ~ 4,
    Year == 2014 & semana == 7  ~ 5,
    Year == 2014 & semana == 8  ~ 6,
    Year == 2014 & semana == 9  ~ 7,
    Year == 2014 & semana == 12 ~ 8,
    Year == 2014 & semana == 13 ~ 9,
    
    Year == 2015 & semana == 6  ~ 10,
    Year == 2015 & semana == 7  ~ 11,
    Year == 2015 & semana == 8  ~ 12,
    
    Year == 2016 & semana == 6  ~ 13,
    Year == 2016 & semana == 7  ~ 14,
    Year == 2016 & semana == 8  ~ 15,
    
    Year == 2017 & semana == 6  ~ 16,
    Year == 2017 & semana == 7  ~ 17,
    Year == 2017 & semana == 8  ~ 18,
    Year == 2017 & semana == 9  ~ 19,
    Year == 2017 & semana == 10 ~ 20,
    
    Year == 2018 & semana == 7  ~ 21,
    Year == 2018 & semana == 8  ~ 22,
    Year == 2018 & semana == 9  ~ 23,
    
    Year == 2019 & semana == 2  ~ 24,
    Year == 2019 & semana == 3  ~ 25,
    Year == 2019 & semana == 4  ~ 26,
    Year == 2019 & semana == 5  ~ 27,
    Year == 2019 & semana == 6  ~ 28,
    
    Year == 2020 & semana == 8  ~ 29,
    Year == 2020 & semana == 9  ~ 30,
    Year == 2020 & semana == 10 ~ 31,
  ))

semana_ano_df <- unique(ef[, c("Year", "semana_ano")])
semana_ano_df$semana_estudo <- unique(ef[, "semana"])
semana_ano_df$semana <- c("2013-W07", "2013-W08", "2013-W09", "2014-W06", "2014-W07", "2014-W08", 
                          "2014-W09", "2014-W12", "2014-W13", "2015-W06", "2015-W07", "2015-W08", 
                          "2016-W06", "2016-W07", "2016-W08", "2017-W06", "2017-W07", "2017-W08",
                          "2017-W09", "2017-W10", "2018-W07", "2018-W08", "2018-W09", "2019-W02",
                          "2019-W03", "2019-W04", "2019-W05", "2019-W06", "2020-W08", "2020-W09", 
                          "2020-W10")
semana_ano_df$inicio_semana <- ISOweek2date(paste0(semana_ano_df$semana, "-1"))
semana_ano_df$fim_semana <- ISOweek2date(paste0(semana_ano_df$semana, "-7"))

# Retirando coluna "semana_ano" do df de esforço

ef <- ef[,-20]

write.csv(semana_ano_df, "semanas.csv", row.names = FALSE)
write.csv(ef, "workable_ef.csv", row.names = FALSE)


#### __________________ Parte 3 - Criação de transectos e segmentos __________________ #### 

#### Parte 3.1 - Criando transectos ####

# Utilização da planilha original e não do workable_ef: as informações de esforço off são importantes aqui.

efw <- ef

ef <- read.csv(file="Effort data humpback and fin whale2013-2020.csv", header=T) 

ef[433046:433254, 5] <- "on" # corrigindo erro IV - on e off subsequentes trocados por "on"
ef$Effort[ef$Effort =="ON"] <- "on" # unificando on
ef$Effort[ef$Effort =="OFF"|ef$Effort=="?"|ef$Effort=="OF"] <- "off" # unificando off
table(ef$Effort)

ef$Hour <- substr(as.character(ef$Date), 11, 16) # coluna só com hora
ef$Data <- as.Date(substr(as.character(ef$Date), 1, 10), format = "%d/%m/%Y") # coluna só com data
ef$Month <- format(as.Date(ef$Data), "%m")
ef$YearMonth <- paste(ef$Year, ef$Month)
ef$YearIndexMonth <- paste(ef$GPSIndex, ef$YearMonth) # coluna com GPSIndex + Ano

# Função para criação de vetor com número do transecto

ef$Transect <- 0 #  Inicializando a coluna dos transectos com zeros
contador_bloco <- 0 # Iniciaizando contador de blocos on/off

# Loop para preencher a coluna de transect a partir do contador de blocos
for (i in 1:nrow(ef)) {
  if (ef$Effort[i] == "on") {
    if (i == 1 || ef$Effort[i - 1] == "off" || ef$Data[i] != ef$Data[i - 1]) {
      contador_bloco <- contador_bloco + 1
    }
    ef$Transect[i] <- contador_bloco
  } else {
    ef$Transect[i] <- 0
  }
}

range(ef$Transect) # conferindo a quantidade de transectos criadas

apply(ef, 2, range) # agora o range da data bate direitinho!

ef <- subset(ef, Effort=="on")

#### Parte 3.2 - Criando shapefiles de pontos e linhas de esforço ####

efw <- ef[,-6]

# Converter para sf
efw <- st_as_sf(efw, coords = c("Longitude", "Latitude"), crs = 4326)
efw <- st_transform(efw, crs = 6932)

# shp de pontos
st_write(efw, 
         "shp_effort_points.shp", 
         driver = "ESRI Shapefile",
         delete_layer = TRUE)


criar_linhas <- function(efw) {
  efw %>%
    group_by(Transect, Data) %>%
    summarize(geometry = st_combine(geometry), .groups = "drop") %>%
    st_cast("LINESTRING")
}

linhas <- criar_linhas(efw)

# Obter o primeiro valor de cada atributo (não-geométrico)
atributos_efw <- efw %>%
  st_drop_geometry() %>%
  group_by(Transect, Data) %>%
  summarize(across(everything(), ~ .x[1]), .groups = "drop")

# Juntar com as linhas
linhas <- linhas %>%
  left_join(atributos_efw, by = c("Transect", "Data"))

linhas <- linhas %>% 
  filter(Transect != 171)

linhas <- linhas %>% 
  filter(Transect != 172)

# shp de transectos
st_write(linhas, 
         "shp_effort_transects.shp", 
         driver = "ESRI Shapefile",
         delete_layer = TRUE)

#### Parte 3.3 - Criando segmentos particionando os transectos com +/- 4km ####

segment_size <- 4000  # 4km, mas está em metros porque é crs 6932

segmentar_linha_preservando_curvas <- function(linha, atributos, tamanho, crs) {
  linha <- st_zm(linha)  # Remove Z/M se existirem
  if (!st_is_valid(linha)) linha <- st_make_valid(linha)
  
  total_length <- as.numeric(st_length(linha))
  num_segments <- floor(total_length / tamanho)
  
  if (num_segments > 0) {
    pontos_corte <- st_line_sample(linha, n = num_segments, type = "regular")
    pontos_corte <- c(
      st_geometry(st_line_sample(linha, sample = 0)),
      pontos_corte,
      st_geometry(st_line_sample(linha, sample = 1))
    )
    
    cortes <- st_cast(pontos_corte, "POINT")
    
    # Aumenta detalhamento da linha
    linha_detalhada <- st_segmentize(linha, dfMaxLength = tamanho / 10)
    
    # Snap para garantir corte
    linha_detalhada <- st_snap(linha_detalhada, cortes, tolerance = 1)
    
    # Cuidado com possíveis erros no split
    recortes <- tryCatch(
      st_split(linha_detalhada, cortes),
      error = function(e) {
        warning("Erro em st_split. Retornando linha original.")
        return(st_sfc(linha, crs = crs))
      }
    )
    
    if (inherits(recortes, "sfc")) {
      segmentos <- recortes
    } else {
      segmentos <- st_collection_extract(recortes, "LINESTRING")
    }
    
    if (length(segmentos) == 0) {
      segmentos <- st_sfc(linha, crs = crs)
    } else {
      comprimentos <- st_length(segmentos)
      ult_len <- as.numeric(comprimentos[length(comprimentos)])
      
      if (ult_len < 2000 && length(segmentos) > 1) {
        penult <- segmentos[length(segmentos) - 1]
        ult <- segmentos[length(segmentos)]
        combinado <- st_union(penult, ult)
        segmentos <- c(segmentos[-c(length(segmentos) - 1, length(segmentos))], combinado)
      }
    }
    
    segmentos_sf <- st_sf(geometry = st_sfc(segmentos, crs = crs))
    for (col in names(atributos)) {
      segmentos_sf[[col]] <- atributos[[col]]
    }
    
    return(segmentos_sf)
    
  } else {
    segmentos_sf <- st_sf(geometry = st_sfc(linha, crs = crs))
    for (col in names(atributos)) {
      segmentos_sf[[col]] <- atributos[[col]]
    }
    return(segmentos_sf)
  }
}


linhas_segmentadas <- pmap_dfr(
  list(
    linha = linhas$geometry,
    atributos = split(st_drop_geometry(linhas), seq_len(nrow(linhas)))
  ),
  function(linha, atributos) {
    segmentar_linha_preservando_curvas(linha, atributos, segment_size, st_crs(linhas))
  }
)

# Remover Z (necessário para shapefile)
linhas_segmentadas <- st_zm(linhas_segmentadas)

# Calcular tamanho dos segmentos (em km, para conferir)
linhas_segmentadas$tam_segmnt <- round(as.numeric(st_length(linhas_segmentadas)) / 1000, 3)

table(linhas_segmentadas$tam_segmnt)

# Transformando em sf
linhas_segmentadas <- st_as_sf(linhas_segmentadas, crs = st_crs(linhas))
linhas_segmentadas$id_segmento <- seq_len(nrow(linhas_segmentadas))

#### Parte 3.4 - Gerando os midpoints dos segmentos ####

# Criando midpoints de cada segmento

midpoints <- st_line_sample(linhas_segmentadas$geometry, sample = 0.5)
midpoints <- st_cast(midpoints, "POINT")
coords <- st_coordinates(midpoints)

midpoints_sf <- st_sf(
  linhas_segmentadas %>% st_drop_geometry(),
  geometry = midpoints,
  lon = coords[, 1],
  lat = coords[, 2]
)

# Converter temporariamente para 4326
midpoints_temp <- st_transform(midpoints_sf, crs = 4326)
coords_geo <- st_coordinates(midpoints_temp)

# Atribuir aos campos Latitude e Longitude (em graus)
midpoints_sf$Longitude <- coords_geo[, 1]
midpoints_sf$Latitude <- coords_geo[, 2]

#### Parte 3.5 - Criando shapefiles de segmentos e midpoints #####

st_write(linhas_segmentadas, 
         "shp_effort_segments.shp", 
         driver = "ESRI Shapefile",
         delete_layer = TRUE)

st_write(midpoints_sf, 
         "shp_effort_midpoints.shp", 
         driver = "ESRI Shapefile",
         delete_layer = TRUE)





#### Parte 5.1 - MANIPULAÇÕES FINAIS: Separação de avistagens por semana ####

# Função para criar as avistagens por semana
for (i in seq_len(nrow(semanas))) {
  data_ini <- semanas$inicio_semana[i]
  data_fim <- semanas$fim_semana[i]
  numero_semana <- semanas$semana_estudo[i]
  
  # Filtrar as avistagens dessa semana
  av_filtro <- av %>% filter(Data >= data_ini & Data <= data_fim)
  
  # Criar objeto com nome avs_semana1, avs_semana2, ...
  assign(paste0("avs_semana", numero_semana), av_filtro)
}

# Conferindo, dentro das avs semanas, quais semanas possuem mais de uma avistagem com o mesmo YearIndexMonth

errossem1 <- avs_semana1 %>%
  group_by(YearIndexMonth) %>%
  filter(n() > 1)

errossem5 <- avs_semana5 %>%
  group_by(YearIndexMonth) %>%
  filter(n() > 1)

errossem9 <- avs_semana9 %>%
  group_by(YearIndexMonth) %>%
  filter(n() > 1)

errossem10 <- avs_semana10 %>%
  group_by(YearIndexMonth) %>%
  filter(n() > 1)

errossem11 <- avs_semana11 %>%
  group_by(YearIndexMonth) %>%
  filter(n() > 1)

errossem17 <- avs_semana17 %>%
  group_by(YearIndexMonth) %>%
  filter(n() > 1)

errossem18 <- avs_semana18 %>%
  group_by(YearIndexMonth) %>%
  filter(n() > 1)

errossem19 <- avs_semana19 %>%
  group_by(YearIndexMonth) %>%
  filter(n() > 1)

errossem21 <- avs_semana21 %>%
  group_by(YearIndexMonth) %>%
  filter(n() > 1)

errossem24 <- avs_semana24 %>%
  group_by(YearIndexMonth) %>%
  filter(n() > 1)

errossem25 <- avs_semana25 %>%
  group_by(YearIndexMonth) %>%
  filter(n() > 1)

errossem26 <- avs_semana26 %>%
  group_by(YearIndexMonth) %>%
  filter(n() > 1)

errossem30 <- avs_semana30 %>%
  group_by(YearIndexMonth) %>%
  filter(n() > 1)

errossem31 <- avs_semana31 %>%
  group_by(YearIndexMonth) %>%
  filter(n() > 1)

# semanas com erro: 1, 5, 9, 10, 11, 17, 18, 19, 21, 24, 25, 26, 30, 31

#### Parte 5.2 - MANIPULAÇÕES FINAIS: Separação de midpoints por semana ####

# Função para criar as midpoints por semana

for (i in seq_len(nrow(semanas))) {
  data_ini <- semanas$inicio_semana[i]
  data_fim <- semanas$fim_semana[i]
  numero_semana <- semanas$semana_estudo[i]
  
  # Filtrar as avistagens dessa semana
  mid_filtro <- midpoints %>% filter(Data >= data_ini & Data <= data_fim)
  
  # Criar objeto com nome avs_semana1, avs_semana2, ...
  assign(paste0("midpoints_semana", numero_semana), mid_filtro)
}
