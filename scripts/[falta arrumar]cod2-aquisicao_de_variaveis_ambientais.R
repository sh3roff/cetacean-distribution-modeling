###################################  AQUISIÇÃO DE VARIVÁVEIS AMBIENTAIS ###################################


#### Parte 4 - VARIABLES_ACQUISITION: Vetor da linha de costa ####

Coast = load_Coastline() # carregando linha de costa

limpeninsula_geo <- st_bbox(c(xmin = -3500000, xmax = -1200000,   # definindo limites da península pelo EPSG:3031
                              ymin = 1000000, ymax = 2600000), 
                            crs = st_crs(6932))

limpeninsula_sf <- st_as_sfc(limpeninsula_geo) # transformando o limite em objeto sf

Lonzero = -60 # considerando longitude -60 como a longitude zero 
coastrot <- Rotate_obj(Coast, Lonzero) # aponta longitude -60 para cima : NÃO ESTÁ FUNCIONANDO!
coastrot <- st_transform(st_as_sf(coastrot), crs = 6932) # transformando a linha de costa para um objeto sf

coastrot_vect <- vect(coastrot) # transformando a linha de costa para um objeto terra
limpeninsula_ext <- ext(as.numeric(c(limpeninsula_geo$xmin,  # definindo a extensão de um objeto terra
                                     limpeninsula_geo$xmax, 
                                     limpeninsula_geo$ymin, 
                                     limpeninsula_geo$ymax)))

corte <- crop(coastrot_vect, limpeninsula_ext) # cortando a linha de costa para os limites da península

mapa <- st_as_sf(corte) # transformando a linha de costa para um objeto sf novamente

bbox <- st_bbox(mapa) # definindo bbox (limites do mapa)

bx=st_as_sfc(bbox) # transforma o bbox em um tipo de objeto sf também, pra plotar um fundo azul

linhas <- st_read("shp_effort_transects.shp")

map_land <- mapa[mapa$surface == "Land", ] # filtrando somente a área coberta por terra
map_ice <- mapa[mapa$surface == "Ice", ] # filtrando somente a área coberta por gelo

# criando mapa da área de estudo
png(filename="z_studyarea_map.png",width=2200,height=2400,res=300)
par(mai = rep(0.01, 4), bg = "white")
plot(bx,col="lightblue")
plot(mapa$geometry, add = TRUE)
plot(map_land$geometry, col="grey", add=TRUE)
plot(map_ice$geometry, col="white", add=TRUE)
add_RefGrid(bb=bbox,ResLat = 2.5,ResLon = 5,lwd=1,fontsize = 0.75, lcol = adjustcolor("black", alpha.f = 0.3))
dev.off()

# criando mapa da área de estudo com esforço
png(filename="z_studyarea_map_WITH-EFFORT-LINES.png",width=2200,height=2400,res=300)
par(mai = rep(0.01, 4), bg = "white")
plot(bx,col="lightblue")
plot(mapa$geometry, add = TRUE)
plot(map_land$geometry, col="grey", add=TRUE)
plot(map_ice$geometry, col="white", add=TRUE)
plot(linhas, col = "red", add = TRUE)
add_RefGrid(bb=bbox,ResLat = 2.5,ResLon = 5,lwd=1,fontsize = 0.75, lcol = adjustcolor("black", alpha.f = 0.3))
dev.off()

shp_coast <- st_cast(mapa, "MULTILINESTRING") # Transformando para vetor

st_geometry_type(shp_coast) # Formato multilinestring

# criando vetor da linha de costa delimitada
st_write(shp_coast, 
         "shp_coastline_delimited.shp", 
         delete_layer = TRUE) 

# criando vetor do polígono de gelo
st_write(map_ice, 
         "shp_ice_polygon.shp", 
         delete_layer = TRUE) 

# criando vetor do polígono de terra
st_write(map_land, 
         "shp_land_polygon.shp", 
         delete_layer = TRUE) 

#### Parte 4.1 - VARIABLES_ACQUISITION: Batimetria - Profundidade, Isóbatas e Declive ####

# BATIMETRIA - PROFUNDIDADE

batimetria <- rast("bathimetry.tif")

batimetria <- project(batimetria, "EPSG:6932")

batimetria_cortada <- crop(batimetria, limpeninsula_ext)
batimetria_cortada <- ifel(batimetria_cortada <= 0, batimetria_cortada, NA)

profundidade <- mask(batimetria_cortada, limpeninsula_ext)

# DECLIVE

declive <- rast("slope.tif")

# LINHA DE COSTA
iso0 <- st_read("add_coastline_high_res_line_v7_10.shp")
iso0 <- st_as_sf(iso0)

# ISÓBATA 200m
iso200 <- st_read("iso_200.shp")
iso200 <-  st_as_sf(iso200)

# ISÓBATA 400m
iso400 <- st_read("iso_400.shp")
iso400 <-  st_as_sf(iso400)

# ISÓBATA 500m
iso500 <- st_read("iso_500.shp")
iso500 <-  st_as_sf(iso500)

# transformando os objetos para terem mesmo CRS
iso0_raster <- st_transform(iso0, crs = st_crs(batimetria))
iso200_raster <- st_transform(iso200, crs = st_crs(batimetria))
iso400_raster <- st_transform(iso400, crs = st_crs(batimetria))
iso500_raster <- st_transform(iso500, crs = st_crs(batimetria))

# uso da função "rasterize" para transformar em raster
iso0_rasterized <- rasterize(iso0_raster, batimetria, field = 1)
iso200_rasterized <- rasterize(iso200_raster, batimetria, field = 1)
iso400_rasterized <- rasterize(iso400_raster, batimetria, field = 1)
iso500_rasterized <- rasterize(iso500_raster, batimetria, field = 1)

# uso da função "distance" para o cálculo da distância de cada um dos pixels do raster para cada um dos objetos
dist_iso0 <- distance(iso0_rasterized) 
dist_iso200 <- distance(iso200_rasterized)
dist_iso400 <- distance(iso400_rasterized)
dist_iso500 <- distance(iso500_rasterized)

# cortando os rasters para o limite da península
dist_costa_cropped <- crop(dist_iso0, limpeninsula_ext)
dist_iso200_cropped <- crop(dist_iso200, limpeninsula_ext)
dist_iso400_cropped <- crop(dist_iso400, limpeninsula_ext)
dist_iso500_cropped <- crop(dist_iso500, limpeninsula_ext)

# Criar intervalos de 10 metros para coloração
dist_costa_cropped[] <- cut(dist_costa_cropped[], breaks = seq(0, max(dist_costa_cropped[], na.rm = TRUE), by = 10), include.lowest = TRUE)
dist_iso200_cropped[] <- cut(dist_iso200_cropped[], breaks = seq(0, max(dist_iso200_cropped[], na.rm = TRUE), by = 10), include.lowest = TRUE)
dist_iso400_cropped[] <- cut(dist_iso400_cropped[], breaks = seq(0, max(dist_iso400_cropped[], na.rm = TRUE), by = 10), include.lowest = TRUE)
dist_iso500_cropped[] <- cut(dist_iso500_cropped[], breaks = seq(0, max(dist_iso500_cropped[], na.rm = TRUE), by = 10), include.lowest = TRUE)

# CRIAÇÃO DOS OBJETOS

writeRaster(profundidade, "raster_profundidade.tif", overwrite = TRUE)
writeRaster(dist_costa_cropped, "raster_dist_costa.tif", overwrite = TRUE)
writeRaster(dist_iso200_cropped, "raster_dist_iso200.tif", overwrite = TRUE)
writeRaster(dist_iso400_cropped, "raster_dist_iso400.tif", overwrite = TRUE)
writeRaster(dist_iso500_cropped, "raster_dist_iso500.tif", overwrite = TRUE)
writeRaster(declive, "raster_slope.tif", overwrite = TRUE)

#### Parte 4.2 - VARIABLES_ACQUISITION: Variáveis dinâmicas - Acesso ao Copernicus Marine Toolbox ####

# repete toda vez que for reiniciar o script (tipo mexer em um outro dia). A última linha precisa responder um "y" no console antes de continuar

#py_config() # instalando o python (só a primeira vez)
virtualenv_create(envname = "CopernicusMarine") # criando ambiente virtual
#virtualenv_install("CopernicusMarine", packages = c("copernicusmarine")) # instalando o pacote toolbox da copernicus
reticulate::use_virtualenv("CopernicusMarine", required = TRUE) # configurando o ambiente virtual
py_config() # vendo se tá no local certo

cmt <- import("copernicusmarine") # importando a biblioteca da copernicus no objeto "cmt"

cmt$login("smagalhaesdossa", "Pintinh0Petit0Cacau") # logando com meu usuário e senha da copernicus

semanas <- read.csv("semanas.csv", dec = ".", sep = ",")

#### Parte 4.3 - VARIABLES_ACQUISITION: Variáveis dinâmicas - Temperatura da água na superfície (SST) ####

# retirado do link: https://data.marine.copernicus.eu/product/SST_GLO_SST_L4_REP_OBSERVATIONS_010_011/description

# esse código do "cmt$subset" pode ser rodado só uma vez por pc, depois que tiver 
# baixado direitinho é só ir direto pra linha com o comando "cmt$open_dataset".
# Renomeei cada imagem depois de baixar, porque ficavam com nomes grandes e "sem1" [...] é mais intuitivo

# IMAGEM DE TEMPERATURA COM MÉDIA DA SEMANA 1

cmt$subset( # baixando as imagens de temperatura, o id do objeto é esse nome grande, dá de achar pelo site deles
  dataset_id="METOFFICE-GLO-SST-L4-REP-OBS-SST",
  minimum_longitude=-85,  
  maximum_longitude=-48, 
  minimum_latitude=-72,  
  maximum_latitude=-58, 
  start_datetime="2013-02-11T00:00:00", 
  end_datetime="2013-02-17T23:59:59",
  output_directory = "C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/sst"
)

dataset_info <- cmt$open_dataset("METOFFICE-GLO-SST-L4-REP-OBS-SST") # informações do dataset

print(dataset_info$variables) # variáveis disponíveis no dataset, as informações de temperatura têm como símbolo o "analysed_sst"

cmt$open_dataset("METOFFICE-GLO-SST-L4-REP-OBS-SST") # abrindo o dataset

# acessando as imagens netcdf pela biblioteca ncdf4

# antes do passo abaixo, trocar por "sem1" o arquivo METOFFICE criado pela função cmt$open_dataset
nc_file <- nc_open("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/sst/sem1.nc")
sst_data <- ncvar_get(nc_file, "analysed_sst") # pegando as informações de temperatura
nc_close(nc_file)  # fechando/terminando a operação
nc_file <- raster("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/sst/sem1.nc", varname="analysed_sst")

# Salvar como GeoTIFF

writeRaster(nc_file, filename="C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/sst/_sem1_sst.tif", format="GTiff", overwrite=TRUE)

#LOOP COM DEMAIS SEMANAS: DE 2 A 31

for (i in 2:nrow(semanas)) {
  cmt$subset(
    dataset_id = "METOFFICE-GLO-SST-L4-REP-OBS-SST",
    minimum_longitude = -85,  
    maximum_longitude = -48, 
    minimum_latitude = -72,  
    maximum_latitude = -58, 
    start_datetime = paste0(semanas$inicio_semana[i], "T00:00:00"), 
    end_datetime = paste0(semanas$fim_semana[i], "T23:59:59"),
    output_directory = paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/sst/sem", i)
  )
}

# loop para criar tiff dos netCDF

for (i in 2:31) {
  nc_file <- raster(paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/sst/", "sem", i, ".nc"), varname = "analysed_sst")
  writeRaster(nc_file, filename = paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/sst/", "_sem", i, "_sst.tif"), format = "GTiff", overwrite = TRUE)
}

#### Parte 4.4 - VARIABLES_ACQUISITION: Variáveis dinâmicas - Salinidade da Água na Superfície (SSS)####

# retirado do link: https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/description

# esse código do "cmt$subset" pode ser rodado só uma vez por pc, depois que tiver 
# baixado direitinho é só ir direto pra linha com o comando "cmt$open_dataset".
# Renomeei cada imagem depois de baixar, porque ficavam com nomes grandes e "sem1" [...] é mais intuitivo

# IMAGEM DE SALINIDADE COM MÉDIA DA SEMANA 1

cmt$subset(  # baixando as imagens de salinidade, o id do objeto é esse nome grande, dá de achar pelo site deles
  dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
  variables=list("so"),
  minimum_longitude=-85,  
  maximum_longitude=-48, 
  minimum_latitude=-68,  
  maximum_latitude=-58, 
  start_datetime="2013-02-11T00:00:00", 
  end_datetime="2013-02-17T23:59:59",
  output_directory = "C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/sss"
)

dataset_info <- cmt$open_dataset("cmems_mod_glo_phy_my_0.083deg_P1D-m") # informações do dataset

print(dataset_info$variables) # variáveis disponíveis no dataset, as informações de salinidade têm como símbolo o "so"

cmt$open_dataset("cmems_mod_glo_phy_my_0.083deg_P1D-m") # abrindo o dataset

# acessando as imagens netcdf pela biblioteca ncdf4
nc_file <- nc_open("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/sss/sem1.nc")
sss_data <- ncvar_get(nc_file, "so") # pegando as informações de salinidade

nc_close(nc_file) # fechando/terminando a operação

nc_file <- raster("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/sss/sem1.nc", varname="so")
# Salvar como GeoTIFF
writeRaster(nc_file, filename="C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/sss/_sem1_sss.tif", format="GTiff", overwrite=TRUE)

#LOOP COM DEMAIS SEMANAS: DE 2 A 31

for (i in 2:nrow(semanas)) {
  cmt$subset(
    dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1D-m",
    variables=list("so"),
    minimum_longitude = -85,  
    maximum_longitude = -48, 
    minimum_latitude = -72,  
    maximum_latitude = -58, 
    start_datetime = paste0(semanas$inicio_semana[i], "T00:00:00"), 
    end_datetime = paste0(semanas$fim_semana[i], "T23:59:59"),
    output_directory = paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/sss/sem", i)
  )
}

# loop para criar tiff dos netCDF

for (i in 2:31) {
  nc_file <- raster(paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/sss/", "sem", i, ".nc"), varname = "so")
  writeRaster(nc_file, filename = paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/sss/", "_sem", i, "_sss.tif"), format = "GTiff", overwrite = TRUE)
}

#### Parte 4.5 - VARIABLES_ACQUISITION: Variáveis dinâmicas - Clorofila (CHL)####

# retirado do link: https://data.marine.copernicus.eu/product/OCEANCOLOUR_GLO_BGC_L4_MY_009_104/description
# plankton, 4km

# esse código do "cmt$subset" pode ser rodado só uma vez por pc, depois que tiver 
# baixado direitinho é só ir direto pra linha com o comando "cmt$open_dataset".
# Renomeei cada imagem depois de baixar, porque ficavam com nomes grandes e "sem1" [...] é mais intuitivo

# IMAGEM DE CLOROFILA COM MÉDIA DA SEMANA 1

cmt$subset(  # baixando as imagens de clorofila, o id do objeto é esse nome grande, dá de achar pelo site deles
  dataset_id="cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D",
  minimum_longitude=-85,  
  maximum_longitude=-48, 
  minimum_latitude=-72,  
  maximum_latitude=-58, 
  start_datetime="2013-02-11T00:00:00", 
  end_datetime="2013-02-17T23:59:59",
  output_directory = "C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/chl"
)

dataset_info <- cmt$open_dataset("cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D") # informações do dataset

print(dataset_info$variables) # variáveis disponíveis no dataset, as informações de clorofila têm como símbolo o "CHL"

cmt$open_dataset("cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D") # abrindo o dataset

# acessando as imagens netcdf pela biblioteca ncdf4
nc_file <- nc_open("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/chl/sem1.nc")

chl_data <- ncvar_get(nc_file, "CHL") # pegando as informações de clorofila

nc_close(nc_file) # fechando/terminando a operação

nc_file <- raster("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/chl/sem1.nc", varname="CHL")
# Salvar como GeoTIFF
writeRaster(nc_file, filename="C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/chl/_sem1_chl.tif", format="GTiff", overwrite=TRUE)


#LOOP COM DEMAIS SEMANAS: DE 2 A 31

for (i in 2:nrow(semanas)) {
  cmt$subset(
    dataset_id = "cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D",
    minimum_longitude = -85,  
    maximum_longitude = -48, 
    minimum_latitude = -72,  
    maximum_latitude = -58, 
    start_datetime = paste0(semanas$inicio_semana[i], "T00:00:00"), 
    end_datetime = paste0(semanas$fim_semana[i], "T23:59:59"),
    output_directory = paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/chl/sem", i)
  )
}

# loop para criar tiff dos netCDF

for (i in 2:31) {
  nc_file <- raster(paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/chl/", "sem", i, ".nc"), varname = "CHL")
  writeRaster(nc_file, filename = paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/chl/", "_sem", i, "_chl.tif"), format = "GTiff", overwrite = TRUE)
}

#### Parte 4.6 - VARIABLES_ACQUISITION: Variáveis dinâmicas - Profundidade da Camada de Mistura (MLD) ####

# retirado do link: https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/description

# esse código do "cmt$subset" pode ser rodado só uma vez por pc, depois que tiver 
# baixado direitinho é só ir direto pra linha com o comando "cmt$open_dataset".
# Renomeei cada imagem depois de baixar, porque ficavam com nomes grandes e "sem1" [...] é mais intuitivo

# IMAGEM DE PROF. DA CAMADA DE MISTURA COM MÉDIA DA SEMANA 1

cmt$subset(  # baixando as imagens de prof. da camada de mistura, o id do objeto é esse nome grande, dá de achar pelo site deles
  dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
  variables=list("mlotst"),
  minimum_longitude=-85,  
  maximum_longitude=-48, 
  minimum_latitude=-72,  
  maximum_latitude=-58, 
  start_datetime="2013-02-11T00:00:00", 
  end_datetime="2013-02-17T23:59:59",
  output_directory = "C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/mld"
)

dataset_info <- cmt$open_dataset("cmems_mod_glo_phy_my_0.083deg_P1D-m") # informações do dataset

print(dataset_info$variables) # variáveis disponíveis no dataset, as informações da camada de mistura têm como símbolo o "m"

cmt$open_dataset("cmems_mod_glo_phy_my_0.083deg_P1D-m") # abrindo o dataset

# acessando as imagens netcdf pela biblioteca ncdf4
nc_file <- nc_open("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/mld/sem1.nc")
mld_data <- ncvar_get(nc_file, "mlotst") # pegando as informações de camada de mistura

nc_close(nc_file) # fechando/terminando a operação

nc_file <- raster("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/mld/sem1.nc", varname="mlotst")
# Salvar como GeoTIFF
writeRaster(nc_file, filename="C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/mld/_sem1_mld.tif", format="GTiff", overwrite=TRUE)

#LOOP COM DEMAIS SEMANAS: DE 2 A 31

for (i in 2:nrow(semanas)) {
  cmt$subset(
    dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1D-m",
    variables=list("mlotst"),
    minimum_longitude = -85,  
    maximum_longitude = -48, 
    minimum_latitude = -72,  
    maximum_latitude = -58, 
    start_datetime = paste0(semanas$inicio_semana[i], "T00:00:00"), 
    end_datetime = paste0(semanas$fim_semana[i], "T23:59:59"),
    output_directory = paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/mld/sem", i)
  )
}

# loop para criar tiff dos netCDF

for (i in 2:31) {
  nc_file <- raster(paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/mld/", "sem", i, ".nc"), varname = "mlotst")
  writeRaster(nc_file, filename = paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/mld/", "_sem", i, "_mld.tif"), format = "GTiff", overwrite = TRUE)
}

#### Parte 4.7 - VARIABLES_ACQUISITION: Variáveis dinâmicas - Velocidade da água direção Norte (VO)####

# retirado do link: https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/description

# IMAGEM DE VO COM MÉDIA DA SEMANA 1

cmt$subset(  
  dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
  variables=list("vo"),
  minimum_longitude=-85,  
  maximum_longitude=-48, 
  minimum_latitude=-68,  
  maximum_latitude=-58, 
  start_datetime="2013-02-11T00:00:00", 
  end_datetime="2013-02-17T23:59:59",
  output_directory = "C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/vo"
)

dataset_info <- cmt$open_dataset("cmems_mod_glo_phy_my_0.083deg_P1D-m") # informações do dataset

print(dataset_info$variables) # variáveis disponíveis no dataset, as informações de salinidade têm como símbolo o "so"

cmt$open_dataset("cmems_mod_glo_phy_my_0.083deg_P1D-m") # abrindo o dataset

# acessando as imagens netcdf pela biblioteca ncdf4
nc_file <- nc_open("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/vo/sem1.nc")
vo_data <- ncvar_get(nc_file, "vo") # pegando as informações de salinidade
nc_close(nc_file) # fechando/terminando a operação

range(tabelao$uo)

nc_file <- raster("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/vo/sem1.nc", varname="vo")
# Salvar como GeoTIFF
writeRaster(nc_file, filename="C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/vo/_sem1_vo.tif", format="GTiff", overwrite=TRUE)

#LOOP COM DEMAIS SEMANAS: DE 2 A 31

for (i in 2:nrow(semanas)) {
  cmt$subset(
    dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1D-m",
    variables=list("vo"),
    minimum_longitude = -85,  
    maximum_longitude = -48, 
    minimum_latitude = -72,  
    maximum_latitude = -58, 
    start_datetime = paste0(semanas$inicio_semana[i], "T00:00:00"), 
    end_datetime = paste0(semanas$fim_semana[i], "T23:59:59"),
    output_directory = paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/vo/sem", i)
  )
}

# loop para criar tiff dos netCDF

for (i in 2:31) {
  nc_file <- raster(paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/vo/", "sem", i, ".nc"), varname = "vo")
  writeRaster(nc_file, filename = paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/vo/", "_sem", i, "_vo.tif"), format = "GTiff", overwrite = TRUE)
}


#### Parte 4.8 - VARIABLES_ACQUISITION: Variáveis dinâmicas - Velocidade da água direção Leste (UO)####

# retirado do link: https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/description

# IMAGEM DE UO COM MÉDIA DA SEMANA 1

cmt$subset(  
  dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
  variables=list("uo"),
  minimum_longitude=-85,  
  maximum_longitude=-48, 
  minimum_latitude=-68,  
  maximum_latitude=-58, 
  start_datetime="2013-02-11T00:00:00", 
  end_datetime="2013-02-17T23:59:59",
  output_directory = "C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/uo"
)

dataset_info <- cmt$open_dataset("cmems_mod_glo_phy_my_0.083deg_P1D-m") # informações do dataset

print(dataset_info$variables) # variáveis disponíveis no dataset, as informações de salinidade têm como símbolo o "so"

cmt$open_dataset("cmems_mod_glo_phy_my_0.083deg_P1D-m") # abrindo o dataset

# acessando as imagens netcdf pela biblioteca ncdf4
nc_file <- nc_open("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/uo/sem1.nc")
uo_data <- ncvar_get(nc_file, "uo") # pegando as informações de salinidade

nc_close(nc_file) # fechando/terminando a operação

nc_file <- raster("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/uo/sem1.nc", varname="uo")
# Salvar como GeoTIFF
writeRaster(nc_file, filename="C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/uo/_sem1_uo.tif", format="GTiff", overwrite=TRUE)

#LOOP COM DEMAIS SEMANAS: DE 2 A 31

for (i in 2:nrow(semanas)) {
  cmt$subset(
    dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1D-m",
    variables=list("uo"),
    minimum_longitude = -85,  
    maximum_longitude = -48, 
    minimum_latitude = -72,  
    maximum_latitude = -58, 
    start_datetime = paste0(semanas$inicio_semana[i], "T00:00:00"), 
    end_datetime = paste0(semanas$fim_semana[i], "T23:59:59"),
    output_directory = paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/uo/sem", i)
  )
}

# loop para criar tiff dos netCDF

for (i in 2:31) {
  nc_file <- raster(paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/uo/", "sem", i, ".nc"), varname = "uo")
  writeRaster(nc_file, filename = paste0("C:/Users/ProjetoBaleias/Cloud-Drive/intermediário/uo/", "_sem", i, "_uo.tif"), format = "GTiff", overwrite = TRUE)
}
