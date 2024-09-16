# Definir o diretório de trabalho
setwd("D:/Paisagens VS/Script")

# Carregar os pacotes necessários
library(sf)
library(raster)
library(landscapemetrics)
library(dplyr)
library(tidyr)

# Lista de anos disponíveis (sem o ano 2016)
anos <- c(2008, 2010, 2012, 2014, 2018, 2020, 2022)

# Diretório onde estão os shapefiles
shape_dir <- "shapes/"  # Subpasta com os shapefiles

# Iniciar contagem de tempo
start_time <- Sys.time()

# Nome do arquivo geopackage de saída
output_gpkg <- "D:/Paisagens VS/Script/61464_VS_metricas_novo.gpkg"

# Loop para processar cada ano
for (ano in anos) {
  # Criar o nome do arquivo shapefile
  nome_arquivo <- paste0(shape_dir, "VS_bacia_61464_", ano, ".shp")
  
  # Passo 1: Carregar o shapefile
  shape_data <- st_read(nome_arquivo)
  
  # Verificar se o shapefile foi carregado corretamente
  if (nrow(shape_data) == 0) {
    warning(paste("Nenhum dado encontrado para o ano", ano))
    next
  }
  
  # Passo 2: Adicionar uma coluna de ID única ao shapefile de fragmentos
  shape_data$patch_id <- 1:nrow(shape_data)
  
  # Passo 3: Definir o tamanho do pixel (resolução) e criar o template do raster
  resolucao <- 30  # Ajuste a resolução conforme necessário
  raster_template <- raster(extent(shape_data), res = resolucao)
  
  # Passo 4: Converter o shapefile para raster, usando a coluna patch_id
  raster_data <- rasterize(shape_data, raster_template, field = "patch_id")
  
  # Passo 5: Calcular as métricas de paisagem
  metrics <- calculate_lsm(raster_data, 
                           what = c("lsm_p_area", "lsm_p_core", "lsm_p_frac", "lsm_p_prox"),
                           level = "patch")
  
  # Verificar se as métricas foram calculadas corretamente
  if (nrow(metrics) == 0) {
    warning(paste("Métricas não calculadas para o ano", ano))
    next
  }
  
  # Passo 6: Transformar as métricas em colunas separadas
  metrics_wide <- metrics %>%
    select(patch_id = id, metric, value) %>%
    pivot_wider(names_from = metric, values_from = value)
  
  # Adicionar uma coluna para o ano
  metrics_wide$ano <- ano
  
  # Passo 7: Converter o raster de volta para shapefile e unir com as métricas
  raster_polygons <- rasterToPolygons(raster_data, dissolve = TRUE)
  fragments_sf <- st_as_sf(raster_polygons)
  
  # Manter o patch_id no shapefile
  fragments_sf$patch_id <- 1:nrow(fragments_sf)
  
  # Unir as métricas ao shapefile usando os IDs dos fragmentos (patch_id)
  result_sf <- fragments_sf %>%
    left_join(metrics_wide, by = "patch_id")
  
  # Certificar que a projeção seja a mesma dos dados de entrada
  st_crs(result_sf) <- st_crs(shape_data)
  
  # Salvar cada camada correspondente ao ano no GeoPackage, sem sobrescrever
  st_write(result_sf, output_gpkg, layer = paste0("VS_metricas_", ano), delete_layer = FALSE, append = TRUE)
}

# Parar contagem de tempo e exibir a duração
end_time <- Sys.time()
duration <- end_time - start_time

print(paste("Tempo total de processamento: ", duration))
