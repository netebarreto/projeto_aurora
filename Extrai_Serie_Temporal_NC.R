
#####
#
# CRIADA EM 04-09-2025 
# Extrai a serie temporal de um arquivo do tipo ncdf

####
rm(list=ls())

# inserir em path_dir a localização do diretório de trabalho
path_dir = "XXX"
setwd(path_dir)

# Pacotes necessários

library(ncdf4)
library(dplyr)
library(lubridate)

# Função para converter tempo do NetCDF em datas reais
converter_tempo <- function(tempo, units) {
  # Exemplo típico: "days since 1900-01-01" ou "hours since 1970-01-01"
  partes <- strsplit(units, " since ")[[1]]
  escala <- partes[1]
  origem <- ymd_hms(paste0(substr(partes[2],1,10), " 00:00:00"))
  
  if (escala == "days") {
    return(origem + days(tempo))
  } else if (escala == "hours") {
    return(origem + hours(tempo))
  } else if (escala == "minutes") {
    return(origem + minutes(tempo))
  } else if (escala == "seconds") {
    return(origem + seconds(tempo))
  } else {
    warning("Escala de tempo não reconhecida, retornando valores brutos")
    return(tempo)
  }
}

# Função para extrair série temporal
extrair_serie <- function(arquivo_nc, varname, lat_alvo, lon_alvo) {
  
  # Abrir o NetCDF
  nc <- nc_open(arquivo_nc)
  
  # Ler as dimensões
  lats <- ncvar_get(nc, "latitude")
  lons <- ncvar_get(nc, "longitude")
  tempo <- ncvar_get(nc, "time")
  tempo_units <- ncatt_get(nc, "time", "units")$value
  
  # Converter tempo
  tempo_convertido <- converter_tempo(tempo, tempo_units)
  
  # Encontrar os índices mais próximos da coordenada desejada
  idx_lat <- which.min(abs(lats - lat_alvo))
  idx_lon <- which.min(abs(lons - lon_alvo))
  
  # Extrair variável na posição [lon, lat, tempo]
  dados <- ncvar_get(nc, varname, start = c(idx_lon, idx_lat, 1), 
                     count = c(1, 1, -1))
  
  # Fechar o arquivo
  nc_close(nc)
  
  # Criar data frame com série temporal
  df <- data.frame(
    Ano = substr(tempo_convertido,1,4),
    Mes = substr(tempo_convertido,6,7),
    Dia = substr(tempo_convertido,9,10),
    valor = round(dados,1)
  )
  colnames(df)[4] <- varname
  
  return(df)
}

# -------------------------------
# Definição das variaveis 
## path0 é o diretório onde se encontram os dados 
path0="../../../XXX/ZZZ/"
files = c("Tmax.nc","Tmax_2.nc")
variavel <- "Tmax"    # nome da variável no NetCDF
lat <- c(-1.3932,0.1007,-3.11889,2.8198,-8.7629,-9.9744,-10.23)
lon <- c(-48.38,-50.94,-60.04,-60.67,-63.89,-67.80,-48.33)            
local <- c("Belem-PA","Macapa-AP","Manaus-AM","Boa_Vista-RR","Porto_Velho-RO","Rio_Branco-AC","Palmas-TO")
serie_1 = NULL

## loop para executar todas as series ao mesmo tempo 

for(i in 1:7)
{
    for(k in files)
    { 
        serie <- extrair_serie(paste0(path0,k), variavel, lat[i], lon[i])
        serie_1<-rbind(serie_1,serie)
        write.table(serie_1,file=paste0("CSV/TMAX_",local[i],"_19810101_20240320.csv"),sep=";",dec=".",row.names=FALSE)
    }
}
