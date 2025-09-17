

library(dplyr)
library(lubridate)


library(ggplot2)
library(tidyr)

path= "Documents/NETE_PROJETOS/AB03-PROJETOS_TERCEIROS/LEIDIANE/LARISSA/"

setwd(path)


ll = c("Belem-PA","Boa_Vista-RR","Macapa-AP","Manaus-AM","Palmas-TO",
"Porto_Velho-RO","Rio_Branco-AC")

local = ll[7]


input = read.table(paste0("CSV_XAVIER/TMAX_",local,"_19810101_20240320.csv"),sep=";",dec=".",header=T)

# Extrair o dia juliano
df <- input %>%
  mutate(Date = as.Date(paste(input$Ano,input$Mes,input$Dia,sep="-")))%>%
  filter(!(month(Date) == 2 & day(Date) == 29))

df <- df %>%
     mutate(Jday=yday(Date)) %>%
     select(Ano,Mes,Dia,Date,Jday,Tmax)

# Função para calcular percentil 90 com janela de 5 dias (circular)
calc_p <- function(doy, tmax,prob) {
  setNames(
    sapply(1:366, function(d) {
      # janela centralizada: d-2 até d+2
      window <- ((d-2):(d+2))
      # Ajusta para dias circulares (1 a 366)
      window <- ((window - 1) %% 365) + 1
      # Filtrar dados do dia juliano dentro da janela
      vals <- df$Tmax[df$Jday %in% window]
      quantile(vals, prob, na.rm = TRUE)
    }),
    1:366
  )
}

calc_heatwaves <- function(data,a=NULL, min_length = 3) {
  n1 = which(colnames(data)==a)
  rle_exceed <- rle(data[,n1])

  # identificar onde há sequências longas de excedência
  heat_ids <- rep(0, length(rle_exceed$values))
  heat_ids[rle_exceed$values == 1 & rle_exceed$lengths >= min_length] <- 1

  # numerar as ondas (1, 2, 3, ...)
  heat_ids <- cumsum(heat_ids)
  heat_ids[rle_exceed$values == 0] <- 0

  # expandir de volta para o comprimento original
  ids <- inverse.rle(list(lengths = rle_exceed$lengths, values = heat_ids))

  data %>%
    mutate(heatwave_id = ids) %>%
    group_by(heatwave_id) %>%
    filter(heatwave_id > 0) %>%
    summarise(
      year = first(Ano),
      start = first(Date),
      end = first(Date) + n(),
      duration = n(),
      Tmax_max = max(Tmax, na.rm = TRUE),
      Tmax_mean = mean(Tmax, na.rm = TRUE),
      Txm_jday = mean(p90, na.rm = TRUE),      
      .groups = "drop") %>%
     arrange(year)
}


# Calcular p90 por DOY
p50_jday <- calc_p(df$Jday, df$Tmax,0.5)
p90_jday <- calc_p(df$Jday, df$Tmax,0.9)
p95_jday <- calc_p(df$Jday, df$Tmax,0.95)
p98_jday <- calc_p(df$Jday, df$Tmax,0.98)

df$p50 <- p50_jday[df$Jday] 
df$p90 <- p90_jday[df$Jday] 
df$p95 <- p95_jday[df$Jday] 
df$p98 <- p98_jday[df$Jday] 

df$exceedP90 <- ifelse(df$Tmax > df$p90, 1, 0)
df$exceedP95 <- ifelse(df$Tmax > df$p95, 1, 0)
df$exceedP98 <- ifelse(df$Tmax > df$p98, 1, 0)

ondas_p90 = calc_heatwaves(df,a="exceedP90",3)
ondas_p95 = calc_heatwaves(df,a="exceedP95",3)
ondas_p98 = calc_heatwaves(df,a="exceedP98",3)

# Calcular o número total de ondas de calor por ano (usando ondas_p90)
anos_todos <- sort(unique(df$Ano))

# Número de ondas por ano (preenchendo com zero onde não houve eventos)
n_ondas_ano <- ondas_p90 %>%
  group_by(year) %>%
  summarise(n_ondas = n(),
  dur_max = max(duration), .groups = "drop") %>%
  right_join(data.frame(year = anos_todos), by = "year") %>%
  mutate(n_ondas = ifelse(is.na(n_ondas), 0, n_ondas),
        dur_max = ifelse(is.na(dur_max), 0, dur_max)) %>%
  arrange(year)



# Número de ondas por ano (preenchendo com zero onde não houve eventos)
n_ondas_ano <- ondas_p90 %>%
  group_by(year) %>%
  summarise(
    n_ondas = n(),
    dur_max = max(duration, na.rm = TRUE),
    Tx_max = max(Tmax_max, na.rm = TRUE),
    Tx_mean = mean(Tmax_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(data.frame(year = anos_todos), by = "year") %>%
  mutate(
    n_ondas = ifelse(is.na(n_ondas), 0, n_ondas),
    dur_max = ifelse(is.na(dur_max), 0, dur_max),
  ) %>%
  arrange(year)


######## Gerar Figuras 


# Organizar os dados em formato longo
box_data <- df %>%
  select(p50, p90, p95, p98) %>%
  pivot_longer(cols = everything(), names_to = "Percentil", values_to = "Valor") %>%
  mutate(Percentil = recode(Percentil,
                            p50 = "Mediana",
                            p90 = "Percentil 90",
                            p95 = "Percentil 95",
                            p98 = "Percentil 98"))

# Gerar o boxplot
p <- ggplot(box_data, aes(x = Percentil, y = Valor, fill = Percentil)) +
  geom_boxplot() +
  theme_minimal() +
  theme_bw() +
  labs(title = paste0("Boxplot dos percentis diários \n",local), y = "Temperatura (°C)", x = "Percentil")

ggsave(paste0("FIGS/",local,"_boxplot_percentis.png"), plot = p, width = 12, height = 6, dpi = 300)
####### 



# Organizar dados em formato longo
n_ondas_ano_long <- n_ondas_ano %>%
  pivot_longer(cols = c(n_ondas, dur_max, Tx_max, Tx_mean),
               names_to = "Variavel", values_to = "Valor")%>%
  mutate(Variavel = recode(Variavel,
                            dur_max = "Duração Máxima",
                            n_ondas = "Número de Ondas de Calor",
                            Tx_max = "Temperatura Maxima Absoluta",
                            Tx_mean = "Temperatura Maxima Média"))

# Customizar cores para os dois primeiros frames (barras)
bar_colors <- c("Número de Ondas de Calor" = "red", "Duração Máxima" = "orange")

p_linhas_facet <- ggplot(n_ondas_ano_long, aes(x = year, y = Valor)) +
  # Barras para os dois primeiros frames
  geom_col(data = subset(n_ondas_ano_long, Variavel %in% c("Número de Ondas de Calor", "Duração Máxima")),
           aes(fill = Variavel), show.legend = FALSE) +
  scale_fill_manual(values = bar_colors) +
  # Linhas e pontos para os demais frames
  geom_line(data = subset(n_ondas_ano_long, Variavel %in% c("Temperatura Maxima Absoluta", "Temperatura Maxima Média")),
            color = "#0072B2", size = 1) +
  geom_point(data = subset(n_ondas_ano_long, Variavel %in% c("Temperatura Maxima Absoluta", "Temperatura Maxima Média")),
             color = "#D55E00") +
  facet_wrap(~Variavel, scales = "free_y", ncol = 2) +
  theme_minimal() +
  theme_bw() +
  labs(title = paste0("Evolução anual das ondas de calor - ", local),
       x = "Ano", y = "Valor")

ggsave(paste0("FIGS/", local, "_linhas_ondas_ano_facet.png"), plot = p_linhas_facet, width = 16, height = 8, dpi = 300)
