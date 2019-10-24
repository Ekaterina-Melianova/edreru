library(sqldf)
library(XLConnectJars)
library(dplyr)

# Connect with SQLite
dbname <- "C:/Country/Russia/Data/SEABYTE/bus.gov/busgov.db"
db <- dbConnect(SQLite(), dbname=dbname)


busgov_SQL_to_R <- function(selected_region, dbname='C:/Country/Russia/Data/SEABYTE/bus.gov/busgov.db'){
  "
  Load from SQL to R.
  Input: region code as character or vector of regions.
  Output: dataframe with all variables from bus.gov.ru for specific region(s).

  Table with Region code and Region name
  01	Республика Адыгея
  02	Республика Башкортостан
  03	Республика Бурятия
  04	Республика Алтай
  05	Республика Дагестан
  06	Республика Ингушетия
  07	Кабардино-Балкарская Республика
  08	Республика Калмыкия
  09	Республика Карачаево-Черкесия
  10	Республика Карелия
  11	Республика Коми
  12	Республика Марий Эл
  13	Республика Мордовия
  14	Республика Саха (Якутия)
  15	Республика Северная Осетия — Алания
  16	Республика Татарстан
  17	Республика Тыва
  18	Удмуртская Республика
  19	Республика Хакасия
  20	Чеченская республика
  21	Чувашская Республика
  22	Алтайский край
  23	Краснодарский край
  24	Красноярский край
  25	Приморский край
  26	Ставропольский край
  27	Хабаровский край
  28	Амурская область
  29	Архангельская область
  30	Астраханская область
  31	Белгородская область
  32	Брянская область
  33	Владимирская область
  34	Волгоградская область
  35	Вологодская область
  36	Воронежская область
  37	Ивановская область
  38	Иркутская область
  39	Калининградская область
  40	Калужская область
  41	Камчатский край
  42	Кемеровская область
  43	Кировская область
  44	Костромская область
  45	Курганская область
  46	Курская область
  47	Ленинградская область
  48	Липецкая область
  49	Магаданская область
  50	Московская область
  51	Мурманская область
  52	Нижегородская область
  53	Новгородская область
  54	Новосибирская область
  55	Омская область
  56	Оренбургская область
  57	Орловская область
  58	Пензенская область
  59	Пермский край
  60	Псковская область
  61	Ростовская область
  62	Рязанская область
  63	Самарская область
  64	Саратовская область
  65	Сахалинская область
  66	Свердловская область
  67	Смоленская область
  68	Тамбовская область
  69	Тверская область
  70	Томская область
  71	Тульская область
  72	Тюменская область
  73	Ульяновская область
  74	Челябинская область
  75	Забайкальский край
  76	Ярославская область
  77	Москва
  78	Санкт-Петербург
  79	Еврейская автономная область
  83	Ненецкий автономный округ
  86	Ханты-Мансийский автономный округ Югра
  87	Чукотский автономный округ
  89	Ямало-Ненецкий автономный округ
  91	Республика Крым
  92	Севастополь
  "
  
  if (length(selected_region) > 1){
    result_df <- sqldf(paste('SELECT * from df_', selected_region[1], sep = ''), dbname = dbname)
    
    for (region_code in selected_region[2:length(selected_region)]){
      temp_df <- sqldf(paste('SELECT * from df_', region_code, sep = ''), dbname = dbname)
      result_df <- rbind.data.frame(result_df, temp_df)
    }
    
  } else {
    result_df <- sqldf(paste('SELECT * from df_', selected_region, sep = ''), dbname = dbname)
  }
  
  return(result_df)
}


add_eca_variables <- function(df, dbname='busgov.db'){
  "
  Keep only ECA related organizations and add Index results and coordinates to dataframe.
  Input: datafame from busgov_SQL_to_R function.
  Output: dataframe with eca related organizations and Index results and coordinates.
  "
  eca_df <- sqldf('SELECT * from organizations_with_eca', dbname = dbname)
  result_df <- merge.data.frame(df, eca_df, by='inn')
  
  return(result_df)
  
}


# Examples
test_example_1 <- busgov_SQL_to_R(selected_region = '01')
test_example_2 <- busgov_SQL_to_R(selected_region = c('02', '03', '04'))
test_example_3 <- busgov_SQL_to_R(selected_region = '50') %>% add_eca_variables


