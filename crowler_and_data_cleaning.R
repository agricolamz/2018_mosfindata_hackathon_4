# https://github.com/infoculture/mosfindata/issues/4
library(rvest)
library(tidyverse)

# crowler -----------------------------------------------------------------

final <- data_frame(municipality = "",
                    category = NA,
                    value = NA)
final <- final[FALSE,]
results <- final


sapply(1:146, function(id){
link <- paste0("http://budget.mos.ru/new_passport?mo=", id)

source <- read_html(link)

source %>% 
  html_nodes(".municipal_block > h3:nth-child(1)") %>% 
  html_text() ->
  municipality

# Доходы
n <- 2
repeat {
css_text <- paste0("div.mo:nth-child(5) > div:nth-child(1) > table:nth-child(3) > tbody:nth-child(1) > tr:nth-child(", 
                 n, 
                 ") > td:nth-child(1)")

css_value <- paste0("div.mo:nth-child(5) > div:nth-child(1) > table:nth-child(3) > tbody:nth-child(1) > tr:nth-child(", 
                     n, 
                     ") > td:nth-child(3)")  
source %>% 
    html_nodes(css_text) %>%
    html_text() ->
    text
if(length(text) == 0) {
  break
} else if (text == "Итого") {
  text <- "Доходы: итого"
}
  final[1, 1] <- municipality
  final[1, 2] <- text
  source %>% 
    html_nodes(css_value) %>%
    html_text() %>% 
    str_replace(",", ".") %>% 
    as.numeric() ->
    final[1, 3]
  results <<- rbind(results, final)
  n <- n + 1
}

# Расходы
n <- 2
repeat {
  css_text <- paste0("div.mo:nth-child(6) > div:nth-child(1) > table:nth-child(3) > tbody:nth-child(1) > tr:nth-child(", 
                     n, 
                     ") > td:nth-child(1)")
  
  css_value <- paste0("div.mo:nth-child(6) > div:nth-child(1) > table:nth-child(3) > tbody:nth-child(1) > tr:nth-child(", 
                      n, 
                      ") > td:nth-child(3)")  
  source %>% 
    html_nodes(css_text) %>%
    html_text() ->
    text
  if(length(text) == 0) {
    break
  } else if (text == "Итого") {
    text <- "Расходы: итого"
  }
  final[1, 1] <- municipality
  final[1, 2] <- text
  source %>% 
    html_nodes(css_value) %>%
    html_text() %>% 
    str_replace(",", ".") %>% 
    as.numeric() ->
    final[1, 3]
  results <<- rbind(results, final)
  n <- n + 1
}
})

results %>% 
  mutate(municipality = str_replace(municipality, "Муниципальный округ ", ""),
         type = if_else(str_detect(category, "[Дд]оход|поступления"), "income", "expenses")) %>% 
  write_csv("municipalities.csv", na = "")


# data from gks.ru --------------------------------------------------------
setwd("/home/agricolamz/work/materials/2018_mosfindata_hackathon/4")
library(tidyverse)
df <- read_csv("not_commit/report.csv")

a <- which(df$category == "Доходы местного бюджета, фактически исполненные, тысяча рублей")

df$municipality <- ""
df$municipality[a-2] <- df$category[a-2]

repetitions <- data_frame(n = c(a[-1], nrow(df))-a, 
                          municipality = df$category[a-1])

final <- character()

sapply(1:nrow(repetitions), function(id){
  final <<- c(final, rep(repetitions$municipality[id], repetitions$n[id]))
})

df$municipality <- c(final, NA,NA)

df %>% 
  slice(-c(a-1)) %>% 
  filter(!is.na(category)) %>% 
  mutate(category = str_replace(category, "Всего", ": итого"),
         municipality = case_when(
           municipality == "Академическое" ~ "Академический",
           municipality == "Алексеевское" ~ "Алексеевский",
           municipality == "Алтуфьевское" ~ "Алтуфьевский", 
           municipality == "Бабушкинское" ~ "Бабушкинский",
           municipality == "Басманное" ~ "Басманный",
           municipality == "Беговое" ~ "Беговой", 
           municipality == "Бескудниковское" ~ "Бескудниковский",
           municipality == "Бутырское" ~ "Бутырский",
           municipality == "Войковское" ~ "Войковский", 
           municipality == "Гагаринское" ~ "Гагаринский",
           municipality == "Головинское" ~ "Головинский",
           municipality == "Даниловское" ~ "Даниловский", 
           municipality == "Косино-Ухтомское" ~ "Косино-Ухтомский",
           municipality == "Красносельское" ~ "Красносельский", 
           municipality == "Левобережное" ~ "Левобережный", 
           municipality == "Ломоносовское" ~ "Ломоносовский", 
           municipality == "Лосиноостровское" ~ "Лосиноостровский", 
           municipality == "Марьина Роща" ~ "Марьина роща", 
           municipality == "Мещанское" ~ "Мещанский", 
           municipality == "Можайское" ~ "Можайский", 
           municipality == "Молжаниновское" ~ "Молжаниновский",     
           municipality == "Нагатинский Затон" ~ "Нагатинский затон",     
           municipality == "Нагорное" ~ "Нагорный",     
           municipality == "Нижегородское" ~ "Нижегородский",     
           municipality == "Обручевское" ~ "Обручевский",     
           municipality == "Останкинское" ~ "Останкинский",     
           municipality == 'поселение "Мосрентген"' ~ 'Поселение "Мосрентген"',
           municipality == "Пресненское" ~ "Пресненский",     
           municipality == "Рязанское" ~ "Рязанский",     
           municipality == "Савеловское" ~ "Савеловский",     
           municipality == "Северное" ~ "Северный",     
           municipality == "Соколиная Гора" ~ "Соколиная гора",  
           municipality == "Таганское" ~ "Таганский",     
           municipality == "Тверское" ~ "Тверской",     
           municipality == "Тимирязевское" ~ "Тимирязевский",     
           municipality == "Филевский Парк" ~ "Филевский парк",
           municipality == "Хорошевское" ~ "Хорошевский",     
           municipality == "Южнопортовое" ~ "Южнопортовый",     
           municipality == "Ярославское" ~ "Ярославский",
           municipality == "городской округ Троицк" ~ "Городской округ Троицк",    
           municipality == "городской округ Щербинка" ~ "Городской округ Щербинка",  
           TRUE ~ municipality),
         category = case_when(
           str_detect(category, "[Бб]езвозмездные поступления") ~ "Безвозмездные поступления",
           str_detect(category, "Доходы от перечисления части прибыли государственных и муниципальных унитарных предприятий, остающейся после уплаты налогов и обязательных платежей") ~ "Неналоговые доходы",
           str_detect(category, "Налог") ~ "Налоговые доходы",
           str_detect(category, "Культура, кинематография") ~ "Культура, кинематография",
           str_detect(category, "Доходы от") ~ "Неналоговые доходы",
           str_detect(category, "Доходы местного") ~ "Неналоговые доходы",
           str_detect(category, "собственные доходы") ~ "Неналоговые доходы",
           str_detect(category, "[Фф]изическая культура и спорт") ~ "Физическая культура и спорт",
           TRUE~category))->
  df

a <- which(df$category == ": итого")
df$category[a] <- paste0(str_extract(df$category[a-1], "Доходы|Расходы"), df$category[a])

df %>% 
  gather(year, value, `2006`:`2017`) %>% 
  filter(!is.na(value)) %>% 
  mutate(source = "http://www.gks.ru",
         type = NA) ->
  df

df2 <- read_csv("municipalities.csv", na = "")
df2 %>% 
  mutate(year = "2018",
         source = "http://budget.mos.ru") %>%
  select(names(df)) %>% 
  rbind(df) %>% 
  mutate(value = str_replace(value, ",", "."),
         value = as.double(value),
         value = if_else(year == 2018, value, value/1000))  ->
  final

write_csv(final, "merged.csv")
  
# correct geojson file ----------------------------------------------------

moscow <- geojsonio::geojson_read("map_moscow_district.geojson", 
                                  what = "sp")
moscow@data %>% 
  mutate(NAME = as.character(NAME),
         NAME = case_when(
           NAME == "Беговое" ~ "Беговой",
           NAME == "Фили Давыдково" ~ "Фили-Давыдково",
           NAME == "Филевский Парк" ~ "Филевский парк",           
           NAME == "Обручевское" ~ "Обручевский",
           NAME == "Нагорное" ~ "Нагорный",
           NAME == "Марьина Роща" ~ "Марьина роща",
           NAME == "Богородский" ~ "Богородское",
           NAME == "Преображенский" ~ "Преображенское",
           NAME == "Соколиная Гора" ~ "Соколиная гора",
           NAME == "Ивановский" ~ "Ивановское",
           NAME == "Косино-Ухтомское" ~ "Косино-Ухтомский",
           NAME == "Очаково-Матвеевский" ~ "Очаково-Матвеевское",
           NAME == "Бюрилево Западное" ~ "Бирюлево Западное",
           NAME == "Мосрентген" ~ 'Поселение "Мосрентген"',
           TRUE ~ NAME)) ->
  moscow@data

geojsonio::geojson_write(moscow, file = "map_moscow_district.geojson")
