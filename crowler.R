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
