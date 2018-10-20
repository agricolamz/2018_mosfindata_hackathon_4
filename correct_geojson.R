# correct geojson file
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
