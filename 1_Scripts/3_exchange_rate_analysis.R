library(rvest)
SI_data <-
read_html(paste0("https://www.sedlabanki.is/",
"default.aspx?PageID=20f749ed-65bd-11e4-93f7-005056bc0bdb&",
"dag1=1&man1=1&ar1=2016&dag2=28&man2=12&ar2=2040",
"&AvgCheck=man&Midgengi=on&Mynt9=V%c3%9eN&Lang=is"))



exchange_data <-
  SI_data %>%
  html_table() %>%
  as.data.frame() %>%
  slice(-1) %>%
  rename(date = 1,
         gengisvisitala = 2) %>%
  mutate(gengisvisitala = parse_number(gengisvisitala, locale = locale(decimal_mark = "," ))) %>%
  mutate(id = row_number()) %>%
  mutate(ym = seq.Date(from = as.Date("2016-01-01"), length.out =  max(id) ,  by = "month"))

exc_infl_data <-
left_join(product_data, exchange_data, by.x = "ym") 

exc_infl_data %>%
  ggplot() + 
  aes(y = lag(new_index), x = gengisvisitala, color = Undirvísitala ) + 
  geom_point() +
  #facet_grid(~`Undirvísitala`) + 
  geom_smooth(method = "lm")

exc_infl_data %>%
  ggplot() + 
  geom_line(aes(y = new_index, x = ym, color = Undirvísitala)) +
  geom_line(aes(y = 100 * gengisvisitala/gengisvisitala[1], x = ym, color = "red")) 
  #facet_grid(~`Undirvísitala`) + 
  geom_smooth(method = "lm")


