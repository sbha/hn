# get data from HN api and aggregrate as a data frame
# get all submissions from the whoishiring account
# get all postings per submission


un <- fromJSON("https://hacker-news.firebaseio.com/v0/user/whoishiring.json")

read_months <- function(x){
  link <- paste0('https://hacker-news.firebaseio.com/v0/item/', x, '.json')
  fromJSON(link) %>% 
    tbl_df() 
}

df_un <- un$submitted[1:10] %>% 
  map_df(~read_months(.))  %>%   
  mutate(date = as.Date(as.POSIXct(as.numeric(time), origin = '1970-01-01', tz = 'GMT'))) 



read_month <- function(x){
  link <- paste0('https://hacker-news.firebaseio.com/v0/item/', x, '.json')
  fromJSON(link) %>% 
    tbl_df() #%>% 
}

df_postings <- 
  df_un %>% 
  filter(str_detect(title, regex('who is hiring', ignore_case = TRUE))) %>% 
  #slice(1:10) %>% 
  select(kids) %>% 
  unlist() %>% 
  map_df(~read_month(.)) %>% 
  nest(kids, .key = "kids") %>% 
  filter(is.na(deleted)) %>% 
  filter(is.na(dead)) %>% 
  mutate(date = as.Date(as.POSIXct(as.numeric(time), origin = '1970-01-01', tz = 'GMT'))) 