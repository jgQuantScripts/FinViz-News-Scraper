require("rvest");require("stringr")

ticker = "ZM"
getFinNews = function(ticker)
{
  Sys.sleep(5)
  url <- paste0("https://finviz.com/quote.ashx?t=",ticker)
  # read finviz news data
  data <- read_html(url)
  # copy xpath
  data = data %>% html_nodes(xpath = "//*[@id='news-table']") %>% html_table()
  tmp = do.call(rbind,data)
  dtime = as.data.frame(tmp[,1])
  # Split Dates & Times
  dtime <- t(as.data.frame(str_split(dtime[,1], pattern = " ")))
  dates <- as.character(dtime[,1])
  tmz   <- as.character(dtime[,2])
  # detect times by using colon ":" & replace with NA
  dates[str_detect(dates,pattern = ":")] <- NA
  dates <- na.locf(dates)
  # combine into a timeStamp
  timeStamp <- as.data.frame(as.POSIXct(paste(dates,tmz), format="%b-%d-%y %I:%M%p"))
  # combine timeStamps with News
  tmp[,1] <- timeStamp
  tmp[,3] <- ticker
  colnames(tmp) <- c("Date","News","Ticker")
  tmp
}

dat <- getFinNews("AAPL")



















