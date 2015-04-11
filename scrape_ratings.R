# retrieve data from www.throng.co.nz
library(RCurl)
library(lubridate)

get_ratings <- function(shows, date) {

  d <- as.Date(date)
  cat("Processing", as.character(d), "\n")

  uri <- sprintf("http://www.throng.co.nz/%04d/%02d/tv-ratings-%d-%s-%d/", year(d), month(d), day(d), tolower(month(d, label=T, abbr=F)), year(d))
  html <- getURI(uri)

  ratings <- numeric(length(shows))
  names(ratings) <- shows
  for (show in seq_along(shows)) {
    h <- html
    vals <- numeric()
    while (TRUE) {
      f <- regexec(sprintf("<li>%s[^<]*: ([0-9,]+)[^<]*</li>", shows[show]), h)
      if (f[[1]][1] == -1) {
        break;
      }
      start <- f[[1]][2]
      end   <- start + attr(f[[1]], "match.length")[2] - 1
      val <- as.numeric(gsub(",", "", substring(h, start, end)))
      vals <- c(vals, val)
      h <- substring(h, end)
    }
    total <- sum(unique(vals))
    ratings[show] <- ifelse(total == 0, NA, total)
  }
  ratings
}

shows <- c("Campbell Live", "Close Up", "Seven Sharp", "One News", "3 News")

start_date <- as.Date("2012-02-20") # first day from which it appears there is consistent data?
end_date   <- today()

dates <- seq(start_date, to=end_date, by=1)

d <- as.data.frame(t(sapply(dates, function(x) { get_ratings(shows, x) })))
d$Date <- dates

write.csv(d, "news_current_affairs.csv", row.names=F)

