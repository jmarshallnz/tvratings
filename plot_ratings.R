library(mgcv)

ratings <- read.csv("news_current_affairs.csv")

ratings$Date <- as.Date(ratings$Date)

# remove a bunch of data
ratings <- ratings[ratings$Date > "2012-02-20",]
ratings$Campbell.Live[ratings$Date == "2015-04-03"] <- NA # Campbell Live not shown at 7pm

# grab show data
show_cols <- setdiff(1:ncol(ratings), which(names(ratings) == "Date"))
show_names <- gsub("\\.", " ", gsub("^X([0-9])+", "\\1", names(ratings)[show_cols]))

cols <- data.frame(r=c(203,73,254,52,163),g=c(15,175,74,135,47),b=c(21,156,75,191,34))
cols <- cols[c(2,3,5,1,4),]

cols_fit  <- rgb(cols$r, cols$g, cols$b, maxColorValue=255)
cols_se   <- rgb(cols$r, cols$g, cols$b, 96, maxColorValue=255)
cols_data <- rgb(cols$r, cols$g, cols$b, 160, maxColorValue=255)
cols_lab  <- rgb(cols$r, cols$g, cols$b, maxColorValue=500) # darken

png("viewers.png", width=800, height=500, res=100)
par(mai=c(0.75,1.25,1,1.25)*0.75)
max_viewers <- max(ratings[,show_cols], na.rm=T)
plot(NULL, xlim=range(ratings$Date), ylim=c(0, max_viewers), ylab="", xaxt="n", xlab="", xaxs="i", yaxt="n", yaxs="i", col.axis="grey50", bty="n")
box(col="grey50")
segments(min(ratings$Date), seq(0,1000000,by=200000), max(ratings$Date), col="grey90")
for (i in 1:length(show_cols)) {
  # data
  lines(ratings$Date, ratings[,i], col=cols_data[i])

  # smooth
  g <- gam(ratings[,i] ~ s(as.numeric(Date), bs="ad"), data=ratings)
  first_date <- min(ratings$Date[!is.na(ratings[,i])])
  last_date  <- max(ratings$Date[!is.na(ratings[,i])])
  date_range <- seq(first_date, to=last_date, by=1)
  y <- predict(g, newdata=data.frame(Date=date_range), se.fit=T)
  y_min <- y$fit - y$se.fit*1.96
  y_max <- y$fit + y$se.fit*1.96
  polygon(c(date_range,rev(date_range)), c(y_max, rev(y_min)), col=cols_se[i], border=NA)
  lines(date_range, y$fit, col=cols_fit[i], lwd=2)
  text(date_range[length(date_range)]+7, y$fit[length(y$fit)], show_names[i], col=cols_lab[i], adj=c(0,0.5), xpd=TRUE, cex=0.7)
}
year_labels <- 2012:2014
year_ticks <- as.Date(sprintf("%d-01-01", c(year_labels[1]-1, year_labels, year_labels[length(year_labels)]+1)))
month_ticks <- seq(as.Date(sprintf("%d-01-01", year_labels[1]-1)), to=as.Date(sprintf("%d-12-31", year_labels[length(year_labels)]+1)),by="month")
month_ticks <- setdiff(month_ticks, year_ticks)
axis(side=1, at=year_ticks, labels=rep("", length(year_ticks)), lwd=0, lwd.ticks=1, col="grey50")
axis(side=1, at=month_ticks, labels=rep("", length(month_ticks)), lwd=0, lwd.ticks=1, tcl=-0.2, col="grey50")
mtext(year_labels, side=1, line=0.5, at = as.Date(sprintf("%d-07-01", year_labels)), col="grey50")
mtext(format(seq(0,1000000,by=200000), scientific=F, big.mark=" "), side=2, line=0.2, at=seq(0,1000000,by=200000), las=1, cex=0.7, col="grey50")
title("Viewership of news and current affairs", col.main="grey30")

# attribution
text(max(ratings$Date)-7, 10000, "Data source: Nielsen Television Audience Measurement, All 5+ via www.throng.co.nz", adj=c(1,0), cex=0.5, col="grey30")

# explanation
text(max(ratings$Date)-7, max_viewers-10000, "General additive model with adaptive smoothing and 95% credible intervals", adj=c(1,1), cex=0.5, col="grey30")

dev.off()
