library(splines)
library(mgcv)
library(dplyr)
library(dygraphs)
library(htmlwidgets)

ratings <- read.csv("news_current_affairs.csv")

ratings$Date <- as.Date(ratings$Date)

# grab show data
show_cols <- c(4,2,3,5,1)
ratings[,1:5] <- ratings[,show_cols]
names(ratings)[1:5] <- names(ratings)[show_cols]
show_names <- gsub("\\.", " ", gsub("^X([0-9])+", "\\1", names(ratings)))

# fit a GAM model to each show
for (i in seq_along(show_cols)) {
  # smooth
  g <- gam(ratings[,i] ~ s(as.numeric(Date), bs="ad"), data=ratings)
  first_date <- min(ratings$Date[!is.na(ratings[,i])])
  last_date  <- max(ratings$Date[!is.na(ratings[,i])])
  date_range <- seq(first_date, to=last_date, by=1)
  y <- predict(g, newdata=data.frame(Date=date_range), se.fit=T)

  # add these as additional columns in the data frame
  ratings[ratings$Date %in% date_range,paste0("fit.", i, ".mean")] <- y$fit
  ratings[ratings$Date %in% date_range,paste0("fit.", i, ".se")] <- y$se.fit*1.96

  # our plotting handler hack relies on 0's in place of NAs where we want
  # to render the model fit, else the custom bar plotter in dygraphs doesn't allow
  # the error bars (which are hacked to be fit + se) to be drawn
  
  # ideally this would be done by a custom dataHandler in dygraphs
  ratings[ratings$Date %in% date_range,i] <- ifelse(is.na(ratings[ratings$Date %in% date_range,i]), 0, ratings[ratings$Date %in% date_range,i])
}

cols <- data.frame(r=c(203,73,254,52,163),g=c(15,175,74,135,47),b=c(21,156,75,191,34))
cols <- cols[c(1,3,5,4,2),]
#cols <- cols[show_cols[c(2,3,5,1,4)],]

# compute faded colour (fade to white)
fade_colour <- function(col, alpha) {
  faded <- (255-alpha)/255 * 255 + alpha/255 * col
  rgb(faded$r, faded$g, faded$b, maxColorValue=255)
}

# value formatter for labels
value_fmt <- "function(y, opts, series_name) {
  if (y != 0)
    return y.toFixed(0);
  return '-';
}"

date_fmt <- "function(x, opts, series_name) {
  day_names = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']
  month_names = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']
  var date = new Date(x);
  return day_names[date.getDay()] + ', ' + date.getDate() + ' ' + month_names[date.getMonth()] + ' ' + (date.getYear()+1900);
}"

# point callback so that our '0' values don't render
point_cb <- "function(g, seriesName, canvasContext, cx, cy, color, pointSize) {
  var area = g.layout_.getPlotArea();
  if (cy != area.y + area.h)
    Dygraph.Circles.DEFAULT(g, seriesName, canvasContext, cx, cy, color, pointSize);
}"

# plotter for the series
plot_se <- "function(e) {
  // create new points for data and model fit
  var newFit = [];
  var newData = [];
  for (var i = 0; i < e.points.length; i++) {
    var o = e.points[i];
    newData.push({
      canvasx:  o.canvasx,
      canvasy:  o.yval == 0 ? NaN : o.canvasy,
      x:        o.x,
      y:        o.yval == 0 ? NaN : o.y
    })
    newFit.push({
      canvasx:  o.canvasx,
      canvasy:  e.plotArea.h * o.y_top + e.plotArea.y,
      x:        o.x,
      y:        o.y_top,
      y_bottom: o.y_top + (1-o.y_bottom),
      y_top:    o.y_top - (1-o.y_bottom)
    })
  }
  // call the base functions
  var f = {
        points: newData,
        setName: e.setName,
        drawingContext: e.drawingContext,
        color: e.color,
        strokeWidth: e.strokeWidth,
        dygraph: e.dygraph,
        axis: e.axis,
        plotArea: e.plotArea,
        seriesIndex: e.seriesIndex,
        seriesCount: e.seriesCount,
        singleSeriesName: e.singleSeriesName,
        allSeriesPoints: e.allSeriesPoints
  };
  Dygraph.Plotters.linePlotter(f);
  f.points = newFit;
  f.strokeWidth = 2;
  Dygraph.Plotters.errorPlotter(f);
  Dygraph.Plotters.linePlotter(f);
}"

cols_fit  <- rgb(cols$r, cols$g, cols$b, maxColorValue=255)
cols_data <- fade_colour(cols, 160)

max_viewers <- 1000001 #max(ratings[,show_cols], na.rm=T)

rownames(ratings) <- ratings$Date

# draw the dygraph using a custom plotter for each series
d <- dygraph(data=ratings[,-6], main="Viewership of news and current affairs");
for (i in seq_along(show_cols))
  d <- d %>% dySeries(c(paste0("fit.",i,".mean"), names(ratings)[i], paste0("fit.",i,".se")), label=show_names[i], color=cols_fit[i], strokeWidth=0.5, plotter=JS(plot_se))

xlab <- "<span style='color:#7f7f7f'>General additive model with adaptive smoothing and 95% credible intervals<br>Data source: Nielsen Television Audience Measurement, All 5+ via <a href='http://www.throng.co.nz'>www.throng.co.nz</a></span>"
gray <- rgb(0.5, 0.5, 0.5)
lightgray <- rgb(0.9, 0.9, 0.9)
d %>% dyOptions(fillAlpha=0.4, maxNumberWidth=7) %>%
      dyAxis('x', label=xlab, labelHeight=12, drawGrid=FALSE,
             axisLineColor=gray, axisLabelColor=gray,
             axisLabelFontSize=10, valueFormatter=JS(date_fmt)) %>%
      dyAxis('y', valueRange=c(0, max_viewers),
             axisLineColor=gray, axisLabelColor=gray,
             axisLabelFontSize=10, gridLineColor=lightgray,
             valueFormatter=JS(value_fmt)) %>%
      dyLegend(width=750) %>%
      dyCSS("tvratings.css") %>%
      dyCallbacks(drawHighlightPointCallback = JS(point_cb))

