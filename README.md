# tvratings

Produce plots of NZ tv ratings through time.

Use scrape_ratings.R to scrape ratings from http://www.throng.co.nz
and produce a .csv file. This takes a while and is hacky (regex to parse
html is not a good idea...), plus the data is pretty patchy as they
only show the top few shows overall.

Ideally the data would come from Nielsen directly, but that source seems
to be some horrid silverlight thing.

Use plot_ratings.R to produce a plot as a PNG. Rather specific to the
current affairs shows.
