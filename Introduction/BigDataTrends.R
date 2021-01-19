# The following script, extracts the Google search
# result(s) trend for 'big data' from 2004 until 
# October 2020

# The main source for the code is:
# https://www.datacareer.ch/blog/analyzing-google-trends-with-r-retrieve-and-plot-with-gtrendsr/

# First we must install and load the package

# install.packages('gtrendsR')

library(gtrendsR)

# We now extract the interest index

# The keyword which we are interest in is defined below
keyword <- "big data"

# The time frame of interest is defined below
time <- "all"

# Thus, having defined the above we can now make use of the 'gtrends' function

trends <- gtrends(keywords, time = time)

# However from the above, we still need to extract the interest itself

time_trend <- trends$interest_over_time

# Having done this, we can now plot the data

library(ggplot2)

time_trend$date <- as.Date(time_trend$date)

plot <- ggplot(data=time_trend, aes(x = date, y = hits)) +
        geom_line(col = 'deepskyblue', lwd = 1) +
        scale_x_date(date_labels = "%Y", date_breaks = "2 year") +
        xlab('Time') + 
        ylab('Relative Interest') + 
        theme_bw()

plot

setwd("C:/Users/danie/Google Drive/Masters/3.Research/Write-up/Sections/Introduction/Images")

ggsave("Big Data Trend.png", units="in", width=5.4, height=4, dpi=600)
