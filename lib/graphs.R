library(ggplot2)
library(grid)
library(scales)
source('lib/constants.R')

# Show multiple ggplot2 plots on a single display
# Adapted from here:
# http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20(ggplot2)/
multiplot <- function(..., plotlist=NULL, cols, hratio=c(1,1)) {
    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # Make the panel
    plotCols = cols                          # Number of columns of plots
    plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols

    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(plotRows, plotCols, heights=hratio)))
    vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
        curRow = ceiling(i/plotCols)
        curCol = (i-1) %% plotCols + 1
        print(plots[[i]], vp = vplayout(curRow, curCol ))
    }

}

# Show our process in graphical form - very hacky
graph_decomp <- function(domain, country, attr, time_window) {

    irregts       <- fetch.ts(domain, country, time_window)
    regts         <- regularize.ts(irregts, time_window)
    univarts      <- make_univariate.ts(regts, attr)
    decomp_by_day <- decomp_by_day.ts(univarts)
    decomp        <- decomp_by_day_then_week.ts(univarts)
    aggts         <- bin.ts(extract_attr.ts(decomp, univarts, DECOMP_ATTR_TO_BIN), BINS_PER_DAY)

    day_seasonal  <- decomp_by_day$time.series[,'seasonal']
    day_trend     <- decomp_by_day$time.series[,'trend']
    day_remainder <- decomp_by_day$time.series[,'remainder']
    week_input    <- decomp_by_day$time.series[,'trend'] + decomp_by_day$time.series[,'remainder']
    seasonal      <- decomp$time.series[,'seasonal']
    trend         <- decomp$time.series[,'trend']
    remainder     <- decomp$time.series[,'remainder']

    data <- c(
        univarts,
        day_seasonal,
        day_trend,
        day_remainder,
        week_input,
        seasonal,
        trend,
        remainder
    )
    groups <- c(
        rep('data', length(univarts)),
        rep('daily pattern', length(day_seasonal)),
        rep('day trend', length(day_trend)),
        rep('day remainder', length(day_remainder)),
        rep('week input', length(week_input)),
        rep('weekly pattern', length(seasonal)),
        rep('general trend', length(trend)),
        rep('remainder', length(remainder))
    )

    index <- as.POSIXct(index(univarts), 'GMT', '1970-1-1')

    h <- data.frame(
        x=index,
        y=data,
        group=groups
    )

    h$group <- factor(h$group, levels=c('data', 'daily pattern', 'day trend', 'day remainder', 'week input', 'weekly pattern', 'general trend', 'remainder'))
    graph1 <- ggplot(h, aes(x=x, y=y, color=group)) +
        #scale_color_brewer(palette='Set1') +
        geom_line() +
        facet_grid(group ~ ., scales='free') +
        scale_x_datetime() +
        ylab(attr) +
        xlab(NULL) +
        opts(
            legend.position='none',
            title=paste('Anomaly Detection in', attr, 'to', domain, 'from', country),
            plot.margin = unit(c(0.3, 0.5, 0.4, 0.25), 'cm'),
            axis.text.x = NULL
        )
    threshold <- sd(coredata(aggts)) * STANDARD_DEVIATIONS_THRESHOLD

    i <- data.frame(
        x = as.POSIXct(index(aggts), 'GMT', '1970-1-1'),
        y = coredata(aggts),
        highlight = ifelse(coredata(aggts) > threshold | coredata(aggts) < -1 * threshold, 1, 0.2)
    )

    graph2 <- ggplot(i, aes(x=x, y=y)) +
        geom_line() +
        ylab(attr) +
        xlab(NULL) +
        opts(
            legend.position='none',
            plot.margin = unit(c(0, 1, 0, 0.25), 'cm'),
            axis.text.x = NULL
        )

    graph3 <- ggplot(i, aes(x=x, y=y)) +
        geom_line() +
        geom_rect(alpha = 0.012, fill="white", xmin = min(index(aggts)), xmax = max(index(aggts)), ymin = -threshold, ymax=threshold) + 
        geom_line(color="red", data = data.frame(x=i$x, y=rep(threshold, length(aggts)))) +
        geom_line(color="red", data = data.frame(x=i$x, y=rep(-1 * threshold, length(aggts)))) + 
        ylab(attr) +
        xlab(NULL) +
        opts(
            legend.position='none',
            plot.margin = unit(c(0, 1, 0, 0.25), 'cm'),
            axis.text.x = NULL
        )

    graph4 <- graph_anomalous_ts2(univarts, aggts, domain, country, attr, threshold = threshold, return_ggplot = TRUE)
    print(multiplot(graph1, graph2, graph3, graph4, cols = 1, hratio=c(3.5,1,1,1.2)))
}

# Old anomaly graph
graph_anomalies <- function(rawts, aggts, threshold, bins_per_day, request_result, domain, country, filename=NULL) {
    # Anomalies graph
    e <- data.frame(x=time(rawts), y=coredata(rawts))
    f <- data.frame(x=time(aggts), bins=coredata(aggts))
    x <- as.POSIXct(index(rawts), 'GMT', '1970-1-1')

    p1 <-   ggplot(f, aes(x=x, y=bins)) +
    geom_bar(stat='identity') +
    scale_x_datetime(limits=c(min(x), max(x))) +
    scale_y_continuous(expand=c(0,0), labels=scientific) +
    geom_line(
              stat        = 'hline',
              yintercept  = function(x) threshold,
              aes(color   = 'red')
              ) +
    ylab(paste('anomalousness (', round(24 / bins_per_day), 'hour bins )')) +
    opts(
         legend.position = 'none',
         title      = paste('Anomalies in', request_result, 'to', domain, 'from', country),
         axis.title.x    = theme_blank(),
         axis.text.x     = theme_blank(),
         axis.text.y     = theme_blank(),
         axis.ticks      = theme_blank(),
         plot.margin     = unit(c(1, 1, -0.7, 0.5), 'lines')
         )

    p2 <-   ggplot(e, aes(x=x, y=y)) +
    geom_line() +
    scale_x_datetime(limits=c(min(x), max(x))) +
    scale_y_continuous(labels=scientific) +
    ylab(request_result) +
    xlab('date') +
    opts(
         legend.position    = 'none',
         panel.grid.minor   = theme_blank(),
         axis.text.y     = theme_blank(),
         axis.ticks      = theme_blank(),
         plot.margin=unit(c(0, 1, 1, 0.5), 'lines')
         )

    multiplot(p1, p2, cols=1, hratio=c(2,1))
}

# Put two graphs of all characteristics next to each other
# This is used for persistent issues graphs
graph_comparison <- function(ts1, domain1, country1, ts2, domain2, country2, filename=NULL) {
    data1 <- as.data.frame(ts1)
    d <- data1
    e <- data.frame(
                    x=time(ts1),
                    y=c(d$visits, d$dns_errors, d$connection_errors, d$not_found_errors),
                    group=c(
                            rep('Successful Visits', length(d$visits)),
                            rep('DNS Errors', length(d$dns_errors)),
                            rep('Connection Errors', length(d$connection_errors)),
                            rep('404 Errors', length(d$not_found_errors))
                            )
                    )
    e$group <- factor(e$group, levels = c('Successful Visits', 'Connection Errors', 'DNS Errors', '404 Errors'))

    data2<- as.data.frame(ts2)
    f <- data2
    g <- data.frame(
                    x=time(ts2),
                    y=c(f$visits, f$dns_errors, f$connection_errors, f$not_found_errors),
                    group=c(
                            rep('Successful Visits', length(f$visits)),
                            rep('DNS Errors', length(f$dns_errors)),
                            rep('Connection Errors', length(f$connection_errors)),
                            rep('404 Errors', length(f$not_found_errors))
                            )
                    )
    g$group <- factor(g$group, levels = c('Successful Visits', 'Connection Errors', 'DNS Errors', '404 Errors'))

    p1 <- ggplot(e, aes(x=x, y=y, color=(group == 'Successful Visits'))) +
    geom_line() +
    facet_grid(group ~ .) +
    scale_x_datetime() + 
    opts(legend.position='none', title=paste('Requests for', domain1, 'from', country1)) +
    xlab('Date') +
    ylab('Request Outcomes (per minute)')

    p2 <- ggplot(g, aes(x=x, y=y, color=(group == 'Successful Visits'))) +
    geom_line() +
    facet_grid(group ~ .) +
    scale_x_datetime() + 
    opts(legend.position='none', title=paste('Requests for', domain2, 'from', country2)) +
    xlab('Date') +
    ylab('Request Outcomes (per minute)')

    multiplot(p1, p2, cols=2)
}

# Old anomaly histogram used to point out data collection errors
graph_anomaly_histogram <- function(input_filename, output_filename) {
    a <- read.csv(input_filename)
    print(
          ggplot(a, aes(x=date))
          + geom_histogram(binwidth=(max(a$date) - min(a$date)) / 49) 
          + scale_x_datetime()
          + xlab('Date')
          + ylab('Anomalies')
          + opts(title='Anomalies by day for top sites by visits')
         )
}

# Make that red/blue time series graph that I like so much
graph_anomalous_ts <- function(
                               time_series,
                               binnedts,
                               domain,
                               country,
                               attr,
                               threshold=NULL,
                               min_time=NULL,
                               max_time=NULL,
                               date_breaks = X_AXIS_BREAKS,
                               return_ggplot = FALSE
                               ) {
    anom = coredata(binnedts)

    if (!is.null(threshold)) {
        anom[anom < threshold & anom > -1 * threshold] = 0
    }

    a <- data.frame(
        tsx = time(time_series),
        tsy = coredata(time_series)
    )

    b <- data.frame(
        binx = time(binnedts),
        biny = max(coredata(window(time_series, start=min_time, end=max_time))) * 1.1,
        anomalousness = abs(anom),
        direction = direction(anom)
    )

    if (!is.null(min_time) & !is.null(max_time)) {
        a = a[a[,'tsx'] >= min_time & a[,'tsx'] <= max_time,]
        b = b[b[,'binx'] >= min_time & b[,'binx'] <= max_time,]
    }

    graph <- ggplot(a, aes(x=tsx)) +
        geom_bar(
            data=b,
            mapping=aes(x=binx, y=biny, fill=direction, alpha=anomalousness),
            stat='identity',
            geom_params=list(width=ceiling((max(b$binx) - min(b$binx)) / length(b$anomalousness)))
        ) +
        geom_line(aes(y=tsy)) +
        scale_x_datetime(breaks = date_breaks(date_breaks), expand=c(0,0), labels=date_format('%a, %b %d')) +
        scale_y_continuous(labels=comma) +
        scale_fill_manual(values = c("greater" = "red", "fewer" = "blue")) +
        scale_alpha(range=c(0, 0.5)) +
        opts(
             title = paste(attr, 'for', domain, 'from', country),
             panel.background = theme_rect(colour='white'),
             legend.position = 'none',
             panel.grid.major = theme_line(colour='gray90', size=0.2),
             panel.grid.minor = theme_blank()
        ) +
        ylab(attr) +
        xlab('date')

    if (return_ggplot) {
        return(graph)
    } else {
        print(graph)
    }
}

# Wow I'm lazy.  Tweak the above graph a bit to fit into our process graph
graph_anomalous_ts2 <- function(
                                time_series,
                                binnedts,
                                domain,
                                country,
                                attr,
                                threshold=NULL,
                                min_time=NULL,
                                max_time=NULL,
                                date_breaks = X_AXIS_BREAKS,
                                return_ggplot = FALSE
                                ) {
    anom = coredata(binnedts)

    if (!is.null(threshold)) {
        anom[anom < threshold & anom > -1 * threshold] = 0
    }

    index <- as.POSIXct(index(time_series), 'GMT', '1970-1-1')

    a <- data.frame(
        tsx = index,
        tsy = coredata(time_series)
    )

    b <- data.frame(
        binx = time(binnedts),
        biny = max(coredata(window(time_series, start=min_time, end=max_time))) * 1.1,
        anomalousness = abs(anom),
        direction = direction(anom)
    )

    if (!is.null(min_time) & !is.null(max_time)) {
        a = a[a[,'tsx'] >= min_time & a[,'tsx'] <= max_time,]
        b = b[b[,'binx'] >= min_time & b[,'binx'] <= max_time,]
    }

    graph <- ggplot(a, aes(x=tsx)) +
        geom_line(aes(y=tsy)) +
        geom_bar(
            data=b,
            mapping=aes(x=binx, y=biny, fill=direction, alpha=anomalousness),
            stat='identity',
            geom_params=list(width=ceiling((max(b$binx) - min(b$binx)) / length(b$anomalousness)))
        ) +
        #scale_x_datetime(breaks = date_breaks('2 weeks'), expand=c(0,0), labels=date_format('%b %d')) +
        scale_fill_manual(values = c("greater" = "red", "fewer" = "blue")) +
        scale_alpha(range=c(0, 0.5)) +
        opts(
            legend.position = 'none',
            plot.margin = unit(c(0, 0.99, 0, 0.34), 'cm')
        ) +
        ylab(attr) +
        xlab('date')

    if (return_ggplot) {
        return(graph)
    } else {
        print(graph)
    }
}
