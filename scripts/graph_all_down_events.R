# Pull all down events from a csv and output pngs into the output directory

source('lib/graphs.R')
source('lib/anomaly_detection.R')
source('lib/constants.R')
source('lib/utils.R')

TIME_WINDOW     <- c(FIRST_GOOD_DATE, LAST_DATE)
SDS             <- STANDARD_DEVIATIONS_THRESHOLD
MARGIN          <- 3 * SECONDS_PER_DAY
IMAGE_WIDTH     <- GRAPH_IMAGE_WIDTH

data <- read.csv('output/down_events.csv')

output_graph <- function(row, time_window, bins_per_day, sds) {
    domain      <- row['domain']
    country     <- row['country']
    attr        <- row['error']
    irregts     <- fetch.ts(domain, country, time_window)
    regts       <- regularize.ts(irregts, time_window)
    univarts    <- make_univariate.ts(regts, attr)
    deseasoned  <- decomp_by_week.ts(univarts)
    aggts       <- bin.ts(extract_attr.ts(deseasoned, univarts, DECOMP_ATTR_TO_BIN), BINS_PER_DAY)
    min_time    <- str_to_timestamp(row['start_time']) - MARGIN
    max_time    <- min_time + as.numeric(row['length']) + 2 * MARGIN
    filename    <- paste(
        as.numeric(row['X']),
        '_', country,
        '_', domain,
        '_', attr,
        '_', bins_per_day,
        '_', sds, sep=''
    )
    png(paste('output/graphs/', filename, '.png', sep=''), width=IMAGE_WIDTH, height=300)
    graph1 <- graph_anomalous_ts(
        univarts, aggts, domain, country, attr,
        threshold = sds * sd(coredata(aggts)),
        min_time = min_time,
        max_time = max_time,
        return_ggplot = TRUE
    )
    attr = 'visits'
    irregts     <- fetch.ts(domain, country, time_window)
    regts       <- regularize.ts(irregts, time_window)
    univarts    <- make_univariate.ts(regts, attr)
    deseasoned  <- decomp_by_week.ts(univarts)
    aggts       <- bin.ts(extract_attr.ts(deseasoned, univarts, DECOMP_ATTR_TO_BIN), BINS_PER_DAY)
    graph2 <- graph_anomalous_ts(
        univarts, aggts, domain, country, attr,
        threshold = sds * sd(coredata(aggts)),
        min_time = min_time,
        max_time = max_time,
        return_ggplot = TRUE,
        date_breaks = '1 day'
    )
    multiplot(graph2, graph1, cols=1)
    dev.off()
}

apply(data, 1, output_graph, TIME_WINDOW, BINS_PER_DAY, SDS)
