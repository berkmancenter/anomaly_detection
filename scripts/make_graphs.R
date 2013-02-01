# Crazy catch-all file that's used to output various graphs on an as-needed basis

source('lib/graphs.R')
source('lib/anomaly_detection.R')
source('lib/constants.R')

TIME_WINDOW     <- c(FIRST_GOOD_DATE, LAST_DATE)
BINS_PER_DAY    <- 24
SDS             <- 4

domain      <- ''
country     <- ''
attr        <- 'visits'
irregts     <- fetch.ts(domain, country, TIME_WINDOW)
regts       <- regularize.ts(irregts, TIME_WINDOW)
univarts    <- make_univariate.ts(regts, attr)
decomp      <- decomp_by_week.ts(univarts)
#aggts       <- bin.ts(extract_attr.ts(deseasoned, univarts, DECOMP_ATTR_TO_BIN), BINS_PER_DAY)
filename    <- 'decomposition'
#domain2      <- ''
#country2     <- ''
#irregts2     <- fetch.ts(domain2, country2, TIME_WINDOW)
#regts2       <- regularize.ts(irregts2, TIME_WINDOW)
#filename    <- paste(
#    domain,
#    '_', country,
#    '_versus_', domain2,
#    '_', country2,
#    sep=''
#)
png(paste('output/graphs/', filename, '.png', sep=''), width=GRAPH_IMAGE_WIDTH, height=1000)

#graphics.off()
#graph_anomalies(univarts, aggts, SDS * sd(coredata(aggts)), BINS_PER_DAY, ATTR, DOMAIN, COUNTRY)
#dev.new()
#graph_comparison(regts, domain, country, regts2, domain2, country2)
graph_decomp(domain, country, attr, TIME_WINDOW)
#png('', width=1800, height=160)
#graph_anomalous_ts(univarts, aggts, DOMAIN, COUNTRY, ATTR, threshold = SDS * sd(coredata(aggts)))
dev.off()
#anoms <- find_anomalies(DOMAIN, COUNTRY, ATTR, bins_per_day = BINS_PER_DAY)
#print(anoms)
