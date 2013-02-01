# Find all anomalies in our top 1000 sites by visit for our time window and spit
# them out into a CSV

source('lib/anomaly_detection.R')

top_1000 <- read.csv('output/top_1000.csv', stringsAsFactors=FALSE)
anomalies <- matrix(nrow=0, ncol=6) 

for (i in 1:nrow(top_1000)) {
    print(paste(i, '/', nrow(top_1000)))
    b <- find_anomalies(
                        top_1000[i,]['url'],
                        top_1000[i,]['cldr_code'],
                        c('visits', 'connection_errors', 'dns_errors', 'not_found_errors'),
                        decomp_extract_attr = DECOMP_ATTR_TO_BIN,
                        sds=STANDARD_DEVIATIONS_THRESHOLD,
                        bins_per_day=BINS_PER_DAY
                        )
    anomalies <- rbind(anomalies, b, deparse.level=0)
}
rownames(anomalies) <- NULL
write.csv(anomalies, paste('output/top_anoms.csv', sep=''))
