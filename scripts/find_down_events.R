# Find all the events in which an error increase corresponds in time with a 
# decrease in visits

source('lib/graphs.R')
source('lib/anomaly_detection.R')
source('lib/constants.R')
source('lib/utils.R')

data <- read.csv('output/top_anoms.csv', stringsAsFactors = FALSE)

sites <- unique(data[,c('domain', 'country')])
      
get_down_events <- function(site) {
    intersections <- data.frame(start_time = c(), length = c(), error = c())
    anoms <- data[data$domain == site['domain'] & data$country == site['country'],]

    print(paste(site['domain'], site['country']))

    fewer_visits <- which(
        anoms$request_result == 'visits'
        & anoms$anom_direction == 'fewer'
    )

    greater_errors <- which(
        (
            anoms$request_result == 'connection_errors'
            | anoms$request_result == 'dns_errors'
            | anoms$request_result == 'not_found_errors'
        )
        & anoms$anom_direction == 'greater'
    )

    indices_to_test <- expand.grid(fewer_visits, greater_errors)

    if (nrow(indices_to_test) > 0) {
        intersections <- do.call(rbind, apply(indices_to_test, 1,
            function(indices) { 
                intersections <- intersection.anoms(anoms[indices[1],], anoms[indices[2],])
                if (nrow(intersections) > 0 && anoms[indices[2],'anom_direction'] == 'greater') {
                    return(cbind(intersections, error = anoms[indices[2],'request_result']))
                } else {
                    return(list())
                }
            }
        ))
    }

    if (!is.null(intersections) && ncol(intersections) > 0) {
        a <- cbind(
           domain = rep(site['domain'], nrow(intersections)),
           country = rep(site['country'], nrow(intersections)),
           start_time = as.formatted.date(intersections[,'start_time']),
           length = intersections[,'length'],
           error = as.character(intersections[,'error'])
        )
        return(a)
    }
}

output <- apply(sites, 1, get_down_events)
output <- do.call(rbind, output)
rownames(output) <- NULL
write.csv(output, 'output/down_events.csv')
