# Look through the top 1000 sites by visits for our time window and find sites
# where the mean number of errors is greater than mean visits times ERROR_THRESHOLD

source('lib/graphs.R')
source('lib/anomaly_detection.R')
source('lib/constants.R')
source('lib/utils.R')

# Connect to the database
m <- dbDriver("MySQL");
top_1000 <- read.csv('output/top_1000.csv', stringsAsFactors=FALSE)

find_persistent_issues <- function(site) {
    con <- dbConnect(m,user=DB_USER,password=DB_PASSWORD,host='localhost',dbname=DB_NAME);
    # Get our results
    if (site['url'] == 'UNKNOWN') {
        site['url'] <- ''
    }

    # Manually surround with quotes because I don't care about SQL injection
    domain <- paste('"', site['url'], '"', sep='')
    country <- paste('"', site['cldr_code'], '"', sep='')

    sql <- paste("SELECT
                 IF(url='', 'UNKNOWN', url) as url, cldr_code,
                 AVG(visits) as visits,
                 AVG(not_found_errors) as not_found_errors,
                 AVG(connection_errors) as connection_errors,
                 AVG(dns_errors) as dns_errors
                 FROM requests
                 JOIN sites ON sites.id = requests.site_id
                 WHERE time >=", FIRST_GOOD_DATE, "AND time <=", LAST_DATE,
                 "AND site_id =
                 (SELECT id FROM sites WHERE cldr_code =", country, " AND url =", domain, ")"
                 )
    print(site)
    res <- dbSendQuery(con, sql)
    a <- fetch(res, n = -1)
    dbDisconnect(con)

    return(a)
}
output <- apply(top_1000, 1, find_persistent_issues)
output <- do.call(rbind, output)
write.csv(output, 'output/persistent_issues.csv')

filter_blocks <- function(row) {
    ERROR_THRESHOLD = 0.5
    if (as.double(row['visits']) * ERROR_THRESHOLD < as.double(row['not_found_errors']) ) {
        return(data.frame(
            domain = row['url'],
            country = row['cldr_code'],
            visit_avg = row['visits'],
            error_avg = row['not_found_errors'],
            error = 'not_found_errors',
            row.names = NULL
        ))
    } else if (as.double(row['visits']) * ERROR_THRESHOLD < as.double(row['connection_errors']) ) {
        print(row)
        return(data.frame(
            domain = row['url'],
            country = row['cldr_code'],
            visit_avg = row['visits'],
            error_avg = row['connection_errors'],
            error = 'connection_errors',
            row.names = NULL
        ))
    } else if (as.double(row['visits']) * ERROR_THRESHOLD < as.double(row['dns_errors'])) {
        return(data.frame(
            domain = row['url'] ,
            country = row['cldr_code'] ,
            visit_avg = row['visits'],
            error_avg = row['dns_errors'],
            error = 'dns_errors',
            row.names = NULL
        ))
    } else {
        return(list())
    }
}

output2 <- apply(output, 1, filter_blocks)
output2 <- do.call(rbind, output2)
print(output2)
