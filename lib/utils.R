# Pretty print dates as strings
as.formatted.date <- function(d) format(as.date(d), '%Y-%m-%d %H:%M', 'GMT')

# This is silly function to get dates from numerical unix timestamps
as.date <- function(x) as.POSIXct(as.numeric(x), 'GMT', '1970-1-1')

str_to_date <- function(string) {
    return(strptime(string, '%Y-%m-%d %H:%M', 'GMT'))
}

date_to_timestamp <- function(date) {
    return(as.numeric(date))
}

str_to_timestamp <- function(str) {
    return(date_to_timestamp(str_to_date(str)))
}

total_data_points <- function(domain, country, request_attr, time_window) {
    # Connect to the database
    m <- dbDriver("MySQL");
    con <- dbConnect(m,user=DB_USER,password=DB_PASSWORD,host='localhost',dbname=DB_NAME);

    # Domain is blank if it is "UNKNOWN"
    if (domain == 'UNKNOWN') {
        domain <- ''
    }

    # Manually surround with quotes because I don't care about SQL injection
    qdomain <- paste('"', domain, '"', sep='')
    qcountry <- paste('"', country, '"', sep='')

    # Get our results
    sql <- paste("SELECT
                 count(*) as total
                 FROM requests
                 WHERE site_id =
                 (SELECT id FROM sites WHERE cldr_code =", qcountry, "AND url =", qdomain, ")
                 AND time >=", time_window[1], "AND time <=", time_window[2], "AND", request_attr, '> 0')
    res <- dbSendQuery(con, sql)
    a <- fetch(res, n = -1)
    dbDisconnect(con)

    return(a$total)
}

direction <- function(x) {
    output = rep('greater', length(x))
    output[which(x < 0)] = 'fewer'
    return(output)
}

bin_direction <- function(rle_timestamps, time_series) {
    bin_means <- apply(rle_timestamps, 1, bin_mean, time_series)
    return(direction(bin_means))
}

bin_mean <- function(rle_timestamp, time_series) {
    return(mean(coredata(
        window(
            time_series,
            start = rle_timestamp['start_time'],
            end = rle_timestamp['start_time'] + rle_timestamp['length']
        )
    )))
}

expand_bin <- function(bin) {
    start_time <- str_to_timestamp(bin['start_time'])
    length <- as.integer(bin['length'])
    return(seq(start_time, start_time + length - 1))
}

intersection.anoms <- function(rle_anoms_1, rle_anoms_2) {
    anomalous_times_1 <- apply(rle_anoms_1, 1, expand_bin)
    anomalous_times_2 <- apply(rle_anoms_2, 1, expand_bin)

    intersection <- intersect(anomalous_times_1, anomalous_times_2)
    return(rle.ts(intersection, 1))
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
