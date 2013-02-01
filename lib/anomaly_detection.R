# Load 'em up
library(DBI)
library(RMySQL)
library(zoo)
library(forecast)
source('lib/constants.R')

# Grab the data from MySQL and fit it into a multi time series
fetch.ts <- function(domain, country, time_window, zero.fill = 124) {
    # Domain is blank if it is "UNKNOWN"
    if (domain == 'UNKNOWN') {
        domain <- ''
    }

    # Manually surround with quotes because I don't care about SQL injection
    qdomain <- paste('"', domain, '"', sep='')
    qcountry <- paste('"', country, '"', sep='')

    # Connect to the database
    m <- dbDriver("MySQL");
    con <- dbConnect(m,user=DB_USER,password=DB_PASSWORD,host='localhost',dbname=DB_NAME);

    # Get our results
    sql <- paste("SELECT
                 time,
                 site_id,
                 IF(visits>124, visits,", zero.fill,") AS visits,
                 IF(dns_errors>124, dns_errors,", zero.fill, ") AS dns_errors,
                 IF(connection_errors>124, connection_errors,", zero.fill, ") AS connection_errors,
                 IF(not_found_errors>124, not_found_errors,", zero.fill, ") AS not_found_errors
                 FROM requests
                 WHERE site_id =
                 (SELECT id FROM sites WHERE cldr_code =", qcountry, " AND url =", qdomain, ")
                 AND time >=", time_window[1], "AND time <=", time_window[2], "ORDER BY time")
    res <- dbSendQuery(con, sql)
    a <- fetch(res, n = -1)
    dbDisconnect(con)

    # Create an irregular time series from the visit data
    time_series <- zoo(as.matrix(a[,-1]), as.integer(a$time))

    return(time_series)
}

# Fill in data for times without data - can't remember why I'm doing it like this
# Seems silly
regularize.ts <- function(time_series, time_window, interval=60, zero.fill=124) {
    # Compute all the times required for a regular time series
    all_times <- seq(time_window[1], time_window[2], interval)

    # Create an empty regular time series
    empty_series <- zoo(, as.integer(all_times))

    # Merge the two time series to reveal areas where the data is missing
    d <- merge.zoo(time_series,empty_series)

    # Use linear interpolation to fill in the gaps
    success <- tryCatch(complete_data <- na.approx(d), error = function(e) { return(FALSE) })
    if (is.logical(success) && success == FALSE) {
        print('whoops')
        complete_data <- na.fill(d, rep(zero.fill, 3))
    }

    return(complete_data)
}

# Make a multi time series a univariate time series
make_univariate.ts <- function(time_series, var_name) {
    return(ts(coredata(time_series[,var_name]), frequency=frequency(time_series), start=time(time_series)[1]))
}

# Decompose twice with different periods
decomp_by_day_then_week.ts <- function(time_series, extract_attr = NULL, passthru_attr = c('trend', 'remainder')) {
    decomp_by_day <- decomp_by_day.ts(time_series)
    if (length(passthru_attr) > 1) {
        decomp_by_day_then_week <- decomp_by_week.ts(decomp_by_day$time.series[,passthru_attr[1]] + decomp_by_day$time.series[,passthru_attr[2]])
    } else {
        decomp_by_day_then_week <- decomp_by_week.ts(decomp_by_day$time.series[,passthru_attr[1]])
    }

    if (is.null(extract_attr)) {
        return(decomp_by_day_then_week)
    } else {
        return(extract_attr.ts(decomp_by_day_then_week, time_series, extract_attr))
    }
}

# Pull a single attribute out of our decomposed results
extract_attr.ts <- function(decomp_time_series, original_time_series, extract_attr) {
    return(ts(
              decomp_time_series$time.series[,extract_attr],
              start=start(original_time_series),
              frequency=frequency(original_time_series)
              )
    )
}

# Decompose the time series into its various bits
decompose.ts <- function(time_series, frequency) {
    return(stl(ts(coredata(time_series), frequency=frequency), s.window="periodic", robust=TRUE))
}

decomp_by_week.ts <- function(time_series) {
    return(decompose.ts(time_series, MINUTES_PER_WEEK))
}

decomp_by_day.ts <- function(time_series) {
    return(decompose.ts(time_series, MINUTES_PER_DAY))
}

decomp_by_hour.ts <- function(time_series) {
    return(decompose.ts(time_series, MINUTES_PER_HOUR))
}

# Smooth or "bin" a time series so it's nicer to look at
bin.ts <- function(t, bins_per_day) {
    agg <- aggregate(t, 1 / MINUTES_PER_DAY / 60 * bins_per_day, FUN=mean)

    return(agg)
}

# Find anomalies
find_anomalies <- function(
                            domain,
                            country,
                            request_result,
                            sds=STANDARD_DEVIATIONS_THRESHOLD,
                            bins_per_day=1,
                            decomp_extract_attr = 'trend',
                            time_window = c(FIRST_GOOD_DATE, LAST_DATE),
                            zero.fill=124
                          ) {

    options(stringsAsFactors = FALSE);
    COLUMN_NAMES = c('domain', 'country', 'start_time', 'length', 'request_result', 'anom_direction')
    SECONDS_PER_BIN <- 24 / bins_per_day * SECONDS_PER_HOUR

    output <- matrix(nrow=0, ncol=length(COLUMN_NAMES), dimnames=list(c(), COLUMN_NAMES))
    irregts <- fetch.ts(domain, country, time_window, zero.fill=zero.fill)
    regts <- regularize.ts(irregts, time_window, zero.fill=zero.fill)

    for (rr in as.vector(request_result)) {
        time_series <- make_univariate.ts(regts, rr)
        decomp <- extract_attr.ts(decomp_by_day_then_week.ts(time_series), time_series, decomp_extract_attr)
        agg <- bin.ts(decomp, bins_per_day)
        binned <- coredata(agg)

        # Find dates where we surpass our threshold
        anoms <- binned > sds * sd(binned) | binned < sds * sd(binned) * -1
        dates <- time(agg)[anoms]

        if (length(dates) > 0) {
            anomalous_timestamps <- as.integer(dates)
            rle_timestamps <- rle.ts(anomalous_timestamps, SECONDS_PER_BIN)
            collapsed_anoms <- cbind(rle_timestamps, direction = bin_direction(rle_timestamps, decomp))

            h <- nrow(collapsed_anoms)
            result_cols <- cbind(
                rep(domain, h),
                rep(country, h),
                as.formatted.date(as.vector(collapsed_anoms$start_time)),
                collapsed_anoms$length,
                rep(rr, h),
                as.vector(collapsed_anoms$direction)
            )
            colnames(result_cols) <- COLUMN_NAMES

            row_order <- sapply(unlist(result_cols[,'start_time']), str_to_timestamp)
            output <- rbind(output, result_cols[order(row_order),])
        }
    }

    rownames(output) <- NULL
    return(output)
}

# Smoosh our anomalies together with RLE
rle.ts <- function(timestamps, bin_width) {
    # Go through, find the diff, for diffs that equal our block size, collapse, otherwise, don't
    secs_between_anoms <- diff(timestamps)
    run_length_encoded <- rle(secs_between_anoms)
    collapsible_indices <- which(run_length_encoded$values == bin_width)

    output <- data.frame(start_time = c(), length = c())
    collapsed_date_indices <- c()

    # If there are runs of anomalies to collapse
    if (length(collapsible_indices) > 0) {

        # Convert between the RLE index and the index in the date vector
        to.date.index <- function(i) {
            if (i > 1) {
                return(sum(run_length_encoded$lengths[1:(i-1)]) + 1)
            } else {
                return(1)
            }
        }

        collapsible_date_indices <- sapply(collapsible_indices, to.date.index)

        output <- rbind(output, data.frame(
            start_time = timestamps[collapsible_date_indices],
            length = (run_length_encoded$lengths[collapsible_indices] + 1) * bin_width
        ))

        # Get the indexes of all collapsed anomalies
        collapsed_date_indices <- unlist(
            sapply(
                collapsible_indices,
                function(a) seq(to.date.index(a), to.date.index(a)+(run_length_encoded$lengths[a]))
            )
        )
    }

    # Get the indexes that aren't collapsed
    not_collapsed_date_indices <- setdiff(index(timestamps), collapsed_date_indices)

    if (length(not_collapsed_date_indices) > 0) {
        output <- rbind(output,
            data.frame(
                start_time = timestamps[not_collapsed_date_indices],
                length = bin_width
            )
        )
    }

    return(output)
}
