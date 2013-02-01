# Just find the top 1000 sites and spit out a CSV
# This doesn't need to be an R script, but whatevs

library(RMySQL)
source('lib/constants.R')
m <- dbDriver("MySQL");
con <- dbConnect(m,user=DB_USER,password=DB_PASSWORD,host='localhost',dbname=DB_NAME);

# Get our results
sql <- paste("SELECT cldr_code, IF(url='', 'UNKNOWN'. url), SUM(visits) as total_visits",
             "FROM requests",
             "JOIN sites ON requests.site_id = sites.id",
             "WHERE requests.time >=", FIRST_GOOD_DATE, "AND requests.time <=", LAST_DATE,
             "GROUP BY site_id", 
             "ORDER BY total_visits DESC",
             "LIMIT 1000"
             )
 res <- dbSendQuery(con, sql)
 a <- fetch(res, n = -1)
 dbDisconnect(con)

 write.csv(a, paste('output/top_1000_sites_by_visit_', FIRST_GOOD_DATE, '_', LAST_DATE, '.csv', sep=''))
