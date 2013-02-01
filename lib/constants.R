source('lib/utils.R')

SECONDS_PER_HOUR                = 60 * 60
SECONDS_PER_DAY                 = SECONDS_PER_HOUR * 24
MINUTES_PER_HOUR                = 60
MINUTES_PER_DAY                 = MINUTES_PER_HOUR * 24
MINUTES_PER_WEEK                = MINUTES_PER_DAY * 7
FIRST_DATE                      = str_to_timestamp('')
FIRST_GOOD_DATE                 = str_to_timestamp('')
LAST_DATE                       = str_to_timestamp('')
X_AXIS_BREAKS                   = '3 days'
DECOMP_ATTR_TO_BIN              = 'remainder' # 'remainder' or 'trend'
DB_USER                         = ''
DB_PASSWORD                     = ''
DB_NAME                         = ''

BINS_PER_DAY                    = 24
BIN_WIDTH                       = SECONDS_PER_DAY / BINS_PER_DAY
STANDARD_DEVIATIONS_THRESHOLD   = 3 # This looks good for right now
GRAPH_WINDOW_MARGIN             = 14 * 24 * 60 * 60
GRAPH_IMAGE_WIDTH               = 700
