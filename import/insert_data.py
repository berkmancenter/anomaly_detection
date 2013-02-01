import sys
import csv
import time
import re
import MySQLdb

UPDATE_INTERVAL = 20000
COMMIT_INTERVAL = 10 * 1000001
MAX_LINES = 10 * 1000000
try:
    conn = MySQLdb.connect( host="localhost", user="", passwd="", db="" )
except:
    exceptionType, exceptionValue, exceptionTraceback = sys.exc_info()
    sys.exit("Database connection failed!\n ->%s" % (exceptionValue))

reader = csv.reader(open("filename", "rb"))
previous_end_time = 0
start_time = time.time()
last_update_time = start_time
params = []

cursor = conn.cursor()

def finish():
    cursor.executemany('INSERT INTO requests (site_id, time, duration, line_no, visits, not_found_errors, connection_errors, dns_errors) VALUES (%s, %s, %s, %s, %s, %s, %s, %s)', params)
    cursor.close()
    conn.commit()
    conn.close()
    sys.exit(0)

cursor.execute('SET AUTOCOMMIT=0')
cursor.execute('SET foreign_key_checks=0')
conn.commit()

for row in reader:
    if reader.line_num % UPDATE_INTERVAL == 0:
        print str(reader.line_num) + '/' + str(MAX_LINES) + ' - ' + str(round(float(reader.line_num) / float(MAX_LINES) * 100.0, 2)) + '% - ' + str(round(time.time() - last_update_time, 2)) + ' sec - ' + str(round(((time.time() - start_time) / reader.line_num) * (MAX_LINES - reader.line_num) / 3600, 2)) + ' hours left'
        last_update_time = time.time()
    if reader.line_num % COMMIT_INTERVAL == 0:
        cursor.executemany('INSERT INTO requests (site_id, time, duration, line_no, visits, not_found_errors, connection_errors, dns_errors) VALUES (%s, %s, %s, %s, %s, %s, %s, %s)', params)
        conn.commit()
        params = []
    if len(row) > 0:
        if re.match('^CLDR', row[0]) is not None:
            region_code = re.match('^CLDR Region Code: ([A-Z]{2})', row[0]).group(1)
        elif re.match('^Domain', row[0]) is not None:
            domain = re.match('^Domain: (.*)$', row[0]).group(1)
            cursor.execute('INSERT INTO sites (url, cldr_code) VALUES (%s, %s)', (domain, region_code))
            cursor.execute('SELECT LAST_INSERT_ID()')
            site_id = cursor.fetchone()[0]
            previous_end_time = 0
        elif re.match('^End Time', row[0]) is None:
            row = map(lambda field: 0 if field == '' else int(field), row)
            interval = row[0] - previous_end_time if previous_end_time > 0 else 60
            params.append((site_id, row[0], interval, reader.line_num, row[1], row[2], row[3], row[4]))
            previous_end_time = row[0]
    if reader.line_num == MAX_LINES:
        finish()
finish() 
