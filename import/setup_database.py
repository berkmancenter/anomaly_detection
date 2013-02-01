import sys
import MySQLdb

try:
	conn = MySQLdb.connect( host="localhost", user="", passwd="", db="" )
except:
    exceptionType, exceptionValue, exceptionTraceback = sys.exc_info()
    sys.exit("Database connection failed!\n ->%s" % (exceptionValue))

cursor = conn.cursor()

cursor.execute('DROP TABLE IF EXISTS requests')
cursor.execute('DROP TABLE IF EXISTS sites')
cursor.execute("CREATE TABLE sites (id INT UNSIGNED AUTO_INCREMENT NOT NULL PRIMARY KEY, url VARCHAR(255), cldr_code VARCHAR(3) NOT NULL, INDEX (url), INDEX (cldr_code))")
cursor.execute("CREATE TABLE requests (id BIGINT UNSIGNED AUTO_INCREMENT NOT NULL PRIMARY KEY, site_id INT UNSIGNED, time INT, duration INT, line_no INT, visits INT, not_found_errors INT, connection_errors INT, dns_errors INT, FOREIGN KEY (site_id) REFERENCES sites(id), INDEX (time))")

cursor.close()
conn.commit()
conn.close()
sys.exit(0)
