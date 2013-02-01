README
======

Description
-----------
Included with this document are a collection of folders, files and scripts in
R and Python that aid in discovering and graphing anomalies within web request
data.  Data of this sort can be harvested from server logs, DNS logs (OpenDNS,
etc.), router logs, etc.  The expected input data consists of counts of visits,
404 errors, connection errors, and DNS errors.  Of course, the scripts can be
revised to handle different or additional data, as is available.

How do I make it go?
--------------------
1. Ensure that Python, R, and MySQL are installed
2. Extract the archive of these scripts to a directory of your choosing and cd
   into it
3. Put a prepared CSV file into _raw\_data/_
4. Create a MySQL database 
6. Put the DB username, password, and DB name into _import/insert\_data.py_,
   _import/setup\_database.py_, and _lib/constants.R_
7. To create the database tables, run:
   
        python import/setup_database.py

8. To parse the raw data and put into into the database, run:
   
        python import/insert_data.py

9. To find the top sites by visit (so we get better data), run: 
   
        R -e 'source("scripts/find_top_sites_by_visits.R")'

10. To find all the anomalies for these top sites, run:
   
        R -e 'source("scripts/find_all_anoms.R")'

11. Open _output/top\_anoms.csv_ with a spreadsheet program and filter and sort to
    heart's content - then save/export as CSV
12. To graph all the anomalies that exist in the CSV from step 11, run:
   
        R -e 'source("scripts/make_all_graphs.R")'

    For each anomaly in _output/top\_anoms.csv_, this script will output a PNG
    into _output/graphs/_.

13. Play with the other scripts to get more data and graphs

What is everything?
-------------------
* __raw\_data/__ - contains all the raw data

* __import/__ - contains scripts to import data from the CSV into MySQL
    * _insert\_data.py_ - parse the raw data and dump into the MySQL database
    * _setup\_database.py_ - create the MySQL database tables

* __lib/__ - Contains the meat of the R scripts used in analysis
    * _anomaly\_detection.R_ - contains the functions involved in finding the 
      anomalies and modifying time series
    * _constants.R_ - a collection of constants and settings that get used throughout
    * _graphs.R_ - contains the scripts that generate various graphs
    * _utils.R_ - miscellaneous functions

* __output/__ - The directory into which graphs and generated CSVs are dumped
    * _graphs/_ - here's where the graphs go
    * _top\_anoms.csv_ - a CSV containing the anomalies that have already been detected

* __scripts/__ - Little R scripts that utilize the files in _lib/_ to output the 
  various CSVs and graphs
    * _find\_all\_anoms.R_ - find all anomalies in our top sites by visit for
      our time window and spit them out into a CSV
    * _find\_down\_events.R_ - find all the events in which an error increase
      corresponds in time with a decrease in visits
    * _find\_persistent\_issues.R_ - look through the top sites by visits 
      for our time window and find sites where the mean number of errors is greater 
      than mean visits times your error threshold
    * _find\_top\_sites\_by\_visits.R_ - just find the top sites and spit out
      a CSV in _output/_
    * _graph\_all\_down\_events.R_ - pull all down events from a CSV generated 
      by _find\_down\_events.R_ and output pngs into _output/graphs/_
    * _make\_all\_graphs.R_ - pull anomaly info from a CSV and output PNGs into
      _output/graphs/_
    * _make\_graphs.R_ - catch-all file that's used to output various graphs 
      on an as-needed basis into _output/graphs_

Additional Notes
----------------
This code is experimental.  Although we believe that it should function as
advertised, the coding is ugly.  In the unlikely event that this code is
needed for more public purposes, we can make the code less ugly; for current
purposes, however, this should be sufficient.

The _find\_down\_events.R_ code, which outputs instances where a decrease in
visits overlaps with an increase in errors, has a known bug that results in the
output of non-intersecting anomalies output in addition to intersecting ones.

TODO
----
We can measure the anomalousness of our anomalies.  This should be spit out
into our CSV so we can sort and filter to get the most novel anomalies.

Contributors
------------
* Ryan Budish  - ryan@herdict.org
* Justin Clark - jclark@cyber.law.harvard.edu

Copyright
---------
Copyright 2012 President and Fellows of Harvard College

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
