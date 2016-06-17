#!/bin/bash
#
# add your solution after each of the 10 comments below
#

# count the number of unique stations
cut -d, -f4 201402-citibike-tripdata.csv | sort | uniq | wc -l
330

# count the number of unique bikes
cut -d, -f12 201402-citibike-tripdata.csv | sort | uniq | wc -l
5700

# extract all of the trip start times
 cut -d, -f2 201402-citibike-tripdata.csv [| cut -d" " -f2 | tr -d '"']

# count the number of trips per day
cut -d, -f2 201402-citibike-tripdata.csv | cut -d" " -f1 | tr -d '"' | uniq -c


# find the day with the most rides
cut -d, -f2 201402-citibike-tripdata.csv | cut -d" " -f1 | tr -d '"' | uniq -c | sort -n | tail -n1

# find the day with the fewest rides
cut -d, -f2 201402-citibike-tripdata.csv | cut -d" " -f1 | tr -d '"' | uniq -c | sort -n | head -n1

# find the id of the bike with the most rides
 cut -d, -f12 201402-citibike-tripdata.csv | sort | uniq -c | sort -n | tail -n1

# count the number of riders by gender and birth year
awk -F, '{if ($15 ~ 1) men[$14]++; else if ($15 ~ 2) women[$14]++;} END {print "women:"; for (key in women) print key"\t"women[key]; print "men:"; for (key in men) print key"\t"men[key];}' 201402-citibike-tripdata.csv

sort 201402-citibike-tripdata.csv | awk -F, '{count[$14][$15]++} END {for (i in count) for (j in count[i])  {if ($j ~ 1) print male; else if ($j ~ 2) print female; print "\t"  count[i][j]}}' 201402-citibike-tripdata.csv

# count the number of trips that start on cross streets that both contain numbers (e.g., "1 Ave & E 15 St", "E 39 St & 2 Ave", ...)
cut -d, -f5 201402-citibike-tripdata.csv | grep ".*[0-9].*&.*[0-9].*" | wc -l
90549

# compute the average trip duration
 cut -d, -f1 201402-citibike-tripdata.csv | tr -d '"' | awk '{ count[i++]=$1 } END { for (i in count) result += count[i]; print (result/(i+1)); }'
874.516 - 14 min 34 sec
#the following tries to get the tripduration from the start trip time and stop trip time fields
cut -d, -f2,3 201402-citibike-tripdata.csv | tr , : | tr -d '"' | tr - : | tr " " : | awk -F: '{ if ($3 ~ $9) count[i++]=(($10*60*60)+($11*60)+$12)-(($5*60)+($4*60*60)+$6); else count[i++]=86400-(($4*60*60)+($5*60)+$6)+(($10*60*60)+($11*60)+$12);  } END { for (i in count) total += count[i]; result=total/(i); print result; }'

