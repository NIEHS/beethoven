#!/bin/sh
s="2023-06-01"
e="2023-06-10"
user="mitchell_manware"
pass="Chssoccer6797?"

until [[ $s > $e ]]; do
	
	year=${s:0:4}
	month=${s:5:2}
	day=${s:8:2}
	
	echo wget --user=$user --password=$pass https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/M2SDNXSLV.5.12.4/$year/$month/MERRA2400.statD_2d_slv_Nx.$year$month$day.nc4
	
	s=$(date -I -d "$s + 1 day")
	> MERRA_urls.txt
done
