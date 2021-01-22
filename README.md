# Shinyapp using Marine Data

This is a Shinyapp made using Marine Data.


## App description
This app:

- allows the user select a vessel type from a dropdown field (they are in alphabetical order)
- allows the user select a vessel from a dropdown field (available vessels correspond to the selected type). They are also in alphabetical order.
- finds, for the vessel selected, the observation when it sailed the longest distance between two consecutive observations. If there is a situation when a vessel moves exactly the same amount of meters, it selects the most recent.
- displays that on the map, showing two points: the beginning and the end of the movement. The points are colored based on whether the vessel was in movement or parked. Changing type and vessel name re-renders the map.
- provide a short note saying how much the ship sailed, in meters.

It also:

- provides a short note saying how much time passed between the observations.
- provides vessel information.
- allows the user to see the timestamps of the observations in the map by hovering over the points.

## Details
[`pre_proc.R`](pre_proc.R) is a file used to **pre proc**ess the data. 

In particular, I read raw data using `fread` package and then, after some calculations, created some
`.RDS` data files that are then loaded by the application. This allowed to upload files with small sizes to shinyapps.io.

One important thing to notice is that I created my own SHIP_ID variable, due to inconsistencies found with the provided one (different vessel names had the same SHIP_ID).

