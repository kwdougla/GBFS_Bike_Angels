#####################################################################################
# kdouglas
# March 2019
# miscellaneous/citi_bike/gbfs/GBFS_Refresher.R
#####################################################################################

rm(list = ls())
options(stringsAsFactors = FALSE)
table = function (..., useNA = 'ifany') base::table(..., useNA = useNA)
dir_prefix = "C:/Users/kdouglas/Documents/github/GBFS_Bike_Angels/"

gc()

require(dplyr)
require(magrittr)
require(geosphere)
require(jsonlite)
require(googlesheets)

load(paste0(dir_prefix, "Loaded_Stations.RData"))

### Re-grab relevant JSON URLs

This_Date = substr(Sys.time(), 1, 10)
This_Time = substr(Sys.time(), 12, 19)

GBFS = fromJSON(txt = "http://gbfs.citibikenyc.com/gbfs/gbfs.json")

URLs = GBFS$data$en$feeds

for (i in 1:nrow(URLs)) {
    assign(URLs$name[i], fromJSON(txt = URLs$url[i]))
}

#####################################################################################

### Create angel point matrix (are Angels data in the Citi Bike GBFS itself?)

Layer_NYC = fromJSON(txt = "https://layer.bicyclesharing.net/map/v1/nyc/stations")$features

### Function to calculate Angel Points vector

Get_Angel_Points = function(from_action, from_points, to_action, to_points) {
    points_eligible = from_action %in% c("take","neutral") & to_action %in% c("neutral","give")
    return(points_eligible * (from_points + to_points))
}

#####################################################################################

### Merge Angels info onto Dist_Matrix

Dist_Matrix = merge(Dist_Matrix, Layer_NYC$properties[,c("station_id","bike_angels_action","bike_angels_points")], by.x = "From", by.y = "station_id", all.x = T)
names(Dist_Matrix)[names(Dist_Matrix) %in% c("bike_angels_action","bike_angels_points")] = c("From_Action","From_Pts")

Dist_Matrix = merge(Dist_Matrix, Layer_NYC$properties[,c("station_id","bike_angels_action","bike_angels_points")], by.x = "To", by.y = "station_id", all.x = T)
names(Dist_Matrix)[names(Dist_Matrix) %in% c("bike_angels_action","bike_angels_points")] = c("To_Action","To_Pts")

Dist_Matrix$Angel_Pts = Get_Angel_Points(Dist_Matrix$From_Action, Dist_Matrix$From_Pts, Dist_Matrix$To_Action, Dist_Matrix$To_Pts)

Dist_Matrix = Dist_Matrix[,!grepl("Action",names(Dist_Matrix))]

Dist_Matrix$Angel_Pts_Per_Mi = Dist_Matrix$Angel_Pts / Dist_Matrix$Mi

### Append on # of bikes and # of docks

Dist_Matrix = merge(Dist_Matrix, station_status$data$stations[,c("station_id","num_bikes_available","num_ebikes_available")],
                    by.x = "From", by.y = "station_id", all.x = TRUE)
names(Dist_Matrix)[names(Dist_Matrix) %in% c("num_bikes_available","num_ebikes_available")] = c("Num_Bikes","Num_E_Bikes")

Dist_Matrix = merge(Dist_Matrix, station_status$data$stations[,c("station_id","num_docks_available")], by.x = "To", by.y = "station_id", all.x = TRUE)
names(Dist_Matrix)[names(Dist_Matrix) %in% c("num_docks_available")] = c("Num_Docks")

### Rejoice!

Dist_Matrix = arrange(Dist_Matrix, desc(Angel_Pts_Per_Mi), desc(Angel_Pts))

#####################################################################################
#####################################################################################
#####################################################################################

### Append datetime and Write trips that meet a threshold to a repository to analyze later

Dist_Matrix$Dist_Type[Dist_Matrix$Dist_Type == "Euclidian"] = "E"

Dist_Matrix[grepl("Mi",names(Dist_Matrix))] = lapply(Dist_Matrix[grepl("Mi",names(Dist_Matrix))], round, 3)

Dist_Matrix$Date = This_Date
Dist_Matrix$Time = This_Time

Dist_Matrix$To %<>% as.numeric()
Dist_Matrix$From %<>% as.numeric()

THRESH = 1.0

write.csv(Dist_Matrix[!is.na(Dist_Matrix$Angel_Pts_Per_Mi) & Dist_Matrix$Angel_Pts_Per_Mi >= THRESH, !(names(Dist_Matrix) %in% c("From_Name", "To_Name"))],
          file = paste0(dir_prefix, "Historical_Data/Qual_Trips_", This_Date, "_", gsub(":", "_", This_Time), ".csv"),
          row.names = FALSE)

#####################################################################################
#####################################################################################
#####################################################################################

### Refresh using the Google Sheets API (googlesheets package)

gs_auth(token = paste0(dir_prefix, "kwdougla_googlesheets_token.rds"))
GBFS_Angels = gs_title("GBFS_Angels")
GBFS_Angels = GBFS_Angels %>% gs_ws_rename(from = gs_ws_ls(GBFS_Angels)[1], to = paste0(This_Date, " @ ", This_Time))
GBFS_Angels = GBFS_Angels %>% gs_edit_cells(input = head(Dist_Matrix[, !(names(Dist_Matrix) %in% c("Date", "Time"))], 25), trim = TRUE)
