#####################################################################################
# kdouglas
# March 2019
# miscellaneous/citi_bike/gbfs/GBFS_Parser.R
#####################################################################################

rm(list = ls())
options(stringsAsFactors = FALSE)
table = function (..., useNA = 'ifany') base::table(..., useNA = useNA)
dir_prefix = "C:/Users/kdouglas/Documents/miscellaneous/citi_bike/gbfs/"

gc()

require(dplyr)
require(magrittr)
require(ggplot2)
require(ggthemes)
require(stringr)
require(geosphere)
require(jsonlite)
require(googlesheets)

load(paste0(dir_prefix, "Loaded_Stations.RData"))

#####################################################################################
#####################################################################################
#####################################################################################

### Grab relevant JSON URLs

GBFS = fromJSON(txt = "http://gbfs.citibikenyc.com/gbfs/gbfs.json")

URLs = GBFS$data$en$feeds
URLs

for (i in 1:nrow(URLs)) {
    assign(URLs$name[i], fromJSON(txt = URLs$url[i]))
}

system_regions
system_alerts
system_information
station_status
station_information

#####################################################################################

### Isolate NYC stations

Dim_Station = station_information$data$stations

dim(Dim_Station)
head(Dim_Station)

table(Dim_Station$region_id) # 70 is JC, 71 is NYC... NA is also NYC, apparently
Dim_Station[is.na(Dim_Station$region_id),]
head(Dim_Station[Dim_Station$region_id %in% 70,])

NYC_Stations = Dim_Station[Dim_Station$region_id %in% c(71, NA),] # 766

dim(NYC_Stations)
head(NYC_Stations)

# write.csv(NYC_Stations[,c("station_id","name","lat","lon")],
#           file = "C:/Users/kdouglas/Documents/miscellaneous/citi_bike/tableau_map/Lat_Longs.csv",
#           row.names = FALSE)

#####################################################################################

### Create distance matrix

Dist_Matrix = data.frame(From = vector(),
                         To = vector(),
                         From_Name = vector(),
                         To_Name = vector(),
                         Dist_Type = vector(),
                         Mi = vector())

for (i in 1:nrow(NYC_Stations)) { # will have two entries for every pair (each way) - important for easy lookup
    Dist_Matrix = rbind(Dist_Matrix, data.frame(From = rep(NYC_Stations$station_id[i], nrow(NYC_Stations)),
                                                To = NYC_Stations$station_id,
                                                From_Name = rep(NYC_Stations$name[i], nrow(NYC_Stations)),
                                                To_Name = NYC_Stations$name,
                                                Dist_Type = "Euclidian",
                                                Mi = distCosine(data.frame(lon = rep(NYC_Stations$lon[i], nrow(NYC_Stations)),
                                                                           lat = rep(NYC_Stations$lat[i], nrow(NYC_Stations))),
                                                                NYC_Stations[,c("lon","lat")], r = 3959)))
}

Dist_Matrix$Mi[Dist_Matrix$From == Dist_Matrix$To] = NA

dim(Dist_Matrix)
head(Dist_Matrix)

tail(table(Dist_Matrix$Mi))

# Are the furthest distances between stations from the IC HQ to the Columbia area? Yep

# Dist_Matrix[!is.na(Dist_Matrix$Mi) & Dist_Matrix$Mi > 11.2769,]

###########################
###########################
###########################
### Add Manhattan distance
###########################
###########################
###########################

# save.image("~/miscellaneous/citi_bike/gbfs/Loaded_Stations.RData")

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################

### Re-grab relevant JSON URLs

This_Date = substr(Sys.time(), 1, 10)
This_Time = substr(Sys.time(), 12, 19)

GBFS = fromJSON(txt = "http://gbfs.citibikenyc.com/gbfs/gbfs.json")

URLs = GBFS$data$en$feeds
URLs

for (i in 1:nrow(URLs)) {
    assign(URLs$name[i], fromJSON(txt = URLs$url[i]))
}

system_regions
system_alerts
system_information
station_status
station_information

#####################################################################################

### Create angel point matrix (are Angels data in the Citi Bike GBFS itself?)

names(station_status$data$stations) # not here

Layer_NYC = fromJSON(txt = "https://layer.bicyclesharing.net/map/v1/nyc/stations")$features

dim(Layer_NYC)
head(Layer_NYC)

table(Layer_NYC$properties$bike_angels_digits)
Layer_NYC[is.na(Layer_NYC$properties$bike_angels_digits),]

table(Layer_NYC$properties$bike_angels_points)
Layer_NYC[Layer_NYC$properties$bike_angels_points %in% 5,]

table(Layer_NYC$properties$bike_angels_action)
table(Layer_NYC$properties$bike_angels_points[Layer_NYC$properties$bike_angels_action %in% "neutral"])

### Function to calculate Angel Points vector

Get_Angel_Points = function(from_action, from_points, to_action, to_points) {
    points_eligible = from_action %in% c("take","neutral") & to_action %in% c("neutral","give")
    return(points_eligible * (from_points + to_points))
}

Get_Angel_Points(head(Layer_NYC$properties$bike_angels_action),
                 head(Layer_NYC$properties$bike_angels_points),
                 head(Layer_NYC$properties$bike_angels_action),
                 head(Layer_NYC$properties$bike_angels_points))

#####################################################################################

### Merge Angels info onto Dist_Matrix

Layer_NYC$properties$station_id[!(Layer_NYC$properties$station_id %in% NYC_Stations$station_id)]
table(Layer_NYC$properties$station_id %in% NYC_Stations$station_id)
table(NYC_Stations$station_id %in% Layer_NYC$properties$station_id) # all TRUE (and even worse)

Dist_Matrix = merge(Dist_Matrix, Layer_NYC$properties[,c("station_id","bike_angels_action","bike_angels_points")], by.x = "From", by.y = "station_id", all.x = T)
names(Dist_Matrix)[names(Dist_Matrix) %in% c("bike_angels_action","bike_angels_points")] = c("From_Action","From_Pts")

Dist_Matrix = merge(Dist_Matrix, Layer_NYC$properties[,c("station_id","bike_angels_action","bike_angels_points")], by.x = "To", by.y = "station_id", all.x = T)
names(Dist_Matrix)[names(Dist_Matrix) %in% c("bike_angels_action","bike_angels_points")] = c("To_Action","To_Pts")

Dist_Matrix$Angel_Pts = Get_Angel_Points(Dist_Matrix$From_Action, Dist_Matrix$From_Pts, Dist_Matrix$To_Action, Dist_Matrix$To_Pts)
table(Dist_Matrix$Angel_Pts)

Dist_Matrix = Dist_Matrix[,!grepl("Action",names(Dist_Matrix))]

Dist_Matrix[Dist_Matrix$Angel_Pts %in% 8,]
head(Dist_Matrix[is.na(Dist_Matrix$Angel_Pts),])

Dist_Matrix$Angel_Pts_Per_Mi = Dist_Matrix$Angel_Pts / Dist_Matrix$Mi

head(table(Dist_Matrix$Angel_Pts_Per_Mi))
tail(table(Dist_Matrix$Angel_Pts_Per_Mi))

# Append on # of bikes and # of docks

Dist_Matrix = merge(Dist_Matrix, station_status$data$stations[,c("station_id","num_bikes_available","num_ebikes_available")],
                    by.x = "From", by.y = "station_id", all.x = TRUE)
names(Dist_Matrix)[names(Dist_Matrix) %in% c("num_bikes_available","num_ebikes_available")] = c("Num_Bikes","Num_E_Bikes")

Dist_Matrix = merge(Dist_Matrix, station_status$data$stations[,c("station_id","num_docks_available")], by.x = "To", by.y = "station_id", all.x = TRUE)
names(Dist_Matrix)[names(Dist_Matrix) %in% c("num_docks_available")] = c("Num_Docks")

# Rejoice!

Dist_Matrix = arrange(Dist_Matrix, desc(Angel_Pts_Per_Mi), desc(Angel_Pts))

dim(Dist_Matrix)
head(Dist_Matrix, 15)

# Lazy analysis to think about a threshold for saving data; keep the Angel Pt NAs for now (aren't meaningless; different than a plain 0)

ggplot(data = Dist_Matrix, aes(Angel_Pts_Per_Mi)) + geom_histogram(fill="skyblue", col="black")

head(table(Dist_Matrix$Angel_Pts_Per_Mi))
tail(table(Dist_Matrix$Angel_Pts_Per_Mi))

head(Dist_Matrix[is.na(Dist_Matrix$Angel_Pts_Per_Mi),])
tail(table(Dist_Matrix$Mi[is.na(Dist_Matrix$Angel_Pts_Per_Mi)]))
head(Dist_Matrix[is.na(Dist_Matrix$Angel_Pts_Per_Mi) & !is.na(Dist_Matrix$Mi),])

summary(Dist_Matrix$Angel_Pts_Per_Mi)
quantile(Dist_Matrix$Angel_Pts_Per_Mi, probs = seq(0.9, 1.0, 0.01), na.rm = T)
quantile(Dist_Matrix$Angel_Pts_Per_Mi, probs = seq(0.95, 1.0, 0.005), na.rm = T)

length(Dist_Matrix$Angel_Pts_Per_Mi[!is.na(Dist_Matrix$Angel_Pts_Per_Mi) & Dist_Matrix$Angel_Pts_Per_Mi >= 1.0]) # this should be fine when trimmed
length(Dist_Matrix$Angel_Pts_Per_Mi[!is.na(Dist_Matrix$Angel_Pts_Per_Mi) & Dist_Matrix$Angel_Pts_Per_Mi >= 1.5])
length(Dist_Matrix$Angel_Pts_Per_Mi[!is.na(Dist_Matrix$Angel_Pts_Per_Mi) & Dist_Matrix$Angel_Pts_Per_Mi >= 2.0])

#####################################################################################
#####################################################################################
#####################################################################################

### Append datetime and Write trips that meet a threshold to a repository to analyze later

Dist_Matrix$Dist_Type[Dist_Matrix$Dist_Type == "Euclidian"] = "E"
table(Dist_Matrix$Dist_Type)

Dist_Matrix[grepl("Mi",names(Dist_Matrix))] = lapply(Dist_Matrix[grepl("Mi",names(Dist_Matrix))], round, 3)

Dist_Matrix$Date = This_Date
Dist_Matrix$Time = This_Time

is.numeric(Dist_Matrix$To)
is.character(Dist_Matrix$To)
is.character(Dist_Matrix$From)
table(Dist_Matrix$To)
table(Dist_Matrix$From)

Dist_Matrix$To %<>% as.numeric()
Dist_Matrix$From %<>% as.numeric()

dim(Dist_Matrix)
head(Dist_Matrix)

THRESH = 1.0

write.csv(Dist_Matrix[!is.na(Dist_Matrix$Angel_Pts_Per_Mi) & Dist_Matrix$Angel_Pts_Per_Mi >= THRESH, !(names(Dist_Matrix) %in% c("From_Name", "To_Name"))],
          file = paste0(dir_prefix, "Historical_Data/Qual_Trips_", This_Date, "_", gsub(":", "_", This_Time), ".csv"),
          row.names = FALSE)

#####################################################################################
#####################################################################################
#####################################################################################

### Try Google Sheets API; for some reason, gs_ls() breaks my machine... lame!

# gs_auth(new_user = TRUE)

# gs_ls()
# gs_ls("GBFS_Angels")

GBFS_Angels = gs_title("GBFS_Angels")
GBFS_Angels

# GBFS_Angels %>% gs_browse()
# GBFS_Angels %>% gs_read()

# Existing_Sheet_Name = gs_ws_ls(GBFS_Angels)
# GBFS_Angels = GBFS_Angels %>% gs_ws_new(ws_title = paste0(This_Date, " @ ", This_Time),
#                                         input = head(Dist_Matrix[, !(names(Dist_Matrix) %in% c("Date", "Time"))], 25),
#                                         trim = TRUE,
#                                         verbose = FALSE)
# GBFS_Angels = GBFS_Angels %>% gs_ws_delete(ws = Existing_Sheet_Name)

GBFS_Angels = GBFS_Angels %>% gs_ws_rename(from = gs_ws_ls(GBFS_Angels)[1], to = paste0(This_Date, " @ ", This_Time))
GBFS_Angels = GBFS_Angels %>% gs_edit_cells(input = head(Dist_Matrix[, !(names(Dist_Matrix) %in% c("Date", "Time"))], 25), trim = TRUE)
