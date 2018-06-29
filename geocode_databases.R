#
# Author: Omar Trejo Navarro
# Email:  otrenav [at] gmail [dot] com
#
# If you have a database with full addresses in it for your
# entities, this file can help you get the coordinates corresponding
# to those addresses so that you can use it with the rest of the
# application. Simply adjust the parameters below to fit your
# data frame.
#

#
# Note: be careful with addresses that are not well formatted
# and avoid sending unnecessary requests to the Google servers.
# You can freely use up to: 2,500 requests per 24 hour period
# and 10 requests per second. Find more information in:
# https://developers.google.com/maps/documentation/geocoding/

# This script is designed to be executed manually part by part.
# First load the package and the function in memory, and then
# run the for loop to geocode your database.

# TODO: Tidy up and generalize.

require(rjson)
#
# Change to the name of your database
#
eventos <- read.csv("./data/db.csv")
eventos <- cbind(eventos,
                 matrix(data = NA,
                        nrow = dim(eventos)[1],
                        ncol = 2))
colnames(eventos)[15:16] <- c("latitude", "longitude")

getCoordinates <- function(address) {
    #
    # Find the coordiates for a given address.
    #
    # In:
    #   address := an address that Google Maps can find
    # Out:
    #   coords := coordinates associated to the given address
    #
    url <- paste(
        "http://maps.google.com/maps/api/geocode/json?address=",
        address, "&sensor=false", sep = "")
    map_data <- fromJSON(paste(readLines(url), collapse = ""))
    if (map_data$status == "OK") {
        coords <- cbind(
            map_data$results[[1]]$geometry$location$lat,
            map_data$results[[1]]$geometry$location$lng)
    } else {
        coords <- NULL
    }
    return(coords)
}

counter <- 1
for (i in 1:dim(eventos)[1]) {
    if (any(is.na(eventos$latitude[i]), is.na(eventos$longitude[i]))) {
        #
        # Change the column naves according to your database
        #
        address <- paste(
            gsub(" ", "+", eventos$ubicacion[i], fixed = TRUE), "+",
            gsub(" ", "+", eventos$colonia[i], fixed = TRUE), ",+",
            gsub(" ", "+", eventos$cp[i], fixed = TRUE), ",+",
            "Mexico", sep = "")
        print(i)
        print(address)
        if (address != "+NA,+") {
            coords <- getCoordinates(address)
            counter <- counter + 1
            if (!is.null(coords)) {
                eventos[i, c("latitude", "longitude")] <- coords
            }
        }
    }
    if (counter %% 10 == 0) {
        # Pause because of Google's limits on
        # the free version of the API.
        Sys.sleep(5)
    }
}

write.csv(eventos, file = "./data/db_with_coords.csv", row.names = FALSE)

