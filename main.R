#
# Author: Omar Trejo Navarro
# Email:  otrenav [at] gmail [dot] com
#
# The main file to be run in the application. All
# modifiable parameters can be adjusted in this file.
#

# TODOS:
#   1. Error checking in all functions.
#   2. Explicit arguments in all functions.

activateSystem <- function(option = "map") {
    #
    # Default parameters
    #
    if (option == "map") {
        keywords.file        <- "./data/keywords.csv"
        entities.file        <- "./data/gasolineras.csv"
        geo.center           <- data.frame(-99.133441, 19.432395)
        radius               <- 30
        tweets.number        <- 100
        since.days.ago       <- NULL
        since.date           <- NULL
        plot                 <- TRUE
        each.confirm         <- FALSE
        real.response        <- FALSE
        full.route           <- TRUE
        lang.code            <- "es"
        save.results         <- FALSE
        alert.accounts       <- NULL
        alert.messages       <- NULL
    } else {
        keywords.file        <- "./data/test_keywords.csv"
        entities.file        <- "./data/gasolineras.csv"
        geo.center           <- data.frame(-99.133441, 19.432395)
        radius               <- NULL
        tweets.number        <- 100
        since.days.ago       <- NULL
        since.date           <- NULL
        plot                 <- FALSE
        each.confirm         <- FALSE
        real.response        <- TRUE
        full.route           <- TRUE
        lang.code            <- "es"
        save.results         <- FALSE
        alert.accounts       <- c("@datata_mx")
        alert.messages       <- c("TIPO")
    }
    colnames(geo.center) <- c("longitude", "latitude")
    #
    # Load functions in memory
    #
    source("./functions.R")
    #
    # Main function routine
    #
    if (plot) {
        mainTwitterGeoSearch(keywords.file  = keywords.file,
                             entities.file  = entities.file,
                             geo.center     = geo.center,
                             radius         = radius,
                             tweets.number  = tweets.number,
                             since.days.ago = since.days.ago,
                             since.date     = since.date,
                             alert.accounts = alert.accounts,
                             alert.messages = alert.messages,
                             each.confirm   = each.confirm,
                             real.response  = real.response,
                             full.route     = full.route,
                             lang.code      = lang.code,
                             plot           = plot,
                             save.results   = save.results)
    } else {
        iter <- 1
        while(TRUE) {
            cat(sprintf("\n[+] Iteration:            %d\n", iter))
            mainTwitterGeoSearch(keywords.file  = keywords.file,
                                 entities.file  = entities.file,
                                 geo.center     = geo.center,
                                 radius         = radius,
                                 tweets.number  = tweets.number,
                                 since.days.ago = since.days.ago,
                                 since.date     = since.date,
                                 alert.accounts = alert.accounts,
                                 alert.messages = alert.messages,
                                 each.confirm   = each.confirm,
                                 real.response  = real.response,
                                 full.route     = full.route,
                                 lang.code      = lang.code,
                                 plot           = plot,
                                 save.results   = save.results)
            iter <- iter + 1
            Sys.sleep(5)
        }
    }
}
activateSystem(option = "twitter")
