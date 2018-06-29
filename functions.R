#
# Author: Omar Trejo Navarro
# Email:  otrenav [at] gmail [dot] com
#
# This file contains the functions used in the application.
# It should be executed from within the main() function.
#

require(stringr)
require(twitteR)
require(ggplot2)
require(plyr)
require(ggmap)

twitterGeoSearch <- function(keywords       = NULL,
                             geo.center     = NULL,
                             radius         = NULL,
                             tweets.number  = 100,
                             since.days.ago = NULL,
                             since.date     = NULL) {
    #
    # Search Twitter for keywords using the REST API and the
    # twitteR package. This is a geographic search.
    #
    # In:
    #   keywords := data frame with one column and a keyword in each row.
    #   geo.center := a vector containing longitude and latitude.
    #   radius := radius around the geo.center (in kilometers).
    #   tweets.number := maximum number of tweets to search for.
    #   since.days.ago := number of days back that are of interest.
    # Out:
    #   search.results := A list of data frames, each with a search
    #   result for the corresponding keyword.
    #
    # Notes:
    #   Warnings are suppressed from the searchTwitter()
    #   function. The warnings that may appear are related
    #   to finding a lower number of tweets than those there
    #   requested. As it's not important to me, I'm supressing
    #   it knowing that I may supress other warnings too.
    #   You can read what warnings were produced with warnings().
    #

    # TODO:
    #   1. Include since.days.ago option.
    #   2. Include until.date option.

    counter <- 1
    search.results <- list()
    geo.zone <- paste(geo.center$latitude, ",",
                      geo.center$longitude, ",", radius, "km", sep = "")
    for (keyword in keywords[, 1]) {
        cat(paste("[+] Searching for:        ", keyword, "\n", sep = ""))
        if (!is.null(geo.center) && !is.null(radius)) {
            #
            # Geographic search
            #
            if (!is.null(since.days.ago)) {
                tweets <- suppressWarnings(
                                searchTwitter(keyword,
                                              n       = tweets.number,
                                              since   = since.days.ago,
                                              until   = NULL,
                                              geocode = geo.zone))
            } else if (!is.null(since.date)) {
                tweets <- suppressWarnings(
                                searchTwitter(keyword,
                                              n       = tweets.number,
                                              since   = since.date,
                                              until   = NULL,
                                              geocode = geo.zone))
            } else {
                tweets <- suppressWarnings(
                                searchTwitter(keyword,
                                              n       = tweets.number,
                                              since   = NULL,
                                              until   = NULL,
                                              geocode = geo.zone))
            }
        } else {
            #
            # Non-geographic search
            #
            if (!is.null(since.days.ago)) {
                tweets <- suppressWarnings(
                                searchTwitter(keyword,
                                              n       = tweets.number,
                                              since   = since.days.ago,
                                              until   = NULL,
                                              geocode = NULL))
            } else if (!is.null(since.date)) {
                tweets <- suppressWarnings(
                                searchTwitter(keyword,
                                              n       = tweets.number,
                                              since   = since.date,
                                              until   = NULL,
                                              geocode = NULL))
            } else {
                tweets <- suppressWarnings(
                                searchTwitter(keyword,
                                              n       = tweets.number,
                                              since   = NULL,
                                              until   = NULL,
                                              geocode = NULL))
            }
        }
        search <- do.call("rbind", lapply(tweets, as.data.frame))
        search.results[[counter]] <- search
        counter <- counter + 1
    }

    search.results <- unifySearchResults(search.results, keywords)
    search.results <- search.results[, c("text", "created", "id",
                                         "screenName", "longitude",
                                         "latitude", "keyword")]
    if (!is.null(geo.center) && !is.null(radius)) {
        search.results <- search.results[!is.na(search.results$latitude), ]
        cat("[+] Keep geo-referenced:  YES\n")
    } else {
        cat("[+] Keep geo-referenced:  NO\n")
    }
    return(search.results)
}

plotCounts <- function(search, frequency = 0) {
    #
    # Barplot of the number of tweets per account.
    #
    # In:
    #   search := formatted data frame of search result.
    #   frequency := show only accounts that have tweeted
    #       at least this number of times.
    #

    # TODO: Test

    counts <- table(search$screenName)
    counts <- subset(counts, counts > frequency)

    # TODO: use ggplot2
    barplot(counts)
}

useUTF8 <- function(search) {
    #
    # Convert tweets to UTF-8
    #
    # In:
    #   search := formatted data frame of search results.
    # Out:
    #   search := Data frame with tweets converted to UTF-8.
    #
    search$text <- sapply(search$text,
                          function(row) iconv(row, to = 'UTF-8'))
    return(search)
}

unifySearchResults <- function(search.results, keywords) {
    #
    # Unify search results from all the keywords.
    #
    # In:
    #   search.results := list of data frames, each with a
    #       search result.
    # Out:
    #   search.results.unified := Unified data frame with
    #   results from all the keywords with a column added
    #   in each one containg what keyword produced the
    #   corresponding result.
    #
    search.results.unified <- NULL
    if (length(search.results) > 0) {
        for (i in 1:length(search.results)) {
            search <- search.results[[i]]
            if (length(search)[1] != 0) {
                search$keyword <- keywords[i, ]
                search.results.unified <- rbind(search.results.unified, search)
            }
        }
    }
    return(search.results.unified)
}

plotResults <- function(search.results, geo.center, radius, entities) {
    #
    # Plot tweet locations of tweets in search results
    # on top of a map of the geographic zone.
    #
    # In:
    #   search.results := data frame containing unified
    #       search results with corresponding keywords.
    #   geo.center := center of the greographic zone that
    #       is being analyzed.
    #   radius := the radius around the geo.center. This
    #       two together define the geographic zone that
    #       is being analyzed.
    #   entities := CSV file route for a file with the
    #       coordinates of public entities (Police stations,
    #       Hospitals, etc.). The file must contain columns
    #       named "Name", "longitude", "latitude" to be utilized.
    # Out:
    #   search.results := The same search.results data frame
    #   with two new columns: daydiff and dotsize.
    #
    search.results$daydiff <- dayDiff(search.results$created)
    search.results$dotsize <- search.results$daydiff
    # If a tweet is older than 6 days, classify it as if
    # it were 6 days old. This is done to keep the graphics
    # visually informative and easily readable. The graph
    # actually says "6 days or more" for this category.
    search.results$dotsize[search.results$dotsize >= 6] <- 6
    for (i in 1:dim(search.results)[1]) {
        # Decreasing order
        search.results$dotsize[i] <- 7 - search.results$dotsize[i]
    }
    geo.coords <- c(geo.center$longitude, geo.center$latitude)

    # TODO: Generalize the zoom level

    map <- suppressWarnings(get_map(location  = geo.coords,
                                    zoom      = 12,
                                    scale     = "auto",
                                    filename  = "./output/map",
                                    maptype   = "roadmap",
                                    source    = "google",
                                    messaging = FALSE))
    mapa <- ggmap(map) +
        coord_equal() +
        geom_point(data = entities,
                   aes(x = as.numeric(longitude),
                       y = as.numeric(latitude)),
                       shape = 3, color = "gray50") +
        geom_point(data = search.results,
                   aes(x = as.numeric(longitude),
                       y = as.numeric(latitude),
                       color = keyword,
                       size = factor(dotsize))) +
        scale_size_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                            labels = c("Hace 6 o más días",
                                       "Hace 5 días",
                                       "Hace 4 días",
                                       "Hace 3 días",
                                       "Hace 2 días",
                                       "Ayer",
                                       "Hoy")) +
        stat_density2d(data = search.results,
                       aes(x = as.numeric(longitude),
                           y = as.numeric(latitude),
                           color = keyword),
                       size = 0.4, color = "red") +
        labs(x = "Longitud",
             y = "Latitud",
             size = "Fecha",
             color = "Palabra clave")
    print(mapa)
    return(search.results)
}

dayDiff <- function(dates) {
    #
    # Compute the difference (in days) between the date
    # a tweet was created and today.
    #
    # In:
    #   dates:= vector containing the dates of the tweets.
    # Out:
    #   diff := vector with the difference in days between each
    #   entry in "dates" and today's date.
    #

    # TODO: Generalize timeframe

    dates.formatted <- lapply(dates, function(date)
        ISOdate(strsplit(as.character(date), "-")[[1]][1],
                strsplit(as.character(date), "-")[[1]][2],
                strsplit(strsplit(
                    as.character(date), "-")[[1]][3], " ")[[1]][1]))

    today <- ISOdate(strsplit(as.character(Sys.Date()), "-")[[1]][1],
                     strsplit(as.character(Sys.Date()), "-")[[1]][2],
                     strsplit(as.character(Sys.Date()), "-")[[1]][3])

    diffs <- sapply(dates.formatted, function(date)
        as.numeric(difftime(today, date, units = "days")))

    return(diffs)
}

findRoute <- function(location.from, location.to, lang.code) {
    #
    # Find the optimal route connecting geographic points using
    # Google's Directions API through an HTTP request.
    #
    # In:
    #   location.from := location of the route's start point
    #   location.to := location of the route's end point
    #   lang.code := language code for driving instructions:
    #       - "en" for english
    #       - "es" for spanish
    # Out:
    #   route := JSON object containing all the relevant information for
    #   the route such as ETA, distance, and step by step instructions
    #   of how to get there. This object is created using Google's
    #   Directions API.
    #
    coord.from <- paste(location.from$latitude, ",",
                        location.from$longitude, sep = "")
    coord.to   <- paste(location.to$latitude, ",",
                        location.to$longitude, sep = "")
    url <- paste("http://maps.googleapis.com/maps/api/directions/json?",
                 "origin=",       coord.from,
                 "&destination=", coord.to,
                 "&language=",    lang.code, sep = "")
    route <- fromJSON(paste(readLines(url), collapse = ""))
    if (route$status == "OK") {
        return(route)
    } else {
        return(NULL)
    }
}

printRouteInfo <- function(route, full.route) {
    #
    # Display general information about the route provided
    # and optionally display step by step instructions.
    #
    # In:
    #   route := JSON object with route information created
    #       using Google's Directions API.
    #   full.route := boolean value indicating whether or not
    #       to print step by step instructions.
    #

    cat(sprintf("[*] Distance:             %s\n",
        route$routes[[1]]$legs[[1]]$distance$text))

    cat(sprintf("[*] Time:                 %s\n",
        route$routes[[1]]$legs[[1]]$duration$text))

    cat(sprintf("[*] Origin:               %s\n",
        route$routes[[1]]$legs[[1]]$start_address))

    cat(sprintf("                          %f, %f\n",
        route$routes[[1]]$legs[[1]]$start_location$longitude,
        route$routes[[1]]$legs[[1]]$start_location$latitude))

    cat(sprintf("[*] Destiniy:             %s\n",
        route$routes[[1]]$legs[[1]]$end_address))

    cat(sprintf("                          %f, %f\n",
        route$routes[[1]]$legs[[1]]$end_location$longitude,
        route$routes[[1]]$legs[[1]]$end_location$latitude))

    if (full.route) {
        cat(sprintf("[*] Instructions:\n"))
        for (i in 1:length(route$routes[[1]]$legs[[1]]$steps)) {
            cat(sprintf("\t\t %d. %s \n", i, remoteFormat(
                route$routes[[1]]$legs[[1]]$steps[[i]]$html_instructions)))
        }
    }
}

remoteFormat <- function(instruction) {
    #
    # Remove HTML tags from driving instructions from
    # Google's Directions API.
    #
    # In:
    #   instruction:= string from which to remove HTML tags.
    # Out:
    #   instruciton:= the same string (instruction) without HTML tags.
    #
    instruction <- gsub('<div(.*)>', "", instruction)
    instruction <- gsub('<(.*?)>', "", instruction)
    instruction <- gsub("<U+00E1>", "á", instruction)
    instruction <- gsub("<U+00E9>", "é", instruction)
    instruction <- gsub('<U+00ED>', "í", instruction)
    instruction <- gsub('<U+00F3>', "ó", instruction)
    instruction <- gsub('<U+00FA>', "ú", instruction)
    return(instruction)
}

nearestEntity <- function(location, entities) {
    #
    # Return the index of the nearest entity in entities.
    #
    # In:
    #   location := vector of geographical coordinates from tweet
    #   entities := entities data frame with geographical coordinates
    # Out:
    #   nearest.entity := Index of the entity that is closest to
    #   the origin of the tweet in the entities data frame.
    #

    # TODO: Finish writing this function (right now it calls the
    # funciton that prints today's routes directly).

    entities$dist.to <-
        (as.numeric(location$longitude) - entities$longitude) ^ 2 +
        (as.numeric(location$latitude)  - entities$latitude)  ^ 2
    min.dist.idx   <- which.min(entities$dist.to)
    nearest.entity <- data.frame(longitude = entities$longitude[min.dist.idx],
                                 latitude = entities$latitude[min.dist.idx])
    return(nearest.entity)
}

respondEmergencies <- function(search.results,
                               entities,
                               alert.accounts,
                               alert.messages,
                               each.confirm,
                               real.response,
                               full.route,
                               lang.code) {
    #
    # Respond tweets that contain keywords and display
    # response information in console.
    #
    # In:
    #   alert.accounts := vector of accounts to alert.
    #   alert.messages := vector of messages to send (one for each account).

    # Load previous responses
    tryCatch({
        suppressWarnings(load("./data/responded.Rda"))
        cat("[!] Previous messages:    YES\n")
    }, error = function(e) {
        cat("[!] Previous messages:    NO\n")
    })
    if (!exists("responded")) {
        responded <- NULL
    }
    for (i in 1:dim(search.results)[1]) {
        send <- TRUE
        if (!is.null(responded)) {
            for (k in 1:dim(responded)[1]) {
                # Find out if we have already responded this message:
                if (search.results$screenName[i] == "datata_auto" ||
                    all(search.results[i, ] == responded[k, ])) {
                    send <- FALSE
                    break
                }
            }
        }
        if (send) {
            responded <- rbind(responded, search.results[i, ])
            cat(sprintf("\n[*] Tweet:                %s\n",
                search.results$text[i]))
            cat(sprintf("[*] Keyword:              %s\n",
                search.results$keyword[i]))
            location       <- search.results[i, c("longitude", "latitude")]
            nearest.entity <- nearestEntity(location, entities)
            route          <- findRoute(location, nearest.entity, lang.code)
            printRouteInfo(route, full.route)

            if (each.confirm) {
                respond <- readline("[+] Respond to this message? [y/n] : ")
                if (respond == "y" || respond == "Y") {
                    for (j in 1:length(alert.accounts)) {
                        message <- paste(alert.messages[j], " ",
                                         alert.accounts[j], ": @",
                                         search.results$screenName[i], " ",
                                         search.results$text[i],
                                         sep = "", collapse = "")
                        cat(paste("\n[+] Message to be sent:   ",
                            message, "\n", sep = "", collapse = ""))
                        if (real.response) {
                            tweet(message)
                            cat("\n[+] Would send response:  YES")
                            cat("\n[+] Real response sent:   YES\n")
                        } else {
                            cat("\n[+] Would send response:  YES")
                            cat("\n[+] Real response sent:   NO\n")
                        }
                    }
                } else {
                    cat("\n[+] Would send response:  NO")
                    cat("\n[+] Real response sent:   NO\n")
                }
            } else {
                for (j in 1:length(alert.accounts)) {
                    message <- paste(alert.messages[j], " ",
                                     alert.accounts[j], ": @",
                                     search.results$screenName[i], " ",
                                     search.results$text[i],
                                     sep = "", collapse = "")
                    cat(paste("\n[+] Message to be sent:   ",
                        message, "\n", sep = "", collapse = ""))
                    if (real.response) {
                        tweet(message)
                        cat("[+] Would send response:  YES\n")
                        cat("[+] Real response sent:   YES\n")
                    } else {
                        cat("[+] Would send response:  YES\n")
                        cat("[+] Real response sent:   NO\n")
                    }
                }
            }
        }
    }
    # Save messages that we have already responded to:
    save(responded, file = "./data/responded.Rda")
}

mainTwitterGeoSearch <- function(keywords.file  = NULL,
                                 entities.file  = NULL,
                                 geo.center     = NULL,
                                 radius         = NULL,
                                 tweets.number  = 100,
                                 since.days.ago = NULL,
                                 since.date     = NULL,
                                 alert.accounts = NULL,
                                 alert.messages = NULL,
                                 each.confirm   = TRUE,
                                 real.response  = FALSE,
                                 full.route     = TRUE,
                                 lang.code      = "en",
                                 plot           = FALSE,
                                 save.results   = FALSE) {
    #
    # This is the main() function of twitter-geo-search.
    #
    # In:
    #   keywords.file := file route containing the keywords to search for.
    #   entities.file := file route containing the entities to compare against
    #       the tweets geographical origin.
    #   geo.center := vector of geographical coordinates
    #       ($longitude, $latitude).
    #   radius := radius around the geo.center to search in (kilometers).
    #   tweets.number := maximum number of tweets to search for.
    #   since.days.ago := search since this many days ago to present.
    #   since.date := search since this date to present.
    #   alert.accounts := vector of accounts to alert.
    #   alert.messages := vector of messages to send (one for each account).
    #   ony.by.one := give a confirmation for every response that will be sent.
    #   plot := binary value indicating whether or not to display the plot.
    # Out:
    #   Data frame in "./output/search_results.csv" containing the results
    #   from the search (beware of overwriting search results with multiple
    #   executions of the program). Also, if asked to plot, the program will
    #   save an image of the plot in "./output/search_graph.png".
    #
    # Note: sending a NULL value in any of the parameters will automatically
    #   turn off that feature when executing the program, i.e. if you want a
    #   feature to be active, call the function with the corresponding
    #   information.
    #

    # TODO:
    #   1. Implement since.date.
    #   2. If user provided both since.date and since.days.ago
    #      alert him and let him pick which one should be used.
    #   3. Avoid nesting messages.

    #
    # Load keywords
    #
    tryCatch({
        keywords <- read.csv(keywords.file, header = FALSE)
        cat("[+] Keywords file loaded: YES\n")
    }, error = function(e) {
        cat("\n[!] Error: could not read 'keywords.file'.\n")
        return(1)
    })
    #
    # Load entities
    #
    if (!is.null(entities.file)) {
        tryCatch({
            entities <- read.csv(entities.file, header = TRUE)
            cat("[+] Entities file loaded: YES\n")
        }, error = function(e) {
            cat("\n[!] Error: could not read 'entities.file'.\n")
            return(1)
        })
    } else {
        cat("[+] Entities file loaded: NO\n")
    }
    #
    # Search
    #
    search.results <- twitterGeoSearch(keywords       = keywords,
                                       geo.center     = geo.center,
                                       radius         = radius,
                                       tweets.number  = tweets.number,
                                       since.days.ago = since.days.ago,
                                       since.date     = since.date)
    if (!is.null(search.results)) {
        #
        # Search results were not empty
        #

        #
        # Responses
        #
        if (!is.null(alert.accounts) && !is.null(alert.messages)) {
            if (length(alert.accounts) == length(alert.messages)) {
                respondEmergencies(search.results,
                                   entities,
                                   alert.accounts,
                                   alert.messages,
                                   each.confirm,
                                   real.response,
                                   full.route,
                                   lang.code)
            } else {
                cat(paste("\n[!] Error: 'alert.accounts' and)
                      'alert.messages' have different lengths (",
                      length(alert.accounts), " vs ",
                      length(alert.messages), ")\n.", sep = ""))
                cat("[+] Responses sent:       NO\n")
            }
        } else {
            cat("[+] Responses sent:       NO\n")
        }
        #
        # Plot
        #
        if (plot) {
            # TODO: find a better method (don't combine things).
            search.results <- plotResults(search.results,
                                          geo.center,
                                          radius,
                                          entities)
        } else {
            cat("[+] Show hotspots plot:   NO\n")
        }
        #
        # Save search results
        #
        if (save.results) {
            write.csv(search.results,
                      file = "./output/search_results.csv", row.names = FALSE)
            cat("[+] Search results saved: YES\n")
        } else {
            cat("[+] Search results saved: NO\n")
        }

    } else {
        #
        # Search results were empty
        #
        cat("[+] Search results:       EMPTY\n")
        cat("[+] Responses sent:       NO\n")
        cat("[+] Show hotspots plot:   NO\n")
    }
}
