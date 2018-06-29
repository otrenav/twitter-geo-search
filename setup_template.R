#
# Author: Omar Trejo Navarro
# Email:  otrenav [at] gmail [dot] com
#
# This file provides the template used when authorizing your
# application to use the Twitter API. Fill in your consumer and
# secret keys (dev.twitter.com) and execute.
#

require(twitteR)

req.url    <- "https://api.twitter.com/oauth/request_token"
access.url <- "https://api.twitter.com/oauth/access_token"
auth.url   <- "https://api.twitter.com/oauth/authorize"
API.key    <- ""
API.secret <- ""
twit.cred  <- OAuthFactory$new(consumerKey    = API.key,
                               consumerSecret = API.secret,
                               requestURL     = req.url,
                               accessURL      = access.url,
                               authURL        = auth.url)
twit.cred$handshake()

# Enter the provided PIN and then execute this:
registerTwitterOAuth(twit.cred)
