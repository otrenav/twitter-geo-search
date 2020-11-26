
[Delta Lab](https://links.deltalab.ai/website) | [Twitter](https://links.deltalab.ai/twitter) | [LinkedIn](https://links.deltalab.ai/linkedin)

# Twitter Geographical Search

- Omar Trejo
- June, 2014

Search Twitter feeds for specified keywords within a geographic zone and
automatically respond to certain keywords. Optionally you can find the nearest
entity in a database (using Google's Directions API) to the origin of a tweet
and send that information through Twitter.

For example, I can use this application to find the nearest gas station when I'm
on the road in Mexico City. The gas stations database for Mexico City is
included in the `data` folder (it's public information).

## Usage

To correctly setup your system you need to rename `setup_template.R` to
`setup.R` and edit the file to include your key and secret codes. To get these
head over to [Twitter's developers site](http://dev.twitter.com), specifically
to [your apps](https://apps.twitter.com/) and register a new application.

> Note: The code is well documented, so you should not have any problems using
> it, but keep in mind that it's not being actively developed any more.

## Instructions

1. You need to setup your system first (use the `setup.R` file for this).
2. You need to specify a CSV file containing the keywords that you want to
   search for
   - It should have one keyword per line.
3. You need to specify a geographic zone
   - Do it by providing a geographic center c(longitude, latitude) and a radius
     around that center (in kilometers).
4. You need to specify a CSV file that contains the entities that will be used
   when determining the closes one ans sending instructions to the original
   poster of the tweet.

> Note: the search result is unsensitive to accents and capitalization. i.e., if
> you search for "mexico", you will get results for "mexico", "Mexico",
> "México", etc.

> Note: if the entities file has addresses instead of coordinates to identify
> locations, you can use the `geocode_database.R` file to find the geographical
> coordinates.
