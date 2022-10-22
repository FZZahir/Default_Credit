# CoffeeAPI
require(tidyverse)
require(httr)

wdjlajdja
client_id <- "q-TLNj3rl_EFIWV279Gi-Q"
client_secret<-"GD11N0By-jC5zQiIuHOa5MMmankgyWgZjKo-gxHsWbiMxzQkbivZqIW4CJDA2Pba7xg6R7jrmdf3sKK8q_sdv23iG2qZ5cNPYoP2Ez0-h30tdOnILZR7irY-AGtPY3Yx" # nolint

res <- POST("https://api.yelp.com/oauth2/token",
            body = list(grant_type = "client_credentials",
                        client_id = client_id,
                        client_secret = client_secret))

token <- content(res)$access_token

yelp <- "https://api.yelp.com"
term <- "coffee & tea"
location <- "Los Angeles, CA"
categories <- NULL
limit <-  50
radius <- 8800
url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                  query = list(term = term, location = location, 
                               limit = limit,
                               radius = radius))
res <- GET(url, add_headers("Authorization" = paste("bearer", client_secret)))

results <- content(res)
results

yelp_httr_parse <- function(x) {

  parse_list <- list(id = x$id,
                     name = x$name,
                     rating = x$rating,
                     review_count = x$review_count,
                     latitude = x$coordinates$latitude,
                     longitude = x$coordinates$longitude,
                     address1 = x$location$address1,
                     city = x$location$city,
                     state = x$location$state,
                     distance = x$distance)
  parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))
  df <- data_frame(id = parse_list$id,
                   name = parse_list$name,
                   rating = parse_list$rating,
                   review_count = parse_list$review_count,
                   latitude = parse_list$latitude,
                   longitude = parse_list$longitude,
                   address1 = parse_list$address1,
                   city = parse_list$city,
                   state = parse_list$state,
                   distance = parse_list$distance)
  df
}
#get reviews text
yelp_httr_parse(results$businesses[[1]])

results_list <- lapply(results$businesses, FUN = yelp_httr_parse)

business_data <- do.call("rbind", results_list)

#write to csv
write.csv(business_data, "business_datalosang.csv")
