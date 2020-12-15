# Title     : Interrogating Github API
# Objective : TO access the Github API
# Created by: ThomasGrant
# Created on: 15/12/2020

#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)


oauth_endpoints("github")

myapp <- oauth_app(appname = "TomGrantAPI",
                   key = "30bd4f5ae3e84aeda4d2",
                   secret = "58acaaddfa070a69a45d8861a32aaea1ac045aa3")

# oauth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# use API
gtoken <- httr::config(token = github_token)
req <- GET("https://api.github.com/users/tongramt/repos", gtoken)

# take action on http error
stop_for_status(req)

# extract content from a request
json1 = content(req)

# convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))


# subset data.frame
gitDF[gitDF$full_name == "tongramt/datasharing", "created_at"]