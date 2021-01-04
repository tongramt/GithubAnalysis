# Title     : Interrogating Github API
# Objective : To access the Github API
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

myData = fromJSON("https://api.github.com/users/tongramt")

#num followers
myData$followers

followers = fromJSON("https://api.github.com/users/tongramt/followers")
followers$login #username of followers
myData$following #num followed

following = fromJSON("https://api.github.com/users/tongramt/following")
following$login #username of followed peopl

myData$public_repos #num repositories

repos = fromJSON("https://api.github.com/users/tongramt/repos")
repos$name #repositories names
repos$created_at #Repository date created
repos$full_name #gives full names of repositiories


LCARepos <- fromJSON("https://api.github.com/repos/tongramt/PythonLCA/commits")
LCARepos$commit$message #Shows commit messages of LCA assignment


#Interrogate the Github API to extract data from another account by switching the username
murrayData = fromJSON("https://api.github.com/users/femurray")
murrayData$followers #lists num followers
murrayData$following #lists number of people followed
murrayData$public_repos #lists number of repositories
murrayData$bio


#Part 2 - Visualisations

#My account is relaively unused, therefore to get more interesting/accurate results, I googled most active github users and found
#Taylor Otwell, who's profile, (taylorotwell) I will be using for my analysis

myData = GET("https://api.github.com/users/taylorotwell/followers?per_page=100;", gtoken)
stop_for_status(myData)
extract = content(myData)
#converts into dataframe
githubDB = jsonlite::fromJSON(jsonlite::toJSON(extract))
githubDB$login

# Retrieve a list of usernames
id = githubDB$login
user_ids = c(id)

# Create an empty vector and data.frame
users = c()
usersDB = data.frame(
  username = integer(),
  following = integer(),
  followers = integer(),
  repos = integer(),
  dateCreated = integer()
)

#loops through users and adds to list
for(i in 1:length(user_ids))
{

  followingURL = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  followingRequest = GET(followingURL, gtoken)
  followingContent = content(followingRequest)

  #Does not add users if they have no followers
  if(length(followingContent) == 0)
  {
    next
  }

  followingDF = jsonlite::fromJSON(jsonlite::toJSON(followingContent))
  followingLogin = followingDF$login

  #Loop through 'following' users
  for (j in 1:length(followingLogin))
  {
    # Check for duplicate users
    if (is.element(followingLogin[j], users) == FALSE)
    {
      #Add user to list
      users[length(users) + 1] = followingLogin[j]

      #get user data
      followingUrl2 = paste("https://api.github.com/users/", followingLogin[j], sep = "")
      following2 = GET(followingUrl2, gtoken)
      followingContent2 = content(following2)
      followingDF2 = jsonlite::fromJSON(jsonlite::toJSON(followingContent2))

      #Retrieve who each user follows
      followingNumber = followingDF2$following

      #Retrieve each user's followers
      followersNumber = followingDF2$followers

      #Retrieve each user's number of repositories
      reposNumber = followingDF2$public_repos

      #Retrieve year which each user joined Github
      yearCreated = substr(followingDF2$created_at, start = 1, stop = 4)

      #Add users data to a new row in dataframe
      usersDB[nrow(usersDB) + 1, ] = c(followingLogin[j], followingNumber, followersNumber, reposNumber, yearCreated)

    }
    next
  }
  #Stop at 100 users
  if(length(users) > 100)
  {
    break
  }
  next
}

#Use plotly to graph

Sys.setenv("plotly_username"="tongramt")
Sys.setenv("plotly_api_key"="ZWfgvS99i38RHxSheqQX")

# plot repositories v followers coloured by year
plot1 = plot_ly(data = usersDB, x = ~repos, y = ~followers,
                text = ~paste("Followers: ", followers, "<br>Repositories: ",
                              repos, "<br>Date Created:", dateCreated), color = ~dateCreated)
plot1

#send to plotly
api_create(plot1, filename = "Repositories vs Followers")

#plot 2 graphs following v followers coloured by year
plot2 = plot_ly(data = usersDB, x = ~following, y = ~followers, text = ~paste("Followers: ", followers, "<br>Following: ", following), color = ~dateCreated)
plot2

#send to plotly
api_create(plot2, filename = "Following vs Followers")

#now attempting to graph the 10 most popular languages used by the 250 users.
languages = c()

for (i in 1:length(users))
{
  RepositoriesUrl = paste("https://api.github.com/users/", users[i], "/repos", sep = "")
  Repositories = GET(RepositoriesUrl, gtoken)
  RepositoriesContent = content(Repositories)
  RepositoriesDF = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent))
  RepositoriesNames = RepositoriesDF$name

  #Loop through all the repositories of an individual user
  for (j in 1: length(RepositoriesNames))
  {
    #Find all repositories and save in data frame
    RepositoriesUrl2 = paste("https://api.github.com/repos/", users[i], "/", RepositoriesNames[j], sep = "")
    Repositories2 = GET(RepositoriesUrl2, gtoken)
    RepositoriesContent2 = content(Repositories2)
    RepositoriesDF2 = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent2))
    language = RepositoriesDF2$language

    #Removes repositories containing no specific languages
    if (length(language) != 0 && language != "<NA>")
    {
      languages[length(languages)+1] = language
    }
    next
  }
  next
}

#Puts 10 most popular languages in table
allLanguages = sort(table(languages), increasing=TRUE)
top10Languages = allLanguages[(length(allLanguages)-9):length(allLanguages)]

#converts to dataframe
languageDF = as.data.frame(top10Languages)

#Plot the data frame of languages
plot3 = plot_ly(data = languageDF, x = languageDF$languages, y = languageDF$Freq, type = "bar")
plot3

Sys.setenv("plotly_username"="tongramt")
Sys.setenv("plotly_api_key"="ZWfgvS99i38RHxSheqQX")
api_create(plot3, filename = "10 Most Popular Languages")

