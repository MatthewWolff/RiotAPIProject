# Name: Matthew Wolff
# File: RiotAPIProject.R
# Class: CS368 - Learning to Program in: R
# Description:
# This project utilizes the company Riot's collected data from their massive-multiplayer
# online game, League of Legends. There's a lot of websites that generate statistics
# for players, ranging from how they perform with a specific champion (playable
# character) to what item sets are best for a champion. I attempted to simulate this
# kind of statistic generation.
# http://leagueofstats.tumblr.com/
# https://developer.riotgames.com/docs/getting-started
# https://uwmadison.box.com/s/30766qgmvrmtyruonbqxa689iytpgd5g

################################ Packages #################################
# install.packages("RJSONIO");install.packages("gdata");install.packages("fields")
rm(list = ls())
library(RJSONIO)
library(gdata)
library(fields)

# functions for converting list to a data.frame, where var is the indices,
# and myList is the list that they're applied to
grabInfo<-function(var, myList)
{
  sapply(myList, function(x) returnData(x, var)) 
}

# checks if data is there. if not, supplies a null. Carries on from grabInfo(). 
# Var = indices
returnData<-function(x, var)
{
  if(!is.null( x[[var]]))
  {
    return( trim(x[[var]]))
  }else{
    return(NA)
  }
}

############################## functions that access API ###############################
#grabs a random summoner from list
randomSummoner = function()
{
  found <- FALSE #sentinel value
  while(!found)
  {
    name <- sample(summonerList, 1, replace = TRUE)
    attempt <- try(summoner(name), silent = TRUE) #player may have since changed name
    if (class(attempt) != "try-error")
      found <- TRUE
  }
  return(attempt[[2]])
}

# the game stores characters as numbers, but players use names. Retrieves name when given number
champion = function(championId)
{
  if(!is.numeric(championId)) #must be numeric
    stop("ChampionId is not a numeric")
  if (any((as.integer(championId) - championId) != 0) == TRUE) # must be integer
    stop("ChampionId must be an integer")
  
  if(length(championId) == 1) # when passed a single integer
    championList[,3][which(championList[,1] == championId)] #returns champion name from ID
  else # when passed multiple numbers
    unlist(lapply(championId, function(x) champion(x))) 
}

# retrieves a player's data from their summoner name
summoner = function(summonerName)
{
  summonerName = gsub(" ", "", summonerName, fixed = TRUE) #remove whitespace
  URL = "https://na.api.pvp.net/api/lol/na/v1.4/summoner/by-name/"
  api_key = "?api_key=826630f6-9e7d-4cfe-802f-9c2614832d97"
  summonerRetrievalURL = paste(URL, summonerName, api_key, sep = "")
  retrieved <- fromJSON(summonerRetrievalURL)
  
  if(retrieved[[1]][[2]] == 404) # will throw error if given bad summoner name
    stop("Summoner not found")
  return(retrieved[[1]])
}

#retrieves a summoner's 10 most recent games from their summoner name, utilizes summoner()
summonerRecent = function(summonerName)
{
  URL = "https://na.api.pvp.net/api/lol/na/v1.3/game/by-summoner/"
  api_key = "/recent?api_key=826630f6-9e7d-4cfe-802f-9c2614832d97"
  summonerRecentRetrievalURL = paste(URL, summoner(summonerName)[[1]][[1]], api_key, sep = "") #warning
  retrieved = fromJSON(summonerRecentRetrievalURL)
  return(retrieved)
}

#retrieves ranked game statistics for a summoner (ranked games contribute to your standing)
summonerRecentRanked = function(summonerName)
{
  URL = "https://na.api.pvp.net/api/lol/na/v1.3/stats/by-summoner/"
  api_key = "/ranked?season=SEASON2016&api_key=826630f6-9e7d-4cfe-802f-9c2614832d97"
  summonerRecentRetrievalURL = paste(URL, summoner(summonerName)[[1]][[1]], api_key, sep = "") #warning
  retrieved = fromJSON(summonerRecentRetrievalURL)
  return(retrieved)
}

# shows the frequency of characters played from last 10 games, utilizes summonerRecent()
recentPlay = function(summonerName) #plots champions from last 10 games
{
  recent = summonerRecent(summonerName); #will generate warnings
  champions = unlist(
    lapply(recent$games[seq(along=recent$games)],
           function(x) champion(x$championId)))
  barplot(table(champions),ylab = "# of Recent Games Played", xlab = "", las = 3, col = "blue", cex.axis = 1,
          main = paste(summonerName,"'s Champions in Recent Matches"))
}
recentWin = function(summonerName)
{
  recentGames <- summonerRecent(summonerName)
  barplot(table(unlist(
    lapply(summonerRecent(summonerName)$games[seq(along=recentGames$games)],
           function(x) x$stats$win))), names.arg = c("Lost", "Won"),
    main = paste(summonerName,"'s Recent Matches", sep = ""), ylab = "Games", col = c("red3", "green3"), horiz = TRUE)
}
#analyze all recent matches of a given summoner
analyzeRecent = function(summonerName)
{
  par(mfrow=c(3,1))
  recentPlay(summonerName) #will throw warning
  recentWin(summonerName)
  recentGames = summonerRecent(summonerName) #grabs their 10 most recent games
  pulledData = lapply(recentGames$games[seq_along(recentGames$games)],
                      function(x) x$stats) #extracts the stats
  gamesWon = which(unlist(
    lapply(pulledData[seq_along(pulledData)],
           function(x) x$win))== TRUE) #checks which games the player won
  barplot(table(champion(unlist(lapply(recentGames$games[gamesWon], function(x) x$championId)))),
          main = paste(summonerName, "'s Winning Champions in Last 10 Games"),
          ylab = "# Games Won", xlab = "Champion", las = 3, col = "cyan4")
}

############################# INPUTS ############################
# sets up RiotAPI as well as some static data I utilize throughout
champList <- fromJSON("https://na.api.pvp.net/api/lol/static-data/na/v1.2/champion?api_key=826630f6-9e7d-4cfe-802f-9c2614832d97")
riotSampleAPI <- fromJSON("/users/matthew/downloads/matches1.json")
championList <- data.frame(sapply(1:4,
                                  function(x) grabInfo(x, champList$data)), stringsAsFactors = FALSE)
colnames(championList) <- unlist(attributes(champList$data$Thresh))
summonerList = c() #list to fill
for(i in 1:100) #grabs players from all 100 games
  summonerList = c(summonerList, unlist(
    lapply(riotSampleAPI$matches[[i]]$participantIdentities[1:10],
           function(x) x$player$summonerName)))




######################### execution #########################
#match lengths and median match length
matches <- riotSampleAPI[[1]]
cat(c("Number of Matches:", length(matches))) #number of games in data set
matchTimes = round(unlist(
  lapply(matches[seq_along(matches)],
         function(x) x$matchDuration/60))) #rounds games out to the minute
plot(table(matchTimes), xlab = "Game Duration (min)", ylab = "% of Games ",
     main = "Distribution of Game Duration from 100 Games")
xline(mean(matchTimes), lwd = 5, col = "red") #average match length
legend("topright", "Average Match Length", pch = 15, inset = .05, col = "red")
###### NOTE: pause and look at plot 

# What do the professionals play? Players in the Challenger Tier receive sponsorship from large
# corporations like Intel, Razer, Geico, and HTC
challenger <- fromJSON("https://na.api.pvp.net/api/lol/na/v2.5/league/challenger?type=RANKED_SOLO_5x5&api_key=826630f6-9e7d-4cfe-802f-9c2614832d97")
challengerName <- challenger$entries[[1]]$playerOrTeamName; challengerName #this list changes up hourly
rankedGames <- summonerRecentRanked(challengerName); #grabs ranked match data
unlist(lapply(rankedGames$champions[seq_along(rankedGames$champions)],
              function(x) champion(x$id))) #grabs champions played
###### NOTE: look at output for champion names
# Looks at 10 most recent games and creates a barplot of champion frequency,
# then looks at how many of those you won. 
randSummoner = randomSummoner()
matches2 <- summonerRecent(randSummoner)
matchTimes <- round(unlist(lapply(matches2$games[seq_along(matches2$games)],
                                  function(x) x$stats$timePlayed/60)))
plot(table(matchTimes),
     main = paste(randSummoner, "'s Game Lengths", sep = ""), ylab = "Number of Games", xlab = "Game Duration (min)")
xline(median(matchTimes), lwd = 5, col = "red") #average match length
legend("topright", "Median Match Length", pch = 15, inset = .05, col = "red")

par(mfrow=c(3,1)); #so I can show all my plots
recentPlay(randSummoner) #function I created
recentWin(randSummoner)  #function I created

#This looks at your last 10 games and determines who you won most as
recentGames = summonerRecent(randSummoner) #grabs their 10 most recent games
pulledData = lapply(recentGames$games[seq_along(recentGames$games)],
                    function(x) x$stats) #extracts the stats
gamesWon = which(unlist(
  lapply(pulledData[seq_along(pulledData)],
         function(x) x$win))== TRUE) #checks which games the player won
barplot(table(champion(unlist(lapply(recentGames$games[gamesWon], function(x) x$championId)))),
        main = paste(randSummoner, "'s Winning Champions in Last 10 Games", sep = ""),
        ylab = "# Games Won", xlab = "Champion", las = 3, col = "cyan4")
###### NOTE: look at plot
