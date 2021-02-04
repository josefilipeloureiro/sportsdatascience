library(rjson)
#library(jsonlite) [you can try jsonlite::fromJSON(x,flatten=TRUE) to see what that does when reading in the JSON file]
library(data.table)
library(magrittr)

####Obtain Competitions####

#Read File from JSON into a list 
competitions <- fromJSON(file="D:\\github\\open-data-master\\data\\competitions.json")


#Convert List into a DataFrame
competitions.df <- data.frame(do.call(rbind,competitions),stringsAsFactors = FALSE)


####Obtain Matches####
match.files <- list.files(path="D:\\github\\open-data-master\\data\\matches",
           full.names = TRUE,recursive = TRUE)

matches.list <- list()
for(i in 1:length(match.files)){
  match.temp <- fromJSON(file=match.files[i]) ##Loop through each file which contains all the matches for a given competition and season and obtain the necessary match information
  
  matches <- lapply(match.temp, function(x) data.frame(t(unlist(x)),stringsAsFactors = FALSE))
  matches.df <- rbindlist(matches,fill=TRUE) #we use rbindlist instead of do.call(rbind,) because of column mismatch
  matches.list[[i]] <- matches.df #this assigns matches.df to the matches.list list that we initialized 
  
}

all.matches.df <- data.frame(rbindlist(matches.list,fill=TRUE)) ###Combines all matches from all competitions into one dataframe

###we are going to remove a lot of columns to just make our dataset clean
columns.to.keep <- names(which(unlist(lapply(all.matches.df,function(x) length(which(is.na(x)))))==0))

all.matches.clean <- all.matches.df[,columns.to.keep] #this selects the columns by column name 
all.matches.clean$match_week <- as.numeric(all.matches.clean$match_week) #convert some variables to numeric
all.matches.clean$home_score <- as.numeric(all.matches.clean$home_score)
all.matches.clean$away_score <- as.numeric(all.matches.clean$away_score)

####Obtain Events####
event.files <- list.files(path="D:\\github\\open-data-master\\data\\events",
                           full.names = TRUE,recursive = TRUE)

event.list <- list()
for(i in 1:length(event.files)){
  event.temp <- fromJSON(file=event.files[i])
  
  #unique(unlist(lapply(event.temp,function(x) x$type$name))) | Let's us see the unique events that happen in a game
  
  teamids <- c() #Get the unique teamids participating in a match
  teamnames <- c()
  #obtain the index where we find the event that talks about Starting XI
  starting.x11.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Starting XI")
  starting.x11.list <- list()
  for(s in 1:2){
    starting.x11.team1 <- data.frame(matrix(t(unlist(event.temp[[s]]$tactics$lineup)),ncol=5,byrow = TRUE),stringsAsFactors = FALSE)
    colnames(starting.x11.team1) <- names(unlist(event.temp[[s]]$tactics$lineup))[1:5]
    starting.x11.team1$formation <- event.temp[[s]]$tactics$formation
    starting.x11.team1$team_id <- event.temp[[s]]$team$id
    
    teamids <- c(teamids,event.temp[[s]]$team$id)
    teamnames <- c(teamnames,event.temp[[s]]$team$name)
    starting.x11.team1$team_name <- event.temp[[s]]$team$name
    starting.x11.list[[s]] <- starting.x11.team1
  }

  pass.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Pass")
  
  #obtain the passes just for team1 (the first element in teamids)
  pass.team1 <- pass.index[which(unlist(lapply(pass.index,function(x) event.temp[[x]]$team$id))==teamids[1])]
  pass.team1.df <- data.frame(matrix(NA,nrow=1,ncol=11))
  colnames(pass.team1.df) <- c("Possession","Passer","X.Pass","Y.Pass",
                               "Pass.Type","Receiver","X.Receive","Y.Receive",
                               "Pass.Length","Pass.Angle","Body.Part")
  
  for(p in 1:length(pass.team1)){
    pass.temp <- event.temp[[pass.team1[p]]]
    possession <- pass.temp$possession
    passer <- pass.temp$player$id
    pass.location <- pass.temp$location
    pass.type <- pass.temp$pass$height$name
    receiver <- pass.temp$pass$recipient$id
    receive.location <- pass.temp$pass$end_location
    pass.length <- pass.temp$pass$length
    pass.angle <- pass.temp$pass$angle
    body.part <- pass.temp$pass$body_part$name
    
    row.toadd <- c(possession,passer,pass.location,pass.type,receiver,receive.location,pass.length,pass.angle,body.part)
    pass.team1.df <- rbind(pass.team1.df,row.toadd)
  }
  pass.team1.df <- pass.team1.df[-1,]
  pass.team1.df[,c(1:4,6:10)] <- lapply(pass.team1.df[,c(1:4,6:10)],as.numeric)
  pass.team1.df <- pass.team1.df %>% dplyr::group_by(Possession) %>% dplyr::mutate(seq = dplyr::row_number())
  pass.team1.df$team_id <- teamids[1]
  
  #obtain the shots just for team2 (the second element in teamids)
  pass.team2 <- pass.index[which(unlist(lapply(pass.index,function(x) event.temp[[x]]$team$id))==teamids[2])]
  pass.team2.df <- data.frame(matrix(NA,nrow=1,ncol=11))
  colnames(pass.team2.df) <- c("Possession","Passer","X.Pass","Y.Pass",
                               "Pass.Type","Receiver","X.Receive","Y.Receive",
                               "Pass.Length","Pass.Angle","Body.Part")
  
  for(p in 1:length(pass.team2)){
    pass.temp <- event.temp[[pass.team2[p]]]
    possession <- pass.temp$possession
    passer <- pass.temp$player$id
    pass.location <- pass.temp$location
    pass.type <- pass.temp$pass$height$name
    receiver <- pass.temp$pass$recipient$id
    receive.location <- pass.temp$pass$end_location
    pass.length <- pass.temp$pass$length
    pass.angle <- pass.temp$pass$angle
    body.part <- pass.temp$pass$body_part$name
    
    row.toadd <- c(possession,passer,pass.location,pass.type,receiver,receive.location,pass.length,pass.angle,body.part)
    pass.team2.df <- rbind(pass.team2.df,row.toadd)
  }
  pass.team2.df <- pass.team2.df[-1,]
  pass.team2.df[,c(1:4,6:10)] <- lapply(pass.team2.df[,c(1:4,6:10)],as.numeric)
  pass.team2.df <- pass.team2.df %>% dplyr::group_by(Possession) %>% dplyr::mutate(seq = dplyr::row_number())
  pass.team2.df$team_id <- teamids[2]
  
  pass.list <- list(pass.team1.df,pass.team2.df)
  
  shot.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Shot")
  
  #obtain the shots just for team1 (the first element in teamids)
  shot.team1 <- shot.index[which(unlist(lapply(shot.index,function(x) event.temp[[x]]$team$id))==teamids[1])]
  shot.team1.df <- data.frame(matrix(NA,nrow=1,ncol=12))
  colnames(shot.team1.df) <- c("Possession","Finisher","X.Shot","Y.Shot",
                               "xG","X.Shot.End","Y.Shot.End",
                               "Outcome","Body.Part","Shot.Type","Team.id","Team.name")
  
  #if statement for teams that did not make any shot in a match
  if(rapportools::is.empty(shot.team1) == FALSE){
    for(p in 1:length(shot.team1)){
      shot.temp <- event.temp[[shot.team1[p]]]
      possession <- shot.temp$possession
      finisher <- shot.temp$player$id
      shot.location <- shot.temp$location[1:2]
      xG <- shot.temp$shot$statsbomb_xg
      end.location <- shot.temp$shot$end_location[1:2]
      outcome <- shot.temp$shot$outcome$name
      body.part <- shot.temp$shot$body_part$name
      shot.type <- shot.temp$type$name
      team.id <- shot.temp$team$id
      team.name <- shot.temp$team$name
      
      row.toadd <- c(possession,finisher,shot.location,xG,end.location,outcome,body.part,shot.type,team.id,team.name)
      shot.team1.df <- rbind(shot.team1.df,row.toadd)
    }
    shot.team1.df <- shot.team1.df[-1,]
    shot.team1.df[,c(1,3:7)] <- lapply(shot.team1.df[,c(1,3:7)],as.numeric)
  } else {
    shot.team1.df$Team.id <- teamids[1] #if the team did not make any pass, still record it's id and name
    shot.team1.df$Team.name <- teamnames[1]
  }
  
  #obtain the shots just for team2 (the second element in teamids)
  shot.team2 <- shot.index[which(unlist(lapply(shot.index,function(x) event.temp[[x]]$team$id))==teamids[2])]
  shot.team2.df <- data.frame(matrix(NA,nrow=1,ncol=12))
  colnames(shot.team2.df) <- c("Possession","Finisher","X.Shot","Y.Shot",
                               "xG","X.Shot.End","Y.Shot.End",
                               "Outcome","Body.Part","Shot.Type","Team.id","Team.name")
  
  #if statement for teams that did not make any shot in a match
  if(rapportools::is.empty(shot.team2) == FALSE){
    for(p in 1:length(shot.team2)){
      shot.temp <- event.temp[[shot.team2[p]]]
      possession <- shot.temp$possession
      finisher <- shot.temp$player$id
      shot.location <- shot.temp$location[1:2]
      xG <- shot.temp$shot$statsbomb_xg
      end.location <- shot.temp$shot$end_location[1:2]
      outcome <- shot.temp$shot$outcome$name
      body.part <- shot.temp$shot$body_part$name
      shot.type <- shot.temp$type$name
      team.id <- shot.temp$team$id
      team.name <- shot.temp$team$name
      
      row.toadd <- c(possession,finisher,shot.location,xG,end.location,outcome,body.part,shot.type,team.id,team.name)
      shot.team2.df <- rbind(shot.team2.df,row.toadd)
    }
    shot.team2.df <- shot.team2.df[-1,]
    shot.team2.df[,c(1,3:7)] <- lapply(shot.team2.df[,c(1,3:7)],as.numeric)
  } else {
    shot.team1.df$Team.id <- teamids[2] #if the team did not make any pass, still record it's id and name
    shot.team1.df$Team.name <- teamnames[2]
  }
  
  shot.list <- list(shot.team1.df,shot.team2.df)
  
  match.id <- strsplit(basename(event.files[i]),"[.]")[[1]][1]
  
  event.list[[match.id]] <- list(starting.x11.list,pass.list,shot.list)
}

####Analysis introduction####
#For the next 3 Analysis, we will use the FA Women's Super League which is competition_id == 37 and season_id == 4

#This gets me the number of matches per competition and season to check which season we have the most data for
#matches.count <- all.matches.clean %>% group_by(competition.competition_id,season.season_id) %>% summarise(count = n())

matches.wsl.1819 <- all.matches.clean[which(all.matches.clean$competition.competition_id==37 & all.matches.clean$season.season_id==4),]
matches.wsl.1819 <- matches.wsl.1819[order(matches.wsl.1819$match_week),]

wsl.teams <- unique(matches.wsl.1819$home_team.home_team_name) #get the unique list of teams so we can loop through each team


####Analysis 1: Let's Look at the Squad Rotation per Match####

squad.rotation.list <- list() #this list is for keeping track of the number of squad rotations per matchweek
team.starting.x11 <- list() #this list is for keeping track of the starting 11 for each match week
for(w in 1:length(wsl.teams)){
  squad.rotation.list[[wsl.teams[w]]] <- list()
  team.starting.x11[[wsl.teams[w]]] <- list()
  team.matches <- matches.wsl.1819[which(matches.wsl.1819$home_team.home_team_name==wsl.teams[w] |
                                           matches.wsl.1819$away_team.away_team_name==wsl.teams[w]),]
  team.matches$GD <- team.matches$home_score-team.matches$away_score
  
  team.events.index <- which(names(event.list) %in% team.matches$match_id)
  team.events <- event.list[team.events.index]
  team.id <- unique(matches.wsl.1819[which(matches.wsl.1819$home_team.home_team_name==wsl.teams[w]),]$home_team.home_team_id)
  team.matches$Team.GD <- ifelse(team.matches$home_team.home_team_id==team.id,team.matches$GD,team.matches$GD*-1)
  team.matches$Result <- ifelse(team.matches$Team.GD>0,"W",
                                ifelse(team.matches$Team.GD==0,"D","L"))
  
  
  for(i in 1:length(team.events)){ #for each game of that particular team, get the starting 11 for them
    starting.x11 <- team.events[[i]][[1]]
    starting.x11.index <- which(lapply(starting.x11, function(x) unique(x$team_id))==team.id)
    
    team.11 <- starting.x11[[starting.x11.index]]
    team.starting.x11[[wsl.teams[w]]][[i]] <- team.11$player.name
  }
  
  num.matches <- length(team.events)
  #for all the matches after the first match, calculate the difference in players from matchweek X and matchweek X+1
  squad.rotation <- c(0,sapply(seq(1:(num.matches-1)),function(x) length(setdiff(team.starting.x11[[w]][[x]],team.starting.x11[[w]][[x+1]]))))
  team.matches$Rotated <- squad.rotation
  squad.rotation.list[[w]] <- team.matches[,c("match_week","Result","Rotated")]
}

result.colors <- c("W"="forestgreen","L"="red","D" = "yellow") #define a set of colors to use in our plot

#ggplot is where you bind the data. the aes stands for aesthetic and defines what data is bound to what part of the graph
ggplot(data=squad.rotation.list[[1]], aes(x=match_week,y=Rotated,fill=Result)) + geom_bar(stat="identity",width=0.5)+
  scale_fill_manual(values=result.colors)

all.squad.rotations <- plyr::ldply(squad.rotation.list,.id="Team") #binds all the rows of the list elements together and adds the list element name as an additional column

ggplot(data=all.squad.rotations, aes(x=match_week,y=Rotated,fill=Result)) + geom_bar(stat="identity",width=0.5)+
  scale_fill_manual(values=result.colors) + facet_grid(rows=vars(Team)) #adds a plot for each team


####Analysis 2: Clustering Passes####
#We want to cluster passes per team to understand the passes tendencies of each team
#We will use the same competition and season

pass.events.index <- which(names(event.list) %in% matches.wsl.1819$match_id)

passes.list <- list()
for(i in 1:length(pass.events.index)){
  match.temp <- event.list[[pass.events.index[i]]][[2]]
  
  all.passes <- do.call(rbind,match.temp)
  
  all.passes.locations <- all.passes[,c("team_id","X.Pass","Y.Pass","X.Receive","Y.Receive")]
  
  passes.list[[i]] <- all.passes.locations
  
}

full.pass.df <- do.call(rbind,passes.list)
full.pass.df <- full.pass.df[which(full.pass.df$Y.Receive<=80),] #cleaning the data
full.pass.df$Y.Pass <- 80 - full.pass.df$Y.Pass #changing the axis so that origin starts at the lower left corner
full.pass.df$Y.Receive <- 80 - full.pass.df$Y.Receive


library(parallel)
library(ggplot2)
library(magrittr)

#perform k-means on the dataset (removing the 1st column because we just need to use the last 4 columns in our analysis)
mc = mclapply(c(25,50,75), function(x,centers) kmeans(x, centers, iter.max=1000), x=full.pass.df[,-1])

full.pass.df$Cluster.25 <- mc[[1]]$cluster #created clusters using 25 clusters
full.pass.df$Cluster.50 <- mc[[2]]$cluster #created clusters using 50 clusters
full.pass.df$Cluster.75 <- mc[[3]]$cluster #created clusters using 75 clusters

cluster.50.summary <- full.pass.df %>% dplyr::group_by(Cluster.50) %>% dplyr::summarise(X.Pass = mean(X.Pass),Y.Pass = mean(Y.Pass),
                                                                                        X.Receive = mean(X.Receive), Y.Receive = mean(Y.Receive),
                                                                                        count = dplyr::n()) #obtain for each cluster id, the average location of the pass

cluster.50.team.summary <- full.pass.df %>% dplyr::group_by(Cluster.50,team_id) %>% dplyr::summarise(count = dplyr::n()) #get a count per team
arsenal.clusters <- cluster.50.team.summary %>% dplyr::group_by(Cluster.50) %>% dplyr::mutate(z.score = (count - mean(count))/sd(count)) %>%
  dplyr::filter(team_id == 968 & z.score >= 1.5) #identify which clusters that arsenal does more than 1.5 sd than the league average

hori5 + geom_segment(data=cluster.50.summary, aes(x=X.Pass,xend=X.Receive,
                                                  y=Y.Pass,yend=Y.Receive,color=count),size=1.5,arrow=arrow(length = unit(0.03, "npc"))) +
  geom_text(data=cluster.50.summary,aes(x=X.Pass,y=Y.Pass,label=Cluster.50))

hori5 + geom_segment(data=cluster.50.summary, aes(x=X.Pass,xend=X.Receive,
                                                  y=Y.Pass,yend=Y.Receive),size=1.5,arrow=arrow(length = unit(0.03, "npc"))) +
  geom_segment(data=cluster.50.summary[which(cluster.50.summary$Cluster.50 %in% arsenal.clusters$Cluster.50),], aes(x=X.Pass,xend=X.Receive,
                                                                                                                    y=Y.Pass,yend=Y.Receive),size=1.5,color="red",arrow=arrow(length = unit(0.03, "npc")))


hori5 + geom_segment(data=full.pass.df[which(full.pass.df$Cluster.50==12 & full.pass.df$team_id==968),], aes(x=X.Pass,xend=X.Receive,
                                                                                                             y=Y.Pass,yend=Y.Receive),size=1.5,arrow=arrow(length = unit(0.03, "npc")))


####To Try At Home: Obtain the Shot Information for each match and create a xG shot graph####
#We will use the same competition and season

wsl.events.index <- which(names(event.list) %in% matches.wsl.1819$match_id)

shots.list <- list()
for(i in 1:length(wsl.events.index)){
  match.temp <- event.list[[wsl.events.index[i]]][[3]]
  
  all.shots <- do.call(rbind,match.temp)
  
  all.shots.locations <- all.shots[,c("X.Shot","Y.Shot","xG","X.Shot.End","Y.Shot.End","Outcome","Team.id","Team.name")]
  
  shots.list[[i]] <- all.shots.locations
  
}

for(k in 1:length(shots.list)){
  
  shots.gamek.df <- data.frame(shots.list[k])
  #changing the axis so that origin starts at the lower left corner
  shots.gamek.df$Y.Shot <- 80 - shots.gamek.df$Y.Shot
  shots.gamek.df$Y.Shot.End <- 80 - shots.gamek.df$Y.Shot.End
  
  #get the index of shots for the away team and mirror the respective x and y coordinates
  away.team.shots.index <- which(unlist(lapply(shots.list[k],function(x) x$Team.id))!=shots.list[[k]][["Team.id"]][[1]])
  shots.gamek.df$X.Shot[away.team.shots.index] <- 120 - shots.gamek.df$X.Shot[away.team.shots.index]
  shots.gamek.df$X.Shot.End[away.team.shots.index] <- 120 - shots.gamek.df$X.Shot.End[away.team.shots.index]
  shots.gamek.df$Y.Shot[away.team.shots.index] <- 80 - shots.gamek.df$Y.Shot[away.team.shots.index]
  shots.gamek.df$Y.Shot.End[away.team.shots.index] <- 80 - shots.gamek.df$Y.Shot.End[away.team.shots.index]
  
  #create a plot name and path to save it later as a PNG file. The path has to be pre-existing
  plot.name = paste("shots",stringr::str_replace_all(shots.gamek.df$Team.name[1]," ","_"),"vs",stringr::str_replace_all(shots.gamek.df$Team.name[away.team.shots.index][1]," ","_"),sep="_")
  plot.name.full = paste(plot.name,".png",sep="")
  plot.path = paste("D:\\github\\meuR\\work_statsbomb_data\\Shots_WSL_teste",plot.name.full,sep="\\")
  png(file=plot.path,width=1200, height=700)
  
  #plotting the shot maps with a color code on xG and a red circle around the ones that resulted in goal
  print(hori5 + geom_point(data=shots.gamek.df, aes(x=X.Shot,y=Y.Shot,color=xG),size=3) +
          geom_text(data=shots.gamek.df,aes(x=X.Shot+sign(60-X.Shot),y=Y.Shot-sign(40-Y.Shot),label=round(xG,digits=2))) +
          geom_point(data=shots.gamek.df[which(shots.gamek.df$Outcome=='Goal'),], aes(x=X.Shot,y=Y.Shot),size=3,color="red",fill = NA, shape = 21, stroke = 2) +
          annotate(geom = "text", x = 125, y = 40, label = paste(shots.gamek.df$Team.name[1],"attacking side"), angle = 90) +
          annotate(geom = "text", x = -5, y = 40, label = paste(shots.gamek.df$Team.name[away.team.shots.index][1],"attacking side"), angle = 90))
  dev.off()
}