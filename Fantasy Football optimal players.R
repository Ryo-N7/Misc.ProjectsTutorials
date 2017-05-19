require(ggplot2)
require(ggthemes)

require(plyr)
require(stringr)

library(lpSolve)

out <- read.csv("https://raw.githubusercontent.com/flovv/flovv.github.io/master/_Rmd/data/com.csv", sep="|")
## data 
out$Score <- as.numeric(out$Punkte)
out$MarketValue <- as.numeric(out$MW)

simple <- out[c("Name","Position", "Score" , "MarketValue")]

simple$Position <- ifelse(simple$Position == "Torwart", "Keeper", simple$Position)
simple$Position <- ifelse(simple$Position == "Abwehr", "Defense", simple$Position)
simple$Position <- ifelse(simple$Position == "Mittelfeld", "Midfield", simple$Position)
simple$Position <- ifelse(simple$Position == "Sturm", "Offense", simple$Position)

## plot 
ggplot(simple, aes(MarketValue / 1000, Score, color=Position)) + geom_point()+ geom_smooth()+ facet_wrap(~Position) + theme_economist() + xlab("Market Value in K$")

f.obj <- simple$Score  ###objective max team score

f.con <- t(simple$MarketValue)  ### constraints max MV <= Budget
player <- rep(1, nrow(simple))  ## constraints max number of players!
f.con <- rbind(f.con, player)

## constrain that per postion can only be a certain number of players be set up. (e.g. just one keeper)
## define matrix   - as a one hot (dummy coding what position the player holds)
A <- as.data.frame(model.matrix(MarketValue ~ Position -1, simple) )
f.con <- rbind(f.con, t(as.matrix(A)))

f.dir <- c("<=", "<=", "=", "<=", "<=" ,"<=")
f.rhs <- c(20000000, 13, 1, 5, 5, 3)  ## right hand side .. not more than Budget, Players, and players per position.

### solve the problem
solved<- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin=TRUE)  ## just binary variables!

###################output!
simple$buy <- solved$solution

sum(out[out$buy == 1,]$MarketValue)  ## what is the Budget
sum(out[out$buy == 1,]$Score) ## what is the Score
sum(out[out$buy == 1,]$buy)   ## number of players bought
paste(out[out$buy == 1,]$Name, collapse=", ")  
############################