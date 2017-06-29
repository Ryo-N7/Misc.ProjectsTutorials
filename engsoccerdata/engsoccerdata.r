require(engsoccerdata)
require(ggplot2)
require(dplyr)
require(magrittr)
library(tidyverse)

engsoccerdata::

EPL <- rbind(england, england_current()) %>% 
       subset(tier == 1 & Season %in% 1992:2015)    # shifted back to 2015 because as of 5.22.17 tables for 16/17 season not fully updated = screws up the HPG tables...

head(EPL, 5)
tail(EPL, 5)

View(england)

LIV <- rbind(england, england_current()) %>% 
  subset(tier == 1 & Season %in% 1992:2015 & home == "Liverpool"| visitor == "Liverpool") 

ESP <- spain %>% 
  subset(tier == 1 & Season %in% 2000:2015)


Barca <- spain %>% 
  subset(tier == 1 & Season %in% 1928:2015) %>% 
  homeaway() %>% 
  filter(team == "FC Barcelona", opp == "Real Madrid") %>% 
  group_by(venue, Season) %>% 
  mutate(totalpts = 3*sum(gf>ga) + sum(gf == ga)) %>%     # instead of summarize to keep other columns
  arrange(Season) %>% 
  mutate(W = if_else(totalpts == 3, 1, 0), D = if_else(totalpts == 1, 1, 0), L = if_else(totalpts == 0, 1, 0)) %>% 
  transform(W = as.numeric(W),
            D = as.numeric(D),
            L = as.numeric(L)) %>% 
  summarize(count = n())           # close... but counting both T and F?

#### trye with add_count()
Barca1 <- spain %>% 
  subset(tier == 1 & Season %in% 1928:2015) %>% 
  homeaway() %>% 
  filter(team == "FC Barcelona", opp == "Real Madrid") %>% 
  group_by(venue, Season) %>% 
  mutate(totalpts = 3*sum(gf>ga) + sum(gf == ga)) %>%     # instead of summarize to keep other columns
  arrange(Season) %>% 
  mutate(W = if_else(totalpts == 3, 1, 0), D = if_else(totalpts == 1, 1, 0), L = if_else(totalpts == 0, 1, 0)) %>% 
  transform(W = as.numeric(W),
            D = as.numeric(D),
            L = as.numeric(L)) %>%
  group_by(W, D, L) %>% 
  tally()
  
Barca %>% filter(W == 0, D == 0, L == 0)    # 4 matches from 1986-1987 season show discrepancies!

# Date Season         team         opp gf ga tier venue totalpts W D L
# 1 1986-10-08   1986 FC Barcelona Real Madrid  1  1    1  away        2 0 0 0
# 2 1987-01-31   1986 FC Barcelona Real Madrid  3  2    1  home        6 0 0 0
# 3 1987-04-12   1986 FC Barcelona Real Madrid  0  0    1  away        2 0 0 0
# 4 1987-05-23   1986 FC Barcelona Real Madrid  2  1    1  home        6 0 0 0


Barca2 <- spain %>% 
  subset(tier == 1 & Season %in% 1985:1988) %>% 
  homeaway() %>% 
  filter(team == "FC Barcelona", opp == "Real Madrid") %>% 
  group_by(venue, Season) %>% 
  mutate(totalpts = 3*sum(gf>ga) + sum(gf == ga)) %>%     # instead of summarize to keep other columns
  arrange(Season) %>% 
  mutate(W = if_else(totalpts == 3, 1, 0), D = if_else(totalpts == 1, 1, 0), L = if_else(totalpts == 0, 1, 0)) %>% 
  transform(W = as.numeric(W),
            D = as.numeric(D),
            L = as.numeric(L))

Barca2.5 <- spain %>% 
  subset(tier == 1 & Season %in% 1985:1988) %>% 
  homeaway() %>% 
  filter(team == "FC Barcelona", opp == "Real Madrid") %>% 
  group_by(venue, Season) %>% 
  mutate(totalpts = 3*sum(gf>ga) + sum(gf == ga)) %>% 
  filter(Date == '1986-10-08' | Date == '1987-01-31' | Date == '1987-04-12' | Date == '1987-05-23') %>% 
  mutate(totalpts = replace(totalpts, totalpts == 6, 3)) %>% 
  mutate(totalpts = replace(totalpts, totalpts == 2, 1))

Barca1 <- spain %>% 
  subset(tier == 1 & Season %in% 1928:2015) %>% 
  homeaway() %>% 
  filter(team == "FC Barcelona", opp == "Real Madrid") %>% 
  group_by(venue, Season) %>% 
  mutate(totalpts = 3*sum(gf>ga) + sum(gf == ga)) %>%     # instead of summarize to keep other columns
  arrange(Season) %>% 
  mutate(W = if_else(totalpts == 3, 1, 0), D = if_else(totalpts == 1, 1, 0), L = if_else(totalpts == 0, 1, 0)) %>% 
  transform(W = as.numeric(W),
            D = as.numeric(D),
            L = as.numeric(L))
# REJOIN TO BARCA1
BarcaFIXED <- full_join(Barca2.5, Barca1, by = c("Date", "Season", "team", "opp", "gf", "ga", "tier", "venue", "totalpts"))

Barca.Fixed <- merge(Barca1, Barca2.5, all = TRUE)

Barca <- spain %>% 
  subset(tier == 1 & Season %in% 1928:2015) %>% 
  homeaway() %>% 
  filter(team == "FC Barcelona", opp == "Real Madrid") %>% 
  group_by(venue, Season) %>% 
  mutate(totalpts = 3*sum(gf>ga) + sum(gf == ga)) %>%     # instead of summarize to keep other columns
  arrange(Season) %>% 
  mutate(W = if_else(totalpts == 3, 1, 0), D = if_else(totalpts == 1, 1, 0), L = if_else(totalpts == 0, 1, 0)) %>% 
  transform(W = as.numeric(W),
            D = as.numeric(D),
            L = as.numeric(L))
  
as.data.frame(table(.$W))

Barca$Wins <- rowSums(Barca$W == 3)
table(Barca$W)
?add_count
aggregate(totalpts, data = Barca, sum)

?aggregate()

summary(Barca)
data.frame(Barca)
class(Barca$W)
list(Barca)
str(Barca)

# if 3 then W, if 1 then draw, if .... etc.????


Barca <- spain %>% 
  subset(tier == 1 & Season %in% 1928:2015) %>% 
  homeaway() %>% 
  filter(team == "FC Barcelona", opp == "Real Madrid") %>% 
  mutate(win = gf > ga, lose = gf < ga, draw = gf == ga)



maketable_eng(df = LIV, Season = 1996, tier = 1) %>% 
  filter(team == "Liverpool") %>% 
  select(-Pos)


maketable_eng(df = england, Season = 1999, tier = 1)


maketable_eng(df = EPL, Season = 2003, tier = 1)



maketable_eng(df = LIV, Season = 1992, tier = 1) %>% 
  maketable_eng(df = LIV, Season = 1993, tier = 1) %>% 
  maketable_eng(df = LIV, Season = 1994, tier = 1) %>% 
  group_by(Season)
# SEASON =/ appear in maketable_eng


maketable_ha(df = LIV, Season = 1992:2015, tier = 1, pts = 3)   # NOT build across seasons...

table <- rbind(england, england_current()) %>% 
  subset(tier ==1 & Season %in% 1992:2015) %>%
  homeaway() %>% 
  group_by(team, venue, Season) %>% 
  summarise(totalpts = 3*sum(gf>ga) + sum(gf == ga)) %>% 
  spread(venue, totalpts) %>% 
  mutate(totalpts = away + home) %>% 
  arrange(Season, desc(totalpts)) %>%           # NOICE
  mutate(GF = gf, GA = ga) %>% 
  # 
  # mutate(GD = GF-GA) %>%
  # group_by(team) %>%
  # summarise(GP = sum(GD<=100),
  #          W = sum(GD>0),
  #          D = sum(GD==0),
  #          L = sum(GD<0),                                # need to add in GF + GA  in above code somewhere....
  #          gf = sum(GF),
  #          ga = sum(GA),
  #          gd = sum(GD)
  # )
  filter(team == "Liverpool") %>% 
  arrange(desc(Season))

# now just add in all the extra info..........

table <- rbind(england, england_current()) %>% 
  subset(tier ==1 & Season %in% 1992:2015) %>%
  homeaway() %>% 
  group_by(team, venue, Season) %>% 
  #mutate(totalpts = 3*sum(gf>ga) + sum(gf == ga))  # how to summarize but keep used variables??  try aggregate() ?????   or... just mutate()... 

?aggregate()

england %>% homeaway() %>% 
aggregate(gf, by = list(team), FUN = "sum")




# Custom function by J.G.:
maketable_ha <- function(df=NULL, Season=NULL, tier=NULL, pts=3, type = c("both", "home", "away")) {
  
  GA<-GF<-ga<-gf<-gd<-GD<-D<-L<-W<-Pts<-.<-Date<-home<-team<-visitor<-hgoal<-opp<-vgoal<-goaldif <-FT<-division<-result<-maxgoal<-mingoal<-absgoaldif<-NULL
  
  #subset by season and tier, if applicable
  if(!is.null(Season) & is.null(tier)) {
    dfx <- df[(df$Season == Season), ]
  } else if(is.null(Season) & !is.null(tier)) {
    dfx <- df[(df$tier == tier), ]
  } else if(!is.null(Season) & !is.null(tier)) {
    dfx <- df[(df$Season == Season & df$tier == tier), ]
  } else {
    dfx <- df
  }
  
  #msubset only home or away fixtures, if applicable
  if(match.arg(type)=="home") {
    temp <- select(dfx, team=home, opp=visitor, GF=hgoal, GA=vgoal)
  } else if(match.arg(type)=="away") {
    temp <- select(dfx, team=visitor, opp=home, GF=vgoal, GA=hgoal)
  } else if(match.arg(type)=="both") {
    temp <-rbind(
      select(dfx, team=home, opp=visitor, GF=hgoal, GA=vgoal),
      select(dfx, team=visitor, opp=home, GF=vgoal, GA=hgoal)
    )
  }
  
  #make table
  table <- temp %>%
    mutate(GD = GF-GA) %>%
    group_by(team) %>%
    summarise(GP = sum(GD<=100),
              W = sum(GD>0),
              D = sum(GD==0),
              L = sum(GD<0),
              gf = sum(GF),
              ga = sum(GA),
              gd = sum(GD)
    ) %>%
    mutate(Pts = (W*pts) + D) %>%
    arrange(-Pts, -gd, -gf) %>%
    mutate(Pos = rownames(.)) %>%
    as.data.frame()
  
  table <- arrange(table, -Pts, -gd, -gf)
  
  return(table)
  
}

??paste0


dd <- lapply(unique(EPL$Season), function(x) {
  
  #league tables for home fixtures
  home <- maketable_ha(EPL, Season = x, tier = 1, type="home") %>%
    mutate(Hpts = Pts, GPH = GP)
  #league tables for away fixtures
  away <- maketable_ha(EPL, Season = x, tier = 1, type="away") %>%
    mutate(Apts = Pts, GPA = GP)
  #combined (real) league table
  both <- maketable_ha(EPL, Season = x, tier = 1, type="both") %>%
    mutate(real_pos = Pos)
  
  #merge together
  plyr::join_all(list(home, away, both), by = "team", type = 'full') %>%
    mutate(Season = x, GP = GPH + GPA, Pos = real_pos) %>%
    select(Season, team, GP, GPH, Hpts, GPA, Apts, Pos) %>%
    mutate(HPR = Hpts / (Hpts + Apts) ) %>%
    arrange(HPR)
} ) %>%
  #collapse this list to a dataframe
  plyr::rbind.fill() %>%
  #order by ascending home points ratio
  arrange(HPR)


#prettify the season variable (e.g. 2016 -> 2016/17)
dd$season <- as.factor(paste0(dd$Season, "-", substr(dd$Season+1, 3, 4)))

dd %>% select(season, team, GP ,Hpts, Apts, HPR) %>% head
dd %>% select(season, team, GP ,Hpts, Apts, HPR) %>% tail


maketable_eng(df = england, Season = 2015, tier = 1)



# Attach season tag to team name. (For plot label)
dd$x <- paste0(dd$team, " (", dd$season, ")")

#convert ratio to percentage
dd$pc <- dd$HPR * 100 - 50

#select 10 top, 10 bottom, and create PL average
ss <- select(dd, x, pc)
ss <- rbind(head(ss, 10), data.frame(
  x = "(PL average)",
  pc = mean(ss$pc)
),
tail(ss, 10) )

#colours for plotting (green favour home fixtures, red favour away fixtures)
ss$clr <- as.factor(c(rep(1, 10), 2, rep(3, 10) ))

#plot
gp1 <- ggplot(ss, aes(x = reorder(x, pc), y = pc, fill=clr) ) + 
  geom_bar(stat="identity") +
  coord_flip() +
  geom_text(aes(x=x, y=2, label=x), hjust=0, size=4, fontface = "bold") +
  xlab("") +
  ylab("Home points / total points (%)") +
  scale_y_continuous(limits=c(-26,51), breaks=c(-25, 0, 25, 50), labels=c(25, 50, 75, 100), expand=c(0,0) ) +
  scale_fill_manual(values=c("#9999CC", "grey70", "#66CC99")) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.title.x = element_text(size=14, colour = "black"),
    axis.text.x = element_text(size=14, colour = "black"),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )

gp1



# For all seasons:

topflight <- rbind(england, england_current()) %>%
  subset(tier == 1)

dd2 <- lapply(unique(topflight$Season), function(x) {
  
  #league tables for home fixtures
  home <- maketable_ha(topflight, Season = x, tier = 1, type="home") %>%
    mutate(Hpts = Pts)
  #league tables for away fixtures
  away <- maketable_ha(topflight, Season = x, tier = 1, type="away") %>%
    mutate(Apts = Pts)
  
  #merge together
  merge(home, away, by="team") %>%
    mutate(Season = x, GP = GP.x + GP.y) %>%
    select(Season, team, GP, Hpts, Apts) %>%
    mutate(HPR = Hpts / (Hpts + Apts) ) %>%
    arrange(HPR)
} ) %>%
  #collapse this list to a dataframe
  plyr::rbind.fill() %>%
  #order by ascending home points ratio
  arrange(HPR)

#prettify the season variable (e.g. 2016 -> 2016/17)
dd2$season <- as.factor(paste0(dd2$Season, "-", substr(dd2$Season+1, 3, 4)))

#append season to team name
dd2$x <- paste0(dd2$team, " (", dd2$season, ")")
#convert ratio to percentage
dd2$pc <- dd2$HPR * 100 - 50
#select 10 top, 10 bottom, and create PL average
ss2 <- select(dd2, x, pc)
ss2 <- rbind(head(ss2, 10), data.frame(
  x = "(Top flight average)",
  pc = mean(ss2$pc)
),
tail(ss2, 10) )

#colours for plotting (green favour home fixtures, red favour away fixtures)
ss2$clr <- as.factor(c(rep(1, 10), 2, rep(3, 10) ))

#plot
gp2 <- ggplot(ss2, aes(x = reorder(x, pc), y = pc, fill=clr) ) + 
  geom_bar(stat="identity") +
  coord_flip() +
  geom_text(aes(x=x, y=2, label=x), hjust=0, size=4, fontface = "bold") +
  xlab("") +
  ylab("Home points / total points (%)") +
  scale_y_continuous(limits=c(-26,51), breaks=c(-25, 0, 25, 50), labels=c(25, 50, 75, 100), expand=c(0,0) ) +
  scale_fill_manual(values=c("#9999CC", "grey70", "#66CC99")) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.title.x = element_text(size=14, colour = "black"),
    axis.text.x = element_text(size=14, colour = "black"),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )

gp2



dd3 <- dd %>% 
  group_by(team) %>%
  summarise(HPR = mean(HPR), GP = sum(GP)) %>%
  arrange(HPR)

dd3 %>% tbl_df %>% print(n = nrow(.))    # n = nrow(.)     . for default 'x'

