# R for LFC ---------------------------------------------------------------

require(engsoccerdata)
library(tidyverse)

EPL <- rbind(england, england_current()) %>% 
  subset(tier == 1 & Season %in% 1992:2015)    # shifted back to 2015 because as of 5.22.17 tables for 16/17 season not fully updated = screws up the HPG tables...

head(EPL, 5)
tail(EPL, 5)

View(england)

LIV <- rbind(england, england_current()) %>% 
  subset(tier == 1 & Season %in% 1992:2015 & (home == "Liverpool"|visitor == "Liverpool")) 

LIV %>% filter(home %in% c("Arsenal", "Chelsea")| visitor %in% c("Arsenal", "Chelsea"))



# LFC Managers ------------------------------------------------------------

LFC_managers <- data.frame(name = c("Grame Souness", "Roy Evans", "Roy Evans\nGerard Houllier",
                                    "Gerard Houllier", "Rafael Benitez", "The Clueless Owl", 
                                    "Kenny Dalglish", "Brendan Rodgers", "Jurgen Klopp"),
                           start = c("1992", "1994", "1998", "1999", "2004",
                                     "2010", "2011", "2012", "2015"))
glimpse(LFC_managers)
LFC_managers$start <- as.character(LFC_managers$start)
LFC_managers$start <- as.numeric(LFC_managers$start)
LFC_managers$start <- as.factor(LFC_managers$start)


# maketable_ha function -----------------------------------------------------------

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

# Analysis ----------------------------------------------------------------



# Need to make from LIVERPOOL's POV...  or just frame it as how teams fared AGAINST LFC.
maketable_eng(df = LIV, Season = 1996, tier = 1) %>% 
  filter(team == "Liverpool") %>% 
  select(-Pos)

maketable_eng(df = LIV, Season = 1996, tier = 1) %>% 
  filter(team != "Liverpool") %>% 
  select(-Pos, -GP) %>% 
  mutate(Season = 1996)

maketable_eng(df = LIV, Season = 1997, tier = 1) %>% 
  filter(team != "Liverpool") %>% 
  select(-Pos, -GP) %>% 
  mutate(Season = 1997)  

maketable_eng(df = LIV, Season = 1998, tier = 1) %>% 
  filter(team != "Liverpool") %>% 
  select(-Pos, -GP) %>% 
  mutate(Season = 1998)

maketable_eng(df = LIV, Season = 2008, tier = 1) %>% 
  filter(team != "Liverpool") %>% 
  select(-Pos, -GP) %>% 
  mutate(Season = "2008-09") %>% 
  arrange((Pts))


# League position. 
maketable_eng(df = england, Season = 1997, tier = 1) %>% 
  filter(team == "Liverpool")
maketable_eng(df = england, Season = 1998, tier = 1) %>% 
  filter(team == "Liverpool")
maketable_eng(df = england, Season = 1999, tier = 1) %>% 
  filter(team == "Liverpool")


alltimerecord(england, "Liverpool")

maketable_eng(df = england, Season = 1999, tier = 1)
# all time record
maketable_all(df=england[england$tier==1,],begin="1992-08-15",
              end="2017-07-01") 

england %>% filter(tier == 1) %>% maketable_all(begin = "1992-08-15", end = "2016-5-25")


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
  # summarise(totalpts = 3*sum(gf>ga) + sum(gf == ga))   # if use summarise...cuts out rest. why later code NOT work. duh.
  mutate(GF = gf, GA = ga) %>% 
  mutate(GD = GF-GA) %>%
  group_by(Season, team) %>%
  summarise(GP = sum(GD<=100),
            W = sum(GD>0),
            D = sum(GD==0),
            L = sum(GD<0),                                # need to add in GF + GA  in above code somewhere....
            gf = sum(GF),
            ga = sum(GA),
            gd = sum(GD)
   )
# just need to add in position from specific seasons...



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

maketable_ha(EPL, Season = 2005, tier = 1) %>% 
  transmute(Pos = rownames(.))

?lapply()


# full table for every EPL season (up to 2015-2016).
library(plyr)
huh <- lapply(unique(EPL$Season), function(x) {
  maketable_eng(EPL, Season = x, tier = 1)
}) 
huh <- rbind.fill(huh)

huh %>% mutate(Season = 1992:2015)

??rep()

z <- 1:22
huh[1:22,] %>% mutate(season = 92)
huh[23:44,] %>% mutate(season = 93)
huh[45:66,] %>% mutate(season = 94)
huh[67:86,] %>% mutate(season = 95)
huh[23:nrow(huh),] %>% mutate(season = rep_len(1:20, length.out = 464))
23+20

nrow(huh)

?rep_len()



# dd dataframe ------------------------------------------------------------

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
    mutate(HPR = Hpts / (Hpts + Apts) ) %>%
    arrange(HPR)
} ) %>%
  #collapse this list to a dataframe
  plyr::rbind.fill() %>%
  #order by ascending home points ratio
  arrange(HPR)

dd %>% group_by(Season) %>% arrange(Pos)

??rbind.fill()

#prettify the season variable (e.g. 2016 -> 2016/17)
dd$season <- as.factor(paste0(dd$Season, "-", substr(dd$Season+1, 3, 4)))

dd %>% select(Season, team, GP ,Hpts, Apts, HPR) %>% head
dd %>% select(Season, team, GP ,Hpts, Apts, HPR) %>% tail


# top_six -----------------------------------------------------------------

top_six <- dd %>% filter(team %in% c("Liverpool", "Arsenal", "Chelsea", 
                          "Tottenham Hotspur", "Manchester United", "Manchester City")) %>% 
       arrange(desc(Season)) %>% 
       select(team, Pos, season, Season)

top_six$Pos <- as.numeric(top_six$Pos)          # or else unary operator error....
top_six$Season <- as.character(top_six$Season)
top_six$Season <- as.numeric(top_six$Season)
top_six$Season <- as.factor(top_six$Season)
glimpse(top_six)

library(scales)

top_six %>% 
  mutate(LFC = ifelse(team == "Liverpool", T, F)) %>% 
  ggplot(aes(season, Pos, group = team)) +
  geom_line(aes(color = LFC, alpha = LFC), size = 2) + 
  geom_point(aes(color = LFC, alpha = LFC), size = 2.3) +
  geom_text(data = top_six %>% filter(season == "1992-93"), aes(label = team, x = "1992-93"), color = "black", size = 4, nudge_x = -1.5) +
  geom_text(data = top_six %>% filter(season == "2015-16"), aes(label = team, x = "2015-16"), color = "black", size = 4, nudge_x = 1.5) +
  scale_x_discrete(expand = c(0.08, 0.08)) +
  scale_y_reverse(breaks = c(1, 2, 3, 4, 5, 6, 10, 15, 20), limits = c(20, 1), expand = c(0, 0.25)) +
  labs(x = "Season", y = "League\nPosition")+
  ggtitle("Top Six Historical Premier League Rankings") +
  theme(legend.position = "none") + 
  scale_color_manual(values = c("#af7a0a","#e0585b")) +
  theme.porttheme

?scale_y_reverse
#   geom_vline(aes(xintercept = start), data = LFC_managers)
# labels = c("1992-93", "1993-94", "1994-95", "1995-96", "1996-97", "1997-98", "1998-99", "1999-00", 
  #          "2000-01", "2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09", "2009-10", 
  #          "2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16")

theme.porttheme <-  
  theme(text = element_text(family = "Gill Sans", color = "#444444")) +
  theme(plot.title = element_text(size = 24, hjust = 0.5)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(angle = 0, vjust = .5, margin = margin(r = 15))) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(margin = margin(t = 10))) +
  theme(legend.title = element_blank()) 



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


