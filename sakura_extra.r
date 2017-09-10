###############################################################
# Misc.
###############################################################

sakura %>% ggplot(aes(bloom)) + geom_freqpoly()

sakura %>% unite(date, c("month", "day"), sep = "-")
sakura %>% ggplot(aes(date, year)) + geom_line()

sak <- sakura$full_flowering_date %>% str_replace_all("(.{1})(.*)", "\\1.\\2") %>% as.data.frame()
colnames(sak)[1] <- "full_flower" 
colnames(sak)
sak <- sak %>% separate(full_flower, c("month", "day"), "\\.")




set.seed(12345)
Date <- seq(as.Date("2010/1/1"), as.Date("2014/1/1"), "week")
Y <- rnorm(n=length(Date), mean=100, sd=1)
df <- data.frame(Date, Y)
df$DayOfYear <- as.numeric(format(df$Date, "%j"))
df$Year <- format(df$Date, "%Y")
df$Month <- format(df$Date, "%b")
df$Day <- format(df$Date, "%d")

df$MonthDay <- format(df$Date, "%d-%b")



ggplot(data = df,
       mapping = aes(x = DayOfYear, y = Y, shape = Year, colour = Year)) +
  geom_point() +
  geom_line() +
  facet_grid(facets = Year ~ .) +
  theme_bw()

ggplot(data = df,
       mapping = aes(x = DayOfYear, y = Y, shape = Year, colour = Year)) +
  geom_point() +
  geom_line() +
  facet_grid(facets = Year ~ .) +
  scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) +
  theme_bw()




# sep vs. collapse....
df <- data.frame(x = c(NA, "a b", "a. d", "b.c c"))
df %>% separate(x, c("A", "B"), sep = "1")
df %>% paste(sep = " ", collapse = "__")

