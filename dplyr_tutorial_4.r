library(dplyr)
msleep <- ggplot2::msleep

glimpse(msleep)

# counts and add counts

msleep %>% 
  count(order, sort = TRUE)

msleep %>% 
  count(order, vore, sort = T)

# add # of obsv. to col

msleep %>% 
  tally()

msleep %>% 
  nrow()

msleep %>% 
  select(1:3) %>% 
  add_tally()

msleep %>% 
  select(1:3) %>% 
  mutate(n = n())

# use add_count(var) to save time of group-mutate-ungroup!
msleep %>% 
  select(name:vore) %>% 
  add_count(vore)


# summarize()

msleep %>% 
  summarize(n = n(), 
            avg = mean(sleep_total), 
            maximum = max(sleep_total))

# by group >>> group_by()

# summarize_all(): summarize ALL cols 
# summarize_if(): if/else
# summarize_at(): specific cols in vars()

msleep %>% 
  group_by(vore) %>% 
  summarize_all(mean, na.rm = TRUE)

# specify own function
msleep %>% 
  group_by(vore) %>% 
  summarize_all(~mean(., na.rm = T) + 5)
# summarize_all(funs(mean(., na.rm = T) + 5)) 

msleep %>% 
  group_by(vore) %>% 
  summarize_if(is.numeric, mean, na.rm = TRUE)

# rename new summarized cols
msleep %>% 
  group_by(vore) %>% 
  summarize_if(is.numeric, mean, na.rm = T) %>% 
  rename_if(is.numeric, ~paste0("avg_", .))

# summariize_at()

msleep %>% 
  group_by(vore) %>% 
  summarize_at(vars(contains("sleep")), mean, na.rm = T) %>% 
  rename_at(vars(contains("sleep")), ~paste0("avg_", .))


# arranging rows:

msleep %>% 
  group_by(vore) %>% 
  summarize(avg_sleep = mean(sleep_total)) %>% 
  arrange(desc(avg_sleep))


# refer to grouped data with ".by_group = TRUE" statement
msleep %>% 
  select(order, name, sleep_total) %>% 
  group_by(order) %>% 
  arrange(desc(sleep_total))#, .by_group = T)


msleep %>% 
  select(order, name, sleep_total) %>% 
  group_by(order) %>% 
  arrange(desc(sleep_total), .by_group = T)


# top_n()

msleep %>% 
  group_by(order) %>% 
  summarize(average = mean(sleep_total)) %>% 
  arrange(desc(average)) %>% 
  top_n(5) # top_n(-5)    specify which col top_n(5, var)

msleep %>% 
  sample_frac(.1)  # 10% of rows

# slice()
msleep %>% 
  slice(52:63)














