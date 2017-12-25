library(tidyverse)
library(readr)
library(stringr)
library(tidytext)
library(scales)

MC_responses <- read_csv("~/R_materials/multipleChoiceResponses.csv", col_names = TRUE)

glimpse(MC_responses)

# Gender:
MC_responses$GenderSelect

MC_responses %>% 
  filter(!is.na(GenderSelect)) %>% 
  count(GenderSelect) %>% 
  ggplot(aes(reorder(GenderSelect, n), n, fill = GenderSelect)) +
  geom_col(show.legend = FALSE) +
  coord_flip()

# By country:

MC_responses %>% 
  count(Country, sort = TRUE)

# CHloropeth map:




# Age
MC_responses %>% 
  select(Age) %>% 
  summary()

is.na(MC_responses$Age)

MC_responses %>% 
  drop_na(Age) %>%
  select(Age) %>% 
  summary()
# drop_na() or filter(!is.na())

MC_responses %>% 
  ggplot(aes(Age)) +
  geom_histogram(bins = 100) +
  geom_vline(aes(xintercept = MC_responses %>% 
                                          drop_na(Age) %>% 
                                          summarize(mean = mean(Age))), 
             color = "red", na.rm = FALSE)

# Education

MC_responses %>% 
  count(FormalEducation) %>% 
  ggplot(aes(reorder(FormalEducation, n), n, fill = FormalEducation)) +
  geom_col() +
  coord_flip()

# Employment Status

MC_responses %>% 
  add_tally(EmploymentStatus)

library(scales)

MC_responses %>% 
  count(EmploymentStatus) %>% 
  ggplot(aes(reorder(EmploymentStatus, n), n, fill = EmploymentStatus)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(breaks = pretty_breaks(), labels = comma)


# Programming experience
MC_responses %>% 
  count(Tenure) %>% 
  ggplot(aes(reorder(Tenure, n), n, fill = Tenure)) +
  geom_col() +
  coord_flip()

MC_responses %>% 
  count(GenderSelect) %>% 
  ggplot(aes(reorder(GenderSelect, n), n, fill = GenderSelect)) +
  geom_col() +
  coord_flip()

library(gridExtra)

MC_sub <- MC_responses %>% 
  mutate(Employment_Status_n = tally(EmploymentStatus), 
            Tenure_n = count(Tenure), 
            Gender_n = tally(GenderSelect))

MC_responses %>% 
  select(EmploymentStatus, Tenure, GenderSelect) %>% 
  summarize(count = n(), sort = TRUE) %>% 
  nest()

MC_responses %>% 
  group_by(.dots = c("EmploymentStatus", "Tenure", "GenderSelect")) %>% 
  mutate(count = count(.), sort = TRUE)


vars

MC_sub <- MC_responses %>% select(EmploymentStatus, Tenure, GenderSelect)

plot_responses <- function(X, na.rm = TRUE, ...) {
  
  vars <- names(X)
  
  for(i in seq_along(vars)) {
    
    n <- X %>% count(vars[i])
    
    g <- ggplot(data = X, aes_string(reorder(vars, n), n, fill = vars)) +
      geom_col() +
      coord_flip()
    print(g)
  }  
}

plot_responses(X = MC_sub)
names(MC_sub)





MC_responses %>% 
  filter(!is.na(LanguageRecommendationSelect)) %>% 
  count(LanguageRecommendationSelect) %>% 
  ggplot(aes(reorder(LanguageRecommendationSelect, n), n, fill = LanguageRecommendationSelect)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(breaks = pretty_breaks())


MC_responses %>% 
  group_by(Age) %>% 
  ggplot(aes(Age), color = LanguageRecommendationSelect) +
  geom_line()

library(ggpubr)

MC_responses %>% 
  filter(!is.na(LanguageRecommendationSelect)) %>% 
  count(Age, LanguageRecommendationSelect) %>% 
  mutate(ratio = n/sum(n)) %>% 
  ggplot(aes(Age, ratio)) +
  geom_line(aes(color = LanguageRecommendationSelect))




# Python + R >>> Job Titles
MC_responses %>% 
  filter(LanguageRecommendationSelect == c("Python", "R")) %>% 
  count(LanguageRecommendationSelect, CurrentJobTitleSelect) %>% 
  ggplot(aes(x = CurrentJobTitleSelect, y = n, fill = LanguageRecommendationSelect)) +
  geom_col(position = "dodge") +
  coord_flip()


# Next Tech
MC_responses %>% 
  filter(!is.na(MLToolNextYearSelect)) %>% 
  count(MLToolNextYearSelect) %>% 
  filter(n > 200) %>% 
  ggplot(aes(reorder(MLToolNextYearSelect, n), n, fill = MLToolNextYearSelect)) +
  geom_col(show.legend = FALSE) +
  coord_flip()

# Next Method
MC_responses %>% 
  filter(!is.na(MLMethodNextYearSelect)) %>% 
  count(MLMethodNextYearSelect) %>% 
  filter(n > 200) %>% 
  ggplot(aes(reorder(MLMethodNextYearSelect, n), n, fill = MLMethodNextYearSelect)) +
  geom_col(show.legend = FALSE) +
  coord_flip()


# Learning DS?
library(stringr)
library(tidytext)

MC_responses %>% 
  select(LearningPlatformSelect) %>% 
  unnest_tokens(responses, LearningPlatformSelect, token = str_split, pattern = ",") %>% 
  filter(!is.na(responses)) %>% 
  count(responses) %>% 
  filter(n > 100) %>% 
  ggplot(aes(reorder(responses, n), n, fill = responses)) +
  geom_col(show.legend = FALSE) +
  coord_flip()



# Learning platform usefullness: Not Useful >> Somewhat Useful >> Very Useful
# library(tidyr) for spread

Platform_Usefullness <- MC_responses %>% 
  select(contains("LearningPlatformUse")) %>% 
  gather(Platform, response) %>% 
  filter(!is.na(response)) %>% 
  group_by(Platform) %>% 
  count(response) %>% 
  mutate(sum = sum(n),
         ratio = n/sum,
         response = as.factor(response)) %>% 
  ungroup()

glimpse(Platform_Usefullness)

platform_names <- str_replace(Platform_Usefullness$Platform, "LearningPlatformUsefulness", "")
Platform_Usefullness$Platform <- platform_names

Platform_Usefullness$Platform

glimpse(Platform_Usefullness)

Platform_Usefullness %>% 
  mutate(ratio = ratio %>% round(digits = 2)) %>% 
  ggplot(aes(response, reorder(Platform, ratio), label = ratio)) + 
  geom_tile(aes(fill = ratio)) +
  geom_text(size = 2)

plat <- Platform_Usefullness %>% tail(12)
plat

plat %>% 
  mutate(ratio = ratio %>% round(digits = 2)) %>% 
  ggplot(aes(response, reorder(Platform, ratio), label = ratio)) + 
  geom_tile(aes(fill = ratio)) +
  geom_text(size = 2)



pie <- MC_responses %>% 
  select(contains("LearningCategory")) %>% 
  gather(platform, response) %>% 
  filter(!is.na(response)) %>% 
  group_by(platform) %>% 
  summarize(mean = mean(response))

platform_names <- str_replace(pie$platform, "LearningCategory", "")
pie$platform <- platform_names

glimpse(pie)

library(scales)
# attempt pie chart:
pie %>% 
  ggplot(aes("", y = mean, fill = platform)) + 
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(y = mean/50 + c(0, cumsum(mean)[-length(mean)]),  # hard to configure labels...
                label = percent(mean/100)), size = 3) +
  coord_polar("y", start = 0) +
  theme(axis.text=element_blank(),
        axis.ticks= element_blank(), 
        panel.grid = element_blank(),
        axis.title = element_blank()) +
  labs(title = "Contribution of each platform to learning DS") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Expense Type",
                    labels=c("Infrastructure & Capital", "Financing", "Operating")) +
  guides(fill = guide_legend(reverse = TRUE))


# Dot plots:

ggplot(pie, aes(x = mean, y = reorder(platform, mean))) +
  geom_point(aes(fill = platform), size = 10, show.legend = FALSE, shape = 21) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(x = "Percentages", y = NULL) +
  ggtitle("Dot plots instead!") +
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour = "grey60"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = rel(1.1), face = "bold"),
        axis.ticks.y = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.key.width = unit(3, "lines")) +
  geom_text(aes(label = percent(round(mean, digits = 2)/100)), size = 2.5)


# Hardware

MC_responses %>% 
  filter(!is.na(HardwarePersonalProjectsSelect)) %>% 
  count(HardwarePersonalProjectsSelect, sort = TRUE)


# Time spent studying:

glimpse(MC_responses)

glimpse(MC_responses$TimeSpentStudying)
unique(MC_responses$TimeSpentStudying)

MC_responses$TimeSpentStudying <- factor(MC_responses$TimeSpentStudying, levels = c(
                                                                         "NA",
                                                                         "0 - 1 hour", 
                                                                         "2 - 10 hours",
                                                                         "11 - 39 hours",
                                                                         "40+"))

glimpse(MC_responses$TimeSpentStudying)

MC_responses %>% 
  filter(!is.na(TimeSpentStudying) & !is.na(EmploymentStatus)) %>% 
  count(TimeSpentStudying, EmploymentStatus, sort = TRUE) %>% 
  ggplot(aes(TimeSpentStudying, n, fill = EmploymentStatus)) +
  geom_col(position = position_dodge()) +
  coord_flip() +
  theme_bw()


# Popular blogs and podcasts:

MC_responses %>% 
  select(BlogsPodcastsNewslettersSelect) %>% 
  unnest_tokens(responses, BlogsPodcastsNewslettersSelect, token = str_split, pattern = ",") %>% 
  filter(!is.na(responses)) %>% 
  count(responses, sort = TRUE) %>% 
  filter(n > 600) %>% 
  ggplot(aes(reorder(responses, n), n, fill = responses)) +
  geom_col(show.legend = FALSE) +
  coord_flip()



# DS course platforms:

MC_responses %>% 
  unnest_tokens(course_platform, CoursePlatformSelect, token = str_split, pattern = ",") %>% 
  count(course_platform, sort = TRUE) %>% 
  filter(!is.na(course_platform)) %>% 
  filter(n > 100) %>% 
  ggplot(aes(reorder(course_platform, n), n, fill = course_platform)) +
  geom_col(show.legend = FALSE) +
  coord_flip()














