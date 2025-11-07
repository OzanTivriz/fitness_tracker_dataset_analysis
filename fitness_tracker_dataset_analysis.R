library(tidyverse)
library(reshape2) # for heatmap

trackings <- read.csv("fitness_tracker_dataset/random_fitness_dataset.csv")

str(trackings)

# some cleaning
trackings_v2 <- trackings

# converting timestamp column from chr to date
trackings_v2$timestamp <- ymd_hms(trackings_v2$timestamp, tz = "UTC")

# adding day_of_week and time column for further anaylysis
trackings_v2$day_of_week <- format(as.Date(trackings_v2$timestamp), "%A")
trackings_v2$time <- as.POSIXct(format(trackings_v2$timestamp, "%H:%M"),format="%H:%M") 

# creating a vector for day_of_week
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
trackings_v2$day_of_week <- factor(trackings_v2$day_of_week, levels = days)

# creating cal_per_min column
trackings_v2$cal_per_min <- round(trackings_v2$calories_burned/trackings_v2$duration_min, 0)

# filtering the data to humanly possible cal_per_min
trackings_v2 <- trackings_v2 %>%
  filter(cal_per_min < 30)

#-----------------------------------------------------------------------------

# for activity duration comparison
mean_activity <- trackings_v2 %>% 
  group_by(activity_type) %>% 
  summarise(
    mean_duration = mean(duration_min),
    med_duration = median(duration_min)
  )

# for calories burned comparison
mean_calories <- trackings_v2 %>% 
  group_by(activity_type) %>% 
  summarise(
    mean_calories = mean(calories_burned),
    med_calories = median(calories_burned)
  )

# for cal_per_min comparison
mean_cal_per_min <- trackings_v2 %>% 
  group_by(activity_type) %>% 
  summarise(
    mean_cal_per_min = mean(cal_per_min),
    med_cal_per_min = median(cal_per_min)
  )

# for age comparison
mean_age <- trackings_v2 %>% 
  group_by(activity_type) %>% 
  summarise(
    mean_age = mean(age),
    med_age = median(age)
  )

# age hearth rate comparison
summary_age <- trackings_v2 %>%
  group_by(age, activity_type) %>%
  summarise(
    heart_rate_avg = round(mean(heart_rate_avg),0))

cor_df <- trackings_v2 %>% 
  select(age,heart_rate_avg,calories_burned,duration_min)

cor_df <- cor_df %>% 
  rename(Age  = age,
         HeartRate = heart_rate_avg,
         Calories = calories_burned,
         Duration = duration_min)
# -----------------------------------------------------------------------------

ggplot(mean_activity, aes(x=activity_type, y=mean_duration)) +
  geom_bar(stat = "identity", fill = "#02bfe7", alpha = 0.8)+
  geom_point(aes(y = med_duration), size = 2, color = "red" ) +
  theme_minimal() +
  geom_text(aes(y=med_duration, label = med_duration),vjust = 1.5) +
  labs(title = "Duration Time per Activity",
       x = "Activity Type",
       y = "Mean Duration Min")


ggplot(mean_calories, aes(x=activity_type, y=mean_calories)) +
  geom_bar(stat = "identity", fill = "#86CD82", alpha = 0.8)+
  geom_point(aes(y = med_calories), size = 2, color = "red" ) +
  theme_minimal() +
  geom_text(aes(y=med_calories, label = med_calories),vjust = 1.5) +
  labs(title = "Calories Burned per Activity",
       x = "Activity Type",
       y = "Mean Calories Burned")

ggplot(mean_cal_per_min, aes(x=activity_type, y=mean_cal_per_min)) +
  geom_bar(stat = "identity", fill = "#e31c3d", alpha = 0.7)+
  geom_point(aes(y = med_cal_per_min), size = 2, color = "#ffffff" ) +
  theme_minimal() +
  geom_text(aes(y=med_cal_per_min, label = med_cal_per_min),vjust = 1.5) +
  labs(title = "Calories Burned per Min by Activity",
       x = "Activity Type",
       y = "Mean Calories Burned")

ggplot(mean_age, aes(x=activity_type, y=mean_age)) +
  geom_bar(stat = "identity", fill = "#aeb0b5", alpha = 0.8)+
  geom_point(aes(y = med_age), size = 2, color = "red" ) +
  theme_minimal() +
  geom_text(aes(y=med_age, label = med_age),vjust = 1.5) +
  labs(title = "Ages by Activity",
       x = "Activity Type",
       y = "Age")


ggplot(trackings_v2) +
  geom_bar(aes(x=activity_type, fill = activity_type)) +
  facet_wrap(~day_of_week)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,),
        legend.position = "none")+
  labs(title = "Activities by Day of Week",
       x = "Activity Type",
       y = "Count"
  )

ggplot(summary_age, aes(x = age, y = heart_rate_avg, color = activity_type)) +
  geom_line(size = 1) +
  facet_wrap(~activity_type) +
  theme_minimal() +
  theme(legend.position = "none") + 
  labs(title = "Age vs Avg. Heart Rate by Activity Type",
       x= "Age",
       y = "Average Hearth Rate")

# for correlation matris
cor(cor_df, use = "complete.obs")

melted_cor <- melt(cor(cor_df[,c("Age","HeartRate","Calories","Duration")],
                       use = "complete.obs"))

# correlation heatmap
ggplot(melted_cor, aes(Var1, Var2, fill = value))+
  geom_tile() +
  geom_text (aes(label=round(value,2)), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", mid = "white", high= "red", midpoint = 0) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs (title = "Correlation Heatmap",
        x= "",
        y= "")

# activities by gender
ggplot(trackings_v2) +
  geom_bar(aes(x = activity_type, fill=gender), position = "dodge") +
  scale_fill_manual(values = c("Male" = "#7CB9E8",
                               "Female" = "pink",
                               "Other" = "gray")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,)) +
  labs (title = "Activities by Gender",
        x = "Activity Type",
        y = "Count",
        fill = "Gender")


# calculating percetange of activity for each gender
activity_summary <- trackings_v2 %>%
  group_by(gender, activity_type) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# finding the top activity for each gender
top_activities <- activity_summary %>%
  group_by(gender) %>%
  slice_max(order_by = percent, n = 1) %>%
  select(gender, activity_type)

# higlighting the column so we can use it on pie chart
activity_summary <- activity_summary %>%
  left_join(top_activities, by = "gender", suffix = c("", "_top")) %>%
  mutate(alpha_val = ifelse(activity_type == activity_type_top, 1, 0.3))

# pie chart
ggplot(activity_summary, aes(x = "", y = percent, fill = activity_type)) +
  geom_bar(stat = "identity",width = 1,color = "white",
           aes(alpha = alpha_val)) +
  scale_alpha_identity(guide = "none") +
  coord_polar(theta = "y") +
  facet_wrap(~gender) +
  geom_text(aes(label = ifelse(activity_type == activity_type_top,
                               paste0(activity_type, "\n", round(percent, 1), "%"), "")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4.5, fontface = "bold") +
  theme_void() +
  theme(
    strip.text = element_text(size = 13),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(size = 15, hjust = 0.5)
    ) +
  ggtitle("Top Activity by Gender")



# finding least activity
least_activities <- activity_summary %>%
  group_by(gender) %>%
  slice_min(order_by = percent, n = 1) %>%
  select(gender, activity_type)

# again for highlighting using alpha (1 ve 0.3 kllan)
activity_summary <- activity_summary %>%
  left_join(least_activities, by = "gender", suffix = c("", "_least")) %>%
  mutate(alpha_val = ifelse(activity_type == activity_type_least, 1, 0.3))

# pie chart for least activities
ggplot(activity_summary, aes(x = "", y = percent, fill = activity_type)) +
  geom_bar(
    stat = "identity",
    width = 1,
    color = "white",
    aes(alpha = alpha_val)
  ) +
  scale_alpha_identity(guide = "none") +
  coord_polar(theta = "y") +
  facet_wrap(~gender) +
  geom_text(aes(label = ifelse(activity_type == activity_type_least,
                               paste0(activity_type, "\n", round(percent, 1), "%"), "")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4.5, fontface = "bold") +
  theme_void() +
  theme(
    strip.text = element_text(size = 13),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(size = 15, hjust = 0.5)
  ) +
  ggtitle("Least Activity by Gender")
