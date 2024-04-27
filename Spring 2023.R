# Initial Analysis of Data
library(tidyverse)
library(moderndive)
library(infer)


########## Newsletter Data ##########
# Dataset: SP23_Final_Email_Campaign_Report_

# Create new variable calculating opens/sends
emails <- emails %>% 
  mutate(opens_sends = Opens/Sends)

###
# Which campaign has the most opens to sends ratio?

# Create month column
emails <- emails %>% 
  mutate(start_month = format(`Start Date`, "%m"))

# Calculate by unique campaigns (campaigns are not grouped)
email_campaigns_2 <- emails %>% 
  group_by(`Campaign Name`) %>%
  summarize(mean_open_sends=mean(opens_sends),
            months=start_month,
            mode_header=mode(claire_header)) %>% 
  arrange(desc(mean_open_sends))
# Plot email campaigns and mean open/sends rate
email_campaigns_2 %>% 
  ggplot(aes(x=mean_open_sends, y=reorder(`Campaign Name`, mean_open_sends), fill=months)) +
  geom_bar(stat="identity") +
  labs(x="Average Opens/Sends Rate", y="Campaigns") +
  ggtitle("Email Campaign Average Opens/Sends Rate")
email_campaigns_2 %>% 
  print(n=64)
# Findings:
# Campaigns with highest open to send ratio:
# 1. Clairity Ball 2022 Tax Receipts (77%)
# 2. glow 2022 eve (73%)
# 3. Glow Ride 2022 Follow Up 2 (59%)
# Campaigns with lowest open to send ratio:
# 1. Year End 20 (13%)
# 2. Valentine's Day 2021 (14%)
# 3. Justin and Gemma (14%) 
# Other Findings:
# Month does not impact average opens/sends rate of campaigns very much


###
# What is the best time to send newsletters to ensure it has the highest opens/sends ratio?
# Reformat time
emails$Time <- format(emails$Time, format="%H:%M:%S")
# Create new variable called Time of Day
emails <- emails %>% 
  mutate(TimeOfDay = ifelse(Time>"21:00:00", "6. Late Evening",
                            ifelse(Time>"18:00:00", "5. Evening",
                                   ifelse(Time>"15:00:00", "4. Late Afternoon",
                                          ifelse(Time>"12:00:00", "3. Early Afternoon",
                                                 ifelse(Time>"09:00:00", "2. Late Morning",
                                                        ifelse(Time>"06:00:00", "1. Early Morning", "7. Late Night")))))))


# Group by time of day and find open/send rate
emails_timeofday <- emails %>% 
  group_by(TimeOfDay) %>% 
  summarize(mean_open_sends=mean(opens_sends)) %>% 
  arrange(desc(mean_open_sends))
emails_timeofday
# Graph Time of Day and Open/Send Rate
emails %>% 
  ggplot(aes(x=TimeOfDay, y=opens_sends)) +
  geom_boxplot() +
  labs(x="Time of Day", y="Open/Send Rate") +
  ggtitle("Email Open/Send Rate Throughout the Day")
# Findings:
# Emails achieve the highest open/send rate in the evening (39.8%),
# and are least effective when sent in the early morning (20.3%).

# Create a variable that calculates unsubscribe rate based on
# proportion of unsubscribers out of people who open the newsletter.
emails <- emails %>% 
  mutate(unsubscribe_rate = Unsubscribes/Opens)

###
# When do most people unsubscribe to newsletters?
unsub_timeofday <- emails %>% 
  group_by(TimeOfDay) %>% 
  summarize(mean_unsub=mean(unsubscribe_rate)) %>% 
  arrange(desc(mean_unsub))
unsub_timeofday
emails %>% 
  ggplot(aes(x=TimeOfDay, y=unsubscribe_rate)) +
  geom_boxplot() +
  labs(x="Time of Day", y="Unsubscribe Rate") +
  ggtitle("Email Unsubscribe Rate Throughout the Day")
# Findings:
# Emails have the highest unsubscribe rate in the late evening (2.5%),
# and have the lowest unsubscribe rate when sent in the evening (1.2%).


###
# What is the trend between open send rate and date that emails are sent?
emails %>% 
  ggplot(aes(x=`Start Date`, y=opens_sends)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(x="Start Date of Email Campaign", y="Open/Sends Rate") +
  ggtitle("Start Date of Email Campaign and Open/Sends Rate")
# From the graph, it is clear that newer emails have had a higher open/sends rate


###
# Which month has the most open/sends rate?
# Group data by month
email_months <- emails %>%
  mutate(months = format(`Start Date`, "%m")) %>%
  group_by(months) %>%
  summarize(mean_opens_sends = mean(opens_sends))
# Graph open/sends rate over months
email_months %>% 
  ggplot(aes(x=months, y=mean_opens_sends)) +
  geom_bar(stat="identity", fill="purple") +
  labs(x="Months", y="Open/Sends Rate") +
  ggtitle("Open/Sends Rate per Month")
email_months
# Findings:
# May has the lowest opens/sends rate (18.6%), while September has the highest (39.5%).
# There seems to be a dip in opens/sends rate during the winter months (Oct, Nov, Dec).
# I thought about recommending the company to avoid sending emails during these months.
# But I think that is 1) unrealistic because we need to market events that occur
# around that time, and 2) not a good analysis because the low opens/sends rate
# can be due to other factors such as the subject of the email, campaign, etc.


###
# Does the header being "Claire's Place Foundation" affect how many people unsubscribe?
# Create new variable that looks at whether "Claire's Place Foundation" is the top header in the newsletter.
emails <- emails %>%
  mutate(claire_header=c("Yes", "No", "Yes", "No", "No", "Yes", "Yes", "Yes", 
                         "No", "No", "No", "No", "No", "Yes", "No", "No", "No",
                         "No", "No", "No", "No", "No", "No", "Yes", "No", "No",
                         "No", "No", "No", "No", "No", "No", "No", "No", "No",
                         "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "No",
                         "Yes", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "No",
                         "Yes", "Yes", "No", "Yes", "Yes", "No", "No", "Yes",
                         "Yes", "Yes", "Yes", "No", "No"
  ))

# Create hypothesis test for the claim: 
# A newsletter with "Claire's Place Foundation" as a header results in a higher
# unsubscribe rate than a newsletter with other headers.

# H0: Same unsubscribe rate regardless of whether header is "Claire's Place Foundation"
# H1: Unsubscribe rate (yes claire_header) > Unsubscribe rate (no claire_header)

# Observed Statistic
obs_header <- emails %>% 
  specify(unsubscribe_rate~claire_header) %>% 
  calculate(stat="diff in means",
            order=c("Yes", "No"))

# Null distribution
null_header <- emails %>% 
  specify(unsubscribe_rate~claire_header) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in means",
            order=c("Yes", "No"))

# Visualize findings
visualize(null_header) +
  shade_p_value(obs_header, direction="right")

# P-Value: 0.002
get_p_value(null_header, obs_header, direction="right")
# Significant, reject null hypothesis
# Findings:
# A newsletter with "Claire's Place Foundation" as a header results in a higher
# unsubscribe rate than a newsletter with other headers.


### What campaigns are the most marketed?
emails_group <- emails %>% 
  group_by(`Campaign Name Unique`) %>% 
  summarize(campaign_count = n(),
            mean_click_open = mean(Clicks/Opens)) %>% 
  arrange(desc(campaign_count)) %>% 
  print(n=44)
# Campaigns with the most sends:
# Clairity Ball 2022: 6
# Glow Ride 2022: 5
# Clairity Ball 2021: 4
# 1 in a Million Campaign: 3
# All other campaigns only have 1 or 2 sends


###
# Does sending multiple emails about the same event help decrease the unsubscribe rate?
# Create a new variable that determines whether multiple emails are sent about the same campaign
emails$repeated_campaign <- ifelse(duplicated(emails$`Campaign Name Unique`) | duplicated(emails$`Campaign Name Unique`, fromLast = TRUE), "Yes", "No")

# Create hypothesis test for the claim: 
# A campaign with multiple emails will have a lower unsubscribe rate
# than a campaign with only one marketing email.

# H0: Same unsubscribe rate regardless of whether campaign is sent multiple times.
# H1: Unsubscribe rate (yes repeated_campaign) < Unsubscribe rate (no repeated_campaign)

# Observed Statistic
obs_emails <- emails %>% 
  specify(unsubscribe_rate~repeated_campaign) %>% 
  calculate(stat="diff in means",
            order=c("Yes", "No"))

# Null distribution
null_emails <- emails %>% 
  specify(unsubscribe_rate~repeated_campaign) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in means",
            order=c("Yes", "No"))

# Visualize findings
visualize(null_emails) +
  shade_p_value(obs_emails, direction="left")

# P-Value: 0.004
get_p_value(null_emails, obs_emails, direction="left")
# Significant, reject null hypothesis
# Findings:
# A newsletter with a campaign marketed multiple times results in a lower
# unsubscribe rate than a newsletter with a single send.


###
# Is claire_header a confounding factor?
header_yes <- filter(emails, claire_header=="Yes")
header_no <- filter(emails, claire_header=="No")

# Claire Header: Yes
obs_emails_YES <- header_yes %>% 
  specify(unsubscribe_rate~repeated_campaign) %>% 
  calculate(stat="diff in means",
            order=c("Yes", "No"))
null_emails_YES <- header_yes %>% 
  specify(unsubscribe_rate~repeated_campaign) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in means",
            order=c("Yes", "No"))
visualize(null_emails_YES) +
  shade_p_value(obs_emails_YES, direction="left")
get_p_value(null_emails_YES, obs_emails_YES, direction="left")
# P-Value: 0.126

# Claire Header: No
obs_emails_NO <- header_no %>% 
  specify(unsubscribe_rate~repeated_campaign) %>% 
  calculate(stat="diff in means",
            order=c("Yes", "No"))
null_emails_NO <- header_no %>% 
  specify(unsubscribe_rate~repeated_campaign) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in means",
            order=c("Yes", "No"))
visualize(null_emails_NO) +
  shade_p_value(obs_emails_NO, direction="left")
get_p_value(null_emails_NO, obs_emails_NO, direction="left")
# P-Value: 0.087

# Conclusion:
# For both emails that have "Claire's Place Foundation" as a header and those who don't,
# their P-Values is larger than 0.05.
# This means that claire_header is NOT a confounding factor.


###
# Explore length of campaign
typeof(emails$`Start Date`) # Start Date is stored as a double
# Create new variable that calculates the number of days between start and end date
# Code adapted from: https://www.stat.berkeley.edu/~s133/dates.html
emails <- emails %>% 
  mutate(duration = difftime(`End Date`, `Start Date`, units='days'))


###
# What is the relationship between newsletter campaign duration and opens to sends rate?
emails %>% 
  ggplot(aes(x=duration, y=opens_sends)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
# Not a super strong relationship

# Build an interactive model of Unsubscribe Rate over Duration of Campaign and Claire Header
lm_unsub <- lm(opens_sends~duration, emails)
summary(lm_unsub)
# Model Equation: Open Sends Rate = 0.322 - 0.001 * Duration
# R^2: 0.010
# Adjusted R^2: -0.006
# P-Value: 0.428

# Plotting regression points
lm_points <- get_regression_points(lm_unsub)
lm_points <- lm_points %>%
  mutate(stdres=rstandard(lm_unsub))
lm_points %>% 
  ggplot(aes(x=opens_sends_hat, y=stdres)) +
  geom_point() +
  scale_y_continuous(breaks=seq(-3, 3, by=1))
lm_points %>% 
  ggplot(aes(sample=stdres)) +
  geom_qq(color="blue") +
  geom_qq_line(color="red")
# Since the residual points are a little curved towards the end,
# the data is not normally distributed.

# Therefore, this model is not great because the P-Value is not significant, 
# the adjusted R^2 value is very low, and the data is not normally distributed



########## Donors Data ##########
# Dataset: SP23_Final_Claires_Donations

###
# Data Cleaning

# Plot donations over time
SP23_Final_Claires_Donations %>% 
  ggplot(aes(x=Date, y=Amount)) +
  geom_point() +
  ggtitle("Donation Amount Over Time: Finding Outliers")
# From this graph, we can see that there are two outliers where the donation
# amount is higher than 20,000.
# Therefore, we choose to remove these outliers from our dataset.
donations <- SP23_Final_Claires_Donations %>% 
  filter(Amount<20000)

# Filter by complete status to remove donations that have not been processed.
donations <- donations %>% 
  filter(Status=="Complete")


###
# Which email campaigns drive the most number of donations?
# Which email campaigns drive the most amount of donations?
donation_emails <- donations %>% 
  group_by(`Email Campaign`) %>% 
  filter(all(!is.na(`Email Campaign`))) %>% 
  summarize(count=n(),
            amount_sum=sum(Amount),
            mean_date=mean(Date)) %>% 
  arrange(desc(amount_sum))
donation_emails
# Plot email campaigns and number of donations
donation_emails %>% 
  ggplot(aes(x=count, y=reorder(`Email Campaign`, count))) +
  geom_bar(stat="identity", fill="purple") +
  labs(x="Number of Donations", y="Email Campaign") +
  ggtitle("Email Campaign Donation Number")
# Plot email campaigns and amount of donations
donation_emails %>% 
  ggplot(aes(x=amount_sum, y=reorder(`Email Campaign`, amount_sum))) +
  geom_bar(stat="identity", fill="purple") +
  labs(x="Amount of Donations", y="Email Campaign") +
  ggtitle("Email Campaign Donation Amount")
# Findings:
# Campaigns with high donation number and amounts are:
# - GivingTuesday 2021 2
# - CPF + Kendra Scott
# - We're Glowing 2



########## YouTube Channel Data ########## 
# Dataset: Claires_Table_data

# Change column names for simpler analysis
colnames(Claires_Table_data) <- c('viewer_age','viewer_gender','views_percent', 'watchtime_hours')


###
# What age range has the most views and watch times of Claire's YouTube Channel?
# Group by age range
claire_yt_age <- Claires_Table_data %>% 
  group_by(viewer_age) %>% 
  summarize(view_sum=sum(views_percent),
            watchtime_avg=mean(watchtime_hours))
# Visualize using bar plot
ggplot(claire_yt_age, aes(x=viewer_age, y=view_sum)) +
  geom_bar(stat="identity", fill="purple") +
  labs(x="Viewer Age", y="Total Viewing Time (%)") +
  ggtitle("Viewing Distribution by Age Range")
ggplot(claire_yt_age, aes(x=viewer_age, y=watchtime_avg)) +
  geom_bar(stat="identity", fill="purple") +
  labs(x="Viewer Age", y="Average Watch Time") +
  ggtitle("Average Watch Time by by Age Range")
# Findings:
# The 18-24 age range has the highest percentage of total viewing time and average watch time.


###
# What gender has the most views and watch times of Claire's YouTube Channel?
claire_yt_gender <- Claires_Table_data %>% 
  group_by(viewer_gender) %>% 
  summarize(view_sum=sum(views_percent),
            watchtime_avg=mean(watchtime_hours))
# Visualize viewing time by gender
ggplot(claire_yt_gender, aes(x=viewer_gender, y=view_sum)) +
  geom_bar(stat="identity", fill="purple") +
  labs(x="Viewer Gender", y="Total Viewing Time (%)") +
  ggtitle("Viewing Distribution by Gender")
# Visualize watch time by gender
ggplot(claire_yt_gender, aes(x=viewer_gender, y=watchtime_avg)) +
  geom_bar(stat="identity", fill="purple") +
  labs(x="Viewer Gender", y="Average Watch Time") +
  ggtitle("Average Watch Time by Gender")
# Findings:
# Women have a higher viewing time and average watch time than men.


