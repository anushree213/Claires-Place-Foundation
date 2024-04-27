library(tidyverse)
library(moderndive)
library(gapminder)
library(infer)
library(Hmisc)
library(reshape2)
library(dplyr)


write.csv(Patients, "Patients_Data.csv")
getwd()

######### Patients ##############

# 1. Based on the EHSG (Extended Hospital Stay Grant) data provided, can you investigate the impact of our most recent policy change beginning in June 2023?

# Extended Hospital Stay Fund: The fund provides grants to families with children that are experiencing a hospital stay of at least 14 consecutive days. Extended stays are a financial stress and often the children are in a city far from home. The grants cover essential expenses such as rent, mortgage and groceries.

# Policy: EHSG Grant policy: Used to give grants based on applicant request. Usually over 90% of the requested amount covered rent, utilities, car payments, and mortgages. However, this policy limited the number of patients who could receive grants.
# Since June 1, 2023: the approved grant will only cover one month of rent or mortgage.

# Reasons:
# 1. Rent or mortgage requests have consistently been the most common among patient applications.
# 2. These expenses are deemed more critical than others, such as utilities and car payments, in supporting patients' ability to maintain their normal lives.
# 3. Providing support for one month is often sufficient time for a family to stabilize their financial situation.
# 4. By focusing the grants on specific needs per application, the organization can extend its reach to a larger number of patients in immediate need of assistance.

######### Exploration patients ##############

Patients <- X_Claires_Patients_FA23

unique(Patients$State)
unique(Patients$Date)
unique(Patients$Amt_granted)
ls(Patients)
summary(Patients)
sapply(Patients, class)
describe(Patients)
describe(before_policy)
describe(after_policy)

# Date
ggplot(Patients, aes(Date))+
  geom_histogram()
# Big dip in the beginning of 2023

# Amount Granted
ggplot(Patients, aes(Amt_granted))+
  geom_boxplot()
# Beyond 4000 may be outliers

# Decided not to filter the data as:
# No major outliers
# No missing values (No NA)

######### New Variables and other changes ##########

# Date
# convert character to date format
Patients$Date <- as.Date(Patients$Date, format="%m/%d/%Y")

# separating year, month, day
Patients <- Patients %>%
  mutate(Year = as.numeric(substring(Date, 1, 4)),
         Month = as.numeric(substring(Date, 6, 7)),
         Day = as.numeric(substring(Date, 9, 10)))


# Creating new variable for before and after policy
Patients <- Patients %>%
  mutate(PolicyCat =ifelse(Date < as.Date("06/01/2023", "%m/%d/%Y"), "Before", "After"))


# Creating new variable for whether policy was accepted or rejected
Patients <- Patients %>%
  mutate(GrantCat =ifelse(Amt_granted == 0.00, "Rejected", "Accepted"))

# mutating column to represent seasons
Patients <- Patients %>% 
  mutate(Season=ifelse(Month %in% c(11, 12, 1), "Winter", 
                       ifelse(Month %in% c(2, 3, 4), "Spring", 
                              ifelse(Month %in% c(5, 6, 7), "Summer",
                                     ifelse(Month %in% c(8, 9, 10), "Fall", NA)))))

# State
# To combine OH and Oh
Patients <- Patients %>%
  mutate(State = toupper(State))

# filter states by region
Patients <- Patients %>% 
  mutate(Region=ifelse(State %in% c("MA", "NH", "NJ", "PA", "CT", "NY", "VT", "ME"), "East", 
                       ifelse(State %in% c("WA", "CA", "CO", "NV", "MT", "OR", "AZ", "ID", "WY", "HI"), "West", 
                              ifelse(State %in% c("IL", "OH", "MI", "NE", "IN", "MO", "WI", "MN", "IA"), "Midwest",
                                     ifelse(State %in% c("TX", "NC", "MD", "VA", "AL", "FL", "GA", "SC", "TN", "OK", "LA", "KY"), "South", NA)))))

# Decided region based on the US Government division.
# https://www.businessinsider.com/united-states-regions-new-england-midwest-south-2018-4#the-west-11

######### New Data Sets ###############

# Filter EHSG data before and after June 2023
before_policy <- filter(Patients, Date < as.Date("06/01/2023", "%m/%d/%Y"))
after_policy <- filter(Patients, Date >= as.Date("06/01/2023", "%m/%d/%Y"))

# filter by time of the year
# Fall: Filtering out fall since only the fall season has completely passed by since the implementation of the policy
# This helps in comparing with previous years as the limited time after policy made the data hard to compare with before policy
Fall_Patients <- filter(Patients, Season == "Fall")

######### Heat Map #####

patients.numeric <- select_if(Patients, is.numeric)

patients.cor <- round(cor(patients.numeric),2)
patients.cor

patients.cor.melt <- melt(patients.cor)

patients.cor.melt %>% 
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="red", high="light blue", mid="white", midpoint=0)+
  geom_text(aes(x=Var1, y=Var2, label=value), size=3)+
  theme(axis.text.x=element_text(size=8, angle=30, hjust=0.8))

# Very small correlations between the numeric variables
# Seems like amount granted decreases by year and by month


######### Exploration Continued #######

# Calculate total average grant amounts before and after the policy change
mean(before_policy$Amt_granted)
# 1631.692
mean(after_policy$Amt_granted)
# 1373.597

Patients %>% 
  group_by(PolicyCat, GrantCat) %>% 
  tally()

# Before
12/(262+12)
# 4% of applications were rejected out of the total applications before policy

# After
6/(6+12)
# 33% were rejected out of the total applications after polciy


Patients %>%
  ggplot(aes(x = Region, fill = PolicyCat)) +
  geom_bar(position = position_dodge(preserve = "single"))

# both before and after the policy, most applications have come from the south
# Midwest and the west are slightly lower than the east after the policy, while before the policy the south was followed by the west, then the midwest, then east.


Patients %>%
  ggplot(aes(x = Region, fill = GrantCat)) +
  geom_bar(position = position_dodge(preserve = "single")) +
  facet_wrap(~PolicyCat)

Fall_Patients %>%
  ggplot(aes(x = Region, fill = GrantCat)) +
  geom_bar(position = position_dodge(preserve = "single")) +
  facet_wrap(~PolicyCat)

# before the policy there were not a lot of rejections while it seems like the number acceptances and rejections are similar after the policy
# also the east is receiving more acceptances after the policy. The midwest and west seem to have experienced a decrease in the proportion of acceptances.


Patients %>%
  ggplot(aes(x = Region, fill = Season)) +
  geom_bar(position = position_dodge(preserve = "single")) +
  facet_wrap(~PolicyCat)

# Hard to compare as not all seasons have passed since the policy was implemented
# Looking at data for just the fall

Fall_Patients %>%
  ggplot(aes(x = Region, fill = Region)) +
  geom_bar(position = position_dodge(preserve = "single")) +
  facet_wrap(~PolicyCat)

# Midwest experienced a decline this fall in number of applications
# The East did better than the midwest which is generally not the case

######### Hypothesis test for change in amount granted ##############

# H0: same amount granted before and after policy
# H1: amount granted after policy > amount granted before policy 

obs_amt <- Patients %>%
  specify(Amt_granted~PolicyCat) %>%
  calculate(stat="diff in means",
            order=c("After", "Before"))


null_amt <- Patients %>%
  specify(Amt_granted~PolicyCat) %>%
  hypothesize(null="independence") %>%
  generate(reps=1000, type = "permute") %>%
  calculate(stat="diff in means",
            order=c("After", "Before"))

visualise(null_amt) +
  shade_p_value(obs_amt, direction = "right")

null_amt %>% 
  get_p_value(obs_amt, direction = "right")

# 0.939
# fail to reject null hypothesis.

# H0: same amount granted before and after policy
# H1: amount granted after policy < amount granted before policy

visualise(null_amt) +
  shade_p_value(obs_amt, direction = "left")

null_amt %>% 
  get_p_value(obs_amt, direction = "left")

# 0.061
# fail to reject null hypothesis

# checking to see if limiting data to the fall shows any significant findings

# H0: same amount granted before and after policy in the fall
# H1: amount granted after policy > amount granted before policy in the fall

obs_amt_fall <- Fall_Patients %>%
  specify(Amt_granted~PolicyCat) %>%
  calculate(stat="diff in means",
            order=c("After", "Before"))

null_amt_fall <- Fall_Patients %>%
  specify(Amt_granted~PolicyCat) %>%
  hypothesize(null="independence") %>%
  generate(reps=1000, type = "permute") %>%
  calculate(stat="diff in means",
            order=c("After", "Before"))

visualise(null_amt_fall) +
  shade_p_value(obs_amt_fall, direction = "right")

null_amt_fall %>% 
  get_p_value(obs_amt_fall, direction = "right")

# 0.834
# fail to reject null hypothesis

# H0: same amount granted before and after policy in the fall
# H1: amount granted after policy < amount granted before policy in the fall

visualise(null_amt_fall) +
  shade_p_value(obs_amt_fall, direction = "left")

null_amt_fall %>% 
  get_p_value(obs_amt_fall, direction = "left")

# 0.166
# fail to reject null hypothesis

######### Hypothesis test for proportion of acceptances vs rejections ############

# H0: same proportion of people granted before and after policy
# H1: proportion of acceptances after policy > proportion of acceptances before policy

# observed
obs_grant <- Patients %>% 
  specify(GrantCat~PolicyCat, success = "Accepted") %>% 
  calculate(stat="diff in props",
            order=c("After", "Before"))

# null
null_grant <- Patients %>% 
  specify(GrantCat~PolicyCat, success = "Accepted") %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in props",
            order=c("After", "Before"))

# p value
null_grant %>% 
  get_p_value(obs_grant, direction="right")

# 0.997
# fail to reject null hypothesis

# H0: same proportion of people granted before and after policy
# H1: proportion of acceptances after policy < proportion of acceptances before policy

# p value
null_grant %>% 
  get_p_value(obs_grant, direction="left")

# 0.016
# reject null hypothesis
# proportion of acceptances after policy < proportion of acceptances before policy

# While the first test shows no significant increase in the proportion of acceptances after the policy, the second test suggests a significant decrease. This indicates that the policy change may have had a negative impact on the proportion of acceptances.
# This is not in line with what the organization wanted as it wanted to be able to serve more patients after the policy

# visualization to highlight increase in rejections

proportion_data <- Patients %>%
  group_by(PolicyCat) %>%
  summarise(Count = n(),
            Accepted = sum(GrantCat == "Rejected")) %>%
  mutate(Proportion = Accepted / Count)


ggplot(proportion_data, aes(x = PolicyCat, y = Proportion, fill = PolicyCat)) +
  geom_bar(stat = "identity") +
  labs(title = "Grant Rejections Before And After Policy",
       x = "Policy Category",
       y = "Proportion of Rejections", fill = "Policy") +
  geom_text(aes(label = round(Proportion, 3)), vjust = -0.5) +
  scale_fill_manual(values = c("Before" = "darkorchid4", "After" = "red3"))  +
  theme(panel.background = element_rect(fill = "lavender"))

########### amount granted by region before and after policy #######

Patients %>%
  ggplot(aes(x = Region, y = Amt_granted, fill = Region)) +
  geom_bar(stat = "identity") +
  facet_wrap(~PolicyCat) +
  labs(title = "Amount Granted per Region Before and After Policy", y = "Amount Granted", fill = "Region")  +
  scale_fill_manual(values = c("South" = "darkorchid4", "West" = "red3", "Midwest" = "white", "East" = "gold"))  +
  theme(panel.background = element_rect(fill = "lavender"))

Fall_Patients %>%
  ggplot(aes(x = Region, y = Amt_granted, fill = Region)) +
  geom_bar(stat = "identity") +
  facet_wrap(~PolicyCat) +
  labs(title = "Amount Granted per Region Before and After Policy in the Fall", y = "Amount Granted", fill = "Region") +
  scale_fill_manual(values = c("South" = "darkorchid4", "West" = "red3", "Midwest" = "white", "East" = "gold"))  +
  theme(panel.background = element_rect(fill = "lavender"))

########### Average grant amount by region before and after policy ########

Patients_avg <- Patients %>%
  group_by(Region, PolicyCat) %>%
  summarise(Avg_Amt_granted = mean(Amt_granted, na.rm = TRUE))

Patients_avg %>%
  ggplot(aes(x = Region, y = Avg_Amt_granted, fill = Region)) +
  geom_bar(stat = "identity") +
  facet_wrap(~PolicyCat) +
  labs(title = " Average Amount Granted per Region Before and After Policy", y = " Average Amount Granted", fill = "Region") +
  scale_fill_manual(values = c("South" = "darkorchid4", "West" = "red3", "Midwest" = "white", "East" = "gold"))  +
  theme(panel.background = element_rect(fill = "lavender"))


# comparing with just the fall

Fall_Patients_avg <- Fall_Patients %>%
  group_by(Region, PolicyCat) %>%
  summarise(Avg_Amt_granted_fall = mean(Amt_granted, na.rm = TRUE))

Fall_Patients_avg %>%
  ggplot(aes(x = Region, y = Avg_Amt_granted_fall, fill = Region)) +
  geom_bar(stat = "identity") +
  facet_wrap(~PolicyCat) +
  labs(title = " Average Amount Granted per Region Before and After Policy in the Fall", y = " Average Amount Granted", fill = "Region") +
  scale_fill_manual(values = c("South" = "darkorchid4", "West" = "red3", "Midwest" = "white", "East" = "gold"))  +
  theme(panel.background = element_rect(fill = "lavender"))

# Average amount granted in the East and West is higher in the general and in the fall.
# Also, average amount granted for the east and west after and before the policy is higher than the south but the total amount granted for the south is higher both before and after. This suggests that the south receives many donations of smaller amounts while the West and east receive fewer donations of big amounts
# Decline in average amount for the Midwest


# Hypothesis test

# H0: same average amount granted before and after policy
# H1: average amount granted after policy > average amount granted before policy 

obs_avg_amt <- Patients_avg %>%
  specify(Avg_Amt_granted~PolicyCat) %>%
  calculate(stat="diff in means",
            order=c("After", "Before"))


null_avg_amt <- Patients_avg %>%
  specify(Avg_Amt_granted~PolicyCat) %>%
  hypothesize(null="independence") %>%
  generate(reps=1000, type = "permute") %>%
  calculate(stat="diff in means",
            order=c("After", "Before"))


visualise(null_avg_amt) +
  shade_p_value(obs_avg_amt, direction = "right")

null_avg_amt %>% 
  get_p_value(obs_avg_amt, direction = "right")

# 0.787
# fail to reject null hypothesis.

# H0: same average amount granted before and after policy
# H1: average amount granted after policy < average amount granted before policy 
null_avg_amt %>% 
  get_p_value(obs_avg_amt, direction = "left")

# 0.225
# fail to reject null hypothesis


# T-test

west_grants <- Fall_Patients[Fall_Patients$Region == "West", "Amt_granted"]

south_grants <- Fall_Patients[Fall_Patients$Region == "South", "Amt_granted"]

t_test_result <- t.test(west_grants, south_grants)

# Viewing the results
print(t_test_result)

########### Next Steps #########

# Additional data to track:
# Reason for rejection of grant application
# Commonalities between rejected applicants
# Additional follow-up questionnaires on to what extent did covering the rent or mortgage alone help the patients 

# Next step:
# More channels of advertising for this grant especially to households where kids suffer from CF 
# Increase advertising efforts in the midwest to encourage more applications from the region

########### WPP ###################

# For our updated Work Proudly Program, can you please provide suggestions on data to collect and potential metrics to apply in order to evaluate the program’s impact?

# Work Proudly Program: The foundation’s newest program provides job training and equipment needed for work-from-home employment to adults with CF and caregivers.

WPP$Date <- as.Date(WPP$Date, format="%m/%d/%y")

# separating year, month, day
WPP <- WPP %>%
  mutate(Year = as.numeric(substring(Date, 1, 4)),
         Month = as.numeric(substring(Date, 6, 7)),
         Day = as.numeric(substring(Date, 9, 10)))

WPP_filter <- WPP %>%
  filter(Year>2019)
# Since invites were sent out in the year 2020.

WPP_filter <- WPP_filter %>% 
  mutate(policychange=ifelse(Date>as.Date("2023-09-19"), "after", "before"))

WPP_filter <- WPP_filter[!is.na(WPP_filter$Employed) & WPP_filter$Employed != "", ]


WPP %>%
  group_by(Status, Gender) %>%
  summarize(count=n())
# There are a lot of NA values in the gender data
# Only females have withdrawn from the program

WPP %>%
  group_by(Gender, Employed) %>%
  summarize(count=n())
#In general, more females enrolled and got employed in the program.
# There are a lot of NA values in the gender data

#bar graph for Social Service of applicants
WPP_filter %>%
  ggplot(aes(x=SocialServices, fill = policychange)) +
  geom_bar(position="dodge") +
  ggtitle("distribution of applicants by Social Service") +
  theme(axis.text.x=element_text(size=8, angle=20, hjust=0.8)) +
  scale_fill_manual(values = c("before" = "red3", "after" = "darkorchid4"))
# only food stamps and SSI, SSDI after the policy change.



########### Donations ###############

# Recent trends in the philanthropy sector show a national decline. Given this context, can you analyze our organization's donation data to determine if we are experiencing a similar trend?

unique(FA23_Final_Claires_Donations$Region)

unique(FA23_Final_Claires_Donations$`State/Province`)

# Summary statistics
summary(FA23_Final_Claires_Donations)

# Boxplot for Amount column

# Creating a boxplot using ggplot
ggplot(FA23_Final_Claires_Donations, aes(y = Amount)) +
  geom_boxplot() +
  labs(title = "Donation Amounts Boxplot", y = "Donation Amount")


# Convert character vector to dates
FA23_Final_Claires_Donations$Date <- as.Date(FA23_Final_Claires_Donations$Date, format="%m/%d/%y")

# Extract month from Date column
FA23_Final_Claires_Donations <- FA23_Final_Claires_Donations %>%
  mutate(Month = format(Date, "%Y-%m"))


Donation_US <- filter(FA23_Final_Claires_Donations, Region=="US")

# Summarize data by month
monthly_summary <- Donation_US %>%
  group_by(Month) %>%
  summarise(Total_Donation = sum(Amount))

# Create a bar chart with lines
ggplot(monthly_summary, aes(x = Month, y = Total_Donation)) +
  geom_col(fill = "skyblue") +
  geom_line(group = 1, color = "blue") +
  labs(title = "Donation Trends Over Time (by Month)", x = "Month", y = "Total Donation Amount") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#   Are there specific campaigns that significantly impact donation amounts?
# Bar plot showing donation amounts by campaign
ggplot(Donation_US, aes(x = Form, y = Amount)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Donation Amounts by Campaign", x = "Campaign", y = "Total Donation Amount")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

#   How do donation amounts vary across different states/regions?
# Bar plot or choropleth map showing donation amounts by state/province
ggplot(Donation_US, aes(x = `State/Province`, y = Amount)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Donation Amounts by State/Province", x = "State/Province", y = "Total Donation Amount")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# What are the overall donation trends over time?
# Use 'Date' as a time series variable and 'Amount' as numeric

# Q-Q plot for normality check
qqnorm(Donation_US$Amount)
qqline(Donation_US$Amount)

# Fit linear regression model
lm_model <- lm(Amount ~ Date, data = Donation_US)

get_regression_table(lm_model)
summary(lm_model)

# Assuming 'Date' and 'Amount' columns are correctly formatted

# Fit linear regression model with 'Date' as predictor
lm_model <- lm(Amount ~ as.numeric(Date), data = Donation_US)

# View summary of the regression model
summary(lm_model)

# Statistical Significance:
#   Both coefficients are statistically significant (indicated by the low p-values), suggesting that both the intercept and the effect of 'Date' on donation amounts are unlikely to be due to random chance alone.
# R-squared:
#   R-squared (0.00081): This value represents the proportion of variance in donation amounts explained by the 'Date' variable. In this case, it's a very small proportion, indicating that the linear model with 'Date' as a predictor explains only a minimal amount of variation in donations.

# Interpretation:
# The linear regression model aimed to understand the relationship between time (Date) and donation amounts. However, the model's practical significance might be limited due to the very small effect size.

# Statistical Significance: The model and its coefficients were statistically significant, suggesting that the relationship between 'Date' and donations is unlikely to be due to chance.
# Low Practical Significance: The model explains only a very small proportion of the variance in donation amounts, indicating that 'Date' alone might not be an effective predictor of donations.

# Additional Data:
# Campaign-specific Data: Gather detailed information on campaign specifics, donor engagement levels, and marketing strategies for each fundraising event.
# Donor Behavior Data: Collect data on donor demographics, preferences, and past interactions to understand donor behavior and tailor fundraising efforts accordingly.
# External Factors: Capture external influences such as economic conditions, social events, or policy changes that might affect donation patterns.
