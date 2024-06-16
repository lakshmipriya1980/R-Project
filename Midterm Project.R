
# Loading the Datasets
abd <- read.csv("Abandoned.csv")
rs <- read.csv("Reservation.csv")



# 1. It makes sense to retarget customers who did not purchase a vacation package at first since it makes use of previous interactions and data insights, which provide several benefits. It lowers acquisition costs, boosts conversion rates, gives personalized information, and gives a chance to reconnect with potential customers. Retargeting can be more successful and economical if it considers the objections and preferences of the consumer. In the end, it maximizes the return on investment in marketing initiatives by maintaining a competitive edge, boosting brand recognition, and taking advantage of timing.



# 2.  Descriptive Statistics
summary(abd)
table(abd$Test_Control)



3.

# The below statement shows the count of 'test' by different state
abd[abd==""] <- NA
states <- abd[complete.cases(abd['Address']),]
table(states$Test_Control)

# Filter the data to include only rows with 'test' in the 'Test_Control' column
filtered_abd <- abd %>% filter(Test_Control == "test")

# Group the filtered data by 'Address' and 'Test_Control', and compute summary statistics (overall dataset)
summary_stats <- filtered_abd %>%
  group_by(Address, Test_Control) %>%
  summarize(
    Count = n(),  # Count of observations
  )

# Print the summary statistics
print(summary_stats)


4.

# Merge the datasets based on the Caller_ID
matched_customers <- merge(abd, rs, by = "Caller_ID", all = FALSE)
# The "all = FALSE" option ensures that only common Caller_IDs are included in the merged dataset.
# Print the result
print(matched_customers)

# Match based on the "Email" field in both datasets
match_email <- abd$Email[complete.cases(abd$Email)] %in% rs$Email[complete.cases(rs$Email)]
table(match_email)

# Matching based on 'Incoming_Phone'
match_incoming_phone <- abd$Incoming_Phone[complete.cases(abd$Incoming_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)]
table(match_incoming_phone)

# Matching based on 'Contact_Phone'
match_contact_phone <- abd$Contact_Phone[complete.cases(abd$Contact_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)]
table(match_contact_phone)

# Merging datasets based on Email
merged_data <- merge(abd, rs, by = "Email", all.x = TRUE, all.y = TRUE)




#5.1) Treatment group who purchased.

merged_data <- merge(abd, rs, by = "Email", all.x = TRUE, all.y = TRUE)

test_purchased <- subset(merged_data, Test_Control.x == "test" & !is.na(Email))
head(test_purchased)



#5.2) Treatment group who didn’t purchase.

test_not_purchased <- subset(merged_data, Test_Control.x == "test" & is.na(Email))
head(test_not_purchased)



#5.3) Control group who purchased.

control_purchased <- subset(merged_data, Test_Control.x == "control" & !is.na(Email))

head(control_purchased)


#5.4) Control group who didn’t purchase.

control_not_purchased <- subset(merged_data, Test_Control.x == "control" & is.na(Email))
# Displaying the result
head(control_not_purchased)



6)

library(dplyr)
# Create a new column "Flag" based on "Email," "Incoming_Phone," and "Contact_Phone" values
merged_data <-  merged_data %>%
  mutate(Flag = ifelse(!is.na(Email) & !is.na(Incoming_Phone.x) & !is.na(Contact_Phone.x), "Purchased", "Not_Purchased"))
# Print the data frame
print( merged_data)



7)

#Cross tabulation for the people in abd dataset
cross_tab <- table(merged_data$Test_Control.x, merged_data$Flag)

# Printing the cross-tabulation
print(cross_tab)

#Cross tabulation for the people in rs dataset
cross_tab <- table(merged_data$Test_Control.y, merged_data$Flag)

# Printing the cross-tabulation
print(cross_tab)




8)

set.seed(12)  # Set a seed for reproducibility
selected_states <- sample(unique(merged_data$Address.x), 5)

# Create cross-tabulation for each selected state
for (Address in selected_states) {
  cat("State:", Address, "\n")
  cross_tab <- merged_data %>%
    filter(Address.x == Address | Address.y == Address) %>%
    count(Flag)
  print(cross_tab)
  cat("\n")
}


9)

cleaned <- data.frame(
  Customer_ID = ifelse(is.na(merged_data$Email), as.character(merged_data$Caller_ID.x), as.character(merged_data$Email)),
  Test_Group = as.character(merged_data$Test_Control.x),
  Outcome = ifelse(is.na(merged_data$Email), "No_Purchase", "Purchased"),
  State_Available = ifelse(is.na(merged_data$Address.x), "No", "Yes"),
  Email_Available = ifelse(is.na(merged_data$Email), "No", "Yes")
)

write.csv(cleaned, "P:\\cleaned_dataset.csv", row.names = FALSE)




10)

library(stargazer)
library(dplyr)

# Assuming Outcome is determined by whether Email is NA or not
merged_data$Outcome <- ifelse(is.na(merged_data$Email), 0, 1)

# Assuming Test Group is determined by the value in Test_Control.x column ("control" or "test")
merged_data$Test_Group_binary <- ifelse(merged_data$Test_Control.x == "control", 0, 1)



linear_model <- lm(Outcome ~ Test_Group_binary, data = merged_data)

# Displaying the summary of the regression model
summary(linear_model)


#11)

# One-way ANOVA
anova_result <- anova(lm(Outcome ~ Test_Group_binary, data = merged_data))

# Display the results
print(anova_result)


#•	These statistics might suggest a very strong statistical relationship, they don't imply a causal relationship. Since causation is a complicated topic, proper research design is needed. Randomized controlled trials or other techniques are frequently used to demonstrate cause and effect. Finding a significant statistical association in a real-world dataset does not indicate that you may draw conclusions about the retargeting campaign's causality.

#•	The relatively low adjusted R-squared reveals that the "Test_Group_binary" variable explains very little variance in the "Outcome," despite the regression model suggesting a statistically significant association between the test group and "Outcome." This implies that strong causal assertions regarding the effectiveness of the retargeting campaign may not be supported by the model.

#•	A deeper investigation that takes into consideration the experimental design, data over time, and other relevant factors is required in order to draw conclusions regarding the campaign's efficacy's causality. In order to confirm the causal association, more studies and tests must be carried out.


13)

model_x <- lm(Outcome ~ Test_Control.x * Address.x + Email, data = merged_data)

# Print the regression summary
summary(model_x)



14)

14.1) #The experiment could be improved by having a larger and more representative sample size.  I can think about including a few extra columns that offer more context and data in order to improve the analysis and model construction utilizing the provided dataset. These are some ideas for new demographic-related columns that may be added. Like the state, the size of the city, the region, and the time-related elements like the day of the week, the year, and the customer behavior. This indicates a chance to expand the campaign's target audience.

14.2) #With higher-quality data, other options could be considered to improve the retargeting campaign analysis. 

•	Better Data Collection: Gather more thorough and precise information about the interactions, preferences, and behavior of your customers. Monitoring website traffic, click-through rates, product views, and cart abandonment rates are a few examples of this.

•	Improved client Profiling: Compile extra demographic, psychographic, and prior purchase data to create more thorough client profiles. Segments for more specialized marketing may be created as a result.

•	Longitudinal Data: Compile information before and after the campaign over an extended period of time. This offers a more thorough understanding of how retargeting attempts affect consumer behavior.

•	Data Quality Assurance: Reduce missing or incorrect data by putting data quality assurance procedures into place. This includes confirming contact details.

14.3)

•	The examination of the data from the retargeting campaign has practical commercial ramifications. These ramifications have the potential to influence strategic choices and spur advancements in consumer interaction and marketing tactics. The following are some possible takeaways.

•	Targeting according to Segment:
  It is possible from the study that some consumer categories react better to retargeting campaigns. Marketing resources may be allocated more wisely to the categories that have the best conversion rates based on this data.

•	Optimizing Creative and Messaging:
  Through performance analysis of various creatives, ad formats, and message, companies may improve the effectiveness of their advertising material and make it more appealing to consumers. This might entail customizing the message in accordance with the tastes and actions of the audience.

•	Customer Retention: The analysis may indicate that certain customers who did not make an immediate purchase are more likely to become repeat buyers with a tailored follow-up strategy. This knowledge can inform customer retention efforts.

Mapping the Customer Journey:
  •	Businesses may discover critical decision points by having a thorough understanding of the client path and touchpoints prior to conversion. Having this knowledge can help with more accurate targeting throughout crucial parts of the trip.

Suggested Products:
  •	Businesses might suggest goods or services based on the analysis of what customers have viewed or left in their shopping carts. The experience of purchasing may be improved with personalized recommendations.





