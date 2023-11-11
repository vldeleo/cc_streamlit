library(arrow)
library(dplyr)
library(reticulate)
library(ggplot2)
library(maps)
library(lubridate)

cc_data <- read_parquet("C:/Users/torie/Documents/Python Scripts/Streamlit/credit_card_data_da.parquet")

colSums(is.na(cc_data))

  # lots of NAs in merchant state, Zip, Errors, apartment (but that's fine!), rolling_* stats (probably because of transaction count)

#weirdly, the number of users does not match the number of persons in the dataset
cc_data[which(level(cc_data$User) != level(cc_data$Person)),]
  # Hazel Robinson is one


test <- cc_data %>% group_by(Person, User) %>% 
  summarise(numUsers = n()) 
test$Person[which(duplicated(test$Person))]
# Casey El-Mafouk, Hazel Robinson, Lochlan Morris, Magdalena Farhad, Rory Nelson
dupUsers <- c("Casey El-Mafouk", "Hazel Robinson", "Lochlan Morris", "Magdalena Farhad", "Rory Nelson")

nrow(cc_data[cc_data$User %in% dupUsers & cc_data$`Is Fraud?` == "Yes",])
# none of these duplicate users have had fraudulent transactions...



# fill in merchant state/zip?
cc_data$GeoDiff <- cc_data$Zip != cc_data$Zipcode


# how much money is lost to fraud?
sum(cc_data$Amount[cc_data$`Is Fraud?` == "Yes"])
sum(cc_data$`Is Fraud?` == "Yes") #8412 (so not very many, just .122% of transactions are fraud)
# in order to get enough to try classifying, we need probably at least 20k



set.seed(3110)
subset <- cc_data[sample(1:nrow(cc_data), 20000),] # a small set to play with

# do we have enough positive fraud cases?
sum(subset$`Is Fraud?` == "Yes") # only 11? crying


plot(subset$Longitude, subset$Latitude, col = alpha("black",0.01), pch = 20, xlab = "", ylab = "")
points(subset$Longitude[subset$`Is Fraud?`=="Yes"], subset$Latitude[subset$`Is Fraud?`=="Yes"], col = "red", lwd=3)
map(add =T)


#subset %>% group_by(date) %>%
  #mutate(percFraud = )



# okay, now for clustering!
# Select a clustering type: partitional / hierarchical / hybrid, 
  # specify additional required initialization parameters
# this paper:
  # lists sources that recommend density-based clustering or k-means (or binned scatterplot visualization?) or stream clustering






