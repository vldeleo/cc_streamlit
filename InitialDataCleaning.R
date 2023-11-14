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
subset <- cc_data[sample(1:nrow(cc_data), 200000),] # a small set to play with
subset <- subset %>% filter(!is.na(GeoDiff))

# do we have enough positive fraud cases?
sum(subset$`Is Fraud?` == "Yes") # should be enough, a large dataset

# logistic regression?

subset <- subset  %>%
  mutate(FraudBin = ifelse(`Is Fraud?` == "No",0,1))
subset$`Errors?`[is.na(subset$`Errors?`)] <- "No"

log.mod <- glm(FraudBin ~ `Use Chip` + Amount + `Errors?` + GeoDiff + `Yearly Income - Person`, family = 'binomial', data = subset) # gives error, won't converge

# getting fancy with elastic net
library(glmnet)
library(caret)

# train elasticnet model (because un-penalized logistic regression will fail)
train_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              search = "random",
                              verboseIter = TRUE)

elastic_net_model2 <- train(`Is Fraud?` ~ `Use Chip` + Amount + `Errors?` + GeoDiff + `Yearly Income - Person`,
                           data = subset,
                           method = "glmnet",
                           preProcess = c("center", "scale"),
                           tuneLength = 25,
                           trControl = train_control)
# gives an error because only binary outcomes, also the output does not predict any variation in y values??? so I switched to a classifier instead of a probabilistic model

#classifier gave alpha of 0.837 and lambda of 1.85

# Check multiple R-squared
y_hat_enet2 <- predict(elastic_net_model2, subset)
rsq_enet <- cor(subset$FraudBin, y_hat_enet)^2


# ridge instead?
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
ridge_cv <- cv.glmnet(subset[,-c("FraudBin", "Is Fraud?")], FraudBin, alpha = 0, lambda = lambdas_to_try, standardize = TRUE, nfolds = 10)


#ordinarily, I'd use the model output
  #subset$probFraud <- y_hat_enet
#instead, I'm going to fake it
subset$probFraud <- subset$FraudBin + rnorm(nrow(subset), 0.1, sd=0.1)

write_parquet(subset, "cc_data_subset.parquet")



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


library('anomalize')
library('tibble')

# let's get rid of non-variable and factor columns?
  # dropping merchant name, merchant city (state == 11)(mcc = 13)(errors = 14)(33,34: customer geography)


customerSeg <- subset[,c(1,24:27, 29, 32:34,38:44,53:55,60, 61, 62,63, 65, 59 ,15,68)]

#subset2 <- subset[,c(1,7,8,11,12, 14,19,25,26,27,29,33,34,37, 38, 39, 40, 41, 42, 43, 44, 47,51,53,55,59,63,65,15,68,66)] # datetime broken down: ,2:6, 

# convert logical columns to logical, but don't do this for viz
subset2$`Use Chip` <- ifelse(subset2$`Use Chip` == "Chip Transaction", 1, 0) 
subset2$`Errors?` <- ifelse(is.na(subset2$`Errors?`), 0, 1 )
subset2$Gender <- ifelse(subset2$Gender == "Male", 0, 1)
subset2$`Card Type` <- ifelse(subset2$`Card Type` == "Credit", 0, 1)
subset2$`Has Chip` <- ifelse(subset2$`Has Chip` == "Yes", 1, 0)


#
subset2 %>% ggplot(aes(x = Gender, y = probFraud)) +
  geom_boxplot() +
  geom_jitter(height = 0, width = 0.2, alpha = 0.2) +
  geom_jitter(aes(x = Gender, y = FraudBin), col = "red", height = 0)


# trans PCA
transAnomalies <- subset[,c(2,6,7,8,14,19,20,21, 22,47,49,51, 65,15, 68)]

transAnomalies$`Use Chip` <- ifelse(transAnomalies$`Use Chip` == "Chip Transaction", 1, 0) 
transAnomalies$`Errors?` <- ifelse(transAnomalies$`Errors?` == "No", 0, 1 )
transAnomalies$`Card Type` <- ifelse(transAnomalies$`Card Type` == "Credit", 0, 1)
transAnomalies$`Has Chip` <- ifelse(transAnomalies$`Has Chip` == "YES", 1, 0)
transAnomalies$Expires <- as_date(transAnomalies$Expires, format = "%m/%Y")
transAnomalies$time_of_day <- as.numeric(as.factor(transAnomalies$time_of_day))
transAnomalies$DatestoExp <- as.numeric(transAnomalies$Expires - as_date(trunc(transAnomalies$datetime, "months")))
transAnomalies$GeoDiff <- as.numeric(transAnomalies$GeoDiff)

trans.pca <- princomp(transAnomalies[complete.cases(transAnomalies),c(1,3:13)])

trans.pca <- prcomp(transAnomalies[complete.cases(transAnomalies),c(1,3:5,7:10,12,13,16)], scale=TRUE)

PC <- as.data.frame(trans.pca$x)
PC$`Fraud?` <- as.vector(transAnomalies[, 'Is Fraud?'])

plot(PC$PC1, PC$PC2, col = as.factor(transAnomalies$`Is Fraud?`))


PC %>% ggplot(aes(PC1, PC2), col = `Fraud?`) +
  geom_point()

library(factoextra)
fviz_pca_biplot(trans.pca, 
                repel = TRUE,
                col.var = "orchid",
                title = "Biplot", geom="point") 

# last plot: elipses by demographic?


  



# create tibble
cc_tibble <- subset[,c(1:14,16:63,65) ] %>% 
  rownames_to_column() %>% 
  as_tibble() #%>% 
  #mutate(date = as.Date(rowname)) %>% 
  #select(-one_of('rowname'))

output <- cc_tibble %>% anomalize(method = "iqr", alpha = 0.1, max_anoms=0.2)



