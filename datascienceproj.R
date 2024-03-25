data <- read.csv("C://downloads//listings.csv//listings.csv")
library(stringr)
x <- list()

##SPLIT NAME COLUMN AND CREATE A NEW LIST TO USE 

#split name column into a list
x1<-strsplit(data$name,"·")
x1
#loop through all of the lists and makes a new list
for (i in 1:length(x1)){ 
x$name[[i]]<-x1[[i]][1]  
x$rating[[i]]<-x1[[i]][2]
x$bedroomAmt[[i]]<-x1[[i]][3]
x$bedAmt[[i]]<-x1[[i]][4]
x$bathAmt[[i]]<-x1[[i]][5]
}

##GET ALL VALUES THAT ARE IN DIFFERENT COLUMNS AND PUT THEM IN THE RIGHT SPOT ex. move bedAmt value from bedroomAmt to bedAmt

#all indices where ★ is not detected
naind <-which(!str_detect(x$rating,"★"))
#indices where bedroom or studio is not detected
naind2 <-which(!(str_detect(x$bedroomAmt,"room") | str_detect(x$bedroomAmt,"Studio")))
#indices where bath amount is detected in other columns
naind3 <- c(which((str_detect(x$bedAmt,"bath"))))


#replaces all values where bath is in another column
x$bathAmt[naind3] <- x$bedAmt[naind3]
#Move all indices where bed is detected from second column to bed column
x$bedAmt[naind2] <- x$bedroomAmt[naind2]
#Move all indices where bedroom is detected from first column to bedroom column
x$bedroomAmt[naind] <- x$rating[naind]

##GET ALL INDICES WHERE VALUES DO NOT BELONG ex. bath in bed

#indices where bedroom or studio is not detected
naind2 <-which(!(str_detect(x$bedroomAmt,"room") | str_detect(x$bedroomAmt,"Studio")))
#indices where bath amount is detected in other columns
naind4 <- c(which((str_detect(x$bedAmt,"bath"))))

#replace all indices where ★ is not detected with NA
x$rating[naind] <- NA
#replace all indices where bedroom or studio is not detected with NA
x$bedroomAmt[naind2] <- NA
#replace all indices where bed is not detected to 3rd beds column
x$bedAmt[which(!(str_detect(x$bedAmt,"bed")))] <- NA



#flatten all lists 
x$name <- unlist(x$name, recursive = FALSE)
x$rating <- unlist(x$rating, recursive = FALSE)
x$bedroomAmt <- unlist(x$bedroomAmt, recursive = FALSE)
x$bedAmt <- unlist(x$bedAmt, recursive = FALSE)
x$bathAmt <- unlist(x$bathAmt, recursive = FALSE)
df<-as.data.frame(x)
data$name<-df$name
data$rating<-df$rating
data$bedroomAmt<-df$bedroomAmt
data$bedAmt<-df$bedAmt
data$bathAmt<-df$bathAmt
str(data)

Data$price <- substr(cleanData$price,2,10)

head(Data$price)

Data$price <- as.numeric(Data$price)

Data$minimum_nights <- as.numeric(Data$minimum_nights)

Data$reviews_per_month <- as.numeric(Data$reviews_per_month)

Data$monthly_activity <- Data$price * Data$minimum_nights * Data$reviews_per_month

head(Data$monthly_activity)

Data$license[which(str_detect(Data$license,"OSE-STR"))] <- "OSE-STRREG"

Data$license[which(str_detect(Data$license,"Ose-str"))] <- "OSE-STRREG"

Data$license[which(str_detect(Data$license,"ose-str"))] <- "OSE-STRREG"

library(ggplot2)

ggplot(Data, aes(x=license, y=monthly_activity)) + geom_point()

length(Data$license[which(str_detect(Data$license,"OSE-STR"))])

length(Data$license[which(str_detect(Data$license,"Exempt"))])

sum(is.na(Data$license),na.rm=FALSE)

head(Data$monthly_activity)

res <- quantile(Data$monthly_activity, probs = c(0,0.25,0.5,0.75,1), na.rm=TRUE)

data$success <- 0
data$success[which(data$monthly_activity) >= 14974] <- 1 

newdf <- data.frame(as.factor(data$success),data$neighborhood,data$bedroomAmt,data$bathAmt, data$property_type, data$accommodates)
trainList <- createDataPartition(y=newdf$success,p=.6,list=FALSE)
trainSet <- newdf[trainList,]
testSet <- newdf[-trainList,]

model.ksvm.train <- train(trainSet$success ~ neighborhood,bedroomAmt,bathAmt,property_type,accommodates, data=trainSet,method="svmRadial",preProc=c("center","scale"))
predicted <- predict(model.ksvm.train,newdata=testSet,type="raw")
confusionMatrix(predicted,testSet$success)

# Your monthly listing counts

listings <- c(43241, 43303, 43566, 43729, 43582, 39453, 39160, 39627, 39719, 39202)

# Creating a data frame with month as a predictor

# Assuming these listings are for 10 consecutive months

AirbnbMonthlyListings <- data.frame(
  
  Month = 1:10, # Sequential months as a simple time predictor
  
  Listings = listings
  
)

# Fit a linear model to predict Listings based on Month

model <- lm(Listings ~ Month, data=AirbnbMonthlyListings)

# Summary of the model

summary(model)

# Load the ggplot2 package

library(ggplot2)

# Assuming you have the AirbnbMonthlyListings dataframe from the previous example

#AirbnbMonthlyListings <- data.frame(Month = 1:10, Listings = c(43241, 43303, 43566, 43729, 43582, 39453, 39160, 39627, 39719, 39202))

# Fit a linear model (if not already done)

model <- lm(Listings ~ Month, data=AirbnbMonthlyListings)

# Add a predictions column to the dataframe

AirbnbMonthlyListings$Predictions <- predict(model)

# Use ggplot to plot the data and the model

ggplot(AirbnbMonthlyListings, aes(x = Month)) +
  
  geom_point(aes(y = Listings), color = 'blue') + # Actual data points
  
  geom_line(aes(y = Predictions), color = 'red') + # Linear model predictions
  
  labs(title = "Airbnb Monthly Listings and Linear Model Prediction",
       
       x = "Month", y = "Listings") +
  
  theme_minimal()

library(ggplot2)

# Original data

months <- 1:10

listings <- c(43241, 43303, 43566, 43729, 43582, 39453, 39160, 39627, 39719, 39202)

data <- data.frame(Month = months, Listings = listings, Type = 'Actual')

# Future months for prediction

future_months <- data.frame(Month = 10:16)

# Generate predictions

future_months$Listings <- predict(model, newdata = future_months)

future_months$Type <- 'Predicted'

# Combine actual and predicted data

combined_data <- rbind(data, future_months)

# Plot

ggplot(combined_data, aes(x = Month, y = Listings, color = Type)) +
  
  geom_point() + # for points
  
  geom_line() + # for lines
  
  labs(title = "Airbnb Listings: Actual and Predicted",
       
       x = "Month",
       
       y = "Listings") +
  
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  
  theme_minimal()
