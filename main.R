# IMPORTING LIBRARY
library(ggplot2)
library(reshape2)
library(rsample)

# READING THE CSV
# Replace 'your_file.csv' with the path to your CSV file
file_path <- "customer_shopping_data_1695379411426.csv"

# Read the CSV file into a data frame
data <- read.csv(file_path)

# Preliminary data analysis
mean_age <- mean(data$age)
cat("mean age of overall value is", mean_age, "\n")
# Check for missing values in the 'invoice_date' column
missing_invoice_date <- sum(is.na(data$invoice_date))
cat("number of missing invoice date is", missing_invoice_date, "\n")
# Check for missing values in the 'quantity' column
missing_quantity <- sum(is.na(data$quantity))
cat("number of missing quantity is", missing_quantity, "\n")

# Convert "invoice_date" to a proper date format
data$invoice_date <- as.Date(data$invoice_date, format = "%m/%d/%Y")
# Aggregate quantity by month
agg_data_quantity <- aggregate(quantity ~ format(invoice_date, "%Y-%m"), data, sum)
# Find the month with the maximum and minimum quantities
max_month_quantity <- agg_data_quantity[which.max(agg_data_quantity$quantity),]
min_month_quantity <- agg_data_quantity[which.min(agg_data_quantity$quantity),]
# Print the results
cat("Month with the maximum quantity:", max_month_quantity$`format(invoice_date, "%Y-%m")`, "\n")
cat("Maximum quantity:", max_month_quantity$quantity, "\n")
cat("Month with the minimum quantity:", min_month_quantity$`format(invoice_date, "%Y-%m")`, "\n")
cat("Minimum quantity:", min_month_quantity$quantity, "\n")


# Aggregate price by month
agg_data_price <- aggregate(price ~ format(invoice_date, "%Y-%m"), data, sum)

# Find the month with the maximum and minimum prices
max_month_price <- agg_data_price[which.max(agg_data_price$price),]
min_month_price <- agg_data_price[which.min(agg_data_price$price),]

# Print the results
cat("Month with the maximum price:", max_month_price$`format(invoice_date, "%Y-%m")`, "\n")
cat("Maximum price:", max_month_price$price, "\n")
cat("Month with the minimum price:", min_month_price$`format(invoice_date, "%Y-%m")`, "\n")
cat("Minimum price:", min_month_price$price, "\n")


# TIME SERIES PLOT
total_spending_per_month <- aggregate(price ~ format(invoice_date, "%Y-%m"), data, sum)
colnames(total_spending_per_month) <- c("Month", "Total Spending")

# Add a grouping variable based on category
total_spending_per_month$category <- "Total"

# Create a time series plot for total spending per month
ggplot(total_spending_per_month, aes(x = Month, y = `Total Spending`, group = category)) +
  geom_line() +
  labs(x = "Month", y = "Total Spending") +
  theme_minimal() +
  scale_x_discrete(labels = scales::date_format("%Y-%m"))


# CORRELATION
# Calculate correlations between numeric input features and 'quantity'
correlations_age_price_to_quantity <- cor(data[, c("age", "price")], data$quantity)
correlations_price_to_quantity <- cor(data$price, data$quantity)
correlations_age_to_quantity <- cor(data$age, data$quantity)

# Print the correlations
cat ("correlation of age price and quantity")
print(correlations_age_price_to_quantity)

cat("correlation of price to qunatity")
print(correlations_price_to_quantity)

cat("correlation of age to quantity")
print(correlations_age_to_quantity)


# Scatterplot for price vs. quantity correlation
plot(data$price, data$quantity,
     xlab = "Price", ylab = "Quantity",
     main = "Scatterplot of Price vs. Quantity",
     pch = 19, col = "blue")

# Create a correlation matrix
cor_matrix <- cor(data[, c("price", "age", "quantity")])
print(cor_matrix)


# Create a heatmap
ggplot(data = melt(cor_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Correlation Heatmap")


## MODEL TRAINING


# # Encode categorical variables using one-hot encoding
data$gender <- as.numeric(factor(data$gender, levels = c("Female", "Male")))
# Convert 'category' to numeric (assign one int to one type)
data$category <- as.numeric(factor(data$category))

x <- data.frame(
  x1 = data[, "category"],
  x2 = data[, "gender"],
  x3 = data[, "age"],
  x4=data[,"price"]
)

y <- data.frame(y=data[,"quantity"])
df <- cbind(x, y)
ones = matrix(1 , nrow(x),1)


## TASK 2.1 ##

x_model_1<-cbind(ones,(x[,"x4"]),(x[,"x1"])^2,(x[,"x1"])^3,(x[,"x2"])^4,(x[,"x1"])^4)
theta_hat_1 <- solve(t(as.matrix(x_model_1)) %*% as.matrix(x_model_1)) %*% t(as.matrix(x_model_1)) %*% as.matrix(y)
print(theta_hat_1)

x_model_2<-cbind(ones,(x[,"x4"]),(x[,"x1"])^3,(x[,"x3"])^4)
theta_hat_2 <- solve(t(as.matrix(x_model_2)) %*% as.matrix(x_model_2)) %*% t(as.matrix(x_model_2)) %*% as.matrix(y)
print(theta_hat_2)

x_model_3<-cbind(ones,(x[,"x3"])^3,(x[,"x3"])^4)
theta_hat_3 <- solve(t(as.matrix(x_model_3)) %*% as.matrix(x_model_3)) %*% t(as.matrix(x_model_3)) %*% as.matrix(y)
print(theta_hat_3)

x_model_4<-cbind(ones,(x[,"x2"]),(x[,"x1"])^3,(x[,"x3"])^4)
theta_hat_4 <- solve(t(as.matrix(x_model_4)) %*% as.matrix(x_model_4)) %*% t(as.matrix(x_model_4)) %*% as.matrix(y)
print(theta_hat_4)

x_model_5 <- cbind(ones,(x[,"x4"]),(x[,"x1"])^2,(x[,"x1"])^3,(x[,"x3"])^4)
theta_hat_5 <- solve(t(as.matrix(x_model_5)) %*% as.matrix(x_model_5)) %*% t(as.matrix(x_model_5)) %*% as.matrix(y)
print(theta_hat_5)

## TASK 2.2 ##

x_model_1 <- as.matrix(x_model_1)
theta_hat_1 <- as.matrix(theta_hat_1)
Y_hat_model_1 <- x_model_1 %*% theta_hat_1
RSS_model_1 <- sum((y - Y_hat_model_1)^2)
print(sprintf("RSS value of the model 1 is %0.4f", RSS_model_1))

x_model_2 <- as.matrix(x_model_2)
theta_hat_2 <- as.matrix(theta_hat_2)
Y_hat_model_2 <- x_model_2 %*% theta_hat_2
RSS_model_2 <- sum((y - Y_hat_model_2)^2)
print(sprintf("RSS value of the model 2 is %0.4f", RSS_model_2))


x_model_3 <- as.matrix(x_model_3)
theta_hat_3 <- as.matrix(theta_hat_3)
Y_hat_model_3 <- x_model_3 %*% theta_hat_3
RSS_model_3 <- sum((y - Y_hat_model_3)^2)
print(sprintf("RSS value of the model 3 is %0.4f", RSS_model_3))

x_model_4 <- as.matrix(x_model_4)
theta_hat_4 <-  as.matrix(theta_hat_4)
Y_hat_model_4 <- x_model_4 %*% theta_hat_4
RSS_model_4 <- sum((y - Y_hat_model_4)^2)
print(sprintf("RSS value of the model 4 is %0.4f", RSS_model_4))

x_model_5 <- as.matrix(x_model_5)
theta_hat_5 <- as.matrix(theta_hat_5)
Y_hat_model_5 <- x_model_5 %*% theta_hat_5
RSS_model_5 <- sum((y - Y_hat_model_5)^2)
print(sprintf("RSS value of the model 5 is %0.4f", RSS_model_5))


## Task 2.3 ##
N <- nrow(y)
Variance_model_1 = RSS_model_1/(N-1)
print(sprintf("Variance of model 1 is %0.4f", Variance_model_1))
likehood_model_1 <- -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model_1))-(1/(2*Variance_model_1))*RSS_model_1
print(sprintf("Log-likelihood of model 1 is %0.4f", likehood_model_1))


Variance_model_2 = RSS_model_2/(N-1)
print(sprintf("Variance of model 2 is %0.4f", Variance_model_2))
likehood_model_2 <- -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model_2))-(1/(2*Variance_model_2))*RSS_model_2
print(sprintf("Log-likelihood of model 2 is %0.4f", likehood_model_2))


Variance_model_3 = RSS_model_3/(N-1)
print(sprintf("Variance of model 3 is %0.4f", Variance_model_3))
likehood_model_3 <- -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model_3))-(1/(2*Variance_model_3))*RSS_model_3
print(sprintf("Log-likelihood of model 3 is %0.4f", likehood_model_3))


Variance_model_4 = RSS_model_4/(N-1)
print(sprintf("Variance of model 4 is %0.4f", Variance_model_4))
likehood_model_4 <- -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model_4))-(1/(2*Variance_model_4))*RSS_model_4
print(sprintf("Log-likelihood of model 4 is %0.4f", likehood_model_4))


Variance_model_5 = RSS_model_5/(N-1)
print(sprintf("Variance of model 5 is %0.4f", Variance_model_5))
likehood_model_5 <- -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model_5))-(1/(2*Variance_model_5))*RSS_model_5
print(sprintf("Log-likelihood of model 5 is %0.4f", likehood_model_5))

## Task 2.4 ##
AIC_1 <- 2* length(x_model_1[1,]) - 2 * likehood_model_1
print(sprintf("AIC of model 1 is %0.4f", AIC_1))

AIC_2 <- 2* length(x_model_2[1,]) - 2 * likehood_model_2
print(sprintf("AIC of model 2 is %0.4f", AIC_2))


AIC_3 <- 2* length(x_model_3[1,]) - 2 * likehood_model_3
print(sprintf("AIC of model 3 is %0.4f", AIC_3))

AIC_4 <- 2* length(x_model_4[1,]) - 2 * likehood_model_4
print(sprintf("AIC of model 4 is %0.4f", AIC_4))

AIC_5 <- 2* length(x_model_5[1,]) - 2 * likehood_model_5
print(sprintf("AIC of model 5 is %0.4f", AIC_5))


BIC_1 <- length(x_model_1[1,]) * log(N) - 2 * likehood_model_1
print(sprintf("BIC of model 1 is %0.4f", BIC_1))

BIC_2 <- length(x_model_2[1,]) * log(N) - 2 * likehood_model_2
print(sprintf("BIC of model 2 is %0.4f", BIC_2))

BIC_3 <- length(x_model_3[1,]) * log(N) - 2 * likehood_model_3
print(sprintf("BIC of model 3 is %0.4f", BIC_3))

BIC_4 <- length(x_model_4[1,]) * log(N) - 2 * likehood_model_4
print(sprintf("BIC of model 4 is %0.4f", BIC_4))

BIC_5 <- length(x_model_5[1,]) * log(N) - 2 * likehood_model_5
print(sprintf("BIC of model 5 is %0.4f", BIC_5))


## Task 2.5 ##
model_1_error <- y - Y_hat_model_1
model_2_error <- y - Y_hat_model_2
model_3_error <- y - Y_hat_model_3
model_4_error <- y - Y_hat_model_4
model_5_error <- y - Y_hat_model_5

par(mfrow = c(3,2))
qqnorm(t(model_1_error),col = "grey", main = "Q-Q Plot for Model 1" )
qqline(model_1_error, col = "red", lwd = 1,lty = 2)

qqnorm(t(model_2_error), col= "grey", main = "Q-Q Plot for Model 2" )
qqline(model_2_error, col = "red", lwd = 1,lty = 2)

qqnorm(t(model_3_error), col= "grey", main = "Q-Q Plot for Model 3" )
qqline(model_3_error, col = "red", lwd = 1,lty = 2)

qqnorm(t(model_4_error),col= "grey", main = "Q-Q Plot for Model 4" )
qqline(model_4_error, col = "red", lwd = 1, lty = 2)

qqnorm(t(model_5_error), col= "grey",main = "Q-Q Plot for Model 5" )
qqline(model_5_error, col = "red", lwd = 1,lty = 2)

## Task 2.6 selecting best regression model

# Distribution of prediction error using histogram
par(mfrow = c(3,2))
hist (model_1_error[,1], freq = FALSE, col="blue", las =1)
abline(v = median(model_1_error[,1]), col = "grey", lwd = 5)
abline(v = mean(model_1_error[,1]), col = "purple", lwd = 5)


hist (model_2_error[,1], freq = FALSE, col="green", las =1)
abline(v = median(model_2_error[,1]), col = "grey", lwd = 5)
abline(v = mean(model_2_error[,1]), col = "purple", lwd = 5)
# abline(v = getmode(model_2_error[,1]), col = "red", lwd = 5)

hist (model_3_error[,1], freq = FALSE, col="orange", las =1)
abline(v = median(model_3_error[,1]), col = "grey", lwd = 5)
abline(v = mean(model_3_error[,1]), col = "purple", lwd = 5)


hist (model_4_error[,1], freq = FALSE, col="yellow", las =1)
abline(v = median(model_4_error[,1]), col = "grey", lwd = 5)
abline(v = mean(model_4_error[,1]), col = "purple", lwd = 5)


hist (model_5_error[,1], freq = FALSE, col="pink", las =1)
abline(v = median(model_5_error[,1]), col = "grey", lwd = 5)
abline(v = mean(model_5_error[,1]), col = "purple", lwd = 5)


hist(0, main = "color code")
legend("center", legend = c("median","mean"),
       lwd = 1, col = c("grey", "purple"))


## Task 2.7 ##

split_x <- initial_split(data = as.data.frame(x),prop=.7)
x_training_set <- training(split_x)
x_training_data <- as.matrix(x_training_set)
x_testing_set <- testing(split_x)
x_testing_data <- as.matrix(x_testing_set)
split_y <- initial_split(data = as.data.frame(y),prop=.7)
y_training_set <- training(split_y)
y_training_data <- as.matrix(y_training_set)
y_testing_set <- testing(split_y)
y_testing_data <- as.matrix(y_testing_set)

training_ones <- matrix(1, nrow(x_testing_set),1)
training_model_x <- cbind((x_testing_set[,"x4"]),(x_testing_set[,"x1"])^2,(x_testing_set[,"x1"])^3,(x_testing_set[,"x2"])^4,(x_testing_set[,"x1"])^4)
training_thetahat <-   solve(t(training_model_x) %*% training_model_x) %*% t(training_model_x) %*% y_testing_data
x_testing_data <- cbind(training_ones, x_testing_data)


y_testing_hat <- x_testing_data %*% training_thetahat
RSS_testing <- sum((y_testing_set-y_testing_hat)^2)
print(sprintf("RSS value is testing data %0.4f", RSS_testing))

t.test(y_testing_data, mu=700, alternative="two.sided", conf.level=0.95)

C_I1 <- -0.1625241
C_I2 <- 0.6853514
ggplot(data = data.frame(y_training_data), aes(x = y_training_data)) +
  geom_density(col = "black", fill = "black" ) +
  geom_vline(xintercept = C_I1, col = "cyan", linetype = "dashed") +
  geom_vline(xintercept = C_I2, col = "cyan", linetype = "dashed") +
  geom_rug()+

  ggtitle("Distribution of training Y data with 95% confidence intervals")+
  xlab("Y Training Data") +
  ylab("Density")

## Task 3 ##
numbers <- c(theta_hat_1)
sorted_numbers <- sort(abs(numbers), decreasing=TRUE)
largest_two_values <- sorted_numbers[1:2]
print(largest_two_values)

theta_bias <- largest_two_values[1]
theta_four <- largest_two_values[2]
theta_one  <-  0.010038614
theta_three   <- -0.001912836

arr_1 = 0
arr_2=0
f_value=0
s_value=0

# 3.2

epsilon <- RSS_model_2 * 2
num <- 100
counter <- 0
for (i in 1:num) {
  range1 <- runif(1,-0.483065688,0.483065688)
  range2 <- runif(1,-0.1435789,0.1435789)
  New_thetahat <- matrix(c(range1,range2,theta_one,theta_three))
  New_Y_Hat <- x_model_2 %*% New_thetahat
  new_RSS <- sum((y - New_Y_Hat)^2)
  if (new_RSS > epsilon){
    arr_1[i] <- range1
    arr_2[i] <- range2
    counter = counter+1
    f_value <- matrix(arr_1)
    s_value <- matrix(arr_2)
  }
}

# 3.4

ggplot(data.frame(f_value), aes(x=f_value)) +
  geom_histogram(color = 'black', fill = "grey") +
  geom_rug()+ #Show the individual observations with black lines
  labs(title = "Frequency distribution of the f_value"
       ) +
  xlab("f_value") +
  ylab("Frequency ")

## marginal and posterior distribution
df <- data.frame(f_value, s_value, legend=rep(c("f_value","s_value"), each=length(f_value)/2))

# Plot the scatter plot using and hiding legends
p <- ggplot(df, aes(x=f_value, y=s_value, color=legend)) +
  geom_point()+
  theme(legend.position="bottom")+ # show legend in bottom
  theme(legend.title = element_blank())+ # hide legend word
   #guides(color=FALSE)+ # Uncomment to hide legend
  ggtitle("Joint and Marginal Posterior Distribution")
#
# Show the plot
print(p)