#Data Source - https://archive.ics.uci.edu/ml/machine-learning-databases/00352/

#Set working directory
setwd('G:/DSP/Project/Case Study')

#Package Installation:

install.packages('readxl')
library(readxl)
install.packages('DataExplorer') 
library(DataExplorer)
install.packages('ggplot2')
library(ggplot2)
install.packages('rfm')
library(rfm)

#Data Extraction from excel to R:

data_1 = read_excel('Customer_Data.xlsx', sheet = 1)


#Will keep data_1 as backup and perform operations on data:
data = data_1

#Check dimension of the data:
dim(data) #Data consists of 5,41,909 observations and 8 variables

#Check unique customer ID which will give us number of customers we have in total: 
length(unique(data$CustomerID))   #We have total 4373 customers

#Check strucutre of the data:
str(data)

#check missing values:

colSums(is.na(data))  #Data has missing values only in CustomerID and we will remove these missing values

#We will perform quick EDA through Data Explorer Package and will get the report in HTML file:

create_report(data)

#Remove missing values in CustomerID column:

data <- subset(data, !is.na(data$CustomerID))

#We will check dimensions now:
dim(data)   #New data has 4,06,829 observations and 8 variables

#Check the starting date and end date:

range(data$InvoiceDate)

#Data is more than a year and its already large, so we will restrict it to one year:

data <- subset(data, InvoiceDate >= "2010-12-09")

#Considering scope of this case study is limited to one country and also customer clusters vary by geography,
#So we will focus on country having maximum customers and we will restrict the data to one geographic unit.

table(data$Country)

#As United Kingdom has more observations, we will select the data related to UK only

data <- subset(data, Country == "United Kingdom")

# Will check dimension again:

dim(data)   # So now we have 349806 observations and 8 variables:

length(unique(data$InvoiceNo))  #Total 19140 unique invoice numbers
length(unique(data$CustomerID)) #Total 3891 unique customers

#To calculate the recency and frequency variables below,it will be necessary to distinguish invoices with purchases from invoices with returns
#So we will create new column which will differentiate returns and purchases

data$item.return <- grepl("C", data$InvoiceNo, fixed=TRUE)
data$purchase.invoice <- ifelse(data$item.return=="TRUE", 0, 1)


#We will use RFM (Recency, frequency and monetary) analysis:


#Create new dataset (customers) having R,F,M Variables with customer ID as well:

#################################
# Create customer-level dataset #
#################################

#Create customers dataset having Customer ID as variable:

customers <- as.data.frame(unique(data$CustomerID))
head(customers)

#Name the column as 'CustomerID'

names(customers) <- "CustomerID"

###########
# Recency #
###########

#Check datatype of 'InoviceDate':

class(data$InvoiceDate)

# Column 'InvoiveDate' also consists of time, we need to remove the time using strtrim function:

data$InvoiceDate = strtrim(data$InvoiceDate, 10)

#data$InvoiceDate = as.Date(data$InvoiceDate, format = '%d-%m-%y')

head(data$InvoiceDate)

#Create new column named 'recency' which will consists value in number of days:

data$recency <- (as.Date("2011-12-10") - as.Date(data$InvoiceDate))

head(data$recency)
unique(data$recency)

# Remove returns, so only consider the data of 'purchase'
temp <- subset(data, purchase.invoice == 1)

head(temp)

# Obtain number of days since most recent purchase

recency <- aggregate(recency ~ CustomerID, data=temp, FUN=min, na.rm=TRUE)

#remove(temp)
head(recency)

# Add 'recency' column into customers dataset:

customers <- merge(customers, recency, by="CustomerID", all=TRUE, sort=TRUE)

#remove(recency)

customers$recency <- as.numeric(customers$recency)

head(customers)

#############
# Frequency #
#############

customer.invoices <- subset(data, select = c("CustomerID","InvoiceNo", "purchase.invoice"))

dim(customer.invoices)
head(customer.invoices)

#Remove duplicate entries in customer.invoices:

customer.invoices <- customer.invoices[!duplicated(customer.invoices), ]
dim(customer.invoices)

#Order the customer.invoices dataset by customerID:

customer.invoices <- customer.invoices[order(customer.invoices$CustomerID),]
dim(customer.invoices)
head(customer.invoices)

#rownames are into thousands, we can remove these rownames:
row.names(customer.invoices) <- NULL

head(customer.invoices)

# Find number of invoices for each customer:

annual.invoices <- aggregate(purchase.invoice ~ CustomerID, data=customer.invoices, FUN=sum, na.rm=TRUE)

# Rename the column to frequency:
names(annual.invoices)[names(annual.invoices)=="purchase.invoice"] <- "frequency"

# Add Number of invoices to customers dataset
customers <- merge(customers, annual.invoices, by="CustomerID", all=TRUE, sort=TRUE)
#remove(customer.invoices, annual.invoices)

# Check the range of frequency:
range(customers$frequency)
table(customers$frequency)

# Remove customers who have not made any purchases in the past year
customers <- subset(customers, frequency > 0)
dim(customers)

###############################
# Monetary Value of Customers #
###############################

# Total spent on each item on an invoice
data$Amount <- data$Quantity * data$UnitPrice

# Aggregated total sales for each customer
annual.sales <- aggregate(Amount ~ CustomerID, data=data, FUN=sum, na.rm=TRUE)
names(annual.sales)[names(annual.sales)=="Amount"] <- "monetary"

# Add monetary value to customers dataset
customers <- merge(customers, annual.sales, by="CustomerID", all.x=TRUE, sort=TRUE)
#remove(annual.sales)

# Identify customers with negative monetary value numbers, as they were presumably returning purchases from the preceding year

customers$monetary <- ifelse(customers$monetary < 0, 0, customers$monetary) # reset negative numbers to zero


################
# Histogram #
################

#With histogram, we can examine the distribution of :

# monetary value (total revenue generated by each customer)
# recency days (days since the most recent visit for each customer)
# frequency (transaction count for each customer)

rfm_histograms(rfm_result)  #So we can see histograms are right skewed. We will use log to transform them


#As we can see, RFM values are positvely Skewed, So will use log transformation to reduce skewness:

customers$recency.log <- log(customers$recency)
customers$frequency.log <- log(customers$frequency)
customers$monetary.log <- customers$monetary + 0.1 # can't take log(0), so add a small value to remove zeros
customers$monetary.log <- log(customers$monetary.log)


#Recency:

ggplot(data = customers, aes(x = recency.log)) +
  geom_histogram(fill = 'blue', color = 'black', bins = 40) + labs(title = 'Recency')

#Frequency:

ggplot(data = customers, aes(x = frequency.log)) +
  geom_histogram(fill = 'blue', color = 'black', bins = 40) + labs(title = 'Frequency')

#Monetary:

ggplot(data = customers, aes(x = monetary.log)) +
  geom_histogram(fill = 'blue', color = 'black', bins = 40) + labs(title = 'Monetary')


################
# Scatterplot #
################

#Scatter Plots:

# The best customers are those who:
#   
#   * bought most recently
#   * most often
#   * and spend the most

#Log Transformed Frequency Vs Log Transformed Monetary:
#As the frequency of visits increases, the revenue generated also increases:

ggplot(customers, aes(x = frequency.log, y = monetary.log)) + geom_point(aes(colour = recency.log)) +
  xlab("Log-transformed Frequency") + ylab("Log-transformed Monetary Value of Customer") + scale_colour_gradient(name="Log-transformed Recency")+
  labs(title = 'ScatterPlot_FrequencyVsMonetary')

#Log Transformed Recency Vs Log Transformed Monetary:
#Customers who visited more recently generated more revenue compared to those who visited in the distant past.

ggplot(customers, aes(x = recency.log, y = monetary.log )) + geom_point(aes(colour = frequency.log)) +
  xlab("Log-transformed Recency") + ylab("Log-transformed Monetary") + scale_colour_gradient(name="Log-transformed Frequency")+
  labs(title = 'ScatterPlot_RecencyVsMonetary')

#Log Transformed Frequency Vs Log Transformed Recency:
#Customers with low frequency visited in the distant past while those with high frequency have visited in the recent past


ggplot(customers, aes(x = frequency.log, y = recency.log)) + geom_point(aes(colour = monetary.log)) +
  xlab("Log-transformed Frequency") + ylab("Log-transformed Recency") + scale_colour_gradient(name="Log-transformed Monetary")+
  labs(title = 'ScatterPlot_FrequencyVsRecency')


########################################
# Scaling of RFM on the scale of [0,1] #
#######################################

#Define a Function that will scale the values to 0 to 1:

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

customers$recencyScaled = range01(customers$recency)

customers$frequencyScaled =range01(customers$frequency)

customers$monetaryScaled = range01(customers$monetary)

dim(customers)  # [3863 * 10]
head(customers)

customers$CustImp = apply(customers[,c("recencyScaled", "frequencyScaled", "monetaryScaled")], 1, mean)

Customers_Final = customers[order(customers$CustImp, decreasing = T),]

rownames(Customers_Final) = NULL

head(Customers_Final)

#Select top 500 customers

Top_Customers = Customers_Final[seq(1:500),]

head(Top_Customers)

#Export List of Top cutomers:

write.csv(Top_Customers, file = "Top_Customers.csv")


################
# RFM package #
################


class(data)

#Set latest date in data as analysis date:

analysis_date <- lubridate::as_date("2011-12-10", tz = "UTC")

#Change datatype as Date:

data$InvoiceDate = as.Date(data$InvoiceDate)

#Use rfm_table_order which categorises customers and prepares chart:
rfm_result <- rfm_table_order(data, CustomerID, InvoiceDate, Amount, analysis_date)

rfm_result

#Heat Map:
#The heat map shows the average monetary value for different categories of recency and frequency scores.
#Higher scores of frequency and recency are characterized by higher average monetary value as indicated by the darker areas in the heatmap.

rfm_heatmap(rfm_result)


#Bar Chart:
#Bar chart generates the distribution of monetary scores for the different combinations of frequency and recency score

rfm_bar_chart(rfm_result)

