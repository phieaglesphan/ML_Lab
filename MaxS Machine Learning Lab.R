setwd('../Downloads/Machine_Learning_Lab/Machine_Learning_Lab/data')
setwd('..')
getwd()
orders = read.csv('Orders.csv')
names(orders)
dim(orders)
summary(orders)
apply(orders, MARGIN = 2, function(c) sum(is.na(c)))
41296/nrow(orders)


# Problem 1: converting profit and sales to numeric type

orders$Sales <- as.character(orders$Sales)
orders$Profit <- as.character(orders$Profit)
orders$Sales <- as.numeric(gsub('[$,]','',orders$Sales))
orders$Profit <- as.numeric(gsub('[$,]','',orders$Profit))

# Problem 2

# 1. Is there any seasonal trend of inventory in the company?
# 2. Is the seasonal trend the same for different categories?
# For each order, it has an attribute called `Quantity` that indicates the number of product in the order. 
# If an order contains more than one product, there will be multiple observations of the same order.

orders$Order.Date = as.Date(orders$Order.Date, "%m/%d/%Y")

library(lubridate)
library(dplyr)
library(ggplot2)

names(orders)

#ranking quantity of items sold by month
inventory = orders %>% group_by(Order.ID) %>% summarise(out = sum(Quantity),mo = min(month(Order.Date))) %>% 
  arrange(-out) %>% group_by(mo) %>% summarise(out = sum(out)) %>% arrange(mo)

orders %>% group_by(Order.ID) %>% summarise(out = sum(Quantity),mo = min(month(Order.Date))) %>% 
  arrange(-out) %>% group_by(mo) %>% summarise(out = sum(out)) %>% arrange(-out)

# chart
ggplot(inventory,aes(mo,out))+geom_col(fill='darkgreen')

# now by category
catint = orders %>% group_by(Category,Order.ID) %>% summarise(out = sum(Quantity),mo = min(month(Order.Date))) %>% 
  arrange(-out) %>% group_by(Category,mo) %>% summarise(out = sum(out)) %>% arrange(mo)
catint

# and the chart
ggplot(catint,aes(mo,out))+geom_col(aes(fill=Category),position='dodge')

########################################################################################################################

# Problem 3

returns = read.csv('Returns.csv')
names(returns)
names(orders)
nrow(returns)
returns %>% group_by(Order.ID) %>% summarise(n=n()) %>% arrange(n)

orders %>% group_by(Order.ID,) %>% summarise(n=n()) %>% arrange(-n)
orders[orders$Order.ID=="CA-2015-SV20365140-42268",]$Profit

orders$Order.ID=as.character(orders$Order.ID)
returns$Order.ID = as.character(returns$Order.ID)
combo = orders %>% left_join(returns,by='Order.ID')

names(combo)

# 1. How much profit did we lose due to returns each year?
  
ggplot(combo %>% filter(Returned=='Yes') %>% group_by(yr=year(Order.Date)) %>% summarise(losses=sum(Profit)),aes(yr,losses)) +
  geom_col(fill='darkgreen')

# 2. How many customer returned more than once? more than 5 times?

custnum=1
nrow(combo %>% filter(Returned=='Yes') %>% group_by(Customer.ID) %>% summarise(n=n_distinct(Order.ID)) %>% filter(n>custnum))
custnum=5

# 20
# 0

# 3. Which regions are more likely to return orders?

reg1 = combo %>% filter(Returned=='Yes') %>% group_by(Region = Region.y) %>% summarise(returns=n_distinct(Order.ID)) %>% arrange(-returns)
reg2 = combo %>% group_by(Region = Region.x) %>% summarise(total=n_distinct(Order.ID)) %>% arrange(-total)
reg = reg1 %>% left_join(reg2,by='Region')
reg = reg %>% mutate(percent = 100*returns/total)

# by number of return orders
reg %>% arrange(-returns)

# by percent
reg %>% arrange(-percent)

#  4. Which categories (sub-categories) of products are more likely to be returned?
names(combo)
unique(combo$Sub.Category)

subcat1 = combo %>% filter(Returned=='Yes') %>% group_by(Sub.Category) %>% summarise(returned=sum(Quantity)) %>% arrange(-returned)
subcat2 = combo %>% group_by(Sub.Category) %>% summarise(total=sum(Quantity)) %>% arrange(-total)
subcat = subcat1 %>% left_join(subcat2,by='Sub.Category')
subcat %>% mutate(percent=100*returned/total) %>% arrange(-percent)

nrow(subcat)

cat1 = combo %>% filter(Returned=='Yes') %>% group_by(Category) %>% summarise(returned=sum(Quantity)) %>% arrange(-returned)
cat2 = combo %>% group_by(Category) %>% summarise(total=sum(Quantity)) %>% arrange(-total)
cat = cat1 %>% left_join(cat2,by='Category')
cat %>% mutate(percent=100*returned/total) %>% arrange(-percent)


##### PART 2 #############
# Question 4
# Part 1
combo = orders %>% left_join(returns,by='Order.ID')
combo$Returned <- as.character(combo$Returned)
combo$Returned = ifelse(is.na(combo$Returned),combo$Returned<-0,combo$Returned<-1)
y = combo$Returned

# Part 2
#names(combo)
#class(combo$Ship.Date)
#head(combo$Ship.Date)
combo$Ship.Date = as.Date(combo$Ship.Date, "%m/%d/%Y")
combo$Process.Time = combo$Ship.Date-combo$Order.Date
#combo %>% select(Process.Time,Ship.Date,Order.Date)
#class(combo$Process.Time)

# Part 3
productdeets = combo %>% group_by(Product.ID) %>% summarise(tn=n(),ts=sum(Quantity)) %>% arrange(-ts)
productreturns = combo %>% filter(Returned==1) %>% group_by(Product.ID) %>% summarise(n=n(),s=sum(Quantity)) %>% arrange(-s)
productdeets = productdeets %>% left_join(productreturns,by='Product.ID')
productdeets$returnrateN = productdeets$n/productdeets$tn
productdeets$returnrateS = productdeets$s/productdeets$ts
combo = combo %>% left_join(productdeets,by='Product.ID')
combo[is.na(combo$returnrateN),]$returnrateN<-0
combo[is.na(combo$returnrateS),]$returnrateS<-0

################
# Question 5

sat = combo %>% select(Ship.Mode,Segment,Region.x,Category,Process.Time,returnrateN,Shipping.Cost,Order.Priority,Returned)
sat$Returned = as.factor(sat$Returned)

names(combo)
logit.saturated = glm(Returned ~ ., data = sat, family='binomial')
summary(logit.saturated)
set.seed(0)

index <- sample(1:nrow(sat), size = nrow(sat)*0.8)
prob <- predict(logit.saturated, sat[-index, -9], type = "response")
mean((prob > 0.5) == (sat$Returned[-index] == 1))

sim = combo %>% select(returnrateN,Process.Time,Returned)
sim$Returned = as.factor(sim$Returned)

logit.simple = glm(Returned~., data=sim,family='binomial')
prob <- predict(logit.simple, sim[-index, -3], type = "response")
anova(logit.simple,logit.saturated,test='Chisq')

#  anova(model.birdsandyears,model.saturated,test='Chisq')
# P value is not low enough, so we maintain that our birds and years model is better.