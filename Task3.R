#Vaishnavi Rastogi
#EDA

df=read.csv(file.choose(),header=TRUE,sep=",")
summary(df)
str(df)


library(ggplot2)

ggplot(data=df,aes(x=Ship.Mode))+
  geom_bar()+
  labs(y="frequency",title="frequency chart of Ship mode")
#Maximum products are sold via Standard class

ggplot(data=df,aes(x=Ship.Mode,fill=Segment))+
  geom_bar()+
  labs(y="frequency",title="Segment wise distribution of Ship mode")
#Most products are from consumer segment in each Ship mode

ggplot(data=df,aes(fill=Sub.Category,x=Category))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y="frequency",title="Division of Categories into sub-categories")
#This graph can be referred to know about which subcategory is present in which category

ggplot(data=df,aes(fill=Category,x=State))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y="frequency",title="Category wise distribution of States")
#California has maximum frequency with majority of office supplies
#Wyoming and district of Columbia are states with minimum frequency

ggplot(data=df,aes(fill=Category,x=Region))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y="frequency",title="Category wise distribution of Regions")
#West region has maximum frequency

#SINCE EVERY COUNTRY IS USA WE CAN DROP IT
#ALSO THE USE OF POSTCODE IS NOT SIGNIFICANT
d2=df[,-c(3,6)]
library(dplyr)
pro_sum=d2 %>%
  group_by(Region) %>%
  summarise(Frequency=sum(Profit))
prof=data.frame(pro_sum)
prof
barplot(prof$Frequency,names.arg=prof$Region,ylim=c(0,110000),main="Region wise Total profit")
#West region has maximum total profit

pro_sum2=d2 %>%
  group_by(Category) %>%
  summarise(Frequency=sum(Profit))
prof2=data.frame(pro_sum2)
prof2
barplot(prof2$Frequency,names.arg=prof2$Category,main="Category wise Total profit")
#Maximum profit is gained in Technology Category
#Minimum profit is gained in Furniture Category

d2=d2%>%
  mutate(profit_percent=Profit/Sales)
avg_profpercent=d2%>%
  group_by(Category)%>%
  summarise(Frequency=mean(profit_percent))
barplot(avg_profpercent$Frequency,names.arg=avg_profpercent$Category,main="Profit percentage of Category",col="purple")
#Maximum profit percentage is in Technology Category
#Minimum profit percentage is in Furniture Category

avg_profpercent2=d2%>%
  group_by(Category,Sub.Category)%>%
  summarise(Frequency=mean(profit_percent))
barplot(avg_profpercent2$Frequency,ylab="Profit percentage",main="Profit percentage of Sub Category",names.arg=avg_profpercent2$Sub.Category,col="purple",las=2)
#Subcategories like book cases,tables,appliance,binders and machines are facing loss
#Envelopes,labels and papers have maximum profit percentage

avg_profpercent3=d2%>%
  group_by(Region)%>%
  summarise(Frequency=mean(profit_percent))
barplot(avg_profpercent3$Frequency,ylab="Profit percentage",main="Profit percentage of Regions",names.arg=avg_profpercent3$Region,col="purple",las=2)
#Central region is facing loss on an average

sale_sum=d2 %>%
  group_by(Region) %>%
  summarise(Frequency=sum(Sales))
sale=data.frame(sale_sum)
barplot(sale$Frequency,names.arg=sale$Region,main="Region wise Total sales")
#West region has maximum total sales

sale_sum2=d2 %>%
  group_by(Category) %>%
  summarise(Frequency=sum(Sales))
sale2=data.frame(sale_sum2)
barplot(sale2$Frequency,names.arg=sale2$Category,main="Category wise Total sales")
#Maximum sales is in Technology Category
#Minimum sales is in Office supplies Category

#Conclusion
#Central region should be focused on to increase profit
#Sub categories which are facing loss should be improved
#Sales in states with less frequency should be increased
#Furniture category should be expanded more to increase sales
#Ways to increase profit percentage should be explored

