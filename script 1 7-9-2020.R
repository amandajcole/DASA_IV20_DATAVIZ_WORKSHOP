## Step 1 Install the packages

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("rmarkdown")
install.packages("knitr")


library(tidyverse)
library(ggplot2)
library(rmarkdown)
library(knitr)


## Step 2 Assign the Dataset

dataset<-mpg
str(dataset)


## Question what are the top 10 most efficient cars when driving in the city?
task1<- dataset%>%
  select(manufacturer, model, displ, year, cty)%>%
  slice_max(cty,n= 10)
kable(task1)

## Question what are the top 10 most efficient cars when driving in the city?
task2<-dataset%>%
  select(manufacturer, model, displ, year, hwy)%>%
  slice_max(hwy, n= 10)
kable(task2)

# what brand is most efficient in city? show top 10 in barchart
task3<-dataset%>%
  select(manufacturer,cty)%>%
  group_by(manufacturer)%>%
  summarise(mpg_of_manufacturer=mean(cty),.groups="drop")%>%
  slice_max(mpg_of_manufacturer, n=10)
ggplot(task3, aes(x=reorder(manufacturer,-mpg_of_manufacturer),y=mpg_of_manufacturer,fill=manufacturer))+
  labs(x = "Manufacturer")+
  geom_col()

#Now let see which class of vehicle is most efficient in the city? 
#knowing that the smaller car always run better but we want to see how much better? 
#Lets create a bar chart.

task4<-dataset%>%
  select(class,cty)%>%
  group_by(class)%>%
  summarise(mpg=mean(cty),.groups="drop")%>%
  slice_max(mpg, n=10)
ggplot(task4, aes(x=reorder(class,-mpg),y=mpg,fill=class))+
  labs(x= "Class of vehicle", y="MPG") +
  geom_col()#can use geom_bar(stat="identity") can flip this as well

#Facet wrap: Now I want to see how each brand perform in different class of vehicle
# First we see how many model does each brand has for each class

task5<-dataset%>%
  select(manufacturer, class, cty)%>%
  group_by(manufacturer, class)%>%
  summarise(number_of_model=n(),mpg=mean(cty),.groups='drop')%>%
  ungroup()
task5%>%
  ggplot(aes(y=reorder(manufacturer,-mpg),x=mpg, fill= manufacturer))+
  geom_col()+
  facet_wrap(~class)

#drivetrain which is most efficient? barchart
task6<-dataset%>%
  group_by(drv)%>%
  summarise(mpg_of_drv=mean(cty),.groups='drop')%>%
  ungroup()
ggplot(task6,aes(x=reorder(drv,-mpg_of_drv),y=mpg_of_drv, fill=drv))+
  geom_col()

#same thing for manual vs auto
#we need to group them for manual and auto only drop the I3 AV BEHIND THEM
task7<-dataset%>%
  mutate(trans2=str_sub(trans,1,-5))%>%
  #we need to let them do it normally first then we'll see that we have 
  #that we have weird things behind each drivetrains so then we'll introduce mutate and show them how
  #how to alter it and then after that, it's like all other task before
  group_by(trans2)%>%
  summarise(mpg=mean(cty),.groups='drop')%>%
  ungroup()
ggplot(task7,aes(y=reorder(trans2,-mpg),x=mpg, fill=trans2))+
  labs(x= "Miles per Gallon", y="Transmission Type") +
  geom_col()

#compare between years 1999 vs 2008
task8<-dataset%>%
  group_by(manufacturer, year)%>%
  summarise(mpg=mean(cty),.groups='drop')%>%
  ungroup()%>%
  pivot_wider(names_from = year, values_from=mpg)
dataset%>%
  group_by(manufacturer, year)%>%
  summarise(mpg=mean(cty),.groups='drop')%>%
  ungroup()%>%
  ggplot(aes(x=year,y=mpg, color=manufacturer))+
  geom_line()+
  facet_wrap(~manufacturer)+
  theme_bw()
#relationship between displacement and fuel efficiency 
dataset%>%
  ggplot(aes(x=displ, y=cty))+
  geom_point()
  