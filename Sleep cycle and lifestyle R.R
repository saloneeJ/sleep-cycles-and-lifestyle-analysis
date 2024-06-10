library(dplyr)
library(ggplot2)
library(scales)# helps to create custom scales in visualization
getwd()
setwd('C:/Users/Shubhangi/Downloads')
df<- read.csv("Sleep_health_and_lifestyle_dataset (1).csv")
View(df)
df$BMI.Category[df$BMI.Category=="Normal Weight"]<-"slightly Above Normal"
View(df)
names(df)
head(df)
glimpse(df)
summary(df)
sum(duplicated(df))
sum(is.na(df))
char_columns <- df[sapply(df, is.character)]
int_columns <- df[sapply(df, is.integer)]
intd_columns<-cbind(int_columns,df$Sleep.Duration )
intd_columns<-intd_columns%>% rename_at('df$Sleep.Duration', ~'Sleep.Duration')
#In such cases where the desired function takes additional arguments, you will 
#need to use ~ before the function and reference.to represent the column values.
boxplot(x=intd_columns)
ggplot(intd_columns,aes(y=Heart.Rate))+
  geom_boxplot()+
  labs(title ='Heart Rate distributions among people')+
  theme_light()

#to get the values of outliers 
outliers <- boxplot(df$Heart.Rate, plot = FALSE)$out
for (i in outliers){
  print(i)
}
outlier_cells<-length(outliers)
total_cells <- prod(dim(df))
percentage_outlier <- (outlier_cells * 100) / total_cells
cat("Percentage of outliers in the data frame:", percentage_outlier)

#is there any relation between quality of sleep how sleeping duration
ggplot(df,aes(x=Sleep.Duration,y=Quality.of.Sleep, colour = Gender))+
  geom_line()+
  geom_point(size = 3, 
             color = "steelblue")+
  labs(x='Sleep in hours',y='Quality of sleep',
       title= 'Relationship between quality of sleep and sleeping hours',
  subtitle = "On the scale of 1 to 10")+
  theme_bw()

#is there any realtion between bp and heart rate in both sex
ggplot(df,aes(y=Blood.Pressure,x=Gender,fill = Heart.Rate))+
  geom_tile()+
scale_fill_gradient(low='lightblue',high='darkblue')+
  labs(title = 'HEATMAP')+
  theme_bw()


#age wise percentage distribution 
ggplot(df, 
       aes(x = Age, y= after_stat(count/sum(count)))) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 binwidth = 5) + 
  labs(title="Participants by age", 
       y = "Percent",
       x = "Age")+
  scale_y_continuous(labels = percent)+
  theme_bw()


#age wise avg sleep
age_wise_sleep<- df%>% group_by(Age)%>% summarise(avgsleep=mean(Sleep.Duration))
age_wise_sleep<-arrange(age_wise_sleep,avgsleep)
View(age_wise_sleep)

ggplot(age_wise_sleep,aes(x=Age,y=avgsleep))+
 geom_point(color="cornflowerblue", 
             size = 2, 
             alpha=.8)+
  geom_smooth(size = 1.5)+ #geom_smooth add a trend line over an existing plot
  labs(x='Different ages',y='Average hours of sleep',
       title= 'Avg hours of sleep by age')+
  theme_bw()

#which oocupation gets the most sleep
occ_sleep<- df%>% group_by(Occupation)%>% summarise(avgsleep=mean(Sleep.Duration))
occ_sleep<-arrange(occ_sleep,avgsleep)
View(occ_sleep)
ggplot(df,aes(x=Sleep.Duration,y=Occupation))+
  geom_bar(stat='summary',fun='mean',fill='#6600ff')+
  labs(title = 'Average sleep duration for each occupation',
       x='avg hourly sleep',y='Occupations')+
theme_bw()

#The corelation between occupation and sleeping disorder
ggplot(df,aes(x=Sleep.Disorder,y=Occupation))+
geom_bin_2d()+
  scale_fill_gradient(low='#ccccff',high='#6666ff')+
  labs(title = 'HEATMAP')+
  theme_bw()

#Bmi Categories disribution by occupation
ggplot(df, aes(x = BMI.Category, fill = Occupation)) + 
  geom_bar(position = position_dodge(preserve = "single"))+
  labs(title='BMI categories related to occupations')+
  theme_bw()


#btwn gender and bmi category
ggplot(df,aes(x=Gender,y=BMI.Category))+
  geom_bin_2d()+
  scale_fill_gradient(low='#ccccff',high='#6666ff')+
  labs(title = 'HEATMAP')+
  theme_bw()

# whats the relationship between people with obesity and daily steps
ggplot(df,aes(x=BMI.Category,y=Daily.Steps))+
  geom_bar(stat='summary',fun='mean',fill='#6600ff')+
  labs(title = 'Average daily walking steps and BMI',
       x='BMI categories',y='daily steps')+
theme_bw()

#Mean stress for each occupation by gender.
plotdata <- df %>%group_by(Occupation,Gender) %>%
  summarize(mean = mean(Stress.Level))

pd <- position_dodge(0.2)
ggplot(plotdata, 
       aes(x = mean, 
           y = Occupation, group=Gender, color=Gender)) +
  geom_point(position=pd, 
             size=3) +
  geom_line(position=pd, 
            size = 1)+
  labs(title='Mean stress for each occupation by gender',
       subtitle = 'Stress on scale 1 to 10',
       y='Occupations',
       x='Mean stress on scale 1 to 10')+
  theme_bw()
  
#stress vs heart rate in occupations 
ggplot(df, aes(x = Stress.Level, y = Heart.Rate, 
               color=Occupation,shape=Gender)) +
    geom_point()+
    labs(title = 'relationship of heart rate and stress in occupation')+
  theme_bw()

#sleep hours in diff age groups with occupation and sleep disorder
##used here facet as it divides ggplot into subplot
#on the basis of one or more categorical variable
ggplot(df, 
       aes(x=Age,y=Sleep.Duration, color=Occupation)) +
  geom_point() +
  facet_wrap(~Sleep.Disorder, 
             ncol = 1)+
theme_bw()
  
#Relationship of steps and physical activity and its affect in BMI categories by gender 
ggplot(df, 
       aes(x=Physical.Activity.Level,y=Daily.Steps, color=Gender)) +
  geom_point() +
  facet_wrap(~BMI.Category, 
             ncol = 1)+
  labs(title = 'Relationship of steps and physical activity and its affect in 
       BMI categories by gender')+
  theme_bw()