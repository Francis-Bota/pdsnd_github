
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

wash$Gender<-NA
wash$Birth.Year<-NA

head(chi)

ny$City<-'NYC'
wash$City<-'DMV'
chi$City <- 'CHI'

#Function that joins the 3 datasets at the row level
tBind<-function(d1,d2,d3){
    b1<-rbind(d1,d2)
    b2<-rbind(b1,d3)
    b2
}

shareData<-tBind(ny,wash,chi)

#Converting Trip duration from seconds to minutes
shareData$Trip.Duration<-shareData$Trip.Duration/60

head(shareData)

summary(shareData)

#Plot of the number of User types across Gender
library(ggplot2)
ggplot(aes(x=Gender, fill=User.Type), data=shareData) +
    geom_bar(position='dodge') +
    theme(text = element_text(size = 20)) +
    ggtitle("Count of User Types Across Gender") + xlim("Male","Female")

ggplot(shareData, aes(x = factor(Gender), y = Trip.Duration)) +
  geom_histogram(stat = "summary", fun = "mean", width=0.3) + labs(title="Average Trip Duration Across Genders") + xlab("Gender") + xlim("Male", "Female")

ggplot(aes(x=Birth.Year, fill=City), data=shareData) +
    geom_bar(position='dodge') +
    ggtitle("Birth Year of bikers") +
    scale_x_continuous(breaks = seq(1900, 2002, by = 10)) +
    labs(x = "Birth Year of bikers")

ggplot(aes(x=Birth.Year, y=Trip.Duration), data=shareData) +
  xlim(1970, 2002) +
  geom_point(alpha=0.05, position = position_jitter(h=0)) +
  coord_trans(y='sqrt') +
  geom_line(stat = 'summary', fun.y=mean) + ylim(c(0,20))

system('python -m nbconvert Explore_bikeshare_data.ipynb')
