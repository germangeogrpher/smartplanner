library(nycflights13)
library(tidyverse)
# 
# im package "nycflights13" gibt es den Datensatz "flights". Dazu folgende Aufgaben:
#   
#   Welches sind die f�nf am weitesten entfernten Ziele?
#   
flights%>%
  group_by(dest)%>%
  summarise(Entfernung=max(distance))%>%
  arrange(desc(Entfernung))%>%
  left_join(airports,by=c("dest"="faa"))%>%
  select(Entfernung,name)%>%
  head(n=5)%>%
  View()

# Wenig �berraschend sind Hawaii, Alaska und die Westk�ste am weitesten entfertn

#   Welches Flugzeug (tailnum) ist im Schnitt am p�nktlichsten und welches am unp�nktlichsten?
#   
# Hier habe ich noch das problem gel�st die verschiedenen Flugzeuge zusammen darzustellen, indem sie in einen neuen Dataframe geschrieben werden:

puenktlichkeit<-flights%>% ##zuerst das Flugzeug mit der geringsten Versp�tung
  group_by(tailnum)%>%
  summarise(P�nktlichkeit=mean(arr_delay))%>%
  arrange(desc(P�nktlichkeit))%>%
  head(n=1)


puenktlichkeit<-flights%>% ## dann das am meisten versp�tetste Flugzeug
  group_by(tailnum)%>%
  summarise(P�nktlichkeit=mean(arr_delay))%>%
  arrange(P�nktlichkeit)%>%
  head(n=1)%>%
  rbind(puenktlichkeit)


puenktlichkeit<-flights%>%
  group_by(tailnum)%>% ## und die Flugzeuge mit durchschnittlicher Versp�tung 0
  summarise(P�nktlichkeit=mean(arr_delay))%>%
  filter(P�nktlichkeit==0)%>%
  rbind(puenktlichkeit)


puenktlichkeit%>%
  left_join(planes)%>% ##Ist ein bestimmter Flugzeugtyp besonders p�nktlich? Scheinbar nicht
  View()
  

#   Visualisiert wie viele Fl�ge pro Airline an eurem Geburtstag (oder einem anderen Tag eurer Wahl) durchgef�hrt wurden.

  
flights%>% ## Als Histogramm
  filter(month==11 & day==22)%>%
  ggplot()+
  geom_histogram(aes(x=carrier),stat = "count")
  
  
flights%>% ## Etwas komplizierter als stacked Barchart
  filter(month==11 & day==22)%>%
  group_by(carrier,origin)%>%
  summarise(number_flights=n())%>%
  ggplot(aes(x=carrier,y=number_flights,fill=origin))+
  geom_bar(stat="identity")+
  ggtitle("2013-11-22")
 

