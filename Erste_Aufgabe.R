

## L�sungen zu den Aufgaben 2020-05-06
# 
# 1.	Wie viele St�dte �ber 1 Mio Einwohner gibt es in dem Datensatz und in wie vielen L�ndern liegen diese?
# 2.	Erstellt ein Histogramm mit den Gro�st�dten (�ber 100k Einwohner) eines Landes
# 3.	Vergleicht mit Boxplots die Gr��enverteilung europ�ischer Hauptst�dte mit den �brigen europ�ischen Gro�st�dten
# 4.	Bonus: Wer findet ein Land bei dem Zipf's Law der St�dtegr��en zutrifft und eins bei dem dies nicht der Fall ist? (https://blogs.cornell.edu/info2040/2016/11/13/zipfs-law-for-cities-a-simple-explanation-for-urban-populations/)
# 

library(tidyverse)
library(ggplot2)

# 1.	Wie viele St�dte �ber 1 Mio Einwohner gibt es in dem Datensatz und in wie vielen L�ndern liegen diese?

## Den Datensatz der Einfachheit halber in eine Variable geladen 
cities<-maps::world.cities

## In Rstudio anschauen
cities%>%
  filter(pop > 1000000)%>%
  group_by(country.etc)%>%
  View()


## Durch ein doppeltes Summarise Anzahl der L�nder und Anzahl der St�dte finden
cities%>%
  filter(pop > 1000000)%>%
  group_by(country.etc)%>%
  summarise(countries=n_distinct(country.etc),big_cities=n_distinct(name))%>%
  summarise(Anzahl_L�nder=sum(countries),Anzahl_Millionenst�dte=sum(big_cities))%>%
  View()



#   2.	Erstellt ein Histogramm mit den Gro�st�dten (�ber 100k Einwohner) eines Landes

##Beispiel Italien
cities%>%
  filter(pop > 100000)%>%
  filter(country.etc == "Italy")%>%
  ggplot()+ ##Auf diese Weise kann man einen gefilterten Datensatz direkt in ggplot visualisieren
  geom_histogram(aes(x=pop),bins=10)+
  scale_x_continuous(labels = scales::comma)+ ##Eine M�glichkeit die Achsenbeschriftung anzupassen
  theme_linedraw()


## Neben der Bev�lkerung ist das ganze auch f�r andere Variablen m�glich, hier mit "lat" f�r die Breitengerade, 
## also Verteilung der Gro�st�dte von S�den nach Norden
cities%>%
  filter(pop > 100000)%>%
  filter(country.etc == "Italy")%>%
  ggplot()+
  geom_histogram(aes(x=lat),bins=10)+
  theme_linedraw()


# 3.	Vergleicht mit Boxplots die Gr��enverteilung europ�ischer Hauptst�dte mit den �brigen europ�ischen Gro�st�dten

## Eine M�glichkeit die Kontinente reinzubringen ist einen anderen Datensatz zu joinen, hier aus dem "countrycode" package:
library(countrycode)
cities<-world.cities%>%
  left_join(codelist,by=c("country.etc"="country.name.en"))%>%
  select(name,country.etc,pop,capital,continent)



cities%>%
  filter(pop > 100000)%>%
  filter(continent == "Europe")%>%
  ggplot()+
  geom_boxplot(aes(y=pop,x= as.factor(capital),fill= as.factor(capital)))+ ##"fill" also F�llfarbe als weitere Unterscheidung  
  scale_y_continuous(labels = scales::comma)+
  scale_x_discrete(labels = c("other","capitals"))+
  scale_fill_brewer(labels = c("other","capitals"))


## Da der Boxplot nicht so aussagekr�ftig ist hier die gleichen Daten noch als Histogramm
cities%>%
  filter(pop > 100000)%>%
  filter(continent == "Europe")%>%
  ggplot()+
  geom_histogram(aes(x=pop, fill = as.factor(capital)))+
  scale_x_continuous(labels = scales::comma)+
  scale_fill_brewer(labels = c("other","capitals"))


# 4.	Bonus: Wer findet ein Land bei dem Zipf's Law der St�dtegr��en zutrifft und eins bei dem dies nicht der Fall ist? (https://blogs.cornell.edu/info2040/2016/11/13/zipfs-law-for-cities-a-simple-explanation-for-urban-populations/)

## Vier Beispiell�nder

ita<-cities%>% ## Auf diese Weise wird der ggplot zwar erstellt, aber nicht direkt geplottet, sondern in eine Variable geschrieben
  #filter(pop > 100000)%>%
  filter(country.etc == "Italy")%>%
  mutate(rank = dense_rank(desc(pop)))%>% ## Zur Darstellung wird eine Rangvariable ben�tigt
  ggplot(aes(x=pop,y=rank))+
  geom_point()+ ## Jede Stadt ist ein Punkt 
  geom_smooth(method="lm", formula= y~x,col="red")+ ## Lineares Regressionsmodell durch alle Punkte
  scale_x_log10(labels = scales::comma)+ 
  scale_y_log10()+
  ggtitle("ITALY")+
  theme_linedraw()

ger<-cities%>%
  #filter(pop > 100000)%>%
  filter(country.etc == "Germany")%>%
  mutate(rank = dense_rank(desc(pop)))%>%
  ggplot(aes(x=pop,y=rank))+
  geom_point()+
  geom_smooth(method="lm", formula= y~x,col="red")+
  scale_x_log10(labels = scales::comma)+
  scale_y_log10()+
  ggtitle("GERMANY")+
  theme_linedraw()

usa<-cities%>%
  #filter(pop > 100000)%>%
  filter(country.etc == "USA")%>%
  mutate(rank = dense_rank(desc(pop)))%>%
  ggplot(aes(x=pop,y=rank))+
  geom_point()+
  geom_smooth(method="lm", formula= y~x,col="red")+
  scale_x_log10(labels = scales::comma)+
  scale_y_log10()+
  ggtitle("USA")+
  theme_linedraw()



nla<-cities%>%
  #filter(pop > 100000)%>%
  filter(country.etc == "Netherlands")%>%
  mutate(rank = dense_rank(desc(pop)))%>%
  ggplot(aes(x=pop,y=rank))+
  geom_point()+
  geom_smooth(method="lm", formula= y~x,col="red")+
  scale_x_log10(labels = scales::comma)+
  scale_y_log10()+
  ggtitle("THE NETHERLANDS")+
  theme_linedraw()

## Die vier Beispiele in einen gemeinsamen Plot gebracht, hier mit der Funktion "plot_grid" aus dem "cowplot" package

library(cowplot)
plot_grid(ita,ger,usa,nla,nrow=2)
