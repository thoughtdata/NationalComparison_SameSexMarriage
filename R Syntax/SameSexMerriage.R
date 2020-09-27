#install.packages("tidyverse")
#install.packages("hrbrthemes")
#install.packages("viridis")
#install.packages("gridExtra")
#install.packages("ggrepel")
install.packages("plotly")
install.packages("gapminder")

library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(gridExtra)
library(ggrepel)
library(plotly)
#using the Chinese fonts you have, check it with font book.  
# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Show a bubbleplot
data %>%
  mutate(pop=pop/100000) %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="bottom")

tmp <- data %>%
  mutate(
    annotation = case_when(
      gdpPercap > 5000 & lifeExp < 60 ~ "yes",
      lifeExp < 30 ~ "yes",
      gdpPercap > 40000 ~ "yes"
    )
  ) %>%
  mutate(pop=pop/1000000) %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country))

# Plot
ggplot( tmp, aes(x=gdpPercap, y=lifeExp, size = pop, color = continent)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none") +
  geom_text_repel(data=tmp %>% filter(annotation=="yes"), aes(label=country), size=4 )
################################################
################################################
################################################
################################################
Chao_Chao <- read_excel("/Users/edsolitude/Dropbox/R_Analysis/ChaoGao_Project/Chao_Chao.xlsx")
#View(Chao_Chao)
Chao_Chao %>%
  mutate(pop=pop/1000000) %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot( aes(x= GNIpercapitaPPP, y=GINI, size = pop, color = continent)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="bottom")

tmp <- Chao_Chao %>%
  mutate(
    annotation = case_when(
      GNIpercapitaPPP > 1000 & GINI >10 ~ "yes",
      GNIpercapitaPPP < 40000  ~ "yes" ,
      )
  ) %>%
  mutate(pop=pop/1000000) %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country))

# Plot
ggplot( tmp, aes(x=GNIpercapitaPPP , y=GINI , size = pop, color = continent)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 40), name="Population (M)") +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none") +
  geom_text_repel(data=tmp %>% filter(annotation=="yes"), aes(label=continent), size=4 )



Chao_Chao %>%
  mutate(pop=pop/1000000) %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot( aes(x= GDPpercapitacurrentUS, y=GINI, size = pop, color = continent)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="bottom")



tmp2 <- Chao_Chao %>%
  mutate(
    annotation = case_when(
      Key==1 ~ "yes" ,
    )
  ) %>%
  mutate(pop=pop/1000000) %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country))

# Plot
ggplot( tmp2, aes(x=GDPpercapitacurrentUS , y=GINI , size = pop, color = SMarriage)) +
  geom_point(alpha=0.4) +
  scale_size(range = c(1.7, 35), name="Population (M)") +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none") +
  geom_text_repel(data=tmp2 %>% filter(annotation=="yes"), aes(label=country), size=4, color = "black" )
  

#Annotation will arch those variable you want to label the name

# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# https://www.data-to-viz.com/graph/bubble.html
# https://ggplot2.tidyverse.org/reference/annotate.html
#https://www.r-graph-gallery.com/320-the-basis-of-bubble-plot/

###########################################
###########2018INFORMATION#################
###########################################

ECO2019 <- read_excel("/Users/edsolitude/Dropbox/R_Analysis/ChaoGao_Project/EconIndex19.xlsx")
#View(Chao_Chao)
ECO2019 %>%
  mutate(pop=pop/1000000) %>%
  arrange(desc(pop)) %>%
  mutate(Country = factor(Country, Country)) %>%
  ggplot( aes(x= `GDP per Capita (USD)`, y=`Wealth Gini Index`, size = pop, color = Smarriage)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="bottom")

tmp <- ECO2019 %>%
  mutate(
    annotation = case_when(
      List_name==1 ~ "yes" ,
    )
  ) %>%
  mutate(pop=pop/1000000) %>%
  arrange(desc(pop)) %>%
  mutate(Country = factor(Country, Country))

# Plot
ggplot( tmp, aes(x= `GDP per Capita (USD)`, y=`Wealth Gini Index`, size = pop, color = Smarriage)) +
  geom_point(alpha=0.4) +
  scale_size(range = c(1.4, 40), name="Population (M)") +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none") +
  geom_text_repel(data=tmp %>% filter(annotation=="yes"), aes(label=Country), size=4, color= "red" )

ggplot( tmp, aes(x= `GDP per Capita (USD)`, y=`Net Income Gini Index`, size = pop, color = Smarriage)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(1.4, 40), name="Population (M)") +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none") +
  geom_text_repel(data=tmp %>% filter(annotation=="yes"), aes(label=Country), size=4, color= "black" )

ggplot( tmp, aes(x=`Net Income Gini Index` , y=`Median Daily Income (USD PPP)`, size = pop, color = Smarriage_New)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(1.4, 40), name="Population (M)") +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none") +
  geom_text_repel(data=tmp %>% filter(annotation=="yes"), aes(label=Country), size=4, color= "black" )

#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
IMF <- read_excel("ChaoGao_Project/IMF WEOApr2019all_new.xlsx")
#View(IMF)
#View(Chao_Chao)
IMF %>%
  mutate(pop=Pop) %>%
  arrange(desc(pop)) %>%
  mutate(Country = factor(Country, Country)) %>%
  ggplot( aes(x= `PPPPC (Purchase power pc)`, y=`Net Income Gini Index_2018`, size = pop, color = SMARRIAGE)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="bottom")

tmp <- IMF %>%
  mutate(
    annotation = case_when(
      LIST_NAME_all==1 ~ "yes")
  ) %>%
  mutate(pop=Pop/1) %>%
  arrange(desc(pop)) %>%
  mutate(NGD=`NGDPDPC(US$)`/1) %>%
  mutate(PPPC=`PPPPC (Purchase power pc)`/1000)%>%
  mutate(Country = factor(Country, Country))

# Plot
ggplot( tmp, aes(x= PPPC, y=`Net Income Gini Index_2018`, size = Pop, color = SMARRIAGE)) +
  geom_point(alpha=0.4) +
  scale_size(range = c(1.4, 40), name="Population (M)") +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none") +
  geom_text_repel(data=tmp %>% filter(annotation=="yes"), aes(label=Country), size=4, color= "red" )

ggplot( tmp, aes(x= PPPC, y=`Net Income Gini Index_2018`, size = Pop, color = SMARRIAGE)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(1.4, 40), name="Population (M)") +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none") +
  geom_text_repel(data=tmp %>% filter(annotation=="yes"), aes(label=Country), size=4, color= "black" )

ggplot( tmp, aes(x= `Net Income Gini Index_2018`, y=PPPC, size = Pop, color = SMARRIAGE)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(1.4, 30), name="Population (M)") +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  ggtitle("Title") +
  labs(x="Gini Index",y="GDP per capital, PPPPC") + 
  geom_text_repel(data=tmp %>% filter(annotation=="yes"), aes(label=Country), size=4, color= "black" )

  

  