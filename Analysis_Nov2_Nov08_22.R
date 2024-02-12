###Analysis for November 2- November 8, 2022
##Load Packages
install.packages("plotly")
install.packages("gganimate")
install.packages("gifski")
install.packages("tidyverse")
install.packages("colorspace")
library(gifski)
library(tidyverse)
library(readxl)
library(colorspace)
library(plotly)
library(gganimate)
library(ggplot2)
hcl_palettes(plot=TRUE)

#Upload Dataset

library(readr)
Analysis_Nov2_08 <- read_csv("C:/Users/ME/Downloads/Analysis_Nov2_08.csv")
View(Analysis_Nov2_08)

#Factor Data
Analysis_Nov2_08$Date<-as.character(Analysis_Nov2_08$Date)

Analysis_Nov2_08$Date<-factor(Analysis_Nov2_08$Date, levels=c("11_02_22","11_03_22",
                                                                              "11_04_22","11_05_22",
                                                                              "11_06_22","11_07_22", "11_08_22"))


#Grand Total Analysis 
Grand_Totals<-Analysis_Nov2_08%>%
  select(Fried_Chicken:Market) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "val") %>%
  group_by(Category) %>%
  summarise(Total = sum(val))


ggplot(Grand_Totals, aes(x=Category, y=Total, fill=Category))+
  geom_bar(stat="identity")+
  geom_text(
    aes(label = after_stat(y), group = Category), 
    stat = 'summary', fun = sum, vjust = -0.3, size=2.5)+
  theme(axis.text.x=element_text(size=7, angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill="white", color="black", linewidth = 1),
        panel.grid=element_line(linetype = 0, color="lightgrey"))+
  scale_y_continuous(expand = c(0,0.1))+
  coord_cartesian(ylim=c(0,100000))+
  labs(title="Grand Totals: November 02- November 08, 2022",
       subtitle="Comparison Across Profit Centers", x="Profit Center",
       y="Total Sales $")


#Plot 1
ggplot(Analysis_Nov2_08, aes(x=Date, y=CFA, fill=Date))+
  geom_bar(stat="identity")+
  geom_text(
    aes(label = after_stat(y), group = Date), 
    stat = 'summary', fun = sum, vjust = -0.3, size=2.5)+
  scale_fill_discrete_qualitative(palette="Cold")+
  labs(title="Fried Chicken Totals", subtitle="November 02- November 08, 2022", y="Total Sales $",
       caption="Week of Home Game")+
  theme(axis.text.x=element_text(size=7, angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill="white", color="black", linewidth = 1),
        panel.grid=element_line(linetype = 0, color="lightgrey"))+
  annotate("label", x=6.2, y=18000, label="Grand Total=$80k", size=3, color="red")+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim=c(0,20000))


#Plot 2
ggplot(Analysis_Nov2_08, aes(x=Date, y=Panda, fill=Date))+
  geom_bar(stat="identity")+
  geom_text(
    aes(label = after_stat(y), group = Date), 
    stat = 'summary', fun = sum, vjust = -0.3, size=2.5)+
  scale_fill_discrete_qualitative(palette="Cold")+
  labs(title="Chinese Restaurant Totals", subtitle="November 02- November 08, 2022", y="Total Sales $",
       caption="Week of Home Game")+
  theme(axis.text.x=element_text(size=7, angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill="white", color="black", linewidth = 1),
        panel.grid=element_line(linetype = 0, color="lightgrey"))+
  annotate("label", x=6.2, y=10000, label="Grand Total=$44k", size=3, color="red")+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim=c(0,12000))

#Plot 3
ggplot(Analysis_Nov2_08, aes(x=Date, y=NPCo, fill=Date))+
  geom_bar(stat="identity")+
  geom_text(
    aes(label = after_stat(y), group = Date), 
    stat = 'summary', fun = sum, vjust = -0.3, size=2.5)+
  scale_fill_discrete_qualitative(palette="Cold")+
  labs(title="Pizza Totals", subtitle="November 02- November 08, 2022", y="Total Sales $",
       caption="Week of Home Game")+
  theme(axis.text.x=element_text(size=7, angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill="white", color="black", linewidth = 1),
        panel.grid=element_line(linetype = 0, color="lightgrey"))+
  annotate("label", x=6.2, y=6200, label="Grand Total=$20k", size=3, color="red")+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim=c(0,10000))

#Plot 4
ggplot(Analysis_Nov2_08, aes(x=Date, y=SBX, fill=Date))+
  geom_bar(stat="identity")+
  geom_text(
    aes(label = after_stat(y), group = Date), 
    stat = 'summary', fun = sum, vjust = -0.3, size=2.5)+
  scale_fill_discrete_qualitative(palette="Cold")+
  labs(title="Upstairs Cafe Totals", subtitle="November 02- November 08, 2022", y="Total Sales $",
       caption="Week of Home Game")+
  theme(axis.text.x=element_text(size=7, angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill="white", color="black", linewidth = 1),
        panel.grid=element_line(linetype = 0, color="lightgrey"))+
  annotate("label", x=6.2, y=10500, label="Grand Total=$51k", size=3, color="red")+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim=c(0,14000))

#Plot 5
ggplot(Analysis_Nov2_08, aes(x=Date, y=Barberitos, fill=Date))+
  geom_bar(stat="identity")+
  geom_text(
    aes(label = after_stat(y), group = Date), 
    stat = 'summary', fun = sum, vjust = -0.3, size=2.5)+
  scale_fill_discrete_qualitative(palette="Cold")+
  labs(title="Mexican Restaurant Totals", subtitle="November 02- November 08, 2022", y="Total Sales $",
       caption="Week of Home Game")+
  theme(axis.text.x=element_text(size=7, angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill="white", color="black", linewidth = 1),
        panel.grid=element_line(linetype = 0, color="lightgrey"))+
  annotate("label", x=6.2, y=3500, label="Grand Total=$10k", size=3, color="red")+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim=c(0,4000))


#Plot 6
ggplot(Analysis_Nov2_08, aes(x=Date, y=CE, fill=Date))+
  geom_bar(stat="identity")+
  geom_text(
    aes(label = after_stat(y), group = Date), 
    stat = 'summary', fun = sum, vjust = -0.3, size=2.5)+
  scale_fill_discrete_qualitative(palette="Cold")+
  labs(title="Store 1 Totals", subtitle="November 02- November 08, 2022", y="Total Sales $",
       caption="Week of Home Game")+
  theme(axis.text.x=element_text(size=7, angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill="white", color="black", linewidth = 1),
        panel.grid=element_line(linetype = 0, color="lightgrey"))+
  annotate("label", x=6.2, y=7600, label="Grand Total=$24k", size=3, color="red")+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim=c(0,9000))

#Plot 7
ggplot(Analysis_Nov2_08, aes(x=Date, y=CBML, fill=Date))+
  geom_bar(stat="identity")+
  geom_text(
    aes(label = after_stat(y), group = Date), 
    stat = 'summary', fun = sum, vjust = -0.3, size=2.5)+
  scale_fill_discrete_qualitative(palette="Cold")+
  labs(title="Store 2 Totals", subtitle="November 02- November 08, 2022", y="Total Sales $",
       caption="Week of Home Game")+
  theme(axis.text.x=element_text(size=7, angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill="white", color="black", linewidth = 1),
        panel.grid=element_line(linetype = 0, color="lightgrey"))+
  annotate("label", x=6.2, y=6900, label="Grand Total=$21k", size=3, color="red")+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim=c(0,8000))


#Plot 8
ggplot(Analysis_Nov2_08, aes(x=Date, y=CBSLC, fill=Date))+
  geom_bar(stat="identity")+
  geom_text(
    aes(label = after_stat(y), group = Date), 
    stat = 'summary', fun = sum, vjust = -0.3, size=2.5)+
  scale_fill_discrete_qualitative(palette="Cold")+
  labs(title="Library Totals", subtitle="November 02- November 08, 2022", y="Total Sales $",
       caption="Week of Home Game")+
  theme(axis.text.x=element_text(size=7, angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill="white", color="black", linewidth = 1),
        panel.grid=element_line(linetype = 0, color="lightgrey"))+
  annotate("label", x=6.2, y=6600, label="Grand Total=$20k", size=3, color="red")+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim=c(0,7500))


#Plot 9
ggplot(Analysis_Nov2_08, aes(x=Date, y=RedClay, fill=Date))+
  geom_bar(stat="identity")+
  geom_text(
    aes(label = after_stat(y), group = Date), 
    stat = 'summary', fun = sum, vjust = -0.3, size=2.5)+
  scale_fill_discrete_qualitative(palette="Cold")+
  labs(title="Coffee Totals", subtitle="November 02- November 08, 2022", y="Total Sales $",
       caption="Week of Home Game")+
  theme(axis.text.x=element_text(size=7, angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill="white", color="black", linewidth = 1),
        panel.grid=element_line(linetype = 0, color="lightgrey"))+
  annotate("label", x=6.2, y=1700, label="Grand Total=$4k", size=3, color="red")+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim=c(0,2000))


#Plot 10
ggplot(Analysis_Nov2_08, aes(x=Date, y=Roth, fill=Date))+
  geom_bar(stat="identity")+
  geom_text(
    aes(label = after_stat(y), group = Date), 
    stat = 'summary', fun = sum, vjust = -0.3, size=2.5)+
  scale_fill_discrete_qualitative(palette="Cold")+
  labs(title="Cafe Totals", subtitle="November 02- November 08, 2022", y="Total Sales $",
       caption="Week of Home Game")+
  theme(axis.text.x=element_text(size=7, angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill="white", color="black", linewidth = 1),
        panel.grid=element_line(linetype = 0, color="lightgrey"))+
  annotate("label", x=6.2, y=3500, label="Grand Total=$2k", size=3, color="red")+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim=c(0,4000))


#Plot 11
ggplot(Analysis_Nov2_08, aes(x=Date, y=Tate, fill=Date))+
  geom_bar(stat="identity")+
  geom_text(
    aes(label = after_stat(y), group = Date), 
    stat = 'summary', fun = sum, vjust = -0.3, size=2.5)+
  scale_fill_discrete_qualitative(palette="Cold")+
  labs(title="Market Totals", subtitle="November 02- November 08, 2022", y="Total Sales $",
       caption="Week of Home Game")+
  theme(axis.text.x=element_text(size=7, angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill="white", color="black", linewidth = 1),
        panel.grid=element_line(linetype = 0, color="lightgrey"))+
  annotate("label", x=6.2, y=4500, label="Grand Total=$20k", size=3, color="red")+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim=c(0,6000))






