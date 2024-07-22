library(ggplot2)
library("readxl")
library('gridExtra')
library(dplyr)
library(hrbrthemes)
library(ggpubr)
library(cowplot)
library(ggthemes)
library("ggsci")

##SA cases and sequences plot

data1<-read_excel('ZAF_owid_30Nov.xlsx')


data2<-read_excel('ZAF_gisaid_2Dec.xlsx')


data1$days<-as.Date(cut(data1$date,
                        breaks = "day",
                        start.on.monday = FALSE))

data1$date<-as.Date(cut(data1$date,
                        breaks = "week",
                        start.on.monday = FALSE))


data2$days<-as.Date(cut(data2$date,
                        breaks = "day",
                        start.on.monday = FALSE))

data2$date<-as.Date(cut(data2$date,
                        breaks = "2 week",
                        start.on.monday = FALSE))
is.numeric(data1$new_deaths)
dateVec <- seq(from = as.Date("2020-01-01"), to = as.Date("2022-12-01"), by = "days")

pEpi_SA1<-ggplot()+
  theme_classic()+
  geom_text(family = "Segoe UI") +
  theme(text = element_text(family = "Segoe UI"))+
  geom_bar(data=data1, aes(x=days, y=new_cases_per_million,fill='Cases'),width=3,stat='identity',color='#DBB539')+
  geom_line(data=data1, aes(x=days, y=new_deaths_smoothed_per_million*40, color='Deaths'), size=0.8)+
  geom_rug(data= data2, aes(x=days, color='Genomes'), alpha=0.05, size = 0.2, outside =FALSE,length = unit(0.08, "npc"),show.legend=TRUE)+  
  theme(axis.title.x = element_text(color="black", size=10, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8))+
  theme(axis.title.y = element_text(color="black", size=10, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=8))+
  scale_x_date(limits = c(min(dateVec), max=max(dateVec)), date_labels = "%b\n%Y",date_breaks = "2 month")+
  scale_color_manual(values=c('#B78727','#404040'), name='')+
  scale_fill_manual(values=c('#DBB539'), name='')+
  xlab(' ')+
  ylab(' ')+
  scale_y_continuous(expand = expansion(mult = c(0.13, 0.4)),
                     
                     # Features of the first axis
                     name = "New daily cases per million",
                     
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~./40, name="New daily deaths per million")
                     
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))+
  theme(legend.position="top",legend.text= element_text(size=10)) +
  ggtitle('South Africa')

pEpi_SA1
ggsave("SApermil.tiff", units="cm", width=15, height=10, dpi=600)


##Same script used for Figures 1 - 5 customised with country data and names

####Figure 6 (same script used for subplots for each country with names customised)

Brazil<-read_excel('Updated_all data_Nov22.xlsx')
Brazil$Nextstrain_variants<-factor(Brazil$Nextstrain_variants,levels = c('Other lineages','Alpha','Beta','Gamma', 'Delta', 'BA.1', 'BA.2', 'BA.3', 'BA.4', 'BA.5', 'Recombinants'))

Brazil$days<-as.Date(cut(Brazil$date,breaks = "day",start.on.monday = FALSE))
Brazil$date<-as.Date(cut(Brazil$date,breaks = "week",start.on.monday = FALSE))
Brazil$date2<-as.Date(cut(Brazil$date,breaks = "2 week",start.on.monday = FALSE))
Brazil$date4<-as.Date(cut(Brazil$date,breaks = "1 month",start.on.monday = FALSE))
dateVec <- seq(from = as.Date("2020-01-01"), to = as.Date("2021-12-01"), by = "days")

Brazil1<-ggplot(data=subset(Brazil, !is.na(Nextstrain_variants)), aes(x = date, fill=Nextstrain_variants))+
  geom_area(stat='bin', position='fill', binwidth=10)+
  theme_classic()+
  #geom_text(family = "Segoe UI") +
  theme(text = element_text(family = "Segoe UI"))+
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8))+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  xlab("Sampling Date")+
  ylab("Proportion of Genomes")+
  scale_x_date(limits = c(min(dateVec), max=max(dateVec)), date_labels = "%b\n%Y",date_breaks = "2 month")+
  scale_fill_manual(values=c('grey90','cadetblue2','darkolivegreen','plum4','wheat', 'darkgrey', 'darkgreen', 'indianred3'), name='Variants', labels=c('Other lineages','Alpha','Beta','Gamma', 'Delta', 'BA.1', 'BA.2', 'BA.3', 'BA.4', 'BA.5', 'Recombinants'))+
  theme(legend.text = element_text(size=11))+
  theme(legend.title = element_text(size=12))+
  theme(legend.direction = "horizontal", legend.position = "bottom")+
  ggtitle('Brazil')+
  theme(plot.title = element_text(face="bold", color="black", size=12))

Brazil1
ggsave("Brazil1.tiff", units="cm", width=20, height=10, dpi=300)

Brazil2<-ggplot(data=subset(Brazil, !is.na(Nextstrain_variants)), aes(x = date,fill=Nextstrain_variants))+
  geom_area(stat='bin', binwidth=20)+
  theme_classic()+
  theme(text = element_text(family = "Segoe UI"))+
  theme(axis.text.x = element_text(color="black", size=16))+
  xlab("Sampling Date")+ 
  scale_x_date(limits = c(min(dateVec), max=max(dateVec)), date_labels = "%b\n%Y",date_breaks = "2 month")+
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8))+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_fill_manual(values=c('grey90','cadetblue2','darkolivegreen','plum4','wheat','blue','orange',  'lightgreen', 'indianred3', 'pink', 'black'), name='Variants', labels=c('Other lineages','Alpha','Beta','Gamma', 'Delta', 'BA.1', 'BA.2', 'BA.3', 'BA.4', 'BA.5', 'Recombinants'))+
  theme(legend.text = element_text(size=11))+
  theme(legend.title = element_text(size=12))+
  theme(legend.direction = "horizontal", legend.position = "bottom")+
  ggtitle('Brazil')+
  theme(plot.title = element_text(face="bold", color="black", size=12))+
  xlab('Date')+
  ylab('Genome Count')

Brazil2
ggsave("Brazil2.tiff", units="cm", width=20, height=10, dpi=300)



###Time to sequence submission plots - Figure 7

Brazil$date<-as.Date(cut(Brazil$date,breaks = "day",start.on.monday = FALSE))
Brazil$date_submitted<-as.Date(cut(Brazil$date_submitted,breaks = "day",start.on.monday = FALSE))
Brazil$lag=Brazil$date_submitted-Brazil$date
Brazil$month<-as.Date(cut(Brazil$date,breaks = "month",start.on.monday = FALSE))

Brazillag<-ggplot(data=Brazil, aes(x=month, y=as.numeric(lag)))+
  theme_classic() +
  geom_jitter(colour="#B8D9A9FF", alpha=0.5, width=0.1)+
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1),geom = "pointrange", color = "black", size=0.8 )+
  #ylab('Days from specimen collection to sequence submission')+
  xlab('')+
  scale_x_date(limits = c(min(dateVec), max=max(dateVec)), date_labels = "%b\n%y",date_breaks = "2 month")+
  theme(axis.text.x = element_text(color="black", size=11))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y = element_text(color="black", size=11))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  ggtitle('Brazil')+
  scale_y_continuous(breaks=seq(0,700,100))


Brazillag

####Figure 8 - sequencing technology

Brazil$Seq_tech<-factor(Brazil$Seq_tech,levels = c("Illumina","Ion Torrent","Oxford Nanopore Technology","Sanger","MGI","Other"))

Brazilseq<-ggplot(data=subset(Brazil, !is.na(Seq_tech)), mapping = aes(x = Collection.date,fill=Seq_tech))+
  geom_bar(position='fill',width=22,color='black', size=0.2)+
  theme_classic()+
  scale_x_date(limits = c(min(dateVec), max=max(dateVec)), date_labels = "%b\n%y",date_breaks = "2 month")+
  theme(axis.text.x = element_text(color="black", size=11))+
  theme(axis.text.y = element_text(color="black", size=11))+
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  scale_fill_manual(values=c('#B8D9A9FF','indianred3','#0c677e','gold2', 'wheat', 'grey40'), name='Platform', labels=c("Illumina","Ion Torrent","Oxford Nanopore Technology","Sanger","MGI", "Other"))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=12))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme(legend.position="top",legend.box = "horizontal", legend.text= element_text(size=11)) +
  ggtitle('Brazil')

Brazilseq

