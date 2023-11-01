Un peu de code à commenter…

1. Un markdown par personne
2. Des étapes bien différenciées
3. Une construction qui apparait progressivement
4. Un bilan des fonctions et de la façon dont le code a été produit
5. Une version ggplot2

setwd("~/Desktop/ACO/M2/Data_Science/Représentation de données hétérogènes")

library(gdata)
library(openxlsx)
ipsos<-read.xlsx("ipsos.xlsx")
sort.ipsos<-ipsos[order(ipsos$Percent) ,]
attach(sort.ipsos)
library(Cairo)
pdf_file<-"barcharts_simple.pdf"
cairo_pdf(bg="grey98", pdf_file,width=9,height=6.5)
par(omi=c(0.65,0.25,0.75,0.75),mai=c(0.3,2,0.35,0),mgp=c(3,3,0),
	family="Arial", las=1)  

x<-barplot(Percent,names.arg=F,horiz=T,border=NA,xlim=c(0,100),col="grey", cex.names=0.85,axes=F)
for (i in 1:length(Country)) {
  if (Country[i] %in% c("Germany","Brazil")) {
    myFont<-"Arial Black"
  }
  else {
    myFont<-"Arial"
  }
  text(-8,x[i],Country[i],xpd=T,adj=1,cex=0.85,family=myFont)
  text(-3.5,x[i],Percent[i],xpd=T,adj=1,cex=0.85,family=myFont)
}
rect(0,-0.5,20,28,col=rgb(191,239,255,80,maxColorValue=255),border=NA)
rect(20,-0.5,40,28,col=rgb(191,239,255,120,maxColorValue=255),border=NA)
rect(40,-0.5,60,28,col=rgb(191,239,255,80,maxColorValue=255),border=NA)
rect(60,-0.5,80,28,col=rgb(191,239,255,120,maxColorValue=255),border=NA)
rect(80,-0.5,100,28,col=rgb(191,239,255,80,maxColorValue=255),border=NA)
myValue2<-c(0,0,0,0,27,0,0,0,0,0,0,0,0,84,0,0)
myColour2<-rgb(255,0,210,maxColorValue=255)
x2<-barplot(myValue2,names.arg=F,horiz=T,border=NA,xlim=c(0,100),col=myColour2,cex.names=0.85,axes=F,add=T)
arrows(45,-0.5,45,20.5,lwd=1.5,length=0,xpd=T,col="skyblue3") 
arrows(45,-0.5,45,-0.75,lwd=3,length=0,xpd=T)
arrows(45,20.5,45,20.75,lwd=3,length=0,xpd=T)
text(41,20.5,"Average",adj=1,xpd=T,cex=0.65,font=3)
text(44,20.5,"45",adj=1,xpd=T,cex=0.65,family="Arial",font=4)
text(100,20.5,"All values in percent",adj=1,xpd=T,cex=0.65,font=3)
mtext(c(0,20,40,60,80,100),at=c(0,20,40,60,80,100),1,line=0,cex=0.80)
mtext("'I Definitely Believe in God or a Supreme Being'",3,line=1.3,adj=0,cex=1.2,family="Arial Black",outer=T)
mtext("was said in 2010 in:",3,line=-0.4,adj=0,cex=0.9,outer=T)
mtext("Source: www.ipsos-na.com, Design: Stefan Fichtel, ixtract",1,line=1,adj=1.0,cex=0.65,outer=T,font=3)
dev.off()




# Chargement de la bibliothèque ggplot2
library(ggplot2)

# Lecture des données
ipsos <- read.xlsx("ipsos.xlsx")

# Trie des données
sort.ipsos <- ipsos[order(ipsos$Percent), ]

# Création du graphique à l'aide de ggplot2
ggplot(data = sort.ipsos, aes(x = Percent, y = reorder(Country, Percent))) +
  geom_bar(stat = "identity", fill = c(rep("grey", 4), "#FF00D2", rep("grey", 8), "#FF00D2", rep("grey", 2)), width = 0.85) +
  geom_rect(data = data.frame(xmin = c(0, 40, 80), xmax = c(20, 60, 100),
                              ymin = 0, ymax = 17),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "LightSkyBlue1", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(data = data.frame(xmin = c(20, 60), xmax = c(40, 80),
                              ymin = 0, ymax = 17),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "LightSkyBlue1", alpha = 0.45, inherit.aes = FALSE) +
  scale_y_discrete(aes(labels = paste(Country, Percent))) +
  geom_text(data = subset(sort.ipsos, !(Country %in% c("Germany", "Brazil"))), aes(x = -16, y = reorder(Country, Percent), label = paste(Country, Percent)), hjust = 0, family = "Arial", size = 4) +
  geom_text(data = subset(sort.ipsos, Country %in% c("Germany", "Brazil")), aes(x = -16, y = reorder(Country, Percent), label = paste(Country, Percent)), hjust = 0, family = "Arial Black", size = 4) +
  
  geom_segment(aes(x = 45, y = 0, yend = 17.5), color = "skyblue3", size = 0.7, xend = 45) +
  geom_segment(aes(x = 45, y = -0.2, yend = 0.15), color = "black", size = 0.9, xend = 45) +
  geom_segment(aes(x = 45, y = 17.15, yend = 17.5), color = "black", size = 0.9, xend = 45) +
  
  
  geom_text(aes(x = 40, y = 17.4, label = "Average"), hjust = 1, family = "Arial", size = 3, fontface = "italic") +
  geom_text(aes(x = 44, y = 17.4, label = "45"), hjust = 1, family = "Arial Black", size = 3) +
  geom_text(aes(x = -20, y = 20, label = "'I Definitely Believe in God or a Supreme Being'"), hjust = 0, family = "Arial Black", size = 3, fontface = "bold") +
  geom_text(aes(x = -20, y = 19, label = "was said in 2010 in:"), hjust = 0, family = "Arial", size = 3) +
  geom_text(aes(x = 85, y = 17.4, label = "All values in percent"), hjust = 0, family = "Arial", size = 3, fontface = "italic") +
  geom_text(aes(x = 72, y = -1, label = "Source: www.ipsos-na.com, Design: Stefan Fichtel, ixtract"), hjust = 0, family = "Arial", size = 2, fontface = "italic") +
  labs(x = element_blank(), y = element_blank(), title = element_blank()) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100))
