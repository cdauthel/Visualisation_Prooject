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
  geom_bar(stat = "identity", fill = "grey") +
  geom_text(aes(label = Percent), hjust = -0.2, family = "Arial", size = 3) +
  geom_text(data = subset(sort.ipsos, Country %in% c("Germany", "Brazil")),
            aes(label = Country), hjust = 1.2, family = "Arial Black", size = 3) +
  geom_rect(data = data.frame(xmin = c(0, 20, 40, 60, 80), xmax = c(20, 40, 60, 80, 100),
                              ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "skyblue", alpha = 0.2, inherit.aes = FALSE) +
  geom_segment(aes(x = 45, y = -Inf, yend = 20), color = "skyblue3", size = 1.5,
               arrow = arrow(type = "closed", length = unit(0.15, "inches")), xend = 45) +
  geom_text(aes(x = 45, y = 21, label = "Average"), hjust = -0.5, family = "Arial", size = 3) +
  geom_text(aes(x = 45, y = -2, label = "45"), hjust = -0.5, family = "Arial", size = 3) +
  labs(x = "Pourcentage", y = "Pays", title = "'Je crois définitivement en Dieu ou en un Être suprême' dit en 2010 à :") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))

<ipsos.xlsx>
<Exemple-de-code-commenté.pdf>

Indispensable pour faire la version ggplot2...

<07. Comparing ggplot2 and R Base Graphics.pdf>