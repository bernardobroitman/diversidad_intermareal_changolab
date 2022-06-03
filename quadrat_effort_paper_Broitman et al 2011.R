##########################################################################################
# DATE-SET  1998-2005; Broitman et al., 2011 RCHN
##########################################################################################
# Percentage cover and density data of sessile and mobile organisms in  .25 m2 quadrats respectively. 
# In order to express density in ind * m2, counts of mobile in  0.25 m2 must be multiplied by 4 and counts in 0.01 m2 by 100 .
##########################################################################################

rm(list=ls()) # remove all variables from workspace

# INSTALL NECCESARY PACKAGES

install.packages("pheatmap")

library(pheatmap)

##########################################################################################
# BIOLOGICAL DATA

d<-read.csv("data_diversity_master_1998-2005.csv", sep = ";", dec = ",")#load data
names(d)

# subsets per HEIGHT

M<-subset(d, HEIGHT=="M")
L<-subset(d, HEIGHT=="L")

# HIST PER HEIGHT AND TOTAL

par(mfrow=c(1,3))
par(bty="l")

hist(d$ROW_N, xlab = "Replicate Number", ylab= "Frequency", main = paste("All quadrats 1998-2005"), ylim = c(0,300))
hist(M$ROW_N, xlab = "Replicate Number", ylab= " ", main = paste("Mid quadrats 1998-2005"), ylim = c(0,300))
hist(L$ROW_N, xlab = "Replicate Number", ylab= " ", main = paste("Low quadrats 1998-2005"), ylim = c(0,300))


##########################################################################################
# COUNT NUMBER OF QUADRAT BY SITE AND YEAR

quad <-tapply(d[["ROW_N"]], 
              list(d[["SITE"]], 
                   d[["YEAR"]]), 
              length)

quad[is.na(quad)]<-NA # Replace NA for "0" 

TOTAL<-apply(quad, 1,sum, na.rm=TRUE)# suma los cuadrates para cada sitio

quad<-cbind(quad, TOTAL)# Agrega la columna con el Total

##########################################################################################
# YEARS AND SITE

colnames(quad) = paste(c( "1998", "1999", "2000", "2003", "2004", "2005", "TOTAL"))

rownames(quad) = paste(c( "1 Huasco", "2 Temblador", "3 C Hornos", "4 Arrayan", "5 Guanaqueros", "6 Pta Talca", 
                         
                         "7 Pto Oscuro ", "8 Huentelauquen", "9 Chigualoco", "10 Los Molles","11 Montemar",
                         
                         "12 Curaumilla","13 Quintay", "14 Tunquen ", "15 El Quisco", "16 Pta Tralca-N", "17 Pta Tralca-S",
                         
                         "18 El Tabo", "19 Las Salinas", "20 ECIMN", "21 Las Cruces", "22 Cartagena", "23 Pelancura", 
                         
                         "24 Sn Antonio", "25 Sto Domingo", "26 Matanzas", "27 Pta Lobos","28 Bucalemu","29 Constitucion",
                         
                         "30 Pelluhue", "31 Buchupureo", "32 Coliumo"))

##########################################################################################
# HEAT MAP

pheatmap(quad, color = colorRampPalette(rev(c("#D73027", "#FC8D59", "#FEE090", "#FFFFBF", "#E0F3F8", "#91BFDB")))(100), 
         main = "N° Quad; Broitman et al., 2011", cellwidth = 15, cellheight = 12,  cluster_row = FALSE, 
         cluster_cols = FALSE, display_numbers = TRUE, number_format ="%.0f", 
         border_color = NA)

##########################################################################################