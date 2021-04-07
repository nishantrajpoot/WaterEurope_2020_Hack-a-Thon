DIR_HAUTEUR = "C:/Users/nisha/MACS/First Semester/Water Europe/HydroEurope/flood frequency/" #Chemin d'accès au fichier de données

Rain = read.csv2(file = paste0(DIR_HAUTEUR,"1994_Flood_data.txt"),sep = "",header = FALSE,dec = ".")
Pluie = data.frame(matrix(data=NA, ncol=2, nrow=nrow(Rain), dimnames = list(NULL, c("Date","Debit"))))

Pluie$Date = as.Date (Rain$V1,format = "%m-%d-%Y")
Pluie$Debit = as.numeric(Rain$V2)
plot(x=Pluie$Date, y = Pluie$Debit, main = "Variabilité temporelle des cumuls journaliers de précipitation", xlab = "Temps", ylab = "Cumuls de précipitation journaliers (mm)")
Pluie$mois = format(Pluie$Date,  "%Y/%m")
Cumul = aggregate(x=Pluie$Debit, by=list(Pluie$mois), FUN=max)
colnames(Cumul) = c("mois","Debit")

Cumul$Compteur = c(1:12)
plot(x=Cumul$Compteur, y = Cumul$Debit)
plot(x=Cumul$Compteur, y = Cumul$Debit, main = "Variabilité temporelle des débits maximums Mensuels", xlab = "Temps en Mois depuis 1986", ylab = "Débits Maximums Mensuels (m3/s)")

Tri_H = sort(Cumul$Debit)
#creation varaiable intermediaire
n_an = length(Tri_H)

#creation d'un vecteur rang
Rang = seq (from=1, to=n_an, by=1)

#creation d'un tableau avec des collones nommées +#frequence
tab_H = data.frame(H = Tri_H, Rang = Rang, Freq = ((Rang-0.5)/n_an))# (nom de colonne , variable insérée)

tab_H$U = qnorm(p=tab_H$Freq)


#graphique de GAUSS
plot(x=tab_H$U, y=tab_H$H, main = "Normal", xlab = "Reduced Variate(U)", ylab = "Discharge (m3/s)",xlim=c(-3,4), ylim = c(-10,2000))
moyenne = mean(tab_H$H)
ecarttype = sd(tab_H$H)
Min = min(tab_H$H)
Max = max(tab_H$H)

abline(a=moyenne,b=ecarttype, col="purple", lwd=5)


  #periode de retour

T_ans = c(120,240,600,1200)
tab_calc = data.frame(T_ans = T_ans)
tab_calc$Freq = 1-1/tab_calc$T_ans
tab_calc$U = qnorm(p=tab_calc$Freq)
tab_calc$H = moyenne + ecarttype*tab_calc$U
points (x=tab_calc$U, y=tab_calc$H, pch=15, cex=2, col="red")

