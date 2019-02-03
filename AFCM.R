
library(FactoMineR)

library(explor)
#importer le jeux de données

data(hobbies)


#supprimer les valeurs null

colnames(hobbies)[unlist(lapply(hobbies, function(x) any(is.na(x))))]
data <- na.omit(hobbies)

#applique l'AFCM


res.mca <- MCA(data ,,quali.sup=19:22,quanti.sup=23)

summary(res.mca,ncp =1:2,nbelements =8)

#********Visualiser le pourcentage de variances expliquées par chaque axe principal « Hobies » :********

eig.val <- res.mca$eig
x11():barplot(eig.val[, 2],
        names.arg = 1:nrow(eig.val),
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2],
      type = "b", pch = 19, col = "red")

#***************Description des axe****************************

dimdesc(res.mca ,axes = 1:2,proba=1e-05) #aide à intrepreter les axes

#***************les graphes****************************

          #****1) modalité************

x11():plot(res.mca, invisible=c("ind","quali.sup"),autoLab="y",cex=0.7,selectMod="cos2 12")

x11():plot(res.mca, invisible=c("ind","quali.sup"),autoLab="y",cex=0.7,selectMod="contrib 12")

          #****2)   ind   ************

x11():plot(res.mca, invisible=c("var","quali.sup"),autoLab="y",cex=0.7,select="cos2 150",unselect = 1,shadowtext = TRUE)

          #****4)   ind+modalité  ************
x11():plot(res.mca, autoLab="y",cex=0.7, select="cos2 150", selectMod="cos2 12",unselect = 1)

      

#*****Ellipses de confiance autour de modalités****

plotellipses(res.mca,keepvar = c("Gardening","Cinema","Exhibition","Show","Travelling","Mechanic"))
