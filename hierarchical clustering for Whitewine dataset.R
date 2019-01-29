install.packages("gclus")
library("gclus")
install.packages("cluster")
library("cluster")
install.packages("seriation")
require("seriation")
install.packages("corrplot")
require("corrplot")
install.packages("car")
require("car")
install.packages("corrgram")
library("corrgram")
require(corrgram)
install.packages("TSP")
library("TSP")
require(TSP)

Wine.all<-Whitewine
Whitewine<-Wine.all[-12]

#Preprocessing

str(Whitewine) #Preprocessing
summary(Whitewine)
require(corrgram)
corrgram(Wine.all, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="Whitewine quality data in pc2/pc1 order")

names(Whitewine)

#Clustering Part 2

d <- dist(Whitewine, method = "euclidean")#Clustering Part 2
fit <- hclust(d, method="complete") 
print(fit)
plot(fit, main="hierarchical clustering for Whitewine dataset")
rect.hclust(fit, k=2, border="red")
groups <- cutree(fit, k=2) 
table(groups)
round(aggregate(Whitewine, FUN = mean, by = list(groups)), 1)

head(Whitewine)
Whitewine2 <- Wine.all[,-12]
d_Whitewine <- dist(Whitewine2) 
hc_Whitewine <- hclust(d_Whitewine, method = "complete")
print(hc_Whitewine)
Whitewine_quality <- rev(levels(Wine.all[,12]))

install.packages("dendextend")
library("dendextend")
require(dendextend)
dend <- as.dendrogram(hc_Whitewine)
dend
dend <- rotate(dend, 1:4898)

library("colorspace")
require(colorspace)
dend <- color_branches(dend, k=2)
dend
plot(dend)
labels_colors(dend)<-rainbow_hcl(12)[sort_levels_values(as.numeric(Wine.all[,12])[order.dendrogram(dend)])]
labels(dend) <- paste(as.character(Wine.all[,12])[order.dendrogram(dend)], "(",labels(dend),")", sep = "")
dend <- hang.dendrogram(dend,hang_height=0.1)
dend 
dend <- set(dend, "labels_cex", 0.5)
dend

plot(dend, 
     main = "Clustered Whitewine data set
     (the labels give the true flower species)", 
     horiz =  TRUE,  nodePar = list(cex = .007))
legend("topleft", legend = Whitewine_quality, fill = rainbow_hcl(13))


some_col_func <- function(n) rev(colorspace::heat_hcl(n, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5)))

install.packages("gplots")
library("gplots")
require(gplots)

gplots::heatmap.2(as.matrix(Whitewine2), 
                  main = "Heatmap for the Whitewine data set",
                  srtCol = 20,
                  dendrogram = "row",
                  Rowv = dend,
        Colv = "NA", # this to make sure the columns are not ordered
        trace="none",          
        margins =c(5,0.1),      
        key.xlab = "Cm",
        denscol = "grey",
        density.info = "density",
        RowSideColors = rev(labels_colors(dend)), col = some_col_func
)


Whitewine_dendlist <- dendlist()
for(i in seq_along(hclust_methods)) {
  hc_Whitewine <- hclust(d_Whitewine, method = hclust_methods[i])   
  Whitewine_dendlist <- dendlist(Whitewine_dendlist, as.dendrogram(hc_Whitewine))
}

names(Whitewine_dendlist) <- hclust_methods
Whitewine_dendlist

Wine_dendlist_cor<-cor.dendlist(Whitewine_dendlist)
Wine_dendlist_cor
corrplot::corrplot(Wine_dendlist_cor)
