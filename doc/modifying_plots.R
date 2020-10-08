## ---- tidy=TRUE---------------------------------------------------------------
suppressPackageStartupMessages(library(vegan))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggordiplots))
data("dune")
data("dune.env")
dune.hel <- decostand(dune, method = "hellinger")
ord <- rda(dune.hel)
my.plot <- gg_ordiplot(ord, groups = dune.env$Management, hull = TRUE, spiders = TRUE, ellipse = FALSE, plot = FALSE)
names(my.plot)

## ---- tidy=TRUE---------------------------------------------------------------
a.plot <- my.plot$plot
a.plot + 
  theme_bw() +
  labs(color="Management", x="PCA 1", y="PCA 2", title="My Title") +
  theme(plot.title = element_text(hjust = 0.5)) # centers main title, ggplot2 version 2.2+

## ---- tidy=TRUE---------------------------------------------------------------
head(my.plot$df_spiders)

## ---- eval=FALSE, tidy=TRUE---------------------------------------------------
#  # Basic ordination plot:
#  xlab <- paste("Axis", choices[1], sep = " ")
#  ylab <- paste("Axis", choices[2], sep = " ")
#  geom_point(data=df_ord, aes(x=x, y=y, color=Group), size=3) +
#      xlab(xlab) + ylab(ylab)
#  
#  # Plot ellipses:
#  geom_path(data = df_ellipse, aes(x=x, y=y, color=Group), show.legend = FALSE)
#  
#  # Plot centroid labels:
#  geom_text(data=df_mean.ord, aes(x=x, y=y, label=Group, color=Group), show.legend = FALSE)
#  
#  # Plot hulls:
#  geom_path(data=df_hull, aes(x=x, y=y, color=Group), show.legend = FALSE)
#  
#  # Plot spiders:
#  geom_segment(data=df_spiders, aes(x=cntr.x, xend=x, y=cntr.y, yend=y, color=Group), show.legend = FALSE)
#  
#  # Plot cluster segments:
#  geom_segment(data=df_segments, aes(x=x, y=y, xend=xend, yend=yend))

## ---- tidy=TRUE---------------------------------------------------------------
vegan:::veganCovEllipse

## ---- tidy=TRUE---------------------------------------------------------------
ord.data <- my.plot$df_ord
head(ord.data)
ord.data$Use <- dune.env$Use
colnames(ord.data) <- c("x", "y", "Management", "Use")
head(ord.data)
ggplot(data=ord.data, aes(x=x, y=y, color=Management, shape=Use)) +
  geom_point(size=3) + xlab("PCA 1") + ylab("PCA 2")

## ---- tidy=TRUE---------------------------------------------------------------
data("dune")
data("dune.env")
dune.bray <- vegdist(dune, method="bray")
ord <- cmdscale(dune.bray, k=nrow(dune)-1, eig=TRUE, add=TRUE)
cl <- hclust(dune.bray, method="single")
clstr.plot <- gg_ordicluster(ord, cluster=cl,  plot = FALSE)
ellipse.plot <- gg_ordiplot(ord, groups = dune.env$Management, plot = FALSE)
ellipse.plot$plot +
  geom_segment(data=clstr.plot$df_segments, aes(x=x, y=y, xend=xend, yend=yend))

