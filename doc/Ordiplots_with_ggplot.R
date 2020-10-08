## ---- eval=FALSE, tidy=TRUE---------------------------------------------------
#  gg_ordiplot(ord, groups, scaling = 1, choices = c(1, 2), kind = c("sd",
#    "se", "ehull"), conf = NULL, show.groups = "all", ellipse = TRUE,
#    label = FALSE, hull = FALSE, spiders = FALSE, plot = TRUE)

## ---- tidy=TRUE---------------------------------------------------------------
suppressPackageStartupMessages(library(vegan))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggordiplots))
data("dune")
data("dune.env")
dune.hel <- decostand(dune, method = "hellinger")
ord <- rda(dune.hel)
gg_ordiplot(ord, groups = dune.env$Management, pt.size = 3)

## ---- tidy=TRUE---------------------------------------------------------------
gg_ordiplot(ord, groups = dune.env$Management, pt.size = 3, kind = "se")

## ---- tidy=TRUE---------------------------------------------------------------
gg_ordiplot(ord, groups = dune.env$Management, kind = "se", conf=0.95, pt.size = 3)

## ---- tidy=TRUE---------------------------------------------------------------
gg_ordiplot(ord, groups = dune.env$Management, choices=c(2,3), show.groups = c("BF", "HF"),  kind = "se", pt.size = 3, conf=0.95)

## ---- tidy=TRUE---------------------------------------------------------------
gg_ordiplot(ord, groups = dune.env$Management, hull = TRUE, label = TRUE, spiders = TRUE, ellipse = FALSE, pt.size = 3, plot = TRUE)

## ---- eval=FALSE, tidy=TRUE---------------------------------------------------
#  gg_envfit(ord, env, groups = NA, scaling = 1, choices = c(1, 2),
#    perm = 999, alpha = 0.05, angle = 20, len = 0.5, unit = "cm",
#    arrow.col = "red", pt.size = 3, plot = TRUE)

## ---- tidy=TRUE---------------------------------------------------------------
data(varespec)
data(varechem)
vare.dist <- vegdist(varespec, method="bray")
set.seed(123)
vare.mds <- monoMDS(vare.dist)
set.seed(123)
gg_envfit(ord=vare.mds, env=varechem, perm = 9999, pt.size = 2, alpha = 0.2)

## ---- tidy=TRUE---------------------------------------------------------------
set.seed(123)
envfit(vare.mds, env=varechem, permutations = 9999)

## ---- tidy=TRUE---------------------------------------------------------------
gg_ordisurf(ord = vare.mds, env.var=varechem$Al, binwidth = 20, pt.size = 1, var.label = "Aluminum")
gg_ordibubble(ord = vare.mds, env.var=varechem$Al, var.label = "Aluminum")

## ---- tidy=TRUE---------------------------------------------------------------
gg_ordisurf(ord = vare.mds, env.var=varechem$Baresoil, binwidth = 5,  var.label = "Bare Soil")
gg_ordibubble(ord = vare.mds, env.var=varechem$Baresoil, var.label = "Bare Soil")

## ---- tidy=TRUE---------------------------------------------------------------
gg_ordibubble(ord = vare.mds, env.var=varechem$Mo, var.label = "Molybdenum")

## ---- eval=FALSE, tidy=TRUE---------------------------------------------------
#  gg_ordicluster(ord, cluster, treatments = NA, choices = c(1, 2),
#    prune = 0, col = 1, plot = TRUE)

## ---- tidy=TRUE---------------------------------------------------------------
data("dune")
dune.bray <- vegdist(dune, method="bray")
ord <- cmdscale(dune.bray, k=nrow(dune)-1, eig=TRUE, add=TRUE)
cl <- hclust(dune.bray, method="single")
gg_ordicluster(ord, cluster=cl)

## ---- tidy=TRUE---------------------------------------------------------------
data(dune.env)
cl <- hclust(dune.bray, method="complete")
gg_ordicluster(ord, treatments = dune.env$Management, cluster = cl, prune = 3, col = cutree(cl, 4), pt.size = 2)

