setwd("/Users/alex/Desktop/TD_REGRESSION")

#install.packages("sf")
library(sf)
library(ggplot2)

GRD <- st_read("PACA_carroyage_1km.gpkg", layer = "filosofi_2015_1km")

#Carte Ind ( densité pop)
ggplot(data = GRD) +
  geom_sf(aes(fill = Ind), size = 0) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") 

# Regression lineaire Revenus surface

GRD_2 <- GRD[c("Ind_snv", "Men_surf")]


eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

ggplot(GRD_2, aes(x=Ind_snv, y=Men_surf)) + geom_point() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y ~ x) +
  geom_text(x = 20, y = 40000, label = eq(GRD_2$Ind_snv,GRD_2$Men_surf), parse = TRUE)

