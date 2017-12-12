## 2017.10.24 #######################################################################

library(raster)

xy = cbind(
  x = c(13.4, 13.4, 13.6, 13.6, 13.4),
  y = c(48.9, 49, 49, 48.9, 48.9)
)
hole.xy <- cbind(
  x = c(13.5, 13.5, 13.45, 13.45, 13.5),
  y = c(48.98, 48.92, 48.92, 48.98, 48.98)
)

xy.sp <- SpatialPolygons(list(
  Polygons(list(Polygon(xy),
    Polygon(hole.xy, hole = TRUE)), "1"),
  Polygons(list(Polygon(xy + 0.2),
    Polygon(xy + 0.35),
    Polygon(hole.xy + 0.2, hole = TRUE)), "2")
))

r <- raster(nrow=100, ncol=100, ext=extent(xy.sp), resolution=0.01)
r[] <- runif(ncell(r))

plot(xy.sp,col="grey")
plot(r,add=T,alpha=0.5)

sampleRandom(mask(r, xy.sp),size = 10)
xy.r <- extract(r, xy.sp)
xy.r.sample <- lapply(xy.r, sample, 10)

## 2017.12.05 #######################################################################





# Export by wave, class5, rural, quadrants, farm sizes, and produce marginal stats
res <- svyCrossProp(c("croparea_3clas", "rural", "class5"), gha.svy.farm[["gha6"]])
res <- copy(res)
res[, svyCode := "gha6"]

#####################################################################################
# Mean proportions
for(i in list(gha.svy.farm[-1], eth.svy.farm, tza.svy.farm)) for(j in names(i)) {
  tmp <- svyCrossProp(c("croparea_3clas", "rural", "class5"), i[[j]])
  tmp <- copy(tmp)
  tmp[, svyCode := j]
  res <- rbind(res, tmp)
}
fwrite(res, "./out/hh_splits.csv")

#####################################################################################
# Counts
res <- svyCrossCount(c("seg_quad", "class5"), gha.svy.shf[["gha6"]])
res[, svyCode := "gha6"]

for(i in list(gha.svy.shf[2:3], eth.svy.shf, tza.svy.shf)) for(j in names(i)) {
  tmp <- svyCrossCount(c("seg_quad", "class5"), i[[j]])
  tmp[, svyCode := j]
  res <- rbind(res, tmp)
}
fwrite(res, "./out/hh_split_counts.csv")

#####################################################################################
# Rural/urban Counts
res <- svyCrossCount(c("rural", "class5"), gha.svy.shf[["gha6"]])
res[, svyCode := "gha6"]

for(i in list(gha.svy.shf[-1], eth.svy.shf, tza.svy.shf)) for(j in names(i)) {
  tmp <- svyCrossCount(c("rural", "class5"), i[[j]])
  tmp[, svyCode := j]
  res <- rbind(res, tmp)
}
fwrite(res, "./out/hh_split_rural_counts.csv")

tabular(Factor(svyCode)*seg_quad~class5*Percent(seg_quad), data=res)



tmp <- svyCrossProp(c("croparea_3clas", "rural", "class5"), tza.svy.farm[["tza3"]])

tmp <- svyCrossTab(list(~croparea_imp, ~rural, ~class5), by=~croparea_4ha, eth.svy.farm[["eth2"]])
tmp <- svyCrossTab(list(~croparea_imp), by=~croparea_4ha, eth.svy.farm[["eth2"]])


tmp <- g2[g2$svyCode=="gha-glss6",]
tm_shape(crop(mask(rquad, tmp), tmp)) + tm_raster(pal=viridis(4), n=4, labels=quad,
  title="Agricultural\nPotential\nQuadrants, \nGhana") +
  tm_layout(legend.outside=T)

spplot(crop(mask(rquad, tmp), tmp))


