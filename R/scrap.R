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

