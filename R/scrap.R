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

## 2018.04.02 #######################################################################

library(viridis)
library(stringr)
library(data.table)
library(survey)
library(broom)

load("./tmp/2017-ispc.RData")

# Update all tables for paper
# Frequencies across segments
svy.farm <- c(gha.svy.farm, eth.svy.farm, tza.svy.farm)
svy.shf <- c(gha.svy.shf, eth.svy.shf, tza.svy.shf)

# Counts
freq <- lapply(svy.farm, function(x) svytable(~rural+croparea_3clas+class5, design=x, round=T))
freq <- lapply(freq, function(x) tidy(ftable(x)))
freq <- rbindlist(freq, idcol="panel")

# Shares, percent
freq.pct <- lapply(svy.farm, function(x) svytable(~rural+croparea_3clas+class5, x, Ntotal=100))
freq.pct <- lapply(freq.pct, function(x) tidy(ftable(x)))
freq.pct <- rbindlist(freq.pct, idcol="panel")

# Verify
freq[, sum(Freq), by=panel]
freq.pct[, sum(Freq), by=panel]

# Export
fwrite(freq, "./out/hh_split_counts.csv")
fwrite(freq.pct, "./out/hh_split_shares.csv")

# Estimate hhld characteristics across segments
vars <- list(
  ~hhsize,
  ~agehead,
  ~I(100*femhead),
  ~I(100*widowhead),
  ~hhlabor,
  ~educhead, ~educave15_60, ~educhigh,
  ~I(100*ownhome), ~I(100*cellphone), ~I(100*telephone), ~I(100*electricity),
  ~distwater, ~distroad, ~distpost, ~distbank, ~disthosptl, ~distmarket,
  ~infraindex_natl, ~tt50k_mean, ~tt100k_mean,

  # Assets
  ~landown,
  #~landshare, ~landrent,
  ~croparea_imp,

  # Income
  ~aggross,
  ~totgross,
  ~I(100*naggross_sh),
  ~cropvalue,
  ~cropsales,
  ~I(100*cropsales_sh),
  ~I(100*shcropsold),
  ~totlvstprod,
  ~totlivsold,

  # Input uses
  ~I(100*seeds), ~I(100*fert_any), ~I(100*fert_inorg), ~I(100*fert_org),
  ~I(100*herb), ~I(100*pest), ~I(100*irr), ~I(100*fuel), ~I(100*hired_labor),

  # Livestock
  ~I(1E3*TLU_horse), ~I(1E3*TLU_cattle), ~I(1E3*TLU_pigs), ~I(1E3*TLU_sheep),
  ~I(1E3*TLU_small), ~I(1E3*TLU_total)

  # Quadrant
  ~suit_glues
  #~ndvi, ~popdens_2015
)

hhchar.mean <- lapply(vars, function(i) {
  print(i)
  lapply(svy.shf,
    function(j) {
      res <- try(svyby(as.formula(i), ~rural+class5, j, svymean, na.rm=T))
      if (class(res)=="try-error") try(svyby(as.formula(i), ~class5, j, svymean, na.rm=T))
      if (class(res)=="try-error") res <- NA
      return(res)
    })})

hhchar.mean <- lapply(hhchar.mean, function(i) lapply(i, function(j) {
  if(is.na(j)) res <- data.table(NA)
  else res <- tidy(ftable(j))
  return(res)
}))

hhchar.mean <- lapply(hhchar.mean, rbindlist, idcol="panel", fill=T)
hhchar.mean <- rbindlist(hhchar.mean, fill=T)
setnames(hhchar.mean, c("Var2", "Var4", "Freq"), c("stat", "varCode", "value"))
hhchar.mean[, varCode := str_replace(varCode, fixed("I(100 * "), "") %>%
    str_replace(fixed("I(1000 * "), "") %>%
    str_replace(fixed(")"), "")]

hhchar.mean[variable=="DIV"]
hhchar.mean[, V1 := NULL]
hhchar.mean[lbl, on="varCode", label := paste0(varLabel, " (", unit, ")")]

# Export
fwrite(hhchar.mean, "./out/hh_char_mean.csv")

# TODO check on input uses

# Redo old quadrant tables, remove the cross-classification, combined with farm typology
varsq <- list(
  ~class5+seg_mkt100k,
  ~class5+seg_suit_gaez,
  ~class5+rural
  #~class5+seg_suit_glues,
  #~class5+seg_ndvi, ~class5+seg_popdens_2015
)

# Counts of SHF (`gha4`` was not georeferenced so omit from tables)
freqq <- lapply(varsq, function(i) lapply(svy.shf[-3], function(j) svytable(as.formula(i), design=j, round=T)))
freqq <- lapply(freqq, function(i) lapply(i, function(j) tidy(ftable(j))))
freqq <- lapply(freqq, rbindlist, fill=T, idcol="panel")
freqq <- lapply(freqq, dcast, formula=...~class5)
file.remove("./out/hh_split_spatial_counts.csv")
lapply(freqq, fwrite, file="./out/hh_split_spatial_counts.csv", append=T, row.names=F)

# Density plots
svyhist(~I(100*shcropsold), svy.shf[[4]], freq=T,
  basecol=function(x) viridis(5)[as.numeric(x$class5)])
svyboxplot(I(100*cropsales_sh)~class5, svy.shf[[4]], col=viridis(5), varwidth=T)
svyboxplot(I(100*shcropsold)~class5, svy.shf[[4]], col=viridis(5), varwidth=T)
svyplot(I(100*naggross_sh)~I(100*shcropsold), svy.shf[[4]],
  style="transparent", alpha=c(0.1, 1), pch=18, ylim=c(0, 100),
  basecol=function(x) viridis(5)[as.numeric(x$class5)],
  main=names(svy.shf)[4],
  xlab="Commercialization", ylab="Non-Farm Income")

n <- c(1:3, 6:8, 4:5)

########
# Scatter plots
png("./out/typo_01_gha.png", width=6, height=2, units="in", res=220, pointsize=7)
par(mfrow=c(1,4))
for(i in 1:3) {
  svyplot(I(100*naggross_sh)~I(100*cropsales_sh), svy.shf[[i]],
    style="transparent", alpha=c(0.2, 1), pch=18, ylim=c(0, 100),
    basecol=function(x) viridis(5)[as.numeric(x$class5)],
    main=names(svy.shf)[i],
    xlab="Commercialization", ylab="Non-Farm Income")
}
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend(1, 1, col=viridis(3), legend=levels(gha$class5), pch=18, bty="n")
dev.off()

png("./out/typo_01_tza.png", width=6, height=2, units="in", res=220, pointsize=7)
par(mfrow=c(1,4))
for(i in 6:8) {
  svyplot(I(100*naggross_sh)~I(100*cropsales_sh), svy.shf[[i]],
    style="transparent", alpha=c(0.2, 1), pch=18, ylim=c(0, 100),
    basecol=function(x) viridis(5)[as.numeric(x$class5)],
    main=names(svy.shf)[i],
    xlab="Commercialization", ylab="Non-Farm Income")
}
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend(1, 1, col=viridis(3), legend=levels(gha$class5), pch=18, bty="n")
dev.off()

png("./out/typo_01_eth.png", width=6, height=2, units="in", res=220, pointsize=7)
par(mfrow=c(1,3))
for(i in 4:5) {
  svyplot(I(100*naggross_sh)~I(100*shcropsold), svy.shf[[i]],
    style="transparent", alpha=c(0.2, 1), pch=18, ylim=c(0, 100),
    basecol=function(x) viridis(5)[as.numeric(x$class5)],
    main=names(svy.shf)[i],
    xlab="Commercialization", ylab="Non-Farm Income")
}
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend(1, 1, col=viridis(3), legend=levels(gha$class5), pch=18, bty="n")
dev.off()

############
# Frequency tables
tb <- lapply(n, function(x) {
  p <- svytable(~naggross_3clas+cropsales_3clas, svy.shf[[x]], Ntotal=100)
  p <- ftable(addmargins(p))
  write(names(svy.shf)[x], "./out/test.csv", append=T)
  write.ftable(p, "./out/test.csv", append=T, digits=0, method="compact", quote=F,
    sep="\t", lsep="\t")
})



prop.table(svytable(~naggross_3clas+cropsales_3clas, gha.svy[["gha5"]], Ntotal=100),
  margin=2) * 100
prop.table(svytable(~naggross_3clas+cropsales_3clas, gha.svy[["gha5"]], Ntotal=100)) * 100



########
f <- list(
  `Commercialization` = I(100*cropsales_sh)~class5,
  `Non-Farm Income` = I(100*naggross_sh)~class5,
  `Time to 50K market` = tt50k_mean~class5
)

svyplot(as.formula(f[[j]]), svy.shf[[i]],
  style="bubble", alpha=c(0.8, 1),  basecol=function(x) viridis(5)[as.numeric(x$class5)],
  xlab = "Age", ylab = "Left-Right Scale",
  main = "Political Preferences and Age")

# Boxplots
png("./out/typo_02_gha.png", width=7, height=7, units="in", res=220, pointsize=7)
par(mfrow=c(3,3))
for(j in seq_along(f)) for(i in 1:3) svyboxplot(as.formula(f[[j]]), svy.shf[[i]],
    varwidth=T, alpha=0.8, ylim=if(j<3) c(0, 100) else c(0,8), col=viridis(5),
    main=names(svy.shf)[i],
    xlab=names(f)[j])
dev.off()

png("./out/typo_02_tza.png", width=7, height=7, units="in", res=220, pointsize=7)
par(mfrow=c(3,3))
for(j in seq_along(f)) for(i in 6:8) svyboxplot(as.formula(f[[j]]), svy.shf[[i]],
  varwidth=T, alpha=0.8, ylim=if(j<3) c(0, 100) else c(0,8), col=viridis(5),
  main=names(svy.shf)[i],
  xlab=names(f)[j])
dev.off()

png("./out/typo_02_eth.png", width=7, height=7, units="in", res=220, pointsize=7)
par(mfrow=c(3,2))
for(j in seq_along(f)) for(i in 4:5) svyboxplot(as.formula(f[[j]]), svy.shf[[i]],
  varwidth=T, alpha=0.8, ylim=if(j<3) c(0, 100) else c(0,8), col=viridis(5),
  main=names(svy.shf)[i],
  xlab=names(f)[j])
dev.off()


############
# Weighted density plots
dt <- list(Ghana=gha, Tanzania=tza, Ethiopia=eth)
par(mfrow=c(1,3))
for(i in seq_along(dt)) {
  plot(0, 0, xlim=c(-1,100), ylim=c(0,0.08), pch=NA, xlab=NA, ylab=NA)
  dt[[i]][!is.na(cropsales_sh), polygon(
    density(100*cropsales_sh, weights=weight/sum(weight), from=0, to=100),
    lwd=0.1, type="l", col=viridis(3)[.GRP], alpha=.4), by=survey]
  legend("topright", levels(dt[[i]]$survey), col=viridis(3), lty=1, lwd=2, bty="n", cex=.8)
  title(xlab=names(dt)[i])
  if(i==1) {
    title(main="Crop Commercialization", adj=0, font.main=1)
    title(main="\n\n\nDensity across survey waves", adj=0, cex.main=.9)
  }
}

# Maps
png("./out/map_ndvi.png", width=7, height=3.8,  units="in", res=220, pointsize=11)
tm_shape(rndvi) + tm_raster(pal=pal.ndvi(255), n=19,
  title="Long-term\nNDVI") +
  tm_shape(g2[g2$svyCode %in% i,], is.master=T) + tm_borders(alpha=.1) +
  tm_facets("ISO3", ncol=3, free.coords=T)
dev.off()

png("./out/map_popdens.png", width=7, height=3.8, units="in", res=220, pointsize=11)
tm_shape(rpop) + tm_raster(pal="YlOrBr",
  breaks=c(0,5,10,15,20,30,40,60,80,100,140000),
  labels=c("0-5","5-10","10-15","15-20","20-30","30-40","40-60","60-80", "80-100", "> 100"),
  title="WorldPop\nPopulation\nDensity\n(2015)") +
  tm_shape(g2[g2$svyCode %in% i,], is.master=T) + tm_borders(alpha=.1) +
  tm_facets("ISO3", ncol=3, free.coords=T)
dev.off()


# Logit regression
m1 <- svyolr(class5~tt50k_mean+distroad+agehead+croparea_imp+TLU_total,
    svy.farm[[1]], method="l")
summary(m1)
regTermTest(m1, ~distroad, method="LRT")
