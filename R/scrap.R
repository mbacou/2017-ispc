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

svyCrossTab <- function(f, by, design, quantile="Q50") {

  # Pass a list of formulas (list(~var1, ~var2, ...))
  # Then use tables::tabular() to format pivot tables

  require(survey)
  require(data.table)
  require(stringr)

  # Reshape means
  m <- lapply(f, function(x) try(
    svyby(x, by, design, svymean, na.rm=TRUE) %>%
      ftable() %>% as.data.table())
  )
  m <- m[sapply(m, function(x) class(x)[1])!="try-error"]
  m <- lapply(m, function(x) {
    setnames(x, names(x)[names(x) %like% "Var"], c("Stat", "Variable"))
    x[Stat=="svymean", Stat := "Mean"]
  })
  res <- rbindlist(m)

  # Reshape quantiles
  if(!is.na(quantile)) {
    m <- lapply(f, function(x) try(
      svyby(x, by, design, svyquantile, quantiles, ci=TRUE, na.rm=TRUE) %>%
        ftable %>% as.data.table())
    )
    m <- m[sapply(m, function(x) class(x)[1])!="try-error"]
    m <- lapply(m, function(x) {
      n <- ncol(x)
      setnames(x, names(x)[names(x) %like% "Var"], c("Stat", "Variable"))
      x[Stat=="svyquantile", Stat := "Q50"]
      x[Stat=="SE", Stat := "SEQ50"]
    })
    m <- rbindlist(m)
    res <- rbind(res, m)
  }

  # Add variable labels
  res <- dcast(res, ...~Stat, value.var="Freq")
  res[, Variable := str_replace(Variable, fixed("I(1000 * "), "") %>%
      str_replace(fixed("I(100 * "), "") %>% str_replace(fixed(")"), "")]
  setkey(res, Variable)
  setkey(lbl, varCode)
  res[lbl, Label := i.varLabel]
  return(res)
}

svyCrossProp <- function(x, design) {
  if(length(x)==1) f <- formula(paste0("~", x))
  if(length(x) > 1) f <- formula(paste0("~I(interaction(", paste(x, collapse=", "), "))"))
  res <- svymean(f, design, na.rm=TRUE)
  rn <- row.names(ftable(res)) %>%
    str_replace(fixed(as.character(f)[2]), "") %>% str_split(fixed(".")) %>%
    as.data.table() %>% t()
  res.cv <- confint(res)
  res <- as.data.table(cbind(res*100, res.cv*100))
  setnames(res, c("mean", "CI1", "CI2"))
  res[CI1<0, CI1 := 0]
  res <- cbind(rn, res)
  res[CI1 < CI1, CI1 := 0]
  res[, `:=`(
    mean = round(mean, digits=2),
    CI1 = round(CI1, digits=1),
    CI2 = round(CI2, digits=1)
  )]
  res[, `:=`(
    CI = paste(CI1, CI2, sep=" - "),
    CI1 = NULL,
    CI2 = NULL
  )]
  setnames(res, seq_along(x), x)
  for (i in x) res[[i]] <- factor(res[[i]], levels=levels(gha[[i]]))
  return(res)
}


# Export by wave, class5, rural, quadrants, farm sizes, and produce marginal stats
res <- svyCrossProp(c("croparea_3clas", "rural", "class5"), gha.svy.farm[["gha6"]])
res <- copy(res)
res[, svyCode := "gha6"]

for(i in list(gha.svy.farm[-1], eth.svy.farm, tza.svy.farm)) for(j in names(i)) {
  tmp <- svyCrossProp(c("croparea_3clas", "rural", "class5"), i[[j]])
  tmp <- copy(tmp)
  tmp[, svyCode := j]
  res <- rbind(res, tmp)
}
fwrite(res, "./out/hh_splits.csv", append=TRUE)


tmp <- svyCrossProp(c("croparea_3clas", "rural", "class5"), tza.svy.farm[["tza3"]])

tmp <- svyCrossTab(list(~croparea_imp, ~rural, ~class5), by=~croparea_4ha, eth.svy.farm[["eth2"]])
tmp <- svyCrossTab(list(~croparea_imp), by=~croparea_4ha, eth.svy.farm[["eth2"]])


