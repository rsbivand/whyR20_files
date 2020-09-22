td <- tempfile()
dir.create(td)
Sys.setenv("PROJ_USER_WRITABLE_DIRECTORY"=td)


## ---- echo = TRUE, mysize=TRUE, size='\\tiny', results='hide', warning=FALSE----
suppressPackageStartupMessages(library(osmdata))
library(sf)

## ---- cache=TRUE, echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny', results='hide', warning=FALSE----
bbox <- opq(bbox = 'bergen norway')
byb0 <- osmdata_sf(add_osm_feature(bbox, key = 'railway',
 value = 'light_rail'))$osm_lines
tram <- osmdata_sf(add_osm_feature(bbox, key = 'railway',
 value = 'tram'))$osm_lines
byb1 <- tram[!is.na(tram$name),]
o <- intersect(names(byb0), names(byb1))
byb <- rbind(byb0[,o], byb1[,o])

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'-----------------------
library(mapview)
mapviewOptions(fgb = FALSE)
mapview(byb)


## ----plot1, cache=TRUE, echo=FALSE, eval=FALSE, results='hide', dev.args=list(family="Fira Sans", bg="transparent"), warning=FALSE----
## library(mapview)
## if (sf:::CPL_gdal_version() >= "3.1.0") mapviewOptions(fgb = FALSE)
## x <- mapview(byb)
## if (!dir.exists("bivand_200927_files")) dir.create("bivand_200927_files")
## if (!dir.exists("bivand_200927_files/figure-beamer")) dir.create("bivand_200927_files/figure-beamer")
## mapshot(x, file="plot1-1.png")
## file.copy("plot1-1.png", "bivand_200927_files/figure-beamer")
## unlink("plot1-1.png")


## ---- echo = TRUE, eval=FALSE, mysize=TRUE, cache=TRUE, size='\\tiny'-----------
## bike_fls <- list.files("bbs")
## trips0 <- NULL
## for (fl in bike_fls) trips0 <- rbind(trips0,
##   read.csv(file.path("bbs", fl), header=TRUE))
## trips0 <- trips0[trips0[, 8] < 6 & trips0[, 13] < 6,]
## trips <- cbind(trips0[,c(1, 4, 2, 9)], data.frame(count=1))
## from <- unique(trips0[,c(4,5,7,8)])
## names(from) <- substring(names(from), 7)
## to <- unique(trips0[,c(9,10,12,13)])
## names(to) <- substring(names(to), 5)
## stations0 <- st_as_sf(merge(from, to, all=TRUE),
##   coords=c("station_longitude", "station_latitude"))
## stations <- aggregate(stations0, list(stations0$station_id),
##   head, n=1)
## suppressWarnings(stations <- st_cast(stations, "POINT"))
## st_crs(stations) <- 4326
## od <- aggregate(trips[,-(1:4)], list(trips$start_station_id,
##   trips$end_station_id), sum)
## od <- od[-(which(od[,1] == od[,2])),]
## library(stplanr)
## od_lines <- od2line(flow=od, zones=stations, zone_code="Group.1",
##   origin_code="Group.1", dest_code="Group.2")
## Sys.setenv(CYCLESTREET="XxXxXxXxXxXxXxXx")
## od_routes <- line2route(od_lines, plan = "fastest")


## ----plot3, cache=TRUE, echo=FALSE, eval=FALSE, results='hide', dev.args=list(family="Fira Sans", bg="transparent"), warning=FALSE----
## od_lines <- readRDS("od_lines.rds")
## x <- mapview(od_lines, alpha=0.2, lwd=(od_lines$x/max(od_lines$x))*10)
## mapshot(x, file="plot3-1.png")
## file.copy("plot3-1.png", "bivand_200927_files/figure-beamer")
## unlink("plot3-1.png")


## ----plot4, cache=TRUE, echo=FALSE, eval=FALSE, results='hide', dev.args=list(family="Fira Sans", bg="transparent"), warning=FALSE----
## od_routes <- readRDS("od_routes.rds")
## x <- mapview(od_routes, alpha=0.2, lwd=(od_lines$x/max(od_lines$x))*10)
## mapshot(x, file="plot4-1.png")
## file.copy("plot4-1.png", "bivand_200927_files/figure-beamer")
## unlink("plot4-1.png")


## ---- echo=TRUE, mysize=TRUE, size='\\tiny'-------------------------------------
suppressPackageStartupMessages(library(spatstat))
intenfun <- function(x, y) 200 * x
set.seed(1)
(ihpp <- rpoispp(intenfun, lmax = 200))


## ----sa1, cache=TRUE, echo=FALSE, eval=TRUE, results='hide', dev.args=list(family="Fira Sans", bg="transparent"), warning=FALSE----
plot(density(ihpp), axes=TRUE)
points(ihpp, col="green2", pch=19)


## ----sa2, cache=TRUE, echo=FALSE, eval=TRUE, results='hide', dev.args=list(family="Fira Sans", bg="transparent"), warning=FALSE----
opar <- par(mfrow=c(1,2))
plot(envelope(ihpp, Kest, verbose=FALSE), main="Homogeneous")
plot(envelope(ihpp, Kinhom, verbose=FALSE), main="Inhomogeneous")
par(opar)


## ---- echo=TRUE, mysize=TRUE, size='\\tiny'-------------------------------------
library(sf)
st_as_sf(ihpp) %>% dplyr::filter(label == "point") -> sf_ihpp
crds <- st_coordinates(sf_ihpp)
sf_ihpp$x <- crds[,1]
sf_ihpp$y <- 100 + 50 * sf_ihpp$x + 20 * rnorm(nrow(sf_ihpp))


## ----sa3, cache=TRUE, echo=FALSE, eval=TRUE, results='hide', dev.args=list(family="Fira Sans", bg="transparent"), warning=FALSE----
plot(sf_ihpp[,"y"], pch=19)


## ---- echo=TRUE, mysize=TRUE, size='\\tiny'-------------------------------------
suppressPackageStartupMessages(library(gstat))
vg0 <- variogram(y ~ 1, sf_ihpp)
vg1 <- variogram(y ~ x, sf_ihpp)


## ----sa4, cache=TRUE, echo=FALSE, eval=TRUE, results='hide', dev.args=list(family="Fira Sans", bg="transparent"), warning=FALSE, message=FALSE----
library(ggplot2)
g0 <- ggplot(vg0, aes(x=dist, y=gamma)) + geom_point() + geom_smooth(span=1) + ylim(300, 575) + ggtitle("Trend ignored")
g1 <- ggplot(vg1, aes(x=dist, y=gamma)) + geom_point() + geom_smooth(span=1) + ylim(300, 575) + ggtitle("Trend included")
gridExtra::grid.arrange(g0, g1, ncol=2)


## ---- echo=TRUE, results='hide', message=FALSE, mysize=TRUE, size='\\tiny'------
suppressPackageStartupMessages(library(spdep))
nb_tri <- tri2nb(crds)
nb_soi <- graph2nb(soi.graph(nb_tri, crds), sym=TRUE)

## ---- echo=TRUE, mysize=TRUE, size='\\tiny'-------------------------------------
comps <- n.comp.nb(nb_soi)
sf_ihpp$comps <- comps$comp.id
comps$nc


## ----sa5, cache=TRUE, echo=FALSE, eval=TRUE, results='hide', dev.args=list(family="Fira Sans", bg="transparent"), warning=FALSE----
plot(nb_soi, crds)
points(crds, col=sf_ihpp$comps, pch=16)


## ---- echo=TRUE, warning=FALSE, message=FALSE, mysize=TRUE, size='\\tiny'-------
lwB <- nb2listw(nb_soi, style="B")
moran.test(sf_ihpp$y, listw=lwB, randomisation=FALSE, alternative="two.sided")


## ---- echo=TRUE, mysize=TRUE, size='\\tiny'-------------------------------------
lm_obj <- lm(y ~ x, data=sf_ihpp)
lm.morantest(lm_obj, listw=lwB, alternative="two.sided")


## ---- echo=TRUE, mysize=TRUE, size='\\tiny'-------------------------------------
lmor0 <- localmoran(sf_ihpp$y, listw=lwB, alternative="two.sided")
lmor1 <- as.data.frame(localmoran.sad(lm_obj, nb=nb_soi, style="B",
  alternative="two.sided"))
sf_ihpp$z_value <- lmor0[,4]
sf_ihpp$z_lmor1_N <- lmor1[,2]
sf_ihpp$z_lmor1_SAD <- lmor1[,4]


## ----sa6, echo=FALSE, warning=FALSE, cache=TRUE, echo=FALSE, eval=TRUE, results='hide', dev.args=list(family="Fira Sans", bg="transparent"), warning=FALSE----
suppressPackageStartupMessages(library(tmap))
tm_shape(sf_ihpp) + tm_symbols(col=c("z_value", "z_lmor1_N", "z_lmor1_SAD"), midpoint=0) + tm_facets(free.scales=FALSE, nrow=2) + tm_layout(panel.labels=c("No trend", "Trend, normal", "Trend, SAD"), bg.color = "transparent")


## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------------
BCrepos <- BiocManager::repositories()
bioc <- available.packages(repo = BCrepos[1])
bioc_ann <- available.packages(repo = BCrepos[2])
bioc_exp <- available.packages(repo = BCrepos[3])
cran <- available.packages()
saveRDS(cran, file="cran_200913.rds")
pdb <- rbind(cran, bioc, bioc_ann, bioc_exp)
saveRDS(pdb, file="pdb_200913.rds")


## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------------
pdb <- readRDS("pdb_200913.rds")
suppressPackageStartupMessages(library(miniCRAN))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(magrittr))
pg <- makeDepGraph(pdb[, "Package"], availPkgs = pdb, suggests=TRUE, enhances=TRUE, includeBasePkgs = FALSE)
pr <- pg %>%
 page.rank(directed = FALSE) %>%
 use_series("vector") %>%
 sort(decreasing = TRUE) %>%
 as.matrix %>%
 set_colnames("page.rank")
 cutoff <- quantile(pr[, "page.rank"], probs = 0.2)
popular <- pr[pr[, "page.rank"] >= cutoff, ]
toKeep <- names(popular)
vids <- V(pg)[toKeep]
gs <- induced.subgraph(pg, vids = toKeep)
cl <- walktrap.community(gs, steps = 3)
topClusters <- table(cl$membership) %>%
 sort(decreasing = TRUE) %>%
 head(25)
cluster <- function(i, clusters, pagerank, n=10){
 group <- clusters$names[clusters$membership == i]
 pagerank[group, ] %>% sort(decreasing = TRUE) %>% head(n)
}
z <- lapply(names(topClusters)[1:15], cluster, clusters=cl, pagerank=pr, n=50)
saveRDS(z, file="all_z_200913.rds")


## ----plot4a, cache=TRUE, echo=FALSE, eval=TRUE, results='hide', dev.args=list(family="Fira Sans", bg="transparent")----
suppressPackageStartupMessages(library(wordcloud))
z <- readRDS("all_z_200913.rds")
oopar <- par(mar=c(0,0,0,0)+0.1)
wordcloud(names(z[[4]]), freq=unname(z[[4]]))
par(oopar)


## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------------
pdb <- readRDS("pdb_200913.rds")
deps_sp <- tools::package_dependencies(packages = "sp", pdb, which = c("Depends", "Imports"), recursive = TRUE, reverse = TRUE)
deps_sf <- tools::package_dependencies(packages = "sf", pdb, which = c("Depends", "Imports"), recursive = TRUE, reverse = TRUE)
deps_sp3 <- tools::package_dependencies(packages = "sp", pdb, which = c("Depends", "Imports", "Suggests"), recursive = TRUE, reverse = TRUE)
deps_sf3 <- tools::package_dependencies(packages = "sf", pdb, which = c("Depends", "Imports", "Suggests"), recursive = TRUE, reverse = TRUE)
deps_sp1 <- tools::package_dependencies(packages = "sp", pdb, which = c("Depends", "Imports"), recursive = FALSE, reverse = TRUE)
deps_sf1 <- tools::package_dependencies(packages = "sf", pdb, which = c("Depends", "Imports"), recursive = FALSE, reverse = TRUE)
deps_sp2 <- tools::package_dependencies(packages = "sp", pdb, which = c("Depends", "Imports", "Suggests"), recursive = FALSE, reverse = TRUE)
deps_sf2 <- tools::package_dependencies(packages = "sf", pdb, which = c("Depends", "Imports", "Suggests"), recursive = FALSE, reverse = TRUE)
save(deps_sp, deps_sf, deps_sp1, deps_sf1, deps_sp2, deps_sf2, deps_sp3, deps_sf3, file="deps_sp_sf_200913.rda")


## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------------
load("deps_sp_sf_200913.rda")
mat <- matrix(0, ncol=4, nrow=5)
mat[1,1] <- length(deps_sp[[1]])
mat[2,1] <- length(deps_sf[[1]])
sp_sf <- match(deps_sp[[1]], deps_sf[[1]])
sf_sp <- match(deps_sf[[1]], deps_sp[[1]])
mat[4,1] <- length(deps_sf[[1]][is.na(sf_sp)])
mat[5,1] <- length(deps_sf[[1]][!is.na(sf_sp)])
mat[3,1] <- length(deps_sp[[1]][is.na(sp_sf)])
mat[1,4] <- length(deps_sp2[[1]])
mat[2,4] <- length(deps_sf2[[1]])
sp_sf2 <- match(deps_sp2[[1]], deps_sf2[[1]])
sf_sp2 <- match(deps_sf2[[1]], deps_sp2[[1]])
mat[4,4] <- length(deps_sf2[[1]][is.na(sf_sp2)])
mat[5,4] <- length(deps_sf2[[1]][!is.na(sf_sp2)])
mat[3,4] <- length(deps_sp2[[1]][is.na(sp_sf2)])
mat[1,2] <- length(deps_sp3[[1]])
mat[2,2] <- length(deps_sf3[[1]])
sp_sf3 <- match(deps_sp3[[1]], deps_sf3[[1]])
sf_sp3 <- match(deps_sf3[[1]], deps_sp3[[1]])
mat[4,2] <- length(deps_sf3[[1]][is.na(sf_sp3)])
mat[5,2] <- length(deps_sf3[[1]][!is.na(sf_sp3)])
mat[3,2] <- length(deps_sp3[[1]][is.na(sp_sf3)])
mat[1,3] <- length(deps_sp1[[1]])
mat[2,3] <- length(deps_sf1[[1]])
sp_sf1 <- match(deps_sp1[[1]], deps_sf1[[1]])
sf_sp1 <- match(deps_sf1[[1]], deps_sp1[[1]])
mat[4,3] <- length(deps_sf1[[1]][is.na(sf_sp1)])
mat[5,3] <- length(deps_sf1[[1]][!is.na(sf_sp1)])
mat[3,3] <- length(deps_sp1[[1]][is.na(sp_sf1)])
rownames(mat) <- c("Sum sp", "Sum sf", "Only sp", "Only sf", "Both")
colnames(mat) <- c("Recursive", "Recursive w/Suggests", "Not recursive", "Not recursive w/Suggests")


## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\scriptsize'-----------------
mat

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'-----------------------------------
rgdal::rgdal_extSoftVersion()

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'-----------------------------------
rgeos::rgeos_extSoftVersion()


## ---- echo = TRUE, mysize=TRUE, size='\\tiny'-----------------------------------
sf_extSoftVersion()


## ---- echo = TRUE, mysize=TRUE, size='\\tiny'-----------------------------------
ellps <- sf_proj_info("ellps")
(clrk66 <- unlist(ellps[ellps$name=="clrk66",]))


## ---- echo = TRUE, mysize=TRUE, size='\\tiny'-----------------------------------
eval(parse(text=clrk66["major"]))
eval(parse(text=clrk66["ell"]))
print(sqrt((a^2-b^2)/a^2), digits=10)


## ---- echo = TRUE, mysize=TRUE, size='\\tiny'-----------------------------------
library(RSQLite)
DB0 <- strsplit(sf:::CPL_get_data_dir(), .Platform$path.sep)[[1]]
DB <- file.path(DB0[length(DB0)], "proj.db")
db <- dbConnect(SQLite(), dbname=DB)
cat(strwrap(paste(dbListTables(db), collapse=", ")), sep="\n")
dbDisconnect(db)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
library(sf)
bp_file <- system.file("gpkg/b_pump.gpkg", package="sf")
b_pump_sf <- st_read(bp_file)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
proj5 <- paste0("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717",
 " +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs")
legacy <- st_crs(proj5)
proj6 <- legacy$proj4string
proj5_parts <- unlist(strsplit(proj5, " "))
proj6_parts <- unlist(strsplit(proj6, " "))
proj5_parts[!is.element(proj5_parts, proj6_parts)]
proj6_parts[!is.element(proj6_parts, proj5_parts)]


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
b_pump_sf1 <- b_pump_sf
st_crs(b_pump_sf1) <- st_crs(st_crs(b_pump_sf1)$proj4string)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
b_pump_sf_ll <- st_transform(b_pump_sf, "OGC:CRS84")
b_pump_sf1_ll <- st_transform(b_pump_sf1, "OGC:CRS84")
st_distance(b_pump_sf_ll, b_pump_sf1_ll)


## ---- echo = FALSE, eval=FALSE, results='hide'----------------------------------
library(mapview)
mapviewOptions(fgb = FALSE)
pts <- rbind(b_pump_sf_ll, b_pump_sf1_ll)
pts$CRS <- c("original", "degraded")
mapview(pts, zcol="CRS", map.type="OpenStreetMap", col.regions=c("green", "red"), cex=18)
## mapshot(webmap1, file="webmap1.png")
## file.copy("webmap1.png", "bivand_200927_files/figure-beamer")
## unlink("webmap1.png")


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
st_crs("EPSG:4326")


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
library(sp)
cat(wkt(CRS("EPSG:4326")), "\n")


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
sf_from_sp <- st_crs(CRS("EPSG:4326"))
o <- strsplit(sf_from_sp$wkt, "\n")[[1]]
cat(paste(o[grep("CS|AXIS|ORDER", o)], collapse="\n"))


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
sp_from_sf <- as(st_crs("EPSG:4326"), "CRS")
o <- strsplit(wkt(sp_from_sf), "\n")[[1]]
cat(paste(o[grep("CS|AXIS|ORDER", o)], collapse="\n"))


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
cat(st_crs("OGC:CRS:84")$wkt, "\n")


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
cat(wkt(CRS("OGC:CRS84")), "\n")


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny', warning=FALSE, message=FALSE----
b_pump_sp <- as(b_pump_sf, "Spatial")
b_pump_sp1 <- as(b_pump_sf1, "Spatial")


## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'-----------------------
## td <- tempfile()
## dir.create(td)
## Sys.setenv("PROJ_USER_WRITABLE_DIRECTORY"=td)

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
library(rgdal)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
WKT <- wkt(b_pump_sp)
o <- list_coordOps(WKT, "EPSG:4326")
aoi0 <- project(t(unclass(bbox(b_pump_sp))), WKT, inv=TRUE)
aoi <- c(t(aoi0 + c(-0.1, +0.1)))
o_aoi <- list_coordOps(WKT, "EPSG:4326", area_of_interest=aoi)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
b_pump_sp_ll <- spTransform(b_pump_sp, "OGC:CRS84")
cat(strwrap(get_last_coordOp()), sep="\n")


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
o <- list_coordOps(wkt(b_pump_sp1), "OGC:CRS84",
  area_of_interest=aoi)
b_pump_sp1_ll <- spTransform(b_pump_sp1, "OGC:CRS84")
cat(strwrap(get_last_coordOp()), sep="\n")


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
enable_proj_CDN()
list.files(td)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
b_pump_sp_llg <- spTransform(b_pump_sp, "OGC:CRS84")
cat(strwrap(get_last_coordOp()), sep="\n")


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
(fls <- list.files(td))
file.size(file.path(td, fls[1]))
disable_proj_CDN()
ll <- st_as_sf(b_pump_sp_ll)
ll1 <- st_as_sf(b_pump_sp1_ll)
llg <- st_as_sf(b_pump_sp_llg)
sf_use_s2(FALSE)
c(st_distance(ll, ll1), st_distance(ll, llg))


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'------------------------
sf_use_s2(TRUE)
c(st_distance(ll, ll1), st_distance(ll, llg))
sf_use_s2(FALSE)



## ----sI, cache=TRUE, echo = TRUE, mysize=TRUE, size='\\tiny'--------------------
sessionInfo()

