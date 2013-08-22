bivarPlot <- function(x, y, subtitle=NULL,...){

y <- y
x <- x
par8 = 'terrain.colors'
par7 = 'Y'
par6 = 'Y'
par5 = '0'
par4 = '0'
par3 = '0'
par2 = '50'
par1 = '50'
ylab = 'y'
xlab = 'x'
main = 'Bivariate Kernel Density'
sub=subtitle
par8 <- 'terrain.colors'
par7 <- 'Y'
par6 <- 'Y'
par5 <- '0'
par4 <- '0'
par3 <- '0'
par2 <- '50'
par1 <- '50'
#'GNU S' R Code compiled by R2WASP v. 1.2.291 ()
#Author: root
#To cite this work: Wessa P. (2012), Bivariate Kernel Density Estimation (v1.0.9) in Free Statistics Software (v$_version), Office for Research Development and Education, URL http://www.wessa.net/Ian.Holliday/rwasp_bidensity.wasp/
#Source of accompanying publication: 
#
par1 <- as(par1,'numeric')
par2 <- as(par2,'numeric')
par3 <- as(par3,'numeric')
par4 <- as(par4,'numeric')
par5 <- as(par5,'numeric')
library('GenKern')
x <- x[!is.na(y)]
y <- y[!is.na(y)]
y <- y[!is.na(x)]
x <- x[!is.na(x)]
if (par3==0) par3 <- dpik(x)
if (par4==0) par4 <- dpik(y)
if (par5==0) par5 <- cor(x,y)
if (par1 > 500) par1 <- 500
if (par2 > 500) par2 <- 500
if (par8 == 'terrain.colors') mycol <- terrain.colors(100)
if (par8 == 'rainbow') mycol <- rainbow(100)
if (par8 == 'heat.colors') mycol <- heat.colors(100)
if (par8 == 'topo.colors') mycol <- topo.colors(100)
if (par8 == 'cm.colors') mycol <- cm.colors(100)
 op <- KernSur(x,y, xgridsize=par1, ygridsize=par2, correlation=par5, xbandwidth=par3, ybandwidth=par4)
 image(op$xords, op$yords, op$zden, col=mycol, axes=TRUE,main=main, sub=subtitle, xlab=xlab,ylab=ylab)
 if (par6=='Y') contour(op$xords, op$yords, op$zden, add=TRUE)
 if (par7=='Y') points(x,y)
 (r<-lm(y ~ x))

 box()
}



