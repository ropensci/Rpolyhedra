 # Rpolyhedra

 Polyhedra database scraped from public available sources using R6 objects and 'rgl' visualizing capabilities. 
 
  | Release | Usage | Development |
|:--------|:------|:------------|
 [![](https://badges.ropensci.org/157_status.svg)](https://github.com/ropensci/onboarding/issues/157)| [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.4.0-blue.svg)](https://cran.r-project.org/) | [![Travis](https://travis-ci.org/qbotics/Rpolyhedra.svg?branch=master)](https://travis-ci.org/qbotics/Rpolyhedra) |
| [![CRAN](http://www.r-pkg.org/badges/version/Rpolyhedra)](https://cran.r-project.org/package=Rpolyhedra) | | [![codecov](https://codecov.io/gh/qbotics/Rpolyhedra/branch/master/graph/badge.svg)](https://codecov.io/gh/qbotics/Rpolyhedra) |

# How to get started
```R
install.packages("Rpolyhedra")
```

# How to get started (Development version)

Install the R package using the following commands on the R console:

```R
install.packages(c("futile.logger", "rgl", "stringr", "R6", "testthat", "devtools"))
devtools::install_github("qbotics/Rpolyhedra")
library(Rpolyhedra)
# if want to switch to fullDB in user filespace, it will download the full database
switchToFullDatabase()

```

# A simple example of 5 regular polyhedra

To get started execute the following commands:

```R
# 1.  Obtain 5 regular solids
polyhedra.2.draw <- getAvailablePolyhedra(source = "netlib")
polyhedra.2.draw <- polyhedra.2.draw %>% 
                        filter(scraped.name %in%
                            c("tetrahedron", "octahedron", "cube", 
                               "icosahedron", "dodecahedron"))

# 2. Setup colors and scales
n <- nrow(polyhedra.2.draw)
polyhedron.colors <- rainbow(n)
polyhedron.scale <- 5

# 3. open and setup RGL window
open3d()
par3d(FOV = 1)
rgl.bg( sphere =FALSE, fogtype = "none", color=c("black"))
rgl.viewpoint(theta = 0, phi=0, zoom=0.8, fov=1)

# 4. for each polyhedron, setup rotation, position and render
for (i in seq_len(n)) {
  # Obtain polyhedron
  polyhedron.row <- polyhedra.2.draw[i,]
  polyhedron.name <- polyhedron.row$scraped.name
  polyhedron <- getPolyhedron(source = polyhedron.row$source, polyhedron.name)
  
  # Setup angles, position into transformationMatrix
  current.angle <- i/n * 2 * pi
  tm <- rotationMatrix(current.angle, 1, 0, 0)
  x.pos <- round(polyhedron.scale * sin(current.angle), 2)
  y.pos <- round(polyhedron.scale * cos(current.angle), 2)
  tm <- tm %*% translationMatrix(x.pos, y.pos, 0)
  
  # Render
  print(paste("Drawing ", polyhedron.name, " rotated ", round(current.angle, 2),
              " in (1,0,0) axis. Translated to (", x.pos, ",", y.pos, ",0)",
              " with color ", polyhedron.colors[i], sep = ""))
  shape.rgl <- polyhedron$getRGLModel(transformation.matrix = tm)
  shade3d(shape.rgl, color = polyhedron.colors[i])
}

```
## sources 
### netlib
 Includes 142 polyhedra definitions.
 The PHD format was created to describe the geometric polyhedron definitions derived mathematically by Andrew Hume and by the Kaleido program of Zvi Har'El.

 PHD files were generated using [poly2](http://www.netlib.org/poly2/readme) library (no longer mantained). Althought the code is available, specific programming skills are required to run it.
 
PDH files can be found in `extdata/www.netlib.org/polyhedra/index.html`

### dmccooey
Includes 767 polyhedra definitions.
The [polyhedra database](http://dmccooey.com/polyhedra/) built by David Mccooey has an open format which has been scraped to feed RPolyhedra database

dmccooney files can be found in `extdata/dmccooey.com/polyhedra/`

# Troubleshooting

## devtools
Ubuntu

```bash
apt-get install libcurl4-openssl-dev
```

Windows

run end user CRAN version

OSX brew

```bash
brew install openssl
```
After, in R:

```R
install.packages("devtools")
```

# rgl

Ubuntu
```bash
sudo apt-get install r-cran-rgl
```
