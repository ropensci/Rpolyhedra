 # Rpolyhedra

 A polyhedra Database scraped from public available sources using R6 (OOP) and 'rgl' visualizing capabilities. 
 
## sources 
### netlib
 Includes 142 polyhedra definitions
 The PHD format was created to describe the geometric polyhedron definitions derived mathematically by Andrew Hume and by the Kaleido program of Zvi Har'El.

 PHD files were generated using  [[http://www.netlib.org/poly2/readme]](poly2) library which is no longer mantained. Althought the code is available, specific programming skills are required to run the code.

### Dmccooey
Includes 767 polyhedra definitions 
The [[http://dmccooey.com/polyhedra/]](polyhedra database) built by David Mccooey has an open format which has been scraped to feed RPolyhedra database


# Exploring PDH files
For navigating
`extdata/www.netlib.org/polyhedra/index.html`

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
```

# A simple example of 5 regular polyhedra

To get started execute the following commands:

```R
polyhedra.2.draw <- getAvailablePolyhedra(source = "netlib")[1:5]
n <- length(polyhedra.2.draw)
polyhedron.colors <- rainbow(n)
polyhedron.scale <- 5

open3d()
par3d(FOV = 1)
rgl.bg( sphere =FALSE, fogtype = "none", color=c("black"))
rgl.viewpoint(theta = 0,phi=0,zoom=0.8,fov=1)
i <- 1
for (polyhedron.name in polyhedra.2.draw) {
  polyhedron <- getPolyhedron(source = "netlib", polyhedron.name)
  current.angle <- i/n * 2 * pi
  shape.rgl <- polyhedron$getRGLModel(1, c(polyhedron.scale * sin(current.angle),
                                           polyhedron.scale * cos(current.angle),
                                           0))
  shade3d(shape.rgl, color = polyhedron.colors[i])
  i <- i + 1
}

```

# Troubleshooting

## devtools
Ubuntu

```bash
apt-get install libcurl4-openssl-dev
```

Windows

Wait for CRAN version


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
