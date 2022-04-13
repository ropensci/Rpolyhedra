Rpolyhedra 0.5
============

### MINOR IMPROVEMENTS
* CI moved to github actions
* Updated documentation

Rpolyhedra 0.4.4
============

### MINOR IMPROVEMENTS
* Roxygen changed the way R6 classes are documented
* Problems in testhat with some tests

Rpolyhedra 0.4.2
============

First version published using devtools::release() on [rOpenSci.org](https://ropensci.org/). 

### MINOR IMPROVEMENTS

* Calculates and normalizes polyhedra size using geometry::convhulln instead of bounding box

### BUG FIXES

* A polyhedron now applies internal transformation matrix


Rpolyhedra 0.4.1
============

First version published on [rOpenSci.org](https://ropensci.org/). 

### MINOR IMPROVEMENTS

* Complies with all the prerequisites of rOpenSci and applies the suggestions made by rOpenSci reviewers. 
* Fixes a test that writes on user space.
* Integrates with codecov.io, which allows for better test coverage. 
* Updated examples.

Rpolyhedra 0.4.0
============

### NEW FEATURES

* `Rpolyhedra` can export polyhedra definitions as XML.


Rpolyhedra 0.3.0
============

### NEW FEATURES

* `Rpolyhedra` now has a new database format based on ascii RDSs, which are meant to use less memory, for example when used in a Shiny App.
* `Rpolyhedra` now uses a transformation matrix for general polyhedra manipulation.

### MINOR IMPROVEMENTS

* Applied suggestions from rOpenSci onboarding process. 

