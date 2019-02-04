# Contributing

When contributing to this repository, please first discuss the change you wish to make via issue,
email, or any other method with the owners of this repository before making a change. 

Please note we have a code of conduct, please follow it in all your interactions with the project.

## Polyhedra Scraping

In vignettes there is documentation and examples for public functions. However the functionality of the packages goes beyond final user. One major feature has to do with reproducing the scraping process. To that extent, the original polyhedra definitions are shipped (A minimal package version named min-pkg within the package, full-db in a secondary repository accesable with switchToFullDB).

All code is documented within R files. Several functions and classes (for developers) are not included in vignettes to avoid final user confusion, but it should be possible for a developer to get insights of the code following test cases, and extend funcionality or make contributions to the project.

#Reproducibility

The project was built from the ground up with reproducibility in mind. To accommodate for that, each run of the scraping functionality stores information about the run in a Ledger that can be later queried for analytical purposes. 

## Pull Request Process

1. Ensure any install or build dependencies are removed before the end of the layer when doing a 
   build.
2. Update the README.md with details of changes to the interface, this includes new environment 
   variables, exposed ports, useful file locations and container parameters.

## Code style

The code style under use is the recommended in the [google style code (GSC)](https://google.github.io/styleguide/Rguide.xml). 
R6 classes has no code style in GSC. The code style defined for a R6 class-object named FooBar is FooBar.class .




