# This is Kate's jotter for doing the packaging things in.

# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# We will probably change the name of this package at some point.
# This website might help with that:
# https://www.njtierney.com/post/2017/10/27/change-pkg-name/

# I previously worked through this:
# https://r-pkgs.org/whole-game.html

#### SETUP ####
# Install packages
# install.packages("devtools")

# Check devtools version
# package?devtools
# We're looking for 2.4.6 to match the website guide.

# Load packages
library(devtools)
library(gitcreds)
library(dplyr)
library(tidyr)
library(terra)
library(sf)

# Declare these dependencies
use_package("sf")
use_package("terra")
use_package("tidyr")
use_package("dplyr")


#### ONE-TIME PACKAGE SETUP THINGS ####
# Create the package
# create_package("~/habitats/habcounter")

# Use Git
# usethis::use_git_config(
#   user.name = "oharakate",
#   user.email = "sadkate@gmail.com"
# )
# use_git()

# Use Github
# create_github_token()
# gitcreds_set()

# use_github()

# Ignore this notebook and everything in dev:
# usethis::use_build_ignore("dev/")

# Use MIT licence
# use_mit_license()
# Also manually update the description at this point.

# Edit DESCRIPTION

# Generate documentation
document()
####



#### Putting functions into the package ####
# load_shape()
use_r("load_shape")
load_all()

# Call this on some test data
load_shape("inst/extdata/test_data.shp")
load_shape("inst/extdata/test_aoi.shp")
# Looks OK.

# Remember to:
# Do Code > Insert roxygen skeleton inside the function
# And edit it

# Also, to get update the manual
document()

# Check the package:
check()

# To use the test data it was necessary to specify its location in
# inst/testdata in the example in man/ like this:
shapefile_path <- system.file(
  "extdata",
  "test_data.shp", # Use the name of your test shapefile
  package = "habcounter"
)
load_shape(shapefile_path)









