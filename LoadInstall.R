#=================================================================#
#            This function loads and installs packages    
#=================================================================#

LoadInstall <- function(dependencies){
  # Summary: Check whether dependencies exits and load/install them
  #
  # Args:
  #   - [Dependencies]: a character vector with all packages names
  # 
  # Returns:
  #   - package name and a logical indicator for each packages, 
  #   whether it has successfully been loaded or not
  #
  installed <- installed.packages()[,1]
  new_packages <- dependencies[which(!(dependencies %in% installed))]
  if (length(new_packages) >= 1) {
    print(paste0("installerar nya paket ",new_packages))
    install.packages(new_packages, repos = 'https://cran.ma.imperial.ac.uk/')
  }
  Vectorize(require)(dependencies, character.only=TRUE)
  
}

itShouldLoadAndInstallPackages <- function(){
  # This is a test function
  
  packdir <- tempfile()
  dir.create(packdir)
  
  # Because packages are global variables, a virtual environment 
  # must be created for the test
  check.packrat <- !("packrat" %in% installed.packages()[,"Package"])
  if(check.packrat == TRUE) install.packages(check.packrat)
  library(packrat)
  packrat::init(packdir, restart = FALSE)
  
  # Test input
  deps <- c("utf8", "survival")
  
  # RUn function
  installed <- LoadInstall(deps)
  
  # Correct output
  gold <- c(TRUE, TRUE)
  names(gold) <- c(deps)
  
  stopifnot(installed == gold)
  
  # Run function again to see that it does nothing
  start <- Sys.time()
  installed <- LoadInstall(deps)
  elapsed <- Sys.time() - start
  stopifnot(as.double(elapsed, units = "secs") < 1)
  stopifnot(installed == gold)
  
  
  packrat::disable(packdir, restart = FALSE)
  
  unlink(packdir, recursive = TRUE)
  
  print("OK")
  
}

#itShouldLoadAndInstallPackages()




