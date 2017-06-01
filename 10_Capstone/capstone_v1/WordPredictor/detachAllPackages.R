##Create the detachAllPackages function - list packages to remove - add
##Package: to each package name - store the results
detachAllPackages <- function() {
        basic.packages.blank <-  c("stats", 
                                   "graphics", 
                                   "grDevices", 
                                   "utils", 
                                   "datasets", 
                                   "methods", 
                                   "base")
        basic.packages <- paste("package:", basic.packages.blank, sep = "")
        #Extract installed packages
        package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1, 
                                        TRUE, 
                                        FALSE)]
        #Store the difference b/between the basic and installed packages
        package.list <- setdiff(package.list, basic.packages)
        
        if (length(package.list) > 0)  for (package in package.list) {
                detach(package, character.only = TRUE)
                print(paste("package ", package, " detached", sep = ""))
        }
}
#Run the function to remove all but the basic packages
#detachAllPackages()