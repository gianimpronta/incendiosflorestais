# deLuxe setup of caret package with allmost all 765 required caret libraries 
# https://github.com/tobigithub/caret-machine-learning
# Tobias Kind (2015)
detachAllPackages <- function() {
      
      basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
      
      package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
      
      package.list <- setdiff(package.list,basic.packages)
      
      if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
      
}

detachAllPackages()
# 1) load caret packages from BioConductor, answer 'n' for updates
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite(c("arm", "gpls", "logicFS", "vbmp"))

# 2) installs most of the 340 caret dependencies + seven commonly used ones
mCom <- c("caret", "AppliedPredictiveModeling", "ggplot2", 
          "data.table", "plyr", "knitr", "shiny", "xts", "lattice")
install.packages(mCom, dependencies = c("Imports", "Depends", "Suggests"))     

# 3) load caret and check which additional libraries 
# covering over 200 models need to be installed
# use caret getModelInfo() to obtain all related libraries
require(caret); sessionInfo();
cLibs <- unique(unlist(lapply(getModelInfo(), function(x) x$library)))
detach("package:caret", unload=TRUE)
install.packages(cLibs, dependencies = TRUE)

# 4) load packages from R-Forge
install.packages(c("CHAID"), repos="http://R-Forge.R-project.org")

# 5) Restart R, clean-up mess, and say 'y' when asked
# All packages that are not in CRAN such as SDDA need to be installed by hand
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite(c("gpls", "logicFS", "vbmp"))
### END