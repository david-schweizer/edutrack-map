#install.packages("shinylive")  
library(shinylive)

# site can be be pushed to github.
shinylive::export(".", "docs")

# test whether it works in general
httpuv::runStaticServer("site/")
httpuv::runStaticServer("docs")