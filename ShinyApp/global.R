
library(Seurat)
library(ShinyCell)

#runs app locally for testing
shiny::runApp("ShinyApp/shinyApp")

#RUN THIS ONE IN BASH TO TEST MEMORY USAGE:
#Rscript ShinyApp/shinyApp/bench_memory_usage.R


#------DEPLOY TO SHINYAPPS.IO------->
if (interactive()) {
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    install.packages("rsconnect")
  }
  rsconnect::setAccountInfo(name='ward-bio', token='NUH-UH', secret='ItsASecret :P')

  #JUST RERUN THIS WHEN MAKING UPDATES
  rsconnect::deployApp(appDir = "ShinyApp/shinyApp", appName = "SpermInteractive", forceUpdate = TRUE)
}
#--------------------------------------->