
library(Seurat)
library(ShinyCell)

shiny::runApp("ShinyApp/shinyApp")

#RUN THIS ONE IN BASH TO TEST MEMORY USAGE:
#Rscript ShinyApp/shinyApp/bench_memory_usage.R


#-------------------------------------->
# Deployment tools should not run on app startup to avoid slowing/interrupting
# interactive sessions. Keep them behind an interactive guard.
if (interactive()) {
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    install.packages("rsconnect")
  }
  rsconnect::setAccountInfo(name='ward-bio', token='NUH-UH', secret='ItsASecret :P')

  #JUST RERUN THIS WHEN MAKING UPDATES
  rsconnect::deployApp(appDir = "ShinyApp/shinyApp", appName = "SpermInteractive", forceUpdate = TRUE)
}
#--------------------------------------->



# #installing:

# Load the preprocessed Seurat objects
#seu <- readRDS("harmonized_seurat.rds") # nolint
#seuS1 <- readRDS("sertoli_A.rds")
#seus2 <- readRDS("shinyApp/specificCellID.rds")

# Load the ShinyCell config files
#scConf_main <- readRDS("scConf_main.rds")
#scConf_SA <- readRDS("scConf_SA.rds") # nolint
#scConf_SC <- readRDS("scConf_SC.rds")
# 
# reqPkg = c("data.table", "Matrix", "hdf5r", "reticulate", "ggplot2", 
#            "gridExtra", "glue", "readr", "RColorBrewer", "R.utils", "Seurat")
# newPkg = reqPkg[!(reqPkg %in% installed.packages()[,"Package"])]
# if(length(newPkg)){install.packages(newPkg)}
# 
# reqPkg = c("shiny", "shinyhelper", "data.table", "Matrix", "DT", "hdf5r", 
#            "reticulate", "ggplot2", "gridExtra", "magrittr", "ggdendro")
# newPkg = reqPkg[!(reqPkg %in% installed.packages()[,"Package"])]
# if(length(newPkg)){install.packages(newPkg)}
# 
# rm(list = ls())
# 
# library(Seurat)
# library(ShinyCell)
# 
# #adding each dataset:
# seu = readRDS("harmonized_seurat.rds")
# seu@images <- list()
# DefaultAssay(seu) <- "RNA"
# seu <- JoinLayers(seu)
# 
# # Remove columns from the Seurat object
# scConf_main = createConfig(seu)
# scConf_main = delMeta(scConf_main, c("orig.ident", "SCT_snn_res.0.2", 
#                            "SCT_snn_res.1.2", "SCT_snn_res.1",
#                            "SCT_snn_res.0.6", "SCT_snn_res.0.4",
#                            "Phase","original_id", "mitoFr"))
# 
# seuS1 = readRDS("sertoli_A.rds")
# seuS1@images <- list()
# DefaultAssay(seuS1) <- "RNA"
# seuS1 <- JoinLayers(seuS1)
# scConf_SA = createConfig(seuS1)
# scConf_SA = delMeta(scConf_SA, c("orig.ident", "SCT_snn_res.0.2", 
#                                      "SCT_snn_res.1.2", "SCT_snn_res.1",
#                                      "SCT_snn_res.0.6", "SCT_snn_res.0.4",
#                                      "Phase","original_id", "mitoFr"))
# 
# packageVersion("Seurat")
# 
# #-------Adding SpecificCellID config--------->
# seuS2 = readRDS("shinyApp/specificCellID.rds")
# class(seuS2)
# is(seuS2, "Seurat")   # Should be TRUE
# Layers(seuS2)
# 
# seuS2@images <- list()
# DefaultAssay(seuS2) <- "RNA"
# seuS2 <- JoinLayers(seuS2)
# 
# # Remove columns from the Seurat object
# scConf_SC = createConfig(seuS2)
# 
# 
# #saving configs for next time:
# saveRDS(scConf_main, file = "scConf_main.rds")
# saveRDS(scConf_SA, file = "scConf_SA.rds")
# saveRDS(scConf_SC, file = "scConf_SC.rds")
# 
# scConf_main$UI
# scConf_SA$UI
# scConf_SC$UI
# 
# #making shiny app files for each data set:
# makeShinyFiles(seu, scConf_main, gex.assay = "RNA", gex.slot = "data",
#               gene.mapping = TRUE, shiny.prefix = "sc1")
# 
# makeShinyFiles(seuS1, scConf_SA, gex.assay = "RNA", gex.slot = "data",
#               gene.mapping = TRUE, shiny.prefix = "sc2")
# 
# makeShinyFiles(seuS2, scConf_SC, gex.assay = "RNA", gex.slot = "data",
#                gene.mapping = TRUE, shiny.prefix = "sc3")
# 
# 
# # # Build the app
# # citation = list(
# #   author  = "Author",
# #   title   = "Title",
# #   journal = "Journal",
# #   volume  = "###",
# #   page    = "Page #",
# #   year    = "Year", 
# #   doi     = "DOI",
# #   link    = "link")
# # 
# # #---------------->
# # makeShinyCodesMulti(
# #  shiny.title = "Hayden Thesis Data", shiny.footnotes = citation,
# #  shiny.prefix = c("sc1", "sc2"),
# #  shiny.headers = c("Harmonized_Seurat", "Sertoli_A"),
# #  shiny.dir = "shinyApp/")
# # #---------------->
# # 
