create_forest_plot <- function(meta_analysis, dataframe, outcome_name, measure, 
                               experimental_group, control_group){
  
  plot_width <- 1000
  plot_height <- 200 + 60* nrow(dataframe) 
  
  png(
    filename = paste0("plots/ForestPlot_", outcome_name, ".png"),
    width    = plot_width,
    height   = plot_height,
    res      = 100
  )
  
  forest(meta_analysis,
         studlab = TRUE,
         comb.fixed = FALSE,
         comb.random = TRUE,
         test.overall.random = TRUE,
         sortvar= dataframe$study,
         leftcols = c("studlab","event.e","n.e",
                      "event.c","n.c", "effect.ci","w.random"),
         label.e = experimental_group,
         label.c = control_group,
         rightcols = FALSE,
         print.I2 = TRUE,
         print.tau2 = TRUE,
         print.stat.Z = TRUE,
         hetstat = TRUE,
         main = paste0(outcome_name, " - ",
                       experimental_group, " x ", control_group),
         label.left = paste0("Favors ", experimental_group),
         label.right = paste0("Favors ", control_group),
         just = "center",
         just.addcols = "left",
         col.diamond = "darkred",
         col.square = "black",
         col.square.lines = "darkred")


  dev.off()
}
