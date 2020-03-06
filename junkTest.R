start_time <- Sys.time()
rmarkdown::render("myAnalyses/0.StateOfHealth/StateOfHealth.Rmd")
end_time <- Sys.time()
end_time - start_time


rmarkdown::render("myAnalyses/0.StateOfHealth/StateOfHealth - Copy.Rmd")
