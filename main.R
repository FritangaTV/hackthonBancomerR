#!/usr/local/bin/Rscript

run_script <- function(){
  source('read_inputs.R')
  source("variable_selection.R")
  source("make_prediction.R")
  source("write_output.R")
}

input_path <- "input.json"
output_path <- getwd()
run_script()

# input_path <- "~/Desktop/Hackaton/Ejemplos/bueno/bueno.json"
# output_path <- "~/Desktop/Hackaton/Ejemplos/bueno/"
# run_script()
# 
# input_path <- "~/Desktop/Hackaton/Ejemplos/promedio/promedio.json"
# output_path <- "~/Desktop/Hackaton/Ejemplos/promedio/"
# run_script()
# 
# input_path <- "~/Desktop/Hackaton/Ejemplos/malo/malo.json"
# output_path <- "~/Desktop/Hackaton/Ejemplos/malo/"
# run_script()
# 
# 
