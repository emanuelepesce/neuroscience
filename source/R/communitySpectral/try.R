cfile <- "./../../../data/toyData/cutted_controls/CTRL_amore.gml"
g <- read.graph(cfile, format="gml")
ugm <- as.undirected(g, "collapse")
MA <- get.adjacency(ugm, attr="weight")
M <- as.matrix(MA)
spc = specc(M, 10, kernel = "rbfdot")

# 
# trySpec <- function(M) {
#   out <- tryCatch(
# {
#   # Just to highlight: if you want to use more than one 
#   # R expression in the "try" part then you'll have to 
#   # use curly brackets.
#   # 'tryCatch()' will return the last evaluated expression 
#   # in case the "try" part was completed successfully
#   
# #   message("This is the 'try' part")
#   
#   spc <- specc(M, 10, kernel = "rbfdot", warn = FALSE)
#   # The return value of `readLines()` is the actual value 
#   # that will be returned in case there is no condition 
#   # (e.g. warning or error). 
#   # You don't need to state the return value via `return()` as code 
#   # in the "try" part is not wrapped insided a function (unlike that
#   # for the condition handlers for warnings and error below)
# },
# error=function(cond) {
#   message("Here's the original error message:")
#   message(cond)
#   # Choose a return value in case of error
#   return(NA)
# },
# warning=function(cond) {
#   # Choose a return value in case of warning
#   return(NULL)
# },
# finally={
#   # NOTE:
#   # Here goes everything that should be executed at the end,
#   # regardless of success or error.
#   # If you want more than one expression to be executed, then you 
#   # need to wrap them in curly brackets ({...}); otherwise you could
#   # just have written 'finally=<expression>' 
# #   message("Some other message at the end")
# }
#   )    
# return(out)
# }

for( i in 1:100){
  print(i)
  spc <- NULL
  repeat{
    err <- FALSE
    tryCatch({ 
      spc <- specc(M, 10, kernel = "rbfdot", warn = FALSE)
    },
    error=function(cond){
      err <- TRUE
    },
    warning=function(cond) {
    },
    finally={    
    }
    )
    if(err == FALSE) break
  }
}




# 
# 
# for(i in 1:100){
#   print(i)
#   trySpec(M) 
# }

