# withProgress(message = 'Making plot', value = 0, {
#   # Number of times we'll go through the loop
#   n <- 10
#   
#   for (i in 1:n) {
#     # Each time through the loop, add another row of data. This is
#     # a stand-in for a long-running computation.
#     dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
#     
#     # Increment the progress bar, and update the detail text.
#     incProgress(1/n, detail = paste("Doing part", i))
#     
#     # Pause for 0.1 seconds to simulate a long computation.
#     Sys.sleep(0.1)
#   }
# })