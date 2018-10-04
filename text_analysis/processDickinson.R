# Process Emily Dickinson Project Gutenberg corpus
# Nicholas Horton, nhorton@amherst.edu   November 1, 2015
location = "gutenberg"
if (!file.exists(location)) {
  dir.create(location)
}

processDickinson = function(source="gutenberg1.txt") {
  ds = readLines(source)
  prefix = source
  poemnum = 1
  counter = 0
  sink(file=paste(location, "/", prefix, sprintf("%03d", poemnum), sep=""))
  for (i in 1:length(ds)) {
    if (ds[i] != "") {   # non blank text
      counter = 0     # reset counter
      cat(ds[i], "\n")  # display line
    } else if (counter <= 3) {  # not yet a poem break
      counter = counter + 1  # add one more line
      cat(ds[i],"\n")  # and display text
    } else {  # must be a new poem!
      # new poem
      poemnum = poemnum+1
      counter = 0
      sink()
      sink(file=paste(location, "/", prefix, sprintf("%03d", poemnum), sep=""))
    }
  }
  sink()
}


