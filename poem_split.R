# TRY TO DO IT WITH PETER PAN
# Code from Nicholas Horton, nhorton@amherst.edu,November 1, 2015
location = "peterpan"
if (!file.exists(location)) {
  dir.create(location)
}

processPP = function(source="peterpan.txt") {
  ds = readLines(source)
  prefix = source
  chapternum = 1
  counter = 0
  sink(file=paste(location, "/", prefix, sprintf("%03d", chapternum), sep=""))
  for (i in 1:length(ds)) {
    if (ds[i] != "") {   # non blank text
      counter = 0     # reset counter
      cat(ds[i], "\n")  # display line
      counter = counter + 1
    } else if (counter <= 3) {  # not yet a poem break
      counter = counter + 1  # add one more line
      cat(ds[i],"\n")  # and display text
    } 
    else {  # must be a new chapter!
      # new chapter
      chapternum = chapternum+1
      counter = 0
      sink()
      sink(file=paste(location, "/", prefix, sprintf("%03d", chapternum), sep=""))
    }
  }
  sink()
}

processPP("peterpan.txt")



