readBinFiles <- function(files, times){
  files <- files[file.exists(files)]
  data.all <- lapply(files, function(readFile){
    file.read = file(readFile, "rb")
    on.exit(close(file.read))
    bytes <- raw(0)
    while((length(buffer <- readBin(file.read, raw(), 0x10000))) > 0)
    {
      bytes <- c(bytes, buffer)
    }
    close(file.read)
    file.read <- rawConnection(bytes)
    
    seek(file.read, 6L, "start")
    
    readUInt32 <- function(p, n)
      readBin(p, integer(), n, size = 4L,signed = TRUE)
    readUInt16 <- function(p, n)
      readBin(p, integer(), n, size = 2L,signed = TRUE)
    
    len <- readUInt32(file.read, 1)
    time <- readUInt32(file.read, len)
    kind <- readUInt16(file.read, len)
    flag <- readUInt16(file.read, len)
    
    time <- time * times
    
    data <- data.frame(time=time, kind=kind, flag=flag)
    return(data)
  }
  )
  return(data.all)
}