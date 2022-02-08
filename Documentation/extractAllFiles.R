# Get all files in:
# 1) Secure Data
# 2) 0.CCB/
# 3) myCCB/
# 4) myUpstream/
# 5) Population Data
# 6) SDOH

nm <- list.files(path="path/to/file")

myPath <- "/mnt/projects/FusionData/0.CCB/myCCB"

myCCB <- list.files(path = myPath, include.dirs = T, full.names = T, recursive = T)

df <- data.frame(filePath = c(myPath, myCCB)) %>%
  mutate(file = sub('.*/', '', filePath), 
         file = ifelse(grepl('[.]', file), file, paste0(file, '/')), 
         temp = sub('/[^/].*$', '', filePath)
         )

library(stringr)
matches <- str_locate_all(df$filePath,"/")
chars <- sapply(matches, function(x) x[nrow(x), ])[1, ]
str_sub(df$filePath, 1, chars)
