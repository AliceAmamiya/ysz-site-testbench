# We wrap the calculation into a function myMisconception, which takes 3 parameter



# programming by Li Fumin, based on the conversion written by James Abbott
# the index from 0 to 101 (since we'll need to do 101-100)

myMisperception <- function(X = 0.1, Y = 0.01, viewer = 10)
{
    i <- 0:101
    
    n_i <- 1/(abs(i-viewer) ^ X)
    
    below_viewer <- n_i[2:viewer] - n_i[1:(viewer-1)]
    
    above_viewer <- abs(n_i[(viewer+3):102] - n_i[(viewer+2):101])
    
    dis_i <- c(below_viewer, 0, above_viewer)
    
    cum_i <- cumsum(dis_i + Y)
    
    # the value for rescaling will be the maximum cumulative sum observed:
    cum_i <- (100/ cum_i[100])*cum_i
    
    return(cum_i)
}

# to view each individual scale in A6.2, just type:
myMisperception(X = 0.1, Y = 0.01, viewer = 10)

# To calculate correlation between any of 2 scales in A6.3:
library("Hmisc")

m <- data.frame(
    Viewer_10=myMisperception(X = 0.1, Y = 0.01, viewer = 10),
    Viewer_20=myMisperception(X = 0.1, Y = 0.01, viewer = 20),
    Viewer_30=myMisperception(X = 0.1, Y = 0.01, viewer = 30),
    Viewer_40=myMisperception(X = 0.1, Y = 0.01, viewer = 40)
)

m2 <- data.frame(
    Viewer_10=myMisperception(X = 0.1, Y = 0.01, viewer = 10),
    Viewer_90=myMisperception(X = 0.1, Y = 0.01, viewer = 90)
)

res <- rcorr(as.matrix(m2))
res
