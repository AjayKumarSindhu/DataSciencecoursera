polutemean2 <- function(pollutant,z=7:10){
  x <- y <- numeric()
  s <- 1
  k <- list.files()[c(z)]
  for (id in 1:length(z)) {
      m <- read.csv(k[id])
      n <- m[pollutant]
      a <- n[!is.na(n)]
      q <- unlist(a)
      x[s] <- sum(q)
      y[s] <- length(q)
      s <- s+1 
    }
  mean <- sum(x)/sum(y)
  return(mean)
}
complete <- function(ID= 1:332){
  No.CompCases <- numeric()
  file <- c(ID)
  s <- 1
  k <- list.files()[c(ID)]
  for (j in 1:length(ID)) {
    m <- read.csv(k[j])
    n <- m[2]
    q <- complete.cases(n)
    No.CompCases[s] <- sum(q)
    s <- s+1
  }
  df <- data.frame(file,No.CompCases)
  return(df)
}
corr <- function(threshold = 0) {
  correlation <- numeric()
  s <- 1
  k <- list.files()
  for (j in 1:length(k)) {
    m <- read.csv(k[j])
    n <- complete.cases(m)
    p <- sum(n)
    if( p[1] > threshold){
     q <- na.omit(m)
     correlation[s] <- cor(q[2],q[3])
     
    }
    s <- s+1
  } 
  
  return(correlation)
}