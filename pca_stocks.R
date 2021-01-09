### 11/29/2020 Authored by Oguzcan Adabuk ###
require(ggplot2)

#Set the current directory to load the data
dn<-dirname(getSourceEditorContext()$path)
setwd(dn)

# Read Daily stock returns and return log returns
get_log_returns<-function(f){
  filename <- paste(f, ".csv", sep="")
  d<-read.csv(filename)
  d<-d$Close
  #d<-d[60:120]
  n<-length(d)
  #d.returns <- 100*log(d[-1]/d[-length(d)])
  d<- ((d[-1]-d[1:n-1])/d[1:n-1]) * 100
  return(d)
}

# Mean center columns of a given matrix
center_apply <- function(x) {
  apply(x, 2, function(y) (y - mean(y)))
}

# Stock tickers and sectors
tickers <- c("AXP","AMGN","AAPL","BA","CAT","CSCO","CVX","GS","HD","HON","IBM","INTC","JNJ","KO","JPM","MCD","MMM","MRK","MSFT","NKE","PG","TRV","UNH","CRM","VZ","V","WBA","WMT","DIS","DOW")
sectors <- c("Financials", "Healthcare", "Technology", "Industrials", "Industrials", "Technology", "Energy", "Financial", "Consumer Cyclical", "Industrials", "Technology", "Technology", "Healthcare", "Consumer Defensive", "Financial", "Consumer Cyclical", "Industrials", "Healthcare", "Technology", "Consumer Cyclical", "Consumer Defensive", "Financial", "Healthcare", "Technology", "Communication Services", "Financial", "Healthcare", "Consumer Defensive", "Communication Services", "Basic Materials")

# Dow Jones Industrial Average Index daily log returns for the last 1 year
dji<-get_log_returns("DJI")


# Create the data matrix A
A <- list()
for(i in 1:length(tickers)){
  A[[tickers[i]]] <- get_log_returns(tickers[i])
}

xrows = length(A[[1]])
xcols = length(A)

# Create a new Matrix B from the centered columns
B<-matrix(sample(0, xcols*xrows, replace=T), nrow=xrows)
B<-center_apply(matrix(unlist(A), nrow=xrows))

# Computer the covariance matrix
C <- (1/(xrows-1)) * t(B) %*% B

# Compute Eigenvectors and Eigenvalues
C.e<-eigen(C)
eigen.vec <- C.e$vectors
eigen.vec[,1] <- eigen.vec[,1] * -1
#eigen.vec[,1] <- eigen.vec[,1]# * -1
eigen.val <- (C.e$values)

# Calculate principal components as linear combinations, store in PCA matrix
pca <- B %*% eigen.vec

# Ratios of stocks we need to keep to replicate DJI
top_cont <- data.frame(cbind(as.numeric(eigen.vec[,1]), tickers, sectors))
colnames(top_cont) <- c("Contribution", "Tickers", "Sectors")
top_cont[order(top_cont$Contribution, decreasing = TRUE),]

# Use built-in princomp() function to validate results from eigendecomposition above
xp <- princomp(matrix(unlist(A), nrow=xrows))

# Plot Contributions of each Principal Component
pcp<-eigen.val/sum(eigen.val)

dfev <- data.frame(
  name=c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8"),
  value=pcp[1:8]
)

# Barplot
ggplot(dfev, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")

# Plots
x<-c(1:xrows)
x_cs<-c(1:xrows)

for(i in 1:xcols){
  x<-cbind(x, A[[i]])
  x_cs<-cbind(x_cs, cumsum(A[[i]])) 
}

cn<-append("x", tickers)
df<-data.frame(x)
colnames(df)<-cn

df_cs<-data.frame(x_cs)
colnames(df_cs)<-cn

n<-length(df$AXP)

# Plot % returns
df_g <- data.frame(x=df$x,
                   y=c(df$AXP, df$AMGN, df$AAPL, df$BA, df$CAT, df$CSCO, df$CVX, df$GS, df$HD, df$HON, df$IBM, df$INTC, df$JNJ, df$KO, df$JPM, df$MCD, df$MMM, df$MRK, df$MSFT, df$NKE, df$PG, df$TRV, df$UNH, df$CRM, df$VZ, df$V, df$WBA, df$WMT, df$DIS, df$DOW),
                   group=c(rep("AXP", n),rep("AMGN", n),rep("AAPL", n),rep("BA", n),rep("CAT", n),rep("CSCO", n),rep("CVX", n),rep("GS", n),rep("HD", n),rep("HON", n),rep("IBM", n),rep("INTC", n),rep("JNJ", n),rep("KO", n),rep("JPM", n),rep("MCD", n),rep("MMM", n),rep("MRK", n),rep("MSFT", n),rep("NKE", n),rep("PG", n),rep("TRV", n),rep("UNH", n),rep("CRM", n),rep("VZ", n),rep("V", n),rep("WBA", n),rep("WMT", n),rep("DIS", n),rep("DOW", n))
                   )

ggplot(df_g, aes(x, y, col=group)) + geom_line() +
  labs(title="Daily % Returns of the Dow 30 Stocks", x="Days", y="% Returns")



# Plot cumulative returns
df_g_cs <- data.frame(x=df_cs$x,
                   y=c(df_cs$AXP, df_cs$AMGN, df_cs$AAPL, df_cs$BA, df_cs$CAT, df_cs$CSCO, df_cs$CVX, df_cs$GS, df_cs$HD, df_cs$HON, df_cs$IBM, df_cs$INTC, df_cs$JNJ, df_cs$KO, df_cs$JPM, df_cs$MCD, df_cs$MMM, df_cs$MRK, df_cs$MSFT, df_cs$NKE, df_cs$PG, df_cs$TRV, df_cs$UNH, df_cs$CRM, df_cs$VZ, df_cs$V, df_cs$WBA, df_cs$WMT, df_cs$DIS, df_cs$DOW),
                   group=c(rep("AXP", n),rep("AMGN", n),rep("AAPL", n),rep("BA", n),rep("CAT", n),rep("CSCO", n),rep("CVX", n),rep("GS", n),rep("HD", n),rep("HON", n),rep("IBM", n),rep("INTC", n),rep("JNJ", n),rep("KO", n),rep("JPM", n),rep("MCD", n),rep("MMM", n),rep("MRK", n),rep("MSFT", n),rep("NKE", n),rep("PG", n),rep("TRV", n),rep("UNH", n),rep("CRM", n),rep("VZ", n),rep("V", n),rep("WBA", n),rep("WMT", n),rep("DIS", n),rep("DOW", n))
)

ggplot(df_g_cs, aes(x, y, col=group)) + geom_line() +
  labs(title="Cumulative Daily Returns of the Dow 30 Stocks", x="Days", y="% Returns")

# Plot DJI
dfj<-data.frame(x=1:xrows, y=dji)
ggplot(dfj, aes(x=x, y=y)) + geom_line(color="blue")+
  labs(title="Daily % Returns of the Dow 30 Index", x="Days", y="% Returns")

# Plot DJI cumulative sum
dfp<-data.frame(x=1:xrows, y=cumsum(dji))
ggplot(dfp, aes(x=x, y=y)) + geom_line(color="blue")+
  labs(title="Cumulative Daily Returns of the Dow 30 Index", x="Days", y="% Returns")

# Plot PC1
dfpc<-data.frame(x=1:xrows, y=pca[,1])
ggplot(dfpc, aes(x=x, y=y)) + geom_line(color="purple")+
  labs(title="Principal Component 1", x="Days", y="% Returns")

# Plot PC1 cumulative sum
dfpc<-data.frame(x=1:xrows, y=cumsum(pca[,1]))
ggplot(dfpc, aes(x=x, y=y)) + geom_line(color="purple")+
  labs(title="Principal Component 1 Cumulative Daily % Returns", x="Days", y="% Returns")

# Plot PC2
dfpc<-data.frame(x=1:xrows, y=pca[,2])
ggplot(dfpc, aes(x=x, y=y)) + geom_line(color="green")+
  labs(title="Principal Component 2", x="Days", y="% Returns")

# Plot PC2 cumulative sum
dfpc<-data.frame(x=1:xrows, y=cumsum(pca[,2]))
ggplot(dfpc, aes(x=x, y=y)) + geom_line(color="green")+
  labs(title="Principal Component 2 Cumulative Daily % Returns", x="Days", y="% Returns")


# Plot Score plot
stock_cluster <- data.frame(A)
cls <- kmeans(x=stock_cluster, centers=3)
stock_cluster$cluster <- as.character(cls$cluster)
head(stock_cluster)

ggplot() +
  geom_point(data = stock_cluster, 
             mapping = aes(x = pca[,1], 
                           y = pca[,2], 
                           colour = cluster)) 



  
