lasso <- function(theta, l){
  return(l*abs(theta))
}

scad <- function(theta, l, a=3.7){
  theta <- abs(theta)
  return((theta <= l)*theta*l + (theta > l & theta <= a*l)*(a*l*theta - (theta^2 + l^2)/2)/(a - 1) + (theta > a*l)*(l^2*(a^2 - 1))/(2*(a - 1)))
}

mcp <- function(theta, l, a=3.7){
  n <- length(theta)
  val <- numeric(n)
  for (i in 1:n){
    x <- abs(theta[i])
    val[i] <- (x < a*l)*(l*x - x^2/(2*a)) + (x >= a*l)*(1/2)*a*l^2
  }
  return(val)
}

### Ploting

range <- 101
x <- seq(-4, 4, len = range)
col <- c("springgreen4", "firebrick2", "deepskyblue3")
lambda <- 1
y <- cbind(lasso(x, lambda), mcp(x, lambda), scad(x, lambda))

pdf("PenaltyTerms.pdf", 12, 8)
matplot(x, y, type='l', bty='n', lty=1, lwd=3, col=col, las=1,
        xlab=expression(beta), ylab=expression(P(beta*'|'*lambda,gamma)))
text(3, 3.5, "LASSO")
text(3, 2.5, "SCAD")
text(3, 1.5, "MCP")
dev.off()

