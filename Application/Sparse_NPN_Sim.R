library(dineR)

n <- 100

p_options <- c(10, 20, 50, 100, 200, 500)
case <- "sparse"

loss_options <- c("lasso", "d-trace", "scad", "mcp")
nlambda <- 20
stop_tol <- 1e-3
perturb <- F
correlation <- F
max_iter <- 500
lambda_min_ratio <- 0.5
tuning <- "EBIC"

times <- c()
TPR_all <- c()
FPR_all <- c()

for(i in 1:length(p_options)){
  
  data <- data_generator(n = n, p = p_options[i], case = case, seed = 123)
  
  X <- data$X
  Y <- data$Y
  
  X <- npn(X)
  Y <- npn(Y)
  
  for(j in 1:length(loss_options)){
    
    result <- estimation(X, Y, loss = loss_options[j], nlambda = nlambda,  
                         stop_tol = stop_tol, perturb = perturb, 
                         correlation = correlation, max_iter = max_iter, 
                         lambda_min_ratio = lambda_min_ratio, tuning = tuning)
    
    times <- c(times, result$elapse)
    
    opt_est <- result$path[[result$loss_index]]
    
    TPR <- sum(data$Delta == 0 & result$path[[result$loss_index]] == 0)/sum(data$Delta == 0)
    FPR <- sum(data$Delta == 0 & result$path[[result$loss_index]] != 0)/sum(data$Delta == 0)
    
    TPR_all <- c(TPR_all, TPR)
    FPR_all <- c(FPR_all, FPR)
    
  }
  
}

lasso_ind <- seq(1, length(times), by = 4)
d_trace_ind <- seq(2, length(times), by = 4)
scad_ind <- seq(3, length(times), by = 4)
mcp_ind <- seq(4, length(times), by = 4)

lasso_times <- times[lasso_ind]
lasso_TPR <- TPR_all[lasso_ind]

d_trace_times <- times[d_trace_ind]
d_trace_TPR <- TPR_all[d_trace_ind]

scad_times <- times[scad_ind]
scad_TPR <- TPR_all[scad_ind]

mcp_times <- times[mcp_ind]
mcp_TPR <- TPR_all[mcp_ind]

times_mat <- cbind(rep(p_options,4), c(lasso_times, d_trace_times, 
                                       scad_times, mcp_times),c(rep(1, 6), rep(2, 6), 
                                                                rep(3, 6), rep(4, 6)))

times_mat[12,2] <- NA

times_data <- as.data.frame(times_mat)
names(times_data) <- c("p", "Time", "Loss")

TPR_mat <- cbind(rep(p_options,4), c(lasso_TPR, d_trace_TPR, 
                                     scad_TPR, mcp_TPR),c(rep(1, 6), rep(2, 6), 
                                                          rep(3, 6), rep(4, 6)))

TPR_data <- as.data.frame(TPR_mat)
names(TPR_data) <- c("p", "TPR", "Loss")

### Plotting

library(ggplot2)

# Time
pdf("Sparse_NPN_Sim_Times.pdf",14,8)
ggplot(data = times_data, aes(x=p, y=Time)) + geom_line(aes(colour=as.factor(Loss)), size = 1.4) +
  xlab("p") + ylab("Time(in seconds)") + 
  scale_color_manual(name = "Loss function", values = c("#1E90FF", "#2E8B57", "#FFA62F", "#C04000"), 
                     labels = c("Lasso", "D-trace", "SCAD", "MCP")) + 
  theme_bw() + theme(text = element_text(size = 17)) 
dev.off()
# TPR
pdf("Asymsparse_NPN_TPR.pdf",14,8)
ggplot(data = TPR_data, aes(fill=as.factor(Loss), y=TPR, x=as.factor(p))) + 
  geom_bar(position="dodge", stat="identity") + xlab("p") + ylab("True Positive Rate") + 
  scale_fill_manual(name = "Loss function", values = c("#1E90FF", "#2E8B57", "#FFA62F", "#C04000"), 
                    labels = c("Lasso", "D-trace", "SCAD", "MCP")) + 
  theme_bw() + coord_cartesian(ylim=c(0.84,1)) + theme(text = element_text(size = 17)) 
dev.off()

