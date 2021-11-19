# LASSO 

### Plotting

sparse_times <- c(0.05, 0.04, 0.51, 2.82, 7.79, 51.17)
asymsparse_times <- c(0.02, 0.05, 0.23, 1.62, 6.11, 37.36)
sparse_NPN_times <- c(0.01, 0.02, 0.18, 2.34, 7.02, 48.17)
asymsparse__NPN_times <- c(1.36, 0.03, 0.38, 1.51, 3.31, 54.81)

p_options <- c(10, 20, 50, 100, 200, 500)

times_mat <- cbind(rep(p_options,4), c(sparse_times, asymsparse_times, 
                                       sparse_NPN_times, asymsparse__NPN_times),
                                       c(rep(1, 6), rep(2, 6), 
                                       rep(3, 6), rep(4, 6)))

times_data <- as.data.frame(times_mat)
names(times_data) <- c("p", "Time", "group")

library(ggplot2)

# Time
pdf("LASSO_Times.pdf",21,8)
ggplot(data = times_data, aes(x=p, y=Time)) + geom_line(aes(colour=as.factor(group)), size = 1.4) +
  xlab("p") + ylab("Time(in seconds)") + 
  scale_color_manual(name = "Data Structure", values = c("#1E90FF", "#2E8B57", "#FFA62F", "#C04000"), 
                     labels = c("Sparse", "Asymsparse", "Sparse with NPN", "Asymsparse with NPN")) + 
  theme_bw() + theme(text = element_text(size = 25)) 
dev.off()