# D-trace

### Plotting

sparse_times <- c(0.06, 0.14, 0.96, 4.86, 140.13, 10388.93)
asymsparse_times <- c(0.06, 0.11, 1.07, 3.23, 51.24, 5451.88)
sparse_NPN_times <- c(0.07, 0.08, 0.75, 5.34, 189.67, 8182.14)
asymsparse__NPN_times <- c(0.06, 0.15, 0.91, 4.22, 29.38, 9252.86)

p_options <- c(10, 20, 50, 100, 200, 500)

times_mat <- cbind(rep(p_options,4), c(sparse_times, asymsparse_times, 
                                       sparse_NPN_times, asymsparse__NPN_times),
                   c(rep(1, 6), rep(2, 6), 
                     rep(3, 6), rep(4, 6)))

times_data <- as.data.frame(times_mat)
names(times_data) <- c("p", "Time", "group")

library(ggplot2)

# Time
pdf("D_trace_Times.pdf",21,8)
ggplot(data = times_data, aes(x=p, y=Time)) + geom_line(aes(colour=as.factor(group)), size = 1.4) +
  xlab("p") + ylab("Time(in seconds)") + 
  scale_color_manual(name = "Data Structure", values = c("#1E90FF", "#2E8B57", "#FFA62F", "#C04000"), 
                     labels = c("Sparse", "Asymsparse", "Sparse with NPN", "Asymsparse with NPN")) + 
  theme_bw() + theme(text = element_text(size = 25)) 
dev.off()