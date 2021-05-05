library(trend)
tiff("dados.tiff",units = "in", width=5, height=5, res=300)
#bmp(filename = "dados.bmp", width = 400, height = 400)
q <- read.table('estancia.txt', header = T)
plot(q[,'ano'],q[,'vazao'], type = 'p', xlab = 'Ano', ylab = 'Vazão (m³/s)',
     cex.axis = 1.25, cex.lab = 1.25)
points(q[,'ano'],q[,'vazao'], col = 'black', pch = 16)
q.res <- pettitt.test(q[,'vazao'])
n <- q.res$nobs
i <- q.res$estimate
s.1 <- mean(q[1:i,"vazao"])
s.2 <- mean(q[(i+1):n,"vazao"])
s.plot1 <- ts(c(rep(s.1,i)))
s.plot2 <- ts(c(rep(s.2,(n-i))))
s = c(s.plot1,s.plot2)
lines(q[1:i,'ano'],s.plot1, lty=2, col = c('red'))
lines(q[(i+1):n,'ano'],s.plot2, lty=2, col = 'blue')
legend('topleft', legend=c(sprintf("Média (%s-%i)",q[1,1],q[i,1]),sprintf("Média (%s-%i)",q[i+1,1],q[n,1])),
       col=c("red", "blue"), lty=2, cex=1)
dev.off()
#valores de U
bmp(filename = "U-vazao.bmp", width = 400, height = 400)
plot(q[,'ano'],q.res$data, type = 'p', xlab = 'Ano', ylab = 'U')
points(q[,'ano'],q.res$data, col = 'black', pch = 16)
lines(q[,'ano'],rep(0,n), lty=2, col = c('black'))
lines(q[,'ano'],q.res$data, col = 'black')
dev.off()
#boxplot
tiff("boxplot-vazao.tiff",units = "in", width=5, height=5, res=300)
#bmp(filename = "boxplot-vazao.bmp", width = 400, height = 400)
b.plot1 <- ts(c(rep(sprintf("(%s-%i)",q[1,1],q[i,1]),i)))
b.plot2 <- ts(c(rep(sprintf("(%s-%i)",q[i+1,1],q[n,1]),(n-i))))
b = c(b.plot1,b.plot2)
boxplot(q[,'vazao']~b, col = 'grey', xlab = 'Período', ylab = 'Vazão (m³/s)',
        cex.axis = 1.25, cex.lab = 1.25)
dev.off()