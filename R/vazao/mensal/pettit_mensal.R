library(trend)
p <- read.table('mensal.txt', header = T)
for (l in 2:13){
p.res <- pettitt.test(p[,l])
n <- p.res$nobs
i <- p.res$estimate
bmp(filename = sprintf("mensal-%s.bmp", (l-1)), width = 400, height = 400)
plot(p[,'ano'],p.res$data, type = 'p', xlab = colnames(p)[l], ylab = 'U')
points(p[,'ano'],p.res$data, col = 'black', pch = 16)
lines(p[,'ano'],rep(0,n), lty=2, col = c('black'))
lines(p[,'ano'],p.res$data, col = 'black')
dev.off()
if(p.res$p.value<=0.05){
bmp(filename = sprintf("serie-%s.bmp", (l-1)), width = 400, height = 400)
s.1 <- mean(p[1:i[1],l])
s.2 <- mean(p[(i[1]+1):n,l])
s.plot1 <- ts(c(rep(s.1,i[1])))
s.plot2 <- ts(c(rep(s.2,(n-i[1]))))
plot(p[,'ano'],p[,l], type = 'p', xlab = 'Ano', ylab = 'Vaz�o (m�/s)', main = colnames(p)[l])
points(p[,'ano'],p[,l], col = 'black', pch = 16)
lines(p[1:i,'ano'],s.plot1, lty=2, col = c('red'))
lines(p[(i+1):n,'ano'],s.plot2, lty=2, col = 'blue')
legend('topleft', legend=c(sprintf("M�dia (%s-%i)",p[1,1],p[i[1],1]),sprintf("M�dia (%s-%i)",p[i[1]+1,1],p[n,1])),
       col=c("red", "blue"), lty=2, cex=0.8)
dev.off()
}}

#Meses com p.value <=0.05
for (l in 2:13){
p.res <- pettitt.test(p[,l])
if (p.res$p.value <=0.05){
print(l-1)
print(p.res$p.value)
}}
