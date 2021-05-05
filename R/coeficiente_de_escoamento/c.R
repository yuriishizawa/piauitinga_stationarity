library(trend)
#coeficiente de escoamento
q <- read.table('estancia.txt', header = T)
p_est <- read.table('estancia_chuva.txt', header = T)
p_sal <- read.table('salgado_chuva.txt', header = T)
p_thiessen = ((p_est[,'prec']*0.2284 + p_sal[,'prec']*(1-0.2284)))
q_ano = q[,'vazao']*365.25*86400/(419.34*(10**3))
p_thiessen
# C do per?oodo completo
c = q_ano/p_thiessen
tiff("dados.tiff",units = "in", width=5, height=5, res=300)
#bmp(filename = "dados.bmp", width = 400, height = 400)
plot(q[,'ano'],c, type = 'p', xlab = 'Ano', ylab = 'C',
     cex.axis = 1.25, cex.lab = 1.25)
points(q[,'ano'],c, col = 'black', pch = 16)
p.res <- pettitt.test(c)
n <- p.res$nobs
i <- p.res$estimate
s.1 <- mean(c[1:i])
s.2 <- mean(c[(i+1):n])
s.plot1 <- ts(c(rep(s.1,i)))
s.plot2 <- ts(c(rep(s.2,(n-i))))
lines(q[1:i,'ano'],s.plot1, lty=2, col = c('red'))
lines(q[(i+1):n,'ano'],s.plot2, lty=2, col = 'blue')
legend('topleft', legend=c(sprintf("Média (%s-%i)",q[1,1],q[i,1]),sprintf("Média (%s-%i)",q[i+1,1],q[n,1])),
       col=c("red", "blue"), lty=2, cex=1)
dev.off()
tiff("boxplot-c.tiff",units = "in", width=5, height=5, res=300)
#bmp(filename = "boxplot-C.bmp", width = 400, height = 400)
b.plot1 <- ts(c(rep(sprintf("(%s-%i)",q[1,1],q[i,1]),i)))
b.plot2 <- ts(c(rep(sprintf("(%s-%i)",q[i+1,1],q[n,1]),(n-i))))
b = c(b.plot1,b.plot2)
boxplot(c~b, col = 'grey', xlab = 'Período', ylab = 'C',
        cex.axis = 1.25, cex.lab = 1.25)
dev.off()
c_med = mean(q_ano)/mean(p_thiessen)

# C pr?-inflex?oo
c_pre = q_ano[1:23]/p_thiessen[1:23]
plot(q[1:23,'ano'],c_pre, type = 'p', xlab = 'Ano', ylab = 'C')
points(q[1:23,'ano'],c_pre, col = 'black', pch = 16)
c_med_pre = mean(q_ano[1:23])/mean(p_thiessen[1:23])

# C p?s-inflex?o
c_pos = q_ano[24:57]/p_thiessen[24:57]
plot(q[24:57,'ano'],c_pos, type = 'p', xlab = 'Ano', ylab = 'C')
points(q[24:57,'ano'],c_pos, col = 'black', pch = 16)
c_med_pos = mean(q_ano[24:57])/mean(p_thiessen[24:57])
