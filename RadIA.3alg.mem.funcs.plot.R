
#------------------------------
# Name: RadIA.3alg.mem.funcs.plot.m
#
# Purpose:
#
# Notes:
#
# Created: 7.10.2020 dserke
#
#------------------------------

#------------------------------
# define fis.SLLW
fis.SLLW <- newfis('SLLW Algo')

# add variable to fis.SSLW
fis.SLLW <- addvar(fis.SLLW, 'input', '', c(  0.0, 10.0))
fis.SLLW <- addvar(fis.SLLW, 'input', '', c(-20.0, 20.0))
fis.SLLW <- addvar(fis.SLLW, 'input', '', c(  0.0, 10.0))
fis.SLLW <- addvar(fis.SLLW, 'input', '', c(  0.0, 10.0))

# add MF to fis.SLLW
fis.SLLW <- addmf(fis.SLLW, 'input', 1, '', 'trapmf', c(  0.00,  0.001, 1.00, 2.50))
fis.SLLW <- addmf(fis.SLLW, 'input', 2, '', 'trapmf', c(-18.00, -10.00, 5.00, 8.00))
fis.SLLW <- addmf(fis.SLLW, 'input', 3, '', 'trapmf', c(  0.00,  0.001, 1.00, 6.00))
fis.SLLW <- addmf(fis.SLLW, 'input', 4, '', 'trapmf', c(  0.00,  0.001, 3.00, 4.50))

# plot MF for SLLW
par(mfrow = c(2, 2))
par(mar   = c(2, 2, 2, 2))
plotmf(fis.SLLW, "input", 1, main="", xlab = "sdevREF",  ylab = "INT value")
plotmf(fis.SLLW, "input", 2, main="", xlab = "meanREF",  ylab = "INT value")
plotmf(fis.SLLW, "input", 3, main="", xlab = "TREF",     ylab = "INT value")
plotmf(fis.SLLW, "input", 4, main="", xlab = "meanTREF", ylab = "INT value")

#------------------------------------
# for SSLW
par(mfrow = c(2, 2))
par(mar   = c(4, 5, 2, 2))
P1 <- TrapezoidalFuzzyNumber(-0.25, -0.1, 0.1, 0.25)
plot(P1, lty=1, col="black", font=2, lwd=5, cex.lab=1.5, xlim=c(-0.3, 0.3), xlab="meanZDR", ylab="INT value")
P1 <- TrapezoidalFuzzyNumber(0.0, 0.0, 0.15, 0.35)
plot(P1, lty=1, col="black", font=2, lwd=5, cex.lab=1.5, xlim=c( 0.0, 0.5), xlab="sdevZDR", ylab="INT value")
P1 <- TrapezoidalFuzzyNumber(-0.07, -0.01, 0.01, 0.07)
plot(P1, lty=1, col="black", font=2, lwd=5, cex.lab=1.5, xlim=c(-0.3, 0.3), xlab="meanKDP", ylab="INT value")
P1 <- TrapezoidalFuzzyNumber(0.0, 0.0, 0.10, 0.20)
plot(P1, lty=1, col="black", font=2, lwd=5, cex.lab=1.5, xlim=c( 0.0, 0.3), xlab="sdevKDP", ylab="INT value")

# for SLLW
par(mfrow = c(2, 2))
par(mar   = c(4, 5, 2, 2))
P1 <- PiecewiseLinearFuzzyNumber(-18, -10, 5, 8,       knot.n=6, knot.alpha=c(0.10,0.20,0.30,0.70,0.80,0.90), knot.left=c(-16.2,-15.6,-14.8,-13.2,-12.4,-11.6), knot.right=c(5.6,5.9,6.2,6.8,7.1,7.4))
plot(P1, lty=1, col="black", font=2, lwd=5, cex.lab=1.5, xlim=c(-20, 10), xlab="meanDBZ", ylab="INT value")
P2 <- PiecewiseLinearFuzzyNumber(-2.5, -1.0, 1.0, 2.5, knot.n=6, knot.alpha=c(0.05,0.20,0.30,0.70,0.80,0.90), knot.left=c(-2.35,-2.05,-1.9,-1.6,-1.45,-1.3),    knot.right=c(1.3,1.45,1.6,1.9,2.05,2.35))
plot(P2, lty=1, col="black", font=2, lwd=5, cex.lab=1.5, xlim=c(0, 5), xlab="sdevDBZ", ylab="INT value")
P3 <- PiecewiseLinearFuzzyNumber(-6.0, -1.0, 1.0, 6.0, knot.n=6, knot.alpha=c(0.10,0.20,0.30,0.70,0.80,0.90), knot.left=c(-5,-4.5,-4,-3,-2.5,-2),               knot.right=c(2,2.5,3,4,4.5,5))
plot(P3, lty=1, col="black", font=2, lwd=5, cex.lab=1.5, xlim=c(0, 10), xlab="TDBZ", ylab="INT value")
P4 <- PiecewiseLinearFuzzyNumber(-4.5, -3.0, 3.0, 4.5, knot.n=6, knot.alpha=c(0.10,0.20,0.30,0.70,0.80,0.90), knot.left=c(-4.2,-4.05,-3.9,-3.6,-3.45,-3.3),     knot.right=c(3.3,3.45,3.6,3.9,4.05,4.2))
plot(P4, lty=1, col="black", font=2, lwd=5, cex.lab=1.5, xlim=c(0, 5), xlab="medTDBZ", ylab="INT value")

# for MPHA
par(mfrow = c(1, 3))
par(mar   = c(4, 5, 2, 2))
P1 <- TrapezoidalFuzzyNumber(0, 10.0, 30.0, 40.0)
plot(P1, lty=1, col="black", font=2, lwd=5, cex.lab=1.5, xlim=c(-10, 50), xlab="meanDBZ", ylab="INT value")
P2 <- TrapezoidalFuzzyNumber(0, 1.0, 3.0, 4.0)
plot(P2, lty=1, col="black", font=2, lwd=5, cex.lab=1.5, xlim=c(-1, 5), xlab="meanZDR", ylab="INT value")
P3 <- TrapezoidalFuzzyNumber(-22.0, -19.0, -12.0, -8.0)
plot(P3, lty=1, col="black", font=2, lwd=5, cex.lab=1.5, xlim=c(-25, 0), xlab="tempNWP", ylab="INT value")

