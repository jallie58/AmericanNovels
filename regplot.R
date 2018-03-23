### Plot Regresion Coefficients
### 
regplot = function(model=model, xlim=c(-.01,.01), pch=23, pointcol="black", sescol="black", num_vars=10, cex.axis = .6) {
  
  coef_summ = as.data.frame(coef(summary(model))[-1,])
  
  coef_summ = coef_summ[order(coef_summ[,4]),][1:num_vars,]
  
  slopes = coef_summ[, 1]  #' slopes
  ses = coef_summ[, 2]  #' SEs
  
  varnames = rownames(coef_summ)
  
  varnames = gsub("`","",varnames)
  varnames = gsub("HalfDecades","",varnames)
  varnames = gsub("MajorPubMajor","MajorPub",varnames)
  varnames = gsub("RACE","",varnames)
  varnames = gsub("GENDER","",varnames)
  
  plot(NA, xlim = xlim, ylim = c(0, num_vars), xlab = "Slope", ylab = "", yaxt = "n")
  # title
  title("Regression Results")
  # y-axis labelling variables
  axis(2, 1:num_vars, varnames, las = 2, cex.axis = cex.axis)
  # vertical line for zero
  abline(v = 0, col = "gray")
  # slopes as points
  points(slopes, 1:num_vars, pch = pch, col = pointcol, bg = pointcol)
  # thick line segments for each 1 SE
  # 
      for (i in 1:num_vars) {
        segments((slopes - ses)[i], i, (slopes + ses)[i], i, col = sescol, lwd = 2)
        # thin line segments for the 2 SEs
        segments((slopes - (2 * ses))[i], i, (slopes + (2 * ses))[i], i, col = sescol, lwd = 1)
      }
}
