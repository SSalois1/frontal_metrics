# Updated test data set - all positive and zeros changed to 0.01
# Input data (m = gradient magnitude, x = grad_x component, y = grad_y component)
m = c(0.0570000,NA,0.500000,0.418000,
      0.418000,NA,0.430000,0.430000,
      0.0570000,NA,	0.0570000,0.0530000,
      0.0530000,0.500000, 0.706000,	0.530000,
      0.0570000,NA, 0.500000,	0.706000,
      0.530000,0.418000, 0.418000,	NA,
      0.430000,0.430000,0.0570000, NA,
      0.0570000,0.0530000,0.0530000,	0.500000,
      0.0570000,NA, 0.500000,	0.0530000,
      0.500000, 0.706000,	0.530000, 0.418000,
      0.418000, NA, 0.430000, 0.430000,
      0.0570000,NA,	0.0570000,0.0530000,
      0.0570000, NA, 0.500000,	0.0570000,
      0.0530000,0.0530000, 0.500000, 0.706000,
      0.530000, 0.418000, 0.418000,NA,
      0.430000, 0.430000,	0.0570000, NA,
      0.0570000,NA, 0.500000,0.0570000,
      NA,	0.0570000,0.0530000,0.0530000,
      0.500000, 0.706000,	0.530000,0.418000,
      0.418000, NA, 0.430000, 0.430000,
      0.0570000, NA, 0.500000, 0.430000,
      0.430000,	0.0570000,NA,	0.0570000,
      0.0530000,0.0530000, 0.500000,	0.706000,
      0.530000, 0.418000, 0.418000,	NA,
      0.0570000, NA,0.500000, 0.418000,
      NA,	0.430000,	0.430000,	0.0570000,
      NA,	0.0570000,0.0530000,0.0530000,
      0.500000, 0.706000, 0.530000,0.418000)

x = c(0.0420000,NA, 5.00000,0.418000,
      0.418000, NA,0.430000, 0.430000,
      0.0420000, NA,0.0420000,0.0190000,
      0.0190000,0.01,0.477000,	0.190000,
      0.0420000,NA,5.00000, 0.477000,
      0.190000, 0.418000, 0.418000,NA,
      0.430000, 0.430000,	0.0420000, NA,
      0.0420000,0.0190000,0.0190000,0.01,
      0.0420000,NA, 5.00000,0.0190000,
      0.01, 0.477000, 0.190000, 	0.418000,
      0.418000, NA, 0.430000,0.430000,
      0.0420000,NA,0.0420000,	0.0190000,
      0.0420000,NA, 5.00000,0.0420000,
      0.0190000,0.0190000,0.01,0.477000,
      0.190000, 0.418000,0.418000,NA,
      0.430000,0.430000,0.0420000,NA,
      0.0420000,NA,5.00000,0.0420000,
      NA,	0.0420000,0.0190000,0.0190000,
      0.01,0.477000,0.190000,0.418000,
      0.418000,NA,0.430000,0.430000,
      0.0420000,NA,5.00000,0.430000,
      0.430000,0.0420000,NA,0.0420000,
      0.0190000,0.0190000,0.01, 0.477000,
      0.190000,0.418000,0.418000,NA,
      0.0420000,NA,5.00000,0.418000,
      NA,0.430000,0.430000,0.0420000,
      NA,0.0420000,0.0190000,0.0190000,
      0.01,0.477000,0.190000,0.418000)

y = c(0.0390000,NA, 0.01, 0.00160000,
      0.00160000,NA,0.0100000,0.0100000,
      0.0390000,NA,	0.0390000,0.0490000,
      0.0490000,0.500000, 0.521000, 0.490000,
      0.0390000,NA, 0.01,0.521000,
      0.490000, 0.00160000,0.00160000,NA,
      0.0100000,0.0100000,0.0390000,NA,
      0.0390000,0.0490000,0.0490000,0.500000,
      0.0390000,NA,0.01,0.0490000,
      0.500000,0.521000,	0.490000,0.00160000,
      0.00160000,NA,	0.0100000,0.0100000,
      0.0390000,NA,	0.0390000,	0.0490000,
      0.0390000,NA,0.01,	0.0390000,
      0.0490000,0.0490000,0.500000,0.521000,
      0.490000,0.00160000,0.00160000,NA,
      0.0100000,0.0100000,0.0390000,NA,
      0.0390000,NA, 0.01,0.0390000,
      NA,	0.0390000,0.0490000,0.0490000,
      0.500000,0.521000,0.490000,0.00160000,
      0.00160000,NA,	0.0100000,0.0100000,
      0.0390000,NA,0.01,	0.0100000,
      0.0100000,0.0390000, NA,0.0390000,
      0.0490000,0.0490000,0.500000,0.521000,
      0.490000,0.00160000, 0.00160000,NA,
      0.0390000,NA,0.01,0.00160000,
      NA,	0.0100000,0.0100000,0.0390000,
      NA,	0.0390000,0.0490000,0.0490000,
      0.500000,0.521000,0.490000, 0.00160000)

# Create an array with dim (4,4,7,3) - lat,lon,time,var (var 1 = m , 2 = x, 3 = y)
sims_array <- aperm(array(c(m,x,y), dim = c(4,4,7,3)), perm = c(2,1,3,4))
# Save sims_array array to two separate objects 
sims <- sims_array
front <- sims_array
threshold = 0.4 # threshold for classifying front
# Create frontal array (via setting setting all values < threshold to 0)
front[front < threshold] <- 0 # set all values < thresh == 0 
front[is.na(front)] <- 0
sum(is.na(front))
length(front)
rm(sims_array)
# -- SET UP FUNs FOR EACH VAR -- # 
# gradient magnitude
fvalid <- apply(front[,,,1], c(1,2), function(x) sum((x > 0)))
m_sum <- apply(front[,,,1], c(1,2), function(x) sum(na.omit(x)))
fclear_m <- apply(sims[,,,1], c(1,2),  function(x) sum(na.omit(x > 0)))

# x
x_based_on_mag <- ifelse(front[,,,1] > 0.0, sims[,,,2], NA) # selects values of x only if pixel indicated as frontal from grad_mag
x_sum <- apply(x_based_on_mag, c(1,2),  function(x) sum(na.omit(x)))
x_sumsqr <- apply(x_based_on_mag, c(1,2), function(x) {
   sum((na.omit(x) - mean(na.omit(x)))^2) })
# x_var <- apply(x_based_on_mag, c(1,2), function(x) {
#    sum((x - mean(x))^2)/(length(x)-1)})

# y
y_based_on_mag <- ifelse(front[,,,1] > 0.0, sims[,,,3], NA)
y_sum <- apply(y_based_on_mag, c(1,2),  function(x) sum(na.omit(x))) # added na.omit to make it only on frontal pixels
y_sumsqr <- apply(y_based_on_mag, c(1,2), function(x) {
   sum((na.omit(x) - mean(na.omit(x)))^2) })
# y_var <- apply(y_based_on_mag, c(1,2), function(x) {
#    sum((x - mean(x))^2)/(length(x)-1) })

# -- COMPUTE FRONTAL METRICS -- #
# fmean = sum of gradient values/total number of frontal pixels 
fmean = (sqrt((x_sum/fvalid)^2 + (y_sum/fvalid)^2))
# fvar = variance of gradient values
fvar = (x_sumsqr + y_sumsqr)/fvalid
# fsd =  sqrt of variance of gradient values
fsd = sqrt(fvar)
# fcv = fsd/fmean
fcv = fsd/fmean
# fprob = front pixels/clear pixels 
fprob = fvalid/fclear_m
# fintst = frontal intensity, fmean * fprob
fintst = fmean * fprob
# fpers = based on  threshold -> if fsd meets threshold for that week, assign 1, else 0
pers_thresh <- 0.08  # some threshold range (0 - 1) indicating low sd, thus persistence * value will depend on actual data
# generates binary array to calc cumulative persistence across time (weeks, months, etc)
fpers = ifelse(fsd <= pers_thresh, 1, 0)




## --- TESTING --- ## 

test = matrix(nrow = 4, ncol = 4)
test[,1] <- c(NA, 0, 1, 0)
test[,2] <- c(NA, 0, 1, 1)
test[,3] <- c(1, 1, 0, 1)
test[,4] <- c(1, 0, 0, 0)

test2 = matrix(nrow = 4, ncol = 4)
test2[,1] <- c(0, 0, 1, 1)
test2[,2] <- c(0, 0, 1, 1)
test2[,3] <- c(1, 1, 0, 1)
test2[,4] <- c(1, 0, 0, 0)


test * fprob
test * fclear_m
test2 * fprob


