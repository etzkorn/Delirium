# calculate expectations of transformations of random effects
# conditinal on paramaters from previous step (phi0)
Ew <- integrate.w(data, phi = phi0, f.w = function(w) w)
Ew2 <- integrate.w(data, phi = phi0, f.w = function(w) w^2)
Eew <- integrate.w(data, phi = phi0, f.w = function(w) exp(w))
Eeaw <- integrate.w(data, phi = phi0, f.w = function(w) exp(phi["alpha"]*w))