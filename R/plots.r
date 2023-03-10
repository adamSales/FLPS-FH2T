

disc <- rbind(
  data.frame(disc=colMeans(tplDraws$disc),mod='2PL'),
  data.frame(disc=colMeans(grmDraws$lambda[,,1]),mod='GRM'),
  data.frame(disc=colMeans(gpcmDraws$lambda[,,1]),mod='GPCM'))
