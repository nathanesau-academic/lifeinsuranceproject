# survival model parameters
A <- 0.00022 
B <- 2.7e-06  
# B <- 2.5e-05
c <- 1.124 
# c = 1.1

# w <- 131 
w <- 120 # limiting age
i <- 0.05 # interest rate

bf <- 12 # benefit frequency
pf <- 12 # premium frequency
AB <- 50000 # annual benefit
gp <- 10 # guarantee period
x <- 30 # min. age
u <- (65-x) # deferral period

lossFunction <- function (t, premium) {
  m <- pf
  kmplus1premium <- t + (1 / pf)
  kmplus1benefit <- t + (1 / bf)
  
  if (0 <= t & t < u) {
    -adueanglenupperm(i, kmplus1premium, pf) * premium
  } else if (t >= u) {
    if (t < (u + gp)) {
      v(i, u) * AB * adueanglenupperm(i, gp, bf) - adueanglenupperm(i, u, pf) * premium
    } else {
      if (t >= (w - x - 1)) {
        v(i, u) * AB * adueanglenupperm(i, w - x - u, bf) - 
          adueanglenupperm(i, u, pf) * premium
      } else { 
        v(i, u) * AB * adueanglenupperm(i, kmplus1benefit - u, bf) - 
          adueanglenupperm(i, u, pf) * premium
      }
    }
  } else {
    0
  }
}

expectedValueLossFunction <- function(premium, x, u, t=0) {
  if (t == 0) {
    AB * udeferredadueselectxguaranteedupperm(u, x, gp, bf) - 
             adueselectxtermupperm(x, u, pf) * premium
  } else if (t == 1) {
    AB * udeferredadueselectxplus1guaranteedupperm((u - t), x, gp, bf) - 
      adueselectxplus1termupperm(x, (u - t), pf) * premium
  } else {
    if (t < u) {
      AB * udeferredaduexguaranteedupperm((u - t), (x + t), gp, bf) - 
        aduextermupperm((x + t), (u - t), pf) * premium
    } else if (t < (u + n)) {
      AB * aduexguaranteedupperm((x + t), (u + gp - t), bf)
    } else {
      if ((x + t) >= w) {
        0
      } else {
        AB * aduexupperm((x + t), bf)
      }
    }
  }
}

varianceLossFunction <- function(premium, x, u, t= 0) {
  n <- gp
  my <- bf
  mz <- pf
  
  covariance <- nEselectx(u, x) * (adueanglenupperm(i, gp, bf) * adueanglenupperm(i, u, pf) + 
                nEx(n, x + u) * aduexupperm(x + u + n, bf) * adueanglenupperm(i, u, pf)) - 
                adueselectxtermupperm(x, u, pf) * udeferredadueselectxguaranteedupperm(u, x, gp, bf)
  variancepremium <- (Aselectxendowmentupperm(x, u, mz, 2) - Aselectxendowmentupperm(x, u, mz) ^ 2) / 
                     (dupperm(i, mz) ^ 2)
  variancebenefit <- (tpx(n, (x + u)) * v(i, (u + n)) ^ 2 * (tqx(n, (x + u)) * aduexupperm(x + u + n, my) ^ 2 + 
                     (Axupperm(x + u + n, my, 2) - 
                      Axupperm(x + u + n, my) ^ 2) / dupperm(i, my) ^ 2)) * tpselectx(u, x) + 
                     (v(i, u) * aduexguaranteedupperm(x + u, n, my)) ^ 2 * tpselectx(u, x) * tqselectx(u, x)
  
  if(t==0) {
    (premium ^ 2) * variancepremium + (AB ^ 2) * variancebenefit - 2 * premium * AB * covariance
  } else {
    0
  }
}

covarianceLossTerm <- function() {
  n <- gp
  nEselectx(u, x) * (adueanglenupperm(i, gp, bf) * adueanglenupperm(i, u, pf) + 
              nEx(n, x + u) * aduexupperm(x + u + n, bf) * adueanglenupperm(i, u, pf)) - 
              adueselectxtermupperm(x, u, pf) * udeferredadueselectxguaranteedupperm(u, x, gp, bf)
}

varianceudeferredadueselectxguaranteedupperm <- function() {
  n <- gp
  my <- bf
  exp_var_ygiveni <- (tpx(n, (x + u)) * v(i, (u + n)) ^ 2 * (tqx(n, (x + u)) * 
                      aduexupperm(x + u + n, my) ^ 2 + (Axupperm(x + u + n, my, 2) - 
                      Axupperm(x + u + n, my) ^ 2) / dupperm(i, my)^ 2)) * tpselectx(u, x)
  var_exp_ygiveni <- (v(i, u) * aduexguaranteedupperm(x + u, n, my)) ^ 2 * tpselectx(u, x) * tqselectx(u, x)
  exp_var_ygiveni + var_exp_ygiveni
}

varianceadueselectxguaranteedupperm <- function() {
  mz <- pf
  
  (Aselectxendowmentupperm(x, u, mz, 2) - Aselectxendowmentupperm(x, u, mz) ^ 2) / (dupperm(i, mz) ^ 2)
}

getPremium <- function() {
  udeferredadueselectxguaranteedupperm(u, x, gp, bf) * AB / adueselectxtermupperm(x, u, pf)
}

adueanglen <- function(i,n) {
  (1-v(i,n))/i 
}

adueanglenupperm <- function(i,n,m) {
  (1-v(i,n))/dupperm(i,m)
}

adueselectx <- function(x) {
  ( 1 - Aselectx(x))/ d(i)
}

adueselectxplus1 <- function(x) {
  (1-Aselectxplus1(x))/d(i)
}

adueselectxplus1term <- function(x,n) {
  (1-Aselectxplus1endowment(x,n))/d(i)
}

adueselectxplus1termupperm <- function(x,n,m) {
  (1-Aselectxplus1endowmentupperm(x,n,m))/dupperm(i,m)
}

adueselectxguaranteed <- function(x,n) {
  if (n == 1) {
    adueanglen(i, 1) + nEselectx(1, x) * adueselectxplus1(x)
  }
  else {
    adueanglen(i, n) + nEselectx(n, x) * aduex(x + n)
  }
}

adueselectxguaranteedupperm <- function(x, n, m) {
  if (n == 1) {
    adueanglenupperm(i, 1, m) + nEselectx(n, x) * adueselectxplus1upperm(x, m)
  } else {
    adueanglenupperm(i, n, m) + nEselectx(n, x) * aduexupperm(x + n, m)
  }
}

adueselectxupperm <- function(x, m) {
  (1 - Aselectxupperm(x, m)) / dupperm(i, m)
}

adueselectxplus1upperm <- function(x, m) {
  (1 - Aselectxplus1upperm(x, m)) / dupperm(i, m)
}

adueselectxterm <- function(x, n) {
  (1 - Aselectxendowment(x, n)) / d(i)
}

adueselectxtermupperm <- function(x, n, m) {
  (1 - Aselectxendowmentupperm(x, n, m)) / dupperm(i, m)
}

aduex <- function(x) {
  (1 - Ax(x)) / d(i)
}

aduexterm <- function(x, n) {
  (1 - Axendowment(x, n)) / d(i)
}

aduextermupperm <- function(x, n, m) {
  (1 - Axendowmentupperm(x, n, m)) / dupperm(i, m)
}

Axendowment <- function(x, n, Moment= 1) {
  Axterm(x, n, Moment) + nEx(n, x, Moment)
}

Axendowmentupperm <- function(x, n, m, Moment= 1) {
  Axtermupperm(x, n, m, Moment) + nEx(n, x, Moment)
}

Axterm<-function(x, n, Moment= 1) {
  # whole life insurance minus a deferred whole life insurance
  Ax(x, Moment) - ndeferredAx(n, x, Moment)
}

Axtermupperm <- function(x, n, m, Moment= 1) {
  # whole life insurance minus a deferred whole life insurance
  Axupperm(x, m, Moment) - ndeferredAxupperm(n, x, m, Moment)
}

aduexguaranteed <- function(x, n){
  adueanglen(i, n) + nEx(n, x) * aduex(x + n)
}

aduexguaranteedupperm <- function(x, n, m) {
  adueanglenupperm(i, n, m) + nEx(n, x) * aduexupperm(x + n, m)
}

aduexupperm <- function(x, m) {
  (1 - Axupperm(x, m)) / dupperm(i, m)
}

Aselectx <- function(x, Moment= 1) {
  v(i, 1, Moment) * tqselectx(1, x) + v(i, 2, Moment) * tqselectxplus1(1, x) * tpselectx(1, x) + 
    v(i, 2, Moment) * tpselectx(2, x) * Ax(x + 2, Moment)
}

Aselectxupperm <- function(x, m,Moment= 1) {
  if (Moment == 2) {
    istar <- exp(2 * log(1 + i)) - 1
    istarupperm <- iupperm(istar, m)
    Aselectx(x, Moment) * istar / istarupperm
  } else {
    Aselectx(x) * i / iupperm(i, m)
  }
}

Aselectxplus1 <- function(x, Moment= 1) {
  v(i, 1, Moment) * tqselectxplus1(1, x) + v(i, 1, Moment) * tpselectxplus1(1, x) * Ax(x + 2, Moment)
}

Aselectxplus1upperm <- function(x, m , Moment= 1) {
  if (Moment == 2) {
    istar <- exp(2 * log(1 + i)) - 1
    istarupperm <- iupperm(istar, m)
    Aselectxplus1(x, Moment) * istar / istarupperm
  } else { 
    Aselectxplus1(x) * i / iupperm(i, m)
  }
}

Aselectxendowment <- function(x, n, Moment= 1) {
  Aselectxterm(x, n, Moment) + nEselectx(n, x, Moment)
}

Aselectxendowmentupperm <- function(x, n, m , Moment = 1) {
  Aselectxtermupperm(x, n, m, Moment) + nEselectx(n, x, Moment)
}

Aselectxterm <- function(x, n, Moment = 1) {
  Aselectx(x, Moment) - ndeferredAselectx(n, x, Moment)
}


Aselectxplus1term <- function(x, n, Moment= 1) {
  Aselectxplus1(x, Moment) - ndeferredAselectxplus1(n, x, Moment)
}

Aselectxplus1termupperm <- function(x, n, m, Moment = 1) {
  Aselectxplus1upperm(x, m, Moment) - ndeferredAselectxplus1upperm(n, x, m, Moment)
}

ndeferredAselectxplus1 <- function(n, x, Moment= 1) {
  if (n == 0) {
    nEselectx(n, x, Moment) * Aselectxplus1(x, Moment)
  } else {
    nEselectx(n, x, Moment) * Ax(x + n, Moment)
  }
}

ndeferredAselectxplus1upperm <- function(n, x, m, Moment= 1) {
  if (n == 0) {
    nEselectx(n, x, Moment) * Aselectxplus1upperm(x, Moment)
  } else {
    nEselectx(n, x, Moment) * Axupperm(x + n, Moment)
  }
}

Aselectxplus1endowment <- function(x, n,Moment= 1) {
  Aselectxplus1term(x, n, Moment) + nEselectxplus1(n, x, Moment)
}

Aselectxplus1endowmentupperm <- function(x, n, m, Moment = 1) {
  Aselectxplus1termupperm(x, n, m, Moment) + nEselectxplus1(n, x, Moment)
}

Aselectxtermupperm <- function(x, n, m, Moment = 1) {
  Aselectxupperm(x, m, Moment) - ndeferredAselectxupperm(n, x, m, Moment)
}

Ax <- function(x, Moment = 1) {
  Total <- 0
  for (k in 0:(w - x - 1)) {
    Total <- Total + udeferredtqx(k, 1, x) * v(i, k + 1, Moment)
  }
  Total
}

Axupperm <- function(x, m, Moment= 1) {
  if (Moment == 2){
    istar <- exp(2 * log(1 + i)) - 1
    istarupperm <- iupperm(istar, m)
    Ax(x, Moment) * istar / istarupperm
  } else {
    Ax(x, Moment) * i / iupperm(i, m)
  }
}

d <-function(i) {
  i / (1 + i)
}

dupperm <- function(i, m) {
  m * (1 - (1 - d(i)) ^ (1 / m))
}

iupperm <- function(i, m) {
  m * ((1 + i) ^ (1 / m) - 1)
}

ndeferredadueselectx <- function(n, x) {
  if (n == 0) {
    nEselectx(n, x) * adueselectx(x)
  } else if (n==1){
    nEselectx(1, x) * adueselectxplus1(x)
  } else {
    nEselectx(n, x) * aduex(x + n)
  } 
}

ndeferredadueselectxupperm <- function(n, x, m ) {
  if (n==0) {
    nEselectx(n, x) * adueselectxupperm(x, m)
  } else if(n==1) {
    nEselectx(1, x) * adueselectxplus1upperm(x, m)
  } else { 
    nEselectx(n, x) * aduexupperm(x + n, m)
  }
}

ndeferredaduex <- function(n, x) {
  nEx(n, x) * aduex(x + n)
}

ndeferredaduexupperm <- function(n, x , m) {
  nEx(n, x) * aduexupperm(x + n, m)
}

ndeferredAselectx <- function(n, x, Moment= 1) {
  if (n == 0) {
    nEselectx(n, x, Moment) * Aselectx(x, Moment)
  } else if (n == 1) {
    nEselectx(1, x, Moment) * Aselectxplus1(x, Moment)
  } else {
    nEselectx(n, x, Moment) * Ax(x + n, Moment)
  }
}

ndeferredAselectxupperm <- function(n, x, m, Moment= 1) {
  if (n == 0) {
    nEselectx(n, x, Moment) * Aselectxupperm(x, m, Moment)
  } else if (n == 1) {
    nEselectx(1, x, Moment) * Aselectxplus1upperm(x, m, Moment)
  } else {
    nEselectx(n, x, Moment) * Axupperm(x + n, m, Moment)
  }
}

ndeferredAx <- function(n, x, Moment= 1) {
  nEx(n, x, Moment) * Ax(x + n, Moment)
}

ndeferredAxupperm <- function(n, x, m, Moment = 1) {
  nEx(n, x, Moment) * Axupperm(x + n, m, Moment)
}

nEselectxplus1 <- function(n, x, Moment= 1) {
  v(i, n, Moment) * tpselectxplus1(n, x)
}

nEselectx <- function(n, x, Moment= 1) {
  v(i, n, Moment) * tpselectx(n, x)
}

nEx <- function(n, x,Moment= 1) {
  v(i, n, Moment) * tpx(n, x)
}

tpselectx <- function(t, x) {
  if((x + t) >= w) {
    return(0)
  }
  
  if (0 <= t & t <= 2) {
    exp(0.9 ^ (2 - t) * ((A * (1 - 0.9 ^ t) / log(0.9)) + 
                           (B * c ^ x * (c ^ t - 0.9 ^ t) / log(0.9 / c))))
  } else {
    exp(0.9 ^ (2 - 2) * ((A * (1 - 0.9 ^ 2) / log(0.9)) + (B * c ^ x * (c ^ 2 - 0.9 ^ 2) / 
                                                             log(0.9 / c)))) * tpx(t - 2, x + 2)
  } 
}

tpselectxplus1 <- function(t, x) {
  if ((x + t) >= (w - 1)) {
    return(0)
  }
  
  if(0 <= t & t <= 1) {
    exp(0.9 ^ (1 - t) * ((A / log(0.9) * (1 - 0.9 ^ t)) + (B * c ^ (x + 1) / 
                                                             log(0.9 / c) * (c ^ t - 0.9 ^ t))))
  } else {
    exp(0.9 ^ (1 - 1) * ((A / log(0.9) * (1 - 0.9 ^ 1)) + (B * c ^ (x + 1) / 
                                              log(0.9 / c) * (c ^ 1 - 0.9 ^ 1)))) * tpx(t - 1, x + 1)
  }
}

tpx <- function(t, x) {
  if ((x + t) >= (w)) {
    0
  } else {
    exp(-((A * t) + (B / log(c)) * (c ^ x) * (c ^ t - 1)))
  }
}

tqselectx <- function(t, x) {
  1 - tpselectx(t, x)
}

tqselectxplus1 <- function(t, x) {
  1 - tpselectxplus1(t, x)
}

tqx <- function(t, x) {
  1 - tpx(t, x)
}
 
udeferredadueselectxguaranteed <- function(u, x, n) {
  if ((u + n) == 0) {
    adueanglen(i, 1) + nEselectx(1, x) * adueselectx(x)
  } else if (u+n==1) {
    adueanglen(i, 1) + nEselectx(1, x) * adueselectxplus1(x)
  } else {
    v(i, u) * tpselectx(u, x) * adueanglen(i, n) + nEselectx(u + n, x) * aduex(x + u + n)
  }
}

udeferredadueselectxguaranteedupperm <- function(u, x, n, m) {
 if ((u + n) == 0) {
    adueanglenupperm(i, 1, m) + nEselectx(n, x) * adueselectxupperm(x, m)
 } else if ((u + n) == 1){
  adueanglenupperm(i, 1, m) + nEselectx(n, x) * adueselectxplus1upperm(x, m)
 } else {
  v(i, u) * tpselectx(u, x) * adueanglenupperm(i, n, m) + nEselectx(u + n, x) * aduexupperm(x + u + n, m)
 }
}

udeferredadueselectxplus1guaranteed <- function(u, x, n) {
  v(i, u) * tpselectxplus1(u, x) * adueanglen(i, n) + nEselectxplus1(u + n, x) * aduex(x + u + n)
}

udeferredadueselectxplus1guaranteedupperm <- function(u, x , n, m) {
  v(i, u) * tpselectxplus1(u, x) * adueanglenupperm(i, n, m) + 
     nEselectxplus1(u + n, x) * aduexupperm(x + u + n, m)
}

udeferredaduexguaranteed <- function (u, x, n) {
  v(i, u) * adueanglen(i, n) * tpx(u, x) + nEx(u + n, x) * aduex(x + u + n)
}

udeferredaduexguaranteedupperm <- function(u, x, n, m) {
  v(i, u) * tpx(u, x) * adueanglenupperm(i, n, m) + nEx(u + n, x) * aduexupperm(x + u + n, m)
}

udeferredtqx <- function(u, t, x) {
  tpx(u, x) - tpx(u + t, x)
}

udeferredtqselectx <- function(u, t, x) {
  tpselectx(u, x) - tpselectx(u + t, x)
}

v <- function(i, n, Moment= 1) {
  (1 + i) ^ -(n * Moment)
}