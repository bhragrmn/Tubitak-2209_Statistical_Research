#TÜBİTAK 2209 ÇALIŞMA DOSYASI
# ustel dagilim  ve weibulll icin ks testi
# d(density(Yogunluk fonskiyonu,PDF ))
# p(probability(birikimli dagilim fonskiyonu,CDF))
# q(quantile(kuantil TERS CDF))
# r(random(rasgele sayi uretme ))

#ustel PDF                              #Weibull PDF
f(x,lambda) =(lambda*exp(-lambda*x))    we??bull=(shape / scale) * (x / scale)^(shape - 1) * exp(- (x / scale)^shape)


# K???S testi CDF ile ??al??????r, ????nk?? test, empirik CDF ile teorik CDF???yi karsilastirir.
# Bu yuzzden ks.test fonksiyonuna p** t??r?? fonksiyon veriyoruz, d*** de??il.

#ustel icin         #weibull icin
dexp(x,rate)        dweibull(x,shape,scale)
#ks pexp(p,rate)        pweibull(p,shape,scale)
qexp(q,rate)        qweibull(q,shape,scale)
rexp(r,rate)        rweibull(r,shape,scale)


#Empirik CDF (ECDF):
# veriden hesaplanan F(X)=P(X<x) 
#Teorik CDF:
# Test edilen dagilimin matematiksel birikimli dagilim fonksiyonu


#D degeri (Test istatistigi):
# Empirik ile teorik CDF arasındaki en buyuk mutlak fark


# D kucukse ??? Veriler ile teorik dagilim arasinda fark azdirr.

# D buyukse ??? Fark fazladir, dagilima uymama ihtimali artar.


#p-de??eri:
Yorumlama:
  # p ??? 0.05: Dagilima uyum var (reddedilemez).
  
  #p < 0.05: Dagilima uyum yok (reddet).
  
  
  # Veri olusttur (orrnek: ??stel)
  set.seed(1)
data <- rexp(30, rate = 0.5)


# Parametre tahmini (?? = 1 / ortalama)
lambda_hat <- 1 / mean(data)

# Empirik CDF
ecdf_func <- ecdf(data)

# Teorik CDF degerleri
x_vals <- seq(min(data), max(data), length.out = 100)
empirical_cdf <- ecdf_func(x_vals)
theoretical_cdf <- pexp(x_vals, rate = lambda_hat)

# D degeri (max fark)
D_val <- max(abs(empirical_cdf - theoretical_cdf))
cat("D de??eri:", D_val, "\n")

# K-S testi
ks_test <- ks.test(data, "pexp", rate = lambda_hat)
print(ks_test)

##MLE
# olabilirlik ve log olabilirlik fonksiyonlar
# 1-ustel dagilim
ustel.olabilirlik=function(par)
{
  l=1/par[1]^n*exp(-sum(x)/par[1])
  return(l) { 
    ustel.logolabilirlik=function(par)
  }
  {
    ll=-n*log(par[1])-sum(x)/par[1]
    return(ll)
  }
  
}

n=1000
theta=4
x=rexp(n,1/theta)
ustel.olabilirlik(4)
ustel.logolabilirlik(4)

1-ustel.olabilirlik=function(par)
{
  l=1/par(1)^n*exp(-sum(x)/par(1))
  return(l){
    ustel.logolabilirlik=function(par)
    {
      ll=-n*log(par(1))-sum(x)/par(1)
      return(ll)
      
    }
  }
}
n=10000
theta=4
x=rexp(n,1/theta)
ustel.olabilirlik(4)
ustel.logolabilirlik(4)

# gama dagilimi
gamma.olabilirlik=function(par)
{
  alpha=par[1]
  beta=par[2]
  l=(1/gamma(alpha)^n*(1/beta^alpha)^n*prod(x^(alpha-1))*exp(-sum(x/beta)))
  return(1) {
    gamma.logolabilirlik=function(par)
    {
      alpha=par[1]
      beta=par[2]
      ll=-n*log(gamma(alpha))-alpha*n*log(beta)+(alpha-1)*sum(x)/beta
      return(ll)
      
    }
  }
  
}

n=150
alpha=2.5
beta=4  
x=rgamma(n,alpha,beta)
gamma.olabilirlik(c(1,1))
gamma.logolabilirlik(c(1,1))

# normal dagilim MLE
rm(list = ls())
n=10000
mu=1.7
sigma=0.9
x=rnorm(n,mu,sigma)
logolabilirlik.normal=function(par){
  -(-n*log(par[2])-n*log(sqrt(2*pi))+sum((-1/2)*(((x-par[1])))/par[2])^2))) # - iile ??arpmay?? unutma{
#par[1]=mu par[2]=sigma olmal??d??r 
optim(c(2,2),logolabilirlik.normal)$par
}
}

#Optimizasyon Yontemleri ve Kullanimi

#  | Yontem              | R Fonksiyonu                    | Notlar                        |
#   | ------------------- | ------------------------------- | ----------------------------- |
#  | Nelder-Mead         | `optim(method = "Nelder-Mead")` | Turev gerektirmez             |
#   | BFGS                | `optim(method = "BFGS")`        | Turev gerektirir              |
#  | L-BFGS-B            | `optim(method = "L-BFGS-B")`    | Sinirlandirilmis optimizasyon |
#   | Simulated Annealing | `GenSA::GenSA()`                | Kuresel optimizasyon          |
#    | Genetik Algoritma   | `GA::ga()`                      | Evrimsel algoritma            |
#mle #ustel ve #weibull

rm(list=ls())
alpha=2
sigma=3
n=1000
x=rweibull(n,alpha,sigma)

log.olabilirlik=function(par)
{
  alpha=par[1]
  sigma=par[2]
  f=alpha/sigma*(x/sigma)^(alpha-1)*exp(-(x/sigma)^alpha)
  ll=sum(log(f))
  return(-ll)
}
sonuc=optim(c(1,1),log.olabilirlik,method = "BFGS")
print(sonuc$par)


rm(list = ls())
x=c(2,3,4,5)
log.olabilirlik=function(par,x)
{
  alpha=par[1]
  sigma=par[2]
  f=alpha/sigma*(xbub/sigma)^(alpha-1)*exp(-(x/sigma)^alpha)
  ll=sum(log(f))
  return(-ll)
}
sonuc=optim(par=c(1,1),fn=log.olabilirlik, x=x, method="L-BFGS-B", lower=c(0.01,0.01))
print(sonuc$par)
-----------------------------------------------------
# ----------------------------------------------------------------------
# PROJEMİZİN TEMEL FONKSİYONU: WEIBULL MLE (İLERLEYEN TİP-II SANSÜRLÜ)
# ----------------------------------------------------------------------

# Girdiler: 
# data_censored: Gözlemlenen M bozulma zamanı (X'_i).
# r_scheme: Her bozulma anında çıkarılan birim sayısı (r_i).
# initial_params: Başlangıç tahminleri (c(beta, alpha)).

log_likelihood_weibull_it2 <- function(par, data_censored, r_scheme) {
  
  # Parametreler
  beta <- par[1]  # Şekil parametresi
  alpha <- par[2]   # Ölçek parametresi
  
  # Kontrol: Parametrelerin negatif olmaması gerekir.
  if (beta <= 0 || alpha <= 0) {
 #   return(1e+20) # Çok yüksek değer döndürerek optimizasyonu bu bölgeden uzaklaştır.
  }
  
  # 1. Kısım: Gözlemlenen Bozulma Zamanlarının Log-PDF'i (sum(ln(f(x_i))))
  # dweibull fonksiyonunun log = TRUE parametresi ile hesaplanır.
  llh_part1 <- sum(dweibull(data_censored, shape = beta, scale = eta, log = TRUE))
  
  # 2. Kısım: Sansürlenmiş Birimlerin Log-Sağkalım Fonksiyonu (Survival)
  # sum(r_i * ln[1 - F(x_i)]) terimi
  
  # 1 - F(t) = exp(-(t/eta)^beta olduğundan, log(1-F(t)) = -(t/eta)^beta
  log_survival <- -(data_censored / eta)^beta
  llh_part2 <- sum(r_scheme * log_survival)
  
  # Toplam Log-Olabilirlik
  Total_LLH <- llh_part1 + llh_part2
  
  # optim() minimize ettiği için, eksi (-) ile çarpılarak döndürülür
  return(-Total_LLH)
}

# ----------------------------------------------------------------------
# ÖRNEK KULLANIM
# ----------------------------------------------------------------------
# Simülasyon Verisi
observed_times <- c(5.2, 8.1, 14.5, 23.9, 30.0) 
r_values <- c(1, 0, 2, 0, 2) 
initial_guess <- c(beta = 1.5, eta = 20) 

# Optimizasyon
sonuc_itt2 <- optim(
  par = initial_guess,
  fn = log_likelihood_weibull_itt2, 
  data_censored = observed_times,
  r_scheme = r_values,
  method = "L-BFGS-B", # Kısıtları uygulamak için L-BFGS-B kullanıldı
  lower = c(0.01, 0.01), # Beta ve Eta'nın 0'dan büyük olma kısıtı
  hessian = TRUE         # Güven aralığı için Hessian matrisi hesaplatılır
)

print(sonuc_itt2$par)
