# A1) (a) #######################################

calc_prob = function(lambda, p, n, m, k) {
  
  k_val = k:m
  
  Poisson_prob = dpois(k_val, lambda)
  
  Geometric_prob = dgeom(k_val, p)
  
  Binomial_prob = dbinom(k_val, n, p)
  
  return(list(Poisson = Poisson_prob, Geometric = Geometric_prob, Binomial = Binomial_prob))
}

lambda = 2
p = 0.3
n = 10
m = 5
k = 2

probabilit = calc_prob(lambda, p, n, m, k)
print(probabilit)

# A1) (b) #######################################

plots = function(lambda, p, n, m, k) {
  probabilit = calc_prob(lambda, p, n, m, k)
  k_val = k:m
  
  layout(matrix(1:3, nrow = 1, ncol = 3))
  
  barplot(probabilit$Poisson, names.arg=k_val, col="pink", xlab="k", ylab="P(X=k)", main="Poisson")
  
  barplot(probabilit$Geometric, names.arg=k_val, col="red", xlab="k", ylab="P(X=k)", main="Geometrica")
  
  barplot(probabilit$Binomial, names.arg=k_val, col="purple", xlab="k", ylab="P(X=k)", main="Binomiala")
  
  layout(1)
}

lambda = 2
p = 0.3
n = 10
m = 5
k = 2

plots(lambda, p, n, m, k)

# A1) (c) #######################################

find_k0 = function(lambda) {
  target_prob = 1 - 10^(-6)
  k0 = 0
  suma_prob = ppois(k0, lambda)
  
  while (suma_prob <= target_prob) {
    k0 = k0 + 1
    suma_prob = ppois(k0, lambda)
  }
  return(k0)
}

lambda = 2
k0 = find_k0(lambda)
print("Cea mai mica valoare a lui k0: ")
print(k0)

# A2) (a) #######################################

read_and_analyze = function(file_path) {
 
  data = read.csv(file_path, header = TRUE)
  
  P = data$P
  S = data$S
  
  freq_abs_P = table(P)
  freq_abs_S = table(S)
  
  freq_rel_P = as.vector(freq_abs_P) / length(P)
  freq_rel_S = as.vector(freq_abs_S) / length(S)
  
  mean_P = mean(P)
  mean_S = mean(S)
  
  return(list(freq_abs_P = freq_abs_P, freq_rel_P = freq_rel_P, freq_abs_S = freq_abs_S, freq_rel_S = freq_rel_S, mean_P = mean_P, mean_S = mean_S))
}

file_path = "note_PS.csv"
results = read_and_analyze(file_path)
print(results)

# A2) (b) #######################################

del = function(file_path, sample_name) {
  
  data = read.csv(file_path, header = TRUE)
  
  data$P = as.numeric(data$P)
  data$S = as.numeric(data$S)
  
  if (sample_name == "P") {
    sample = data$P
  } else if (sample_name == "S") {
    sample = data$S
  } else {
    stop("Numele esantionului trebuie sa fie 'P' sau 'S'")
  }
  
  Q1 = quantile(sample, 0.25)
  Q3 = quantile(sample, 0.75)
  IQR = Q3 - Q1
  lower_bound = Q1 - 1.5 * IQR
  upper_bound = Q3 + 1.5 * IQR
  
  cleaned_sample = sample[sample >= lower_bound & sample <= upper_bound]
  
  intervals = cut(cleaned_sample, breaks = seq(1, 10, by = 1), right = TRUE, include.lowest = TRUE)
  freq_abs = table(intervals)
  freq_rel = as.vector(freq_abs) / length(cleaned_sample)
  
  barplot(freq_rel, main = paste("Distributia frecventelor pentru", sample_name), xlab = "Intervale", ylab = "Frecvente relative", col = "pink")
  
  return(cleaned_sample)
}

cleaned_P = del(file_path, "P")
cleaned_S = del(file_path, "S")





