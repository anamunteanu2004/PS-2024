# B1) ##########################################

estimate_volume <- function(R, r, sample_size) {
  
  x1_min = -R - r
  x1_max = R + r
  x2_min = -R - r
  x2_max = R + r
  x3_min = -r
  x3_max = r
  
  x1 = runif(sample_size, min = x1_min, max = x1_max)
  x2 = runif(sample_size, min = x2_min, max = x2_max)
  x3 = runif(sample_size, min = x3_min, max = x3_max)
  
  interior = (x3^2 + (sqrt(x1^2 + x2^2) - R)^2) < r^2
  prop = mean(interior)
  volum_cub = (x1_max - x1_min) * (x2_max - x2_min) * (x3_max - x3_min)
  volum_aprox = prop * volum_cub
  
  return(volum_aprox)
}

R = 10
r = 3

exact_volume = 2 * (pi^2) * R * r^2

sample_sizes = c(10000, 20000, 50000)

rez = data.frame(SampleSize = sample_sizes, VolumEstimat = sapply(sample_sizes, estimate_volume, R = R, r = r), 
                     VolumExact = exact_volume)

rez$EroareRelativa <- abs(rez$VolumEstimat - rez$VolumExact) / rez$VolumExact

print(rez)

# B2) ##########################################

a <- 0
b <- 2
c <- 0
d <- 2.4

is_inside_triangle <- function(x, y) {
  return(y >= 0 & y <= 2 * x & y <= 6 - 3 * x)
}

estimate_area = function(sample_size) {
  x = runif(sample_size, min = a, max = b)
  y = runif(sample_size, min = c, max = d)
  
  inside_triangle = mapply(is_inside_triangle, x, y)
  proportion_inside = mean(inside_triangle)
  
  area_rectangle = (b - a) * (d - c)
  
  area_triangle_estimate = proportion_inside * area_rectangle
  
  return(area_triangle_estimate)
}

sample_size = 20000

estimated_area <- estimate_area(sample_size)

exact_area = 0.5 * (2 * 2.4)

relative_error <- abs(estimated_area - exact_area) / exact_area

cat("Aria estimata:", estimated_area, "\n")
cat("Aria exacta:", exact_area, "\n")
cat("Eroarea relativa:", relative_error, "\n")

# B3) (a) #######################################

f = function(N,a,b){
  sum = 0
  for(i in 1:N){
    x = runif(1,a,b)
    sum = sum + ((2*x-1)/(x^2-x-6))
  }
  return ((b-a)*sum/N)
}
estimare = f(10000, -1, 1)
valoare_exacta = log(3) - log(2)
eroare = abs (estimare - valoare_exacta)
cat("Estimare B3_a: ", estimare, "\n")
cat("Eroare absolută: ", eroare, "\n")






# B4) (a) #######################################

n = 10000
p = 0.25   
q = 0.01  
n_min = 15000

simulare = function(n, p, q, n_min) {
  years = 0
  while (n < n_min) {
    n_noi = rbinom(1, n, p)
    inactive_users = rbinom(1, n, q)
    n = n + n_noi - inactive_users
    years = years + 1
  }
  return(years)
}

num_simulations = 1000
years_needed = replicate(num_simulations, simulare(n, p, q, target_users))
average_years = mean(years_needed)

cat("Numarul mediu de ani necesari pana cand iSocialize va avea cel putin 15000 de utilizatori: ", average_years)

# B4) (b) #######################################

n_initial = 10000  
p = 0.25            
q = 0.01            
durata_ani = 40 + 10/12
n_min = 15000

n_utilizatori = numeric()

for (ani in 1:durata_ani) {
  n_existent = rbinom(1, n_initial, 1 - q)
  n_noi = rbinom(1, n_existent, p)
  n_initial = n_existent + n_noi
  n_utilizatori = c(n_utilizatori, n_initial)
}

probabilitate = mean(n_utilizatori >= n_min)

cat("Probabilitatea ca dupa", durata_ani, "ani sa fie cel putin", n_min, "utilizatori:", probabilitate, "\n")

# B4) (c) #######################################

B4_c = function(num_simulari, utilizatori_initiali, utilizatori_doriti, n, p, q, e = 0.01, alfa = 0.01) {
  numar_succese = 0
  
  for (simulare in 1:num_simulari) {
    ani_totali = 40 + 10/12
    utilizatori_finali = utilizatori_initiali
    
    for (an in 1:floor(ani_totali)) {
      noi_utilizatori = rbinom(1, utilizatori_finali, p)
      utilizatori_retrasi = rbinom(1, utilizatori_finali, q)
      
      utilizatori_finali = utilizatori_finali + noi_utilizatori - utilizatori_retrasi
    }
    
    if (utilizatori_finali >= utilizatori_doriti) {
      numar_succese = numar_succese + 1
    }
  }
  
  probabilitate_estimata = numar_succese / num_simulari
  z_alfa_2 = qnorm(1 - alfa/2)
  
  limita_inferioara = probabilitate_estimata - z_alfa_2 * sqrt(probabilitate_estimata * (1 - probabilitate_estimata) / num_simulari)
  limita_superioara = probabilitate_estimata + z_alfa_2 * sqrt(probabilitate_estimata * (1 - probabilitate_estimata) / num_simulari)
  
  return(list(probabilitate_estimata = probabilitate_estimata, limita_inferioara = limita_inferioara, limita_superioara = limita_superioara))
}

rezultat = B4_c(10000, 10000, 15000, 1000, 0.25, 0.01)

cat("Probabilitatea estimata:", rezultat$probabilitate_estimata, "\n")
cat("Intervalul de incredere (", rezultat$limita_inferioara, ", ", rezultat$limita_superioara, ")\n")
