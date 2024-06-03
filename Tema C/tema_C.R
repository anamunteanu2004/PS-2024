# C1) (a) #######################################

generare_permutare_aleatoare = function(n) {
  valori_aleatoare = runif(n)
  index_sortat = order(valori_aleatoare)
  permutare = numeric(n)
  for (i in 1:n) {
    permutare[i] = index_sortat[i]
  }
  
  return(permutare)
}

#!EXEMPLU PENTRU EXERCITIU!
n = 5
permutare_aleatoare = generare_permutare_aleatoare(n)
print(permutare_aleatoare)

# C1) (b) #######################################

comparare_lexicografica = function(w1, w2) {
  for (i in 1:min(length(w1), length(w2))) {
    if (w1[i] < w2[i])
      return(-1)
    else if (w1[i] > w2[i])
      return(1)
  }
  
  if (length(w1) < length(w2)) {
    for (i in (length(w1) + 1):length(w2)) {
      new_bit = sample(0:1, 1)
      if (new_bit == 1)
        return(-1)
      else if (new_bit == 0)
        return(1)
    }
  } else if (length(w1) > length(w2)) {
    for (i in (length(w2) + 1):length(w1)) {
      new_bit = sample(0:1, 1)
      if (new_bit == 1)
        return(1)
      else if (new_bit == 0)
        return(-1)
    }
  }
  
  return(0)
}

W1 = c(1, 0, 1, 1, 0)
W2 = c(1, 0, 1, 0, 1)
print(comparare_lexicografica(W1, W2)) #returneaza 1, deoarece W1 este lexicografic mai mare decat W2

W1 = c(1, 0, 1, 1, 0)
W2 = c(1, 0, 1, 1, 0)
print(comparare_lexicografica(W1, W2)) #returneaza 0, deoarece W1 si W2 sunt identice

# C1) (c) #######################################

RandQuickSort = function(matrix) {
  if (nrow(matrix) <= 1) {
    return(matrix)
  }
  pivot_index = sample(1:nrow(matrix), 1)
  pivot = matrix[pivot_index, , drop = FALSE]
  S1 <- matrix[apply(matrix, 1, function(row) comparare_lexicografica(row, pivot)) == -1, , drop = FALSE]
  S2 <- matrix[apply(matrix, 1, function(row) comparare_lexicografica(row, pivot)) == 1, , drop = FALSE]
  sorted_S1 = RandQuickSort(S1)
  sorted_S2 = RandQuickSort(S2)
  return(rbind(sorted_S1, pivot, sorted_S2))
}

lungime_cuvant = 5
numar_cuvinte = 10
matrice_cuvinte = matrix(sample(0:1, lungime_cuvant * numar_cuvinte, replace = TRUE), nrow = numar_cuvinte, ncol = lungime_cuvant)

cat("Matricea inițială:\n")
print(matrice_cuvinte)

matrice_ordonata = RandQuickSort(matrice_cuvinte)

cat("\nMatricea ordonată:\n")
print(matrice_ordonata)

# C1) (d) #######################################

generare_permutare_aleatoare = function(n, k) {
  cuvinte = matrix(sample(0:1, k * n, replace = TRUE), nrow = n, ncol = k)
  
  cat("Cuvintele inițiale:\n")
  print(cuvinte)
  
  cuvinte_ordonate = RandQuickSort(cuvinte)
  
  cat("\nCuvintele ordonate:\n")
  print(cuvinte_ordonate)
  
  permutare = integer(n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (all(cuvinte_ordonate[i, ] == cuvinte[j, ])) {
        permutare[i] = j
        break
      }
    }
  }
  
  return(permutare)
}

n = 10
k = 5
permutare = generare_permutare_aleatoare(n, k)
cat("\nPermutarea:\n")
print(permutare)

# C2) (a) #######################################

generate_graph <- function(num_nodes, prob) {
  adjacency_matrix <- matrix(0, nrow = num_nodes, ncol = num_nodes)
  for (i in 1:(num_nodes-1)) {
    for (j in (i+1):num_nodes) {
      if (runif(1) < prob) {
        adjacency_matrix[i, j] <- 1
        adjacency_matrix[j, i] <- 1
      }
    }
  }
  return(adjacency_matrix)
}

max_cut_random = function(adjacency_matrix) {
  num_nodes = nrow(adjacency_matrix)
  n = num_nodes %/% 2
  
  nodes = 1:num_nodes
  A = sample(nodes, n)
  
  B = setdiff(nodes, A)
  
  cut_edges = 0
  for (u in A) {
    for (v in B) {
      if (adjacency_matrix[u, v] == 1) {
        cut_edges = cut_edges + 1
      }
    }
  }
  
  return(cut_edges)
}

num_nodes = 10
prob = 0.5
adjacency_matrix = generate_graph(num_nodes, prob)

print(adjacency_matrix)

max_cut_size = max_cut_random(adjacency_matrix)
cat("Cardinalul tăieturii de cardinal maxim este:", max_cut_size, "\n")

# C2) (b) #######################################

#Una dintre cele mai simple metode este să repetăm algoritmul de mai multe ori și să păstrăm tăietura cu cardinalul maxim. 
#Această abordare funcționează pe principiul că, cu cât încercăm mai multe tăieturi aleatorii, 
#cu atât avem șanse mai mari să găsim o tăietură aproape optimă.

