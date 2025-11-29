library(igraph)

set.seed(44)

# 2. Wygeneruj sieć Erdős-Rényi o stu wierzchołkach i prawdopodobieństwie krawędzi = 0.05.
n <- 100
p <- 0.05
g <- sample_gnp(n = n, p = p, directed = FALSE, loops = FALSE)

# 3. Wydrukuj podsumowanie grafu - czy graf jest ważony?
print(g)
cat("Czy graf jest ważony? ->", "weight" %in% edge_attr_names(g), "\n\n")
# Graf nie jest ważony

# 4. Wylistuj wszystkie wierzchołki i krawędzie.
nodes <- V(g)
edges <- as_edgelist(g)

cat("Wierzchołki:\n")
print(nodes)

cat("\nKrawędzie:\n")
print(edges)

# 5. Ustaw wagi wszystkich krawędzi na losowe z zakresu 0.01 do 1
E(g)$weight <- runif(ecount(g), 0.01, 1)

# 6. Wydrukuj ponownie podsumowanie grafu - czy teraz graf jest ważony?
print(g)
cat("Czy graf jest ważony? ->", "weight" %in% edge_attr_names(g), "\n\n")
# Graf jest ważony

cat("Krawędzie z wagami:\n")
print(cbind(edges, weight = round(E(g)$weight, 3)))

# 7. Jaki jest stopień każdego węzła? Następnie stwórz histogram stopni węzłów.
deg <- degree(g)
cat("\nStopnie węzłów:\n")
print(deg)

hist(deg,
     main = "Histogram stopni węzłów",
     xlab = "Stopień",
     col = "lightblue",
     border = "black")

# 8. Ile jest klastrów (connected components) w grafie?
comp <- components(g)
cat("\nLiczba klastrów:", comp$no, "\n")
# Są 3 klastry

# 9. Zwizualizuj graf w taki sposób, aby rozmiar węzłów odpowiadał mierze PageRank.
pr <- page_rank(g, weights = E(g)$weight)$vector
node_sizes <- 400 * pr

plot(
  g,
  vertex.size = node_sizes,
  vertex.label = NA,
  vertex.color = "skyblue",
  edge.color = "grey40",
  main = "Wizualizacja grafu (rozmiar węzłów = PageRank)"
)

