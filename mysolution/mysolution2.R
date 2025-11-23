library(igraph)

set.seed(45)

# 2. Wygeneruj graf wedle modelu Barabási-Albert z tysiącem węzłów
g <- sample_pa(n = 1000, power = 1, m = 1, directed = FALSE)

# 3. Zwizualizuj graf layoutem Fruchterman & Reingold
layout_fr <- layout_with_fr(g)

plot(
  g,
  layout = layout_fr,
  vertex.label = NA,
  vertex.size = 4,
  vertex.color = "skyblue",
  edge.color = "grey30",
  main = "Graf Barabási–Albert (layout Fruchterman-Reingold)"
)

# 3. Znajdź najbardziej centralny węzeł według miary betweenness, jaki ma numer?
btw <- betweenness(g, directed = FALSE, normalized = TRUE)
most_central_node <- which.max(btw)

cat("\nNajbardziej centralny węzeł (betweenness):", most_central_node, "\n")
cat("Jego wartość betweenness:", btw[most_central_node], "\n\n")

# 4. Jaka jest średnica grafu?
graph_diameter <- diameter(g)
cat("Średnica grafu:", graph_diameter, "\n")

# 5. W komentarzu napisz czym różnią się grafy Barabási-Albert i Erdős-Rényi.

# Graf Erdős–Rényi powstaje całkowicie losowo, więc większość węzłów ma
# podobną liczbę połączeń i nie pojawiają się wyraźne huby.
# Stopnie węzłów są podobne. Występuje niewiele węzłów o bardzo wysokim stopniu.
# Graf Barabási–Albert rośnie w czasie i nowe węzły częściej dołączają
# do tych już dobrze połączonych, dlatego  tworzą się huby i sieć
# ma strukturę scale-free. Średnica grafu Barabási–Albert jest zwykle mniejsza
# niż w grafie Erdős–Rényi o tej samej liczbie węzłów i krawędzi,
# ze względu na obecność hubów, które skracają ścieżki między węzłami.
# W grafie Barabási–Albert kilka węzłów ma bardzo wysoki stopień, ale większość
# ma niski stopień.
