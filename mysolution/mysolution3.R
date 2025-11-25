library(shiny)
library(bslib)
library(igraph)

build_graph_with_weights <- function(url) {
  # 4. Zaimportuj zbiór out.radoslaw_email_email do data frame i zachowaj tylko pierwsze dwie
  # kolumny (dodatkowo przeskocz dwa pierwsze wiersze), następnie stwórz z tego data
  # frame'a graf skierowany.
  raw <- try(read.table(url(url), header = FALSE, sep = "", stringsAsFactors = FALSE, skip = 2),
             silent = TRUE)
  if (inherits(raw, "try-error")) stop("Nie można pobrać pliku z podanego URL.")
  raw <- raw[, 1:2]
  colnames(raw) <- c("from", "to")

  # 6. Waga na krawędziach niech zostanie ustalona wedle następującego podejścia:
  # wij = cntij / cnti, gdzie
  # wij - waga na krawędzi pomiędzy węzłęm vi a węzłem vj
  # cntij - liczba maili wysłanych przez węzeł vi do węzła vj
  # cnti - liczba wszystkich maili wysłanych przez węzeł vi
  # Powyższa formuła zakłada, że będziesz musiał(a) użyć pierwotnego data frame’a aby
  # wyliczyć te wagi. Zwróć uwagę, że wedle powyższej formuły suma wag krawędzi
  # wychodzących z każdego węzła będzie wynosiła jeden.
  cnt_table <- as.data.frame(table(raw$from, raw$to), stringsAsFactors = FALSE)
  names(cnt_table) <- c("from", "to", "cnt")
  cnt_table <- cnt_table[cnt_table$cnt > 0, ]

  cnt_i <- aggregate(cnt ~ from, data = cnt_table, FUN = sum)
  names(cnt_i) <- c("from", "cnt_out")

  cnt_table <- merge(cnt_table, cnt_i, by = "from")
  cnt_table$weight <- as.numeric(cnt_table$cnt) / as.numeric(cnt_table$cnt_out)

  edges_df <- data.frame(from = cnt_table$from, to = cnt_table$to,
                         weight = cnt_table$weight, stringsAsFactors = FALSE)

  g <- graph_from_data_frame(edges_df, directed = TRUE)

  # 5. Użyj funkcji simplify aby pozbyć się wielokrotnych krawędzi i pętli. Zweryfikuj czy po tej
  # operacji Twój graf ma 167 węzłów i 5783 krawędzie. Jeśli tak jest, możesz kontynuować.
  g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "first")

  if (vcount(g) != 167 || ecount(g) != 5783) {
    stop("Graf nie ma oczekiwanych 167 węzłów i 5783 krawędzi po uproszczeniu.")
  }

  list(graph = g, raw = raw, cnt_table = cnt_table)
}

# 7. Zasymuluj proces rozprzestrzeniania się informacji w grafie wedle następującego opisu...
ic_simulate_once_cumulative <- function(g, seeds, p_scale = 1.0, max_iter) {
  vert_names <- V(g)$name
  vert_count <- vcount(g)

  edges <- as_data_frame(g, what = "edges")
  outmap <- split(edges[, c("to", "weight")], edges$from)

  activated <- rep(FALSE, vert_count)
  names(activated) <- vert_names
  activated[as.character(seeds)] <- TRUE

  frontier <- as.character(seeds)
  attempted_edges <- new.env(hash = TRUE, parent = emptyenv())

  cumulative_active <- integer(max_iter)

  active_count <- sum(activated)

  for (i in seq_len(max_iter)) {
    if (length(frontier) == 0) {
      cumulative_active[i:max_iter] <- active_count
      break
    }

    newly_activated_vector <- character(0)

    for (from in frontier) {
      outs <- outmap[[from]]
      if (is.null(outs) || nrow(outs) == 0) next

      for (edge in seq_len(nrow(outs))) {
        to <- as.character(outs$to[edge])
        key <- paste(from, to, sep = "|")

        if (!exists(key, envir = attempted_edges, inherits = FALSE) && !activated[to]) {
          assign(key, TRUE, envir = attempted_edges)

          prob <- outs$weight[edge] * p_scale

          if (runif(1) <= prob) {
            newly_activated_vector <- c(newly_activated_vector, to)
            activated[to] <- TRUE
          }
        }
      }
    }

    newly_activated_vector <- unique(newly_activated_vector)
    frontier <- newly_activated_vector

    active_count <- active_count + length(newly_activated_vector)
    cumulative_active[i] <- active_count
  }

  return(cumulative_active)
}


ic_simulate_average <- function(g, seeds_func, runs = 100, p_scale = 1.0, max_iter) {
  all_runs <- matrix(0, nrow = runs, ncol = max_iter)

  for (r in seq_len(runs)) {
    seeds <- seeds_func(r)
    res <- ic_simulate_once_cumulative(g, seeds = seeds, p_scale = p_scale, max_iter = max_iter)
    all_runs[r, ] <- res
  }

  colMeans(all_runs)
}

server <- function(input, output, session) {
  data_url <- "https://bergplace.org/share/out.radoslaw_email_email"
  built <- reactiveVal(NULL)
  observeEvent(TRUE, {
    try({
      res <- build_graph_with_weights(data_url)
      built(res)
    }, silent = TRUE)
  }, once = TRUE)

  output$graph_info <- renderPrint({
    res <- built()
    if (is.null(res)) {
      cat("Ładowanie/parsowanie danych...\n")
    } else {
      g <- res$graph
      cat("Graf załadowany:\n")
      cat("Węzły:", vcount(g), "\n")
      cat("Krawędzie:", ecount(g), "\n")
      cat("\nPięć zestawów startowych: outdegree, betweenness, closeness, losowe, k-core.\n")
    }
  })

  sim_result <- eventReactive(input$run_sim, {
    res <- built()
    validate(need(!is.null(res), "Dane nie załadowane jeszcze lub błąd w parsowaniu."))
    g <- res$graph
    max_iter <- as.integer(input$max_iter)
    p_scale <- input$p_pct / 100
    vert_count <- vcount(g)
    seed_share <- 0.05
    seed_count <- max(1, ceiling(vert_count * seed_share))

    # 8. Wykonaj powyższy eksperyment dla pięciu różnych zbiorów węzłów początkowych: (i)
    # węzłów o największym outdegree, (ii) dla najbardziej centralnych węzłów wedle metody
    # betweenness, (iii) węzłów największym closeness, (iv) dowolnych losowych węzłów i (v)
    # węzłów wybranych wedle innej miary (napisz jakiej, może być nawet wymyślona przez
    # Ciebie, w komentarzu opisz wybraną miarę).

    # Wybrano k-core (coreness) jako piątą miarę.
    # K-core (coreness) określa, jak głęboko węzeł znajduje się w gęstym, silnie połączonym rdzeniu grafu.
    # Węzły o wysokim k-core należą do obszaru, w którym każdy ma wielu połączonych sąsiadów.
    # Przez to zwykle lepiej rozprzestrzeniają informację, bo są osadzone w stabilnej i mocno powiązanej części sieci.
    outdeg <- degree(g, mode = "out")
    btw <- betweenness(g, directed = TRUE, normalized = TRUE)
    clo <- closeness(g, mode = "out")
    core_vals <- coreness(g, mode = "out")

    seeds_outdeg <- function(run) {
      names(sort(outdeg, decreasing = TRUE))[1:seed_count]
    }
    seeds_btw <- function(run) {
      names(sort(btw, decreasing = TRUE))[1:seed_count]
    }
    seeds_clo <- function(run) {
      clo2 <- clo
      clo2[is.na(clo2)] <- -Inf
      names(sort(clo2, decreasing = TRUE))[1:seed_count]
    }
    seeds_random <- function(run) {
      set.seed(run + 12345)
      sample(V(g)$name, seed_count)
    }
    seeds_kcore <- function(run) {
      names(sort(core_vals, decreasing = TRUE))[1:seed_count]
    }

    avg_outdeg <- ic_simulate_average(g, seeds_outdeg, p_scale = p_scale, max_iter = max_iter)
    avg_btw <- ic_simulate_average(g, seeds_btw, p_scale = p_scale, max_iter = max_iter)
    avg_clo <- ic_simulate_average(g, seeds_clo, p_scale = p_scale, max_iter = max_iter)
    avg_rand <- ic_simulate_average(g, seeds_random, p_scale = p_scale, max_iter = max_iter)
    avg_kcore <- ic_simulate_average(g, seeds_kcore, p_scale = p_scale, max_iter = max_iter)

    list(iter = seq_len(max_iter),
         outdeg = avg_outdeg,
         btw = avg_btw,
         clo = avg_clo,
         rand = avg_rand,
         kcore = avg_kcore,
         k_seeds = seed_count,
         p_scale = p_scale)
  })

  # 9. Jako podsumowanie realizacji zadania przygotuj wykres obrazujący jak przebiegał proces
  # dyfuzji informacji dla w/w zestawów węzłów początkowych - na osi X nr kolejnej iteracji,
  # na osi Y liczba aktywowanych węzłów w tej iteracji - pięć różnych serii danych - po jednej
  # dla każdego ze sprawdzanych zestawów węzłów.
  output$diff_plot <- renderPlot({
    sim <- sim_result()

    max_iter <- length(sim$iter)
    mat <- cbind(sim$outdeg, sim$btw, sim$clo, sim$rand, sim$kcore)
    ymax <- max(mat) * 1.1
    cols <- c("red", "blue", "forestgreen", "orange", "purple")
    labels <- c(paste0("Out-degree (k=", sim$k_seeds, ")"),
                "Betweenness",
                "Closeness",
                "Random",
                "k-core")

    plot(sim$iter, sim$outdeg, type = "o", pch = 16, lwd = 2, ylim = c(0, ymax),
         xlab = "Iteracja", ylab = "Średnia liczba aktywowanych węzłów",
         main = paste0("Modyfikator prawdopodobieństwa aktywacji: ",
                       sim$p_scale, "\nIteracje: ", max_iter),
         col = cols[1])
    lines(sim$iter, sim$btw, type = "o", pch = 16, lwd = 2, col = cols[2])
    lines(sim$iter, sim$clo, type = "o", pch = 16, lwd = 2, col = cols[3])
    lines(sim$iter, sim$rand, type = "o", pch = 16, lwd = 2, col = cols[4])
    lines(sim$iter, sim$kcore, type = "o", pch = 16, lwd = 2, col = cols[5])
    legend("topright", legend = labels, col = cols, lty = 1, pch = 16, cex = 0.9, bg = "white")
  })
}

# 10. Opublikuj swój kod jako ShinyApp z tym zastrzeżeniem, że prawdopodobieństwo aktywacji
# wij powinno być regulowane suwakiem w taki sposób, że domyślnie wynosi ona 100% (tj.
# podstawową wartość wij), lecz można suwak regulować w zakresie od 10% do 200% wij,
# gdzie przez wybrany procent mnożymy wij. Dzięki temu będziemy widzieć jak zmieniłyby
# się wyniki gdyby prawdopodobieństwo wzrosło lub zmalało. Jeśli wij wynosi lub przekracza
# jeden, oznacza to, że węzeł na pewno aktywuje swojego sąsiada. Dodatkowy suwak określa
# liczbę iteracji – od 1 do 50, domyślnie 10. 
ui <- page_sidebar(
  title = "Symulacja rozprzestrzeniania informacji w sieciach",
  sidebar = sidebar(
    sliderInput("p_pct", "Skalowanie prawdopodobieństwa aktywacji:",
                min = 10, max = 200, value = 100, step = 5),
    sliderInput("max_iter", "Maksymalna liczba iteracji:",
                min = 1, max = 50, value = 10),
    actionButton("run_sim", "Uruchom symulacje"),
    br(),
    verbatimTextOutput("graph_info")
  ),
  plotOutput("diff_plot", height = "600px")
)

shinyApp(ui = ui, server = server)