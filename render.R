source("global.R")

key <- Sys.getenv("QUANDL_API_KEY")
if (key == "") stop("QUANDL_API_KEY environment variable is not set")
Quandl.api_key(key)

QC <- read.csv("CodeList.csv", stringsAsFactors = FALSE)
QC <- data.frame(Code = sapply(QC[, 1], attach), Name = QC$Name, stringsAsFactors = FALSE)
QC <- setNames(split(QC, seq(nrow(QC))), QC$Name)

returns <- returns.data.frame(lapply(QC, read.quandl))
returns[sapply(returns, is.numeric)] <- returns[sapply(returns, is.numeric)] / 100

SPX <- Quandl("CHRIS/CME_SP1", transform = "rdiff", collapse = "monthly", start_date = "2000-01-01")
SPX <- data.frame(SPX$Date, SPX$Settle)
colnames(SPX) <- c("Date", "SPX")

returns <- na.omit(arrange(left_join(SPX, returns, by = "Date"), Date))

dir.create("screenshots", showWarnings = FALSE)
save <- function(p, name) ggsave(file.path("screenshots", name), p, width = 12, height = 8, dpi = 100, device = ragg::agg_png)

# Views 1 & 2: trailing performance curves
for (monthBack in c(12, 60)) {
  r <- add_zeros(filter(returns, Date >= Sys.Date() %m-% months(monthBack)))
  melt.perf <- melt(data.frame(Date = r[, 1], cumprod(1 + r[, -1])), id = "Date")
  g <- ggplot(melt.perf, aes(x = Date, y = value, group = variable)) +
    geom_line(aes(colour = variable)) +
    theme_economist() +
    geom_text(data = filter(melt.perf, Date == max(Date)), aes(label = variable)) +
    labs(y = "Performance Index", title = paste0("Trailing ", monthBack, "-month Hedge Fund Performance Curve")) +
    theme(legend.position = "none")
  save(g, paste0("performance_", monthBack, "m.png"))
}

# View 3: latest performance ranking
snapshot <- tail(returns, 1)
g <- ggplot(melt(snapshot, id = "Date"), aes(x = reorder(variable, value), y = value, group = value)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_economist() +
  geom_text(aes(label = percent(value))) +
  labs(y = "Return (%)", title = paste0("Performance Ranking as of ", snapshot$Date), x = "Strategy") +
  coord_flip()
save(g, "ranking.png")

# View 4: correlation heatmap
corr <- melt(get.upper.tri(round(cor(returns[, -1]), 2)), na.rm = TRUE)
g <- ggplot(corr, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Pearson\nCorrelation") +
  geom_text(aes(label = value), color = "black", size = 4) +
  labs(title = paste0("Correlation Between Different Hedge Fund Strategies\n(between ", min(returns$Date), " and ", max(returns$Date), ")")) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title = element_blank(), panel.grid.major = element_blank(),
        axis.ticks = element_blank(), legend.position = "top") +
  coord_fixed()
save(g, "correlation.png")
