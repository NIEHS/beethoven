library(sparklyr)
library(dplyr)

config <- spark_config()

config[["spark.cores.max"]] <- "64"
config[["spark.executor.cores"]] <- "2"
config[["spark.executor.memory"]] <- "2g"
config[["spark.sql.shuffle.partitions"]] <- "32"

sc <- spark_connect(
  master = readLines("input/.hidden/chr_spark_master.txt"),
  spark_home = readLines("input/.hidden/chr_spark_home.txt"),
  app_name = "sparklyr-shuffle-test",
  config = config
)

cat("\nSpark version:", as.character(spark_version(sc)), "\n")

N <- 100000000L
P <- 512L


cat("Creating", format(N, big.mark = ","), "rows in", P, "partitions\n")

big <- sdf_len(sc, N, repartition = P) %>%
  mutate(
    group = id %% 500000L,
    x1 = (id %% 1000L) / 1000,
    x2 = ((id * 7L) %% 1000L) / 1000,
    x3 = ((id * 13L) %% 1000L) / 1000
  )

cat("Spark partitions:", sdf_num_partitions(big), "\n")

cat("\nStarting GROUP BY / shuffle workload...\n")

timing <- system.time({
  result <- big %>%
    group_by(group) %>%
    summarise(
      n = n(),
      mean_x1 = mean(x1),
      sum_x2 = sum(x2),
      mean_x3 = mean(x3),
      .groups = "drop"
    ) %>%
    arrange(desc(sum_x2)) %>%
    head(20) %>%
    collect()
})

print(timing)
print(result)

cat("\nKeeping Spark application alive for 30 seconds for inspection...\n")

cat("\nPersisting distributed table...\n")

cache_time <- system.time({
  big_cached <- sdf_persist(
    big,
    storage.level = "MEMORY_AND_DISK",
    name = "sparklyr_cache_test"
  )
})

print(cache_time)

cat("\nDataset is now cached. Check the Spark Storage tab.\n")
Sys.sleep(20)


cat("\nFirst scan of cached data...\n")

print(system.time({
  result1 <- big_cached %>%
    summarise(
      rows = n(),
      mean_x1 = mean(x1),
      mean_x2 = mean(x2),
      mean_x3 = mean(x3)
    ) %>%
    collect()
}))

print(result1)


cat("\nSecond scan of the same cached data...\n")

print(system.time({
  result2 <- big_cached %>%
    summarise(
      rows = n(),
      mean_x1 = mean(x1),
      mean_x2 = mean(x2),
      mean_x3 = mean(x3)
    ) %>%
    collect()
}))

print(result2)

Sys.sleep(30)

cat("\nStarting distributed K-means...\n")

ml_data <- big_cached %>%
  select(x1, x2, x3)

kmeans_time <- system.time({
  km <- ml_kmeans(
    ml_data,
    k = 8,
    features = c("x1", "x2", "x3"),
    max_iter = 20,
    seed = 12345
  )
})

print(kmeans_time)
print(km)

cat("\nK-means complete. Holding application open...\n")
Sys.sleep(30)

spark_disconnect(sc)
