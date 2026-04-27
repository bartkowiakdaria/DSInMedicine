library(jsonlite)
library(uuid)

set.seed(123)

dir.create("data/json_logs", recursive = TRUE, showWarnings = FALSE)

nodes <- c("cresselia", "pikachu", "charizard", "snorlax", "bulbasaur")
users <- c("user_a", "user_b", "user_c")

for (i in 1:100) {
  
  start <- Sys.time() - runif(1, 0, 200000)
  duration <- runif(1, 60, 1200)
  end <- start + duration
  
  sha_ok <- sample(c(TRUE, TRUE, TRUE, FALSE), 1)
  seqfu_ok <- sample(c(TRUE, TRUE, TRUE, FALSE), 1)
  
  json <- list(
    "@context" = "http://www.w3.org/ns/prov#",
    "@id" = paste0("urn:uuid:", UUIDgenerate()),
    "@type" = "Activity",
    label = paste("Processing sample", i),
    
    startTime = format(start, "%Y-%m-%dT%H:%M:%SZ"),
    endTime = format(end, "%Y-%m-%dT%H:%M:%SZ"),
    
    executionNode = sample(nodes, 1),
    sourceDirectory = "/data/input/",
    destinationDirectory = paste0("/data/output/run_", i),
    
    wasAssociatedWith = list(
      list("@type"="SoftwareAgent", label="seqfu", version="1.22.3"),
      list("@type"="SoftwareAgent", label="sha256sum", version="8.32"),
      list("@type"="SoftwareAgent", label="Nextflow pipeline"),
      list(
        "@type"="Person",
        label=paste("user:", sample(users,1))
      )
    ),
    
    generated = list(
      list(
        "@type"="Entity",
        label="SHA256",
        value = if (sha_ok) "checksum coincide" else "checksum mismatch"
      ),
      list(
        "@type"="Entity",
        label="Seqfu",
        value = if (seqfu_ok) "OK" else "ERROR"
      ),
      list(
        "@type"="Entity",
        label="FASTQ Files",
        totalSizeBytes = as.character(sample(1e8:5e9, 1)),
        fileCount = as.character(sample(1:4, 1))
      )
    )
  )
  
  write_json(
    json,
    paste0("data/json_logs/log_", i, ".json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}

