library(sampleDB)
library(curl)
library(openssl)
library(base64enc)

# backup_xfer_script.R
args <- commandArgs(trailingOnly = TRUE)

# Extract arguments
database_path <- args[1]
sftp_host <- args[2]
sftp_port <- args[3]
sftp_destination <- args[4]

# Retrieve and decode the encryption key from the environment variable
encoded_key <- Sys.getenv("SDB_BACKUP_KEY")
if (nchar(encoded_key) == 0) {
  stop("Encryption key not found in environment variable 'SDB_BACKUP_KEY'")
}
encryption_key <- base64decode(encoded_key)

# Define path for encrypted credentials
config_dir <- rappdirs::user_config_dir("sampleDB")
config_file <- file.path(config_dir, "sftp_credentials.enc")

# Read and decrypt credentials
encrypted_creds <- readRDS(config_file)
decrypted_creds <- openssl::aes_cbc_decrypt(encrypted_creds, encryption_key)
creds <- unserialize(decrypted_creds)

sftp_user <- creds$username
sftp_password <- creds$password

perform_backup <- function(database_path, sftp_host, sftp_port, sftp_user, sftp_password, sftp_destination) {
  # Perform the database backup

  backup_path <- tempdir()
  full_backup_path <- sampleDB::Backup_SampleDB(database_path, backup_path, checksum = FALSE)

  # Check if a backup was created
  if (!is.null(full_backup_path)) {
    backup_filename <- basename(full_backup_path)
    sftp_url <- paste0("sftp://", sftp_user, ":", sftp_password, "@", sftp_host, ":", sftp_port, "/", sftp_destination, "/", backup_filename)

    # Perform SFTP transfer
    tryCatch({
      curl_upload(file = full_backup_path, url = sftp_url)
      message("Backup successfully transferred via SFTP.")
    }, error = function(e) {
      message("Error during SFTP transfer: ", e$message)
    })

    # Cleanup
    unlink(full_backup_path)
  } else {
    message("No new backup created. Skipping SFTP transfer.")
  }
}

perform_backup(database_path, sftp_host, sftp_port, sftp_user, sftp_password, sftp_destination)
