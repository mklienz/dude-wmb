# Script to download files
# Because Github has a 100MB file size limit

at_gtfs_source = 'https://cdn01.at.govt.nz/data/gtfs.zip'
target_dir = './data-raw/at-gtfs/'
zip_name = paste0(target_dir, 'at-gtfs.zip')

if (!dir.exists(target_dir)) {
  dir.create(target_dir)
}

download.file(at_gtfs_source, zip_name)
unzip(zip_name, exdir = target_dir)
file.remove(zip_name)
