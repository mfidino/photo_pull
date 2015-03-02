require 'bundler/setup'
Bundler.require(:default)
Dotenv.load

photo_paths_log = 'photo_paths.log'

if File.file?(photo_paths_log)
  photo_list = File.open(photo_paths_log, 'r').split("\n")
else 
  photo_list = Dir["#{ENV['PHOTO_PATH']}/**/*.JPG"]
end

tsv = "output_#{ENV['PHOTO_PATH'].split('/').last}.txt"

unless File.file?(tsv) 
  File.write(tsv, "path\tdate_time\tsite\tname\n")
end 

begin 
  photo_path = photo_list.pop
  #File.write(photo_paths_log, photo_list.join("\n"))
  exif = EXIFR::JPEG.new(photo_path)
  image_name = photo_path.split('/').last
  tsv_line = "#{photo_path}\t#{exif.date_time}\t#{image_name[/^[^ ]*/]}\t#{image_name[/\(\d*\)/].gsub(/\D/,"")}"
  File.open(tsv, 'a') { |f| f.puts tsv_line}
end while photo_list.count > 0

