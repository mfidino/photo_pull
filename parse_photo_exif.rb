require 'bundler/setup'
#require 'exifr'
#require 'dotenv'
require 'mini_exiftool'

Bundler.require(:default)
#Dotenv.load


photo_paths_log = ARGV[0]
pattern = /^\w+-\w+-\w+\s\(\d+\)\.JPG/
#if File.file?(photo_paths_log)
  #photo_list = File.open(photo_paths_log, 'r').split("\n")
#else 
# currently works for each transect
  photo_list = Dir.glob("#{photo_paths_log}/**/*.JPG")
#end

tsv = "output_#{ENV['PHOTO_PATH'].split('/').last}.txt"

unless File.file?(tsv) 
  File.write(tsv, "path\tdate_time\tkeywords\tsite_season\tsite\tname\n")
end 

begin 
  photo_path = photo_list.pop
  

  #File.write(photo_paths_log, photo_list.join("\n"))
  exif = MiniExiftool.new(photo_path)
rescue 
	tsv_line = "#{photo_path}\tNA\tNA\tcorrupt file\tNA"
	File.open(tsv, 'a') { |f| f.puts tsv_line} 
else
  image_name = photo_path.split('/').last

  if image_name.match pattern
    tsv_line = "#{photo_path}\t#{exif.datetimeoriginal}\t#{exif.xpkeywords}\t#{image_name[/^[^ ]*/]}\t#{image_name[/^\w+-\w+/]}\t#{image_name[/\(\d+\)/].gsub(/\D/,"")}"
    File.open(tsv, 'a') { |f| f.puts tsv_line}
  else
    tsv_line = "#{photo_path}\tNA\tNA\tincorrect file name\tNA"
    File.open(tsv, 'a') { |f| f.puts tsv_line} 
  end
end while photo_list.count > 0

