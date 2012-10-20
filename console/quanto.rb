#!/usr/bin/env ruby

require "readline"
require "open3"

$seq = 0
$controller = "red_green"

def send_request(qin, modl, fun, json)
  puts "\e<#{$seq}\e,#{$controller}\e,#{modl}\e,#{fun}\e,#{json}\e>"
  sz = qin.write "\e<#{$seq}\e,#{$controller}\e,#{modl}\e,#{fun}\e,#{json}\e>"
  puts "wrote #{sz} chars"
  qin.flush
  $seq+=1
end

def read_until_esc(qout, code)
  str = ''
  loop do
    puts 'about to readchar'
    c = qout.readchar.chr
    puts "[#{c}]"
    if c == "\e"
      c = qout.readchar.chr
      puts "[#{c}]"
      
      if c == "\e"
        str << c # escaped ESC
      elsif c == code
        return str
      else
        raise Exception("Expected escape code: [#{code}], got: [#{c}]")
      end
    else
      str << c
    end
  end
end

def get_response(qout)
  read_until_esc(qout, '<')
  rid = read_until_esc(qout, ',')
  status = read_until_esc(qout, ',')
  json_out = read_until_esc(qout, '>')
  
  return [rid, status, json_out]
end

Open3.popen3("../core/bin/quanto-core") do |qin, qout, qerr, wait_thr|
  qin.sync = true
  qout.sync = true
  qerr.sync = true
  send_request(qin, '!!', 'version', 'null')
  puts 'sent request'
  puts get_response(qout)
  puts 'done'
end

# loop do
#   s = Readline.readline('quanto> ')
#   break if s == nil
#   puts s
# end
