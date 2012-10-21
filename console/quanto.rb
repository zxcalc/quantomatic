#!/usr/bin/env ruby
require 'rubygems'

require 'json'
require "readline"
require "open3"

$debug = true
$seq = 0
$controller = "red_green"

# allows JSON entities at top-level besides lists and objects
def JSON.parse_fragment(str)
  JSON.parse("[#{str}]")[0]
end

def output_str(s)
  $stdout.write(s.gsub(/\e/, '[ESC]'))
end

def send_request(qin, modl, fun, json)
  req = "\e<#{$seq}\e,#{$controller}\e,#{modl}\e,#{fun}\e,#{json}\e>"
  
  if $debug
    $stdout.write('>> ')
    output_str(req)
    puts
  end
  
  qin.write req
  qin.flush
  $seq+=1
end

def read_until_esc(qout, code)
  str = ''
  loop do
    c = qout.readchar.chr
    output_str(c) if $debug
    if c == "\e"
      c = qout.readchar.chr
      output_str(c) if $debug
      
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
  $stdout.write('<< ') if $debug
  
  read_until_esc(qout, '<')
  rid = read_until_esc(qout, ',')
  status = read_until_esc(qout, ',')
  json_out = read_until_esc(qout, '>')
  
  puts if $debug
  
  return [rid, status, JSON.parse_fragment(json_out)]
end

Open3.popen3("../core/bin/quanto-core") do |qin, qout, qerr, wait_thr|
  # qin.sync = true
  # qout.sync = true
  # qerr.sync = true
  
  send_request(qin, '!!', 'version', 'null')
  get_response(qout)
  
  puts "\ndone"
end

# loop do
#   s = Readline.readline('quanto> ')
#   break if s == nil
#   puts s
# end
