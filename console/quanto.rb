#!/usr/bin/env ruby
require 'rubygems'

require 'json'
require "readline"
require "open3"

$debug = false
$seq = 0
$controller = "red_green"

# allows JSON entities at top-level besides lists and objects
def JSON.parse_fragment(str)
  JSON.parse("[#{str}]")[0]
end

def JSON.generate_fragment(obj)
  JSON.generate([obj])[1..-2]
end

def output_str(s)
  $stdout.write(s.gsub(/\e/, '[ESC]'))
end

def send_request(qin, modl, fun, json)
  req = "\e<#{$seq}\e,#{$controller}\e,#{modl}\e,#{fun}\e,#{JSON.generate_fragment(json)}\e>"
  
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

def run_command(qin,qout,cmd)
  if cmd == 'version'
    send_request(qin, '!!', 'version', nil)
    version = get_response(qout)[2]
    puts "Quantomatic v#{version}"
  elsif cmd =~ /^help ([^:]*)(::(.*))?$/
    if $3 != nil
      send_request(qin, '!!', 'help', {:module=>$1, :function=>$3})
      _, status, resp = get_response(qout)
      
      if status == 'OK'
        puts "Function: #{$1}::#{$3}\n  #{resp}\n\n"
      else
        puts "ERROR: #{resp['message']}"
      end
    else
      send_request(qin, '!!', 'help', {:module=>$1})
      _, status, resp = get_response(qout)
      
      if status == 'OK'
        puts "\nModule: #{$1}\n  #{resp}\n\n"
      else
        puts "ERROR: #{resp['message']}"
      end
    end
  end
end

Open3.popen3("../core/bin/quanto-core") do |qin, qout, qerr, wait_thr|
  
  send_request(qin, '!!', 'version', nil)
  version = get_response(qout)[2]
  puts "Quantomatic v#{version}"
  
  loop do
    begin
      cmd = Readline.readline('quanto> ')
      break if cmd == nil or cmd == 'exit'
      run_command(qin,qout,cmd)
    rescue Interrupt
      puts "^C"
      break
    end
  end
  
  puts "\ndone"
end
