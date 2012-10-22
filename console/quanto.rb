#!/usr/bin/env ruby
require 'rubygems'

require 'yajl'
require "readline"
require "open3"

$debug = false

class QuantoCore
  attr_accessor :controller
  
  def initialize(quanto, controller)
    @seq = 0
    @quanto = quanto
    @controller = controller
    @parser = Yajl::Parser.new
    @encoder = Yajl::Encoder.new
    @parser.on_parse_complete = method(:parsed_json)
    @reader_thr = nil
    @json_stack = []
  end
  
  def start
    @qin, @qout, @qerr = Open3.popen3(@quanto)
    @reader_thr = Thread.new do
      loop do
        c = @qout.readchar.chr
        #puts "[#{c}]"
        @parser << c
      end
    end
  end
  
  def stop
    @qin.close
    @qout.close
    @qerr.close
  end
  
  def parsed_json(obj)
    @json_stack << obj
  end
  
  def pull_json
    json = nil
    
    loop do
      if ! @json_stack.empty?
        json = @json_stack.pop
        break
      else
        sleep 0.01
      end
    end
    
    return json
  end
  
  def request(modl, function, input=nil)
    # return nil if quanto has not been started
    return nil if @reader_thr == nil
    
    obj = {
      :request_id => @seq,
      :controller => @controller,
      :module     => modl,
      :function   => function,
      :input      => input
    }
    
    @encoder.encode(obj, @qin)
    @seq += 1
    
    return self.pull_json
  end
end

$core = QuantoCore.new('../core/bin/quanto-core', 'red_green')
$core.start

version = $core.request('!!', 'version')['output']

puts "Quantomatic v#{version}"

def q(modl, function, input=nil)
  $core.request(modl, function, input)
end

