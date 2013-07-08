#!/usr/bin/env ruby
require 'rubygems'

require 'yajl'
require "readline"
require "open3"

$debug = false

class CoreException < Exception
end

# class ControllerModule
#   def initialize(core, name)
#     @name = name
#     @core = core
#   end
  
#   def help(fn=nil)
#     if fn == nil
#       return @core.call_function('!!', 'help', {:controller=>@core.controller,:module=>@name})
#     else
#       return @core.call_function('!!', 'help',
#         {:controller=>@core.controller,:module=>@name, :function=>fn})
#     end
#   end
  
#   def method_missing(*args)
#     fn = args[0].to_s
#     return @core.call_function(@name, fn, args[1])
#   end
# end

class QuantoCore
  attr_accessor :controller
  attr_reader :main
  
  def initialize(quanto, controller)
    @seq = 0
    @quanto = quanto
    @controller = controller
    @parser = Yajl::Parser.new(:symbolize_keys=>true)
    @encoder = Yajl::Encoder.new
    @parser.on_parse_complete = method(:parsed_json)
    @reader_thr = nil
    @handlers = {}
    
    # modules
    # @main = ControllerModule.new(self, 'Main')
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
  
  def parsed_json(resp)
    if resp[:success]
      rid = resp[:request_id]
      f = @handlers[rid]
      @handlers.delete(rid)
      f.call(resp[:output])
    else
      raise CoreException.new(resp[:message])
    end
  end
  
  # def pull_json
  #   json = nil
    
  #   loop do
  #     if ! @json_stack.empty?
  #       json = @json_stack.pop
  #       break
  #     else
  #       sleep 0.01
  #     end
  #   end
    
  #   return json
  # end
  
  def request(obj)
    # return nil if quanto has not been started
    return nil if @reader_thr == nil
    
    @encoder.encode(obj, @qin)
    @seq += 1
    
    return (@seq - 1)
  end
  
  def call_function(modl, function, input, &handler)
    obj = {
      :request_id => @seq,
      :controller => @controller,
      :module     => modl,
      :function   => function,
      :input      => input
    }
    
    @handlers[:request_id] = handler
    self.request(obj)
  end
  
  def version
    self.call_function('!!', 'version')
  end
end

if $0 == 'irb'
  $q = QuantoCore.new('../core/bin/quanto-core', 'red_green')
  $q.start
  puts "Quantomatic v#{$q.version}"
end


