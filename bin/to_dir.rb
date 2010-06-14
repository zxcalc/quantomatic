# Convert a theory XML file to a theory directory.

require 'rubygems'
require 'hpricot'

ARGV.each do |theory|
  doc = Hpricot(open(theory + ".xml"))
  thy = theory + ".theory"
  
  Dir.mkdir(thy); Dir.mkdir(thy + "/rewrites")
  Dir.chdir(thy + "/rewrites")
  (doc/"rule").each do |rule|
    rl = (rule/"/name").inner_text
    Dir.mkdir(rl)
    
    f = File.open(rl + "/lhs.graph",'w')
    f.puts((rule/"/lhs").inner_html); f.close
    
    f = File.open(rl + "/rhs.graph",'w')
    f.puts((rule/"/rhs").inner_html); f.close
  end
end

