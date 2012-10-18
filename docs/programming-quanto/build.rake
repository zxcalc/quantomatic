if !(defined? $MAIN_JOB)
  fail "No MAIN_JOB given"
end
if !(defined? $DIST_NAME)
  $DIST_NAME = $MAIN_JOB
end
if !(defined? $EXTRA_INCLUDES)
  $EXTRA_INCLUDES = []
end
if !(defined? $VERBOSE_MSGS)
  $VERBOSE_MSGS = false
end

RakeFileUtils.verbose($VERBOSE_MSGS)

# internal
$BUILD_FILES = []
$MAIN_FILE = $MAIN_JOB + '.tex'
$INCLUDE_FILES = Dir[ 'tex/*', 'figures/*'] | $EXTRA_INCLUDES
$INCLUDE_FILES << $MAIN_FILE

if ENV['BUILD_DIR']
  $BUILD_DIR = ENV['BUILD_DIR']
end
if ENV['LATEX']
  $LATEX = ENV['LATEX']
end
if ENV['BIBTEX']
  $BIBTEX = ENV['BIBTEX']
end
if ENV['DVIPS']
  $BIBTEX = ENV['DVIPS']
end
if ENV['PS2PDF']
  $PS2PDF = ENV['PS2PDF']
end
if ENV['EPSTOPDF']
  $EPSTOPDF = ENV['EPSTOPDF']
end

if !(defined? $BUILD_DIR)
  $BUILD_DIR = 'build'
end
if !(defined? $LATEX)
  $LATEX = 'pdflatex'
end
if !(defined? $BIBTEX)
  $BIBTEX = 'bibtex'
end
if !(defined? $DVIPS)
  $DVIPS = 'dvips'
end
if !(defined? $DVIPS_OPTS)
  $DVIPS_OPTS = []
end
if !(defined? $PS2PDF)
  $PS2PDF = 'ps2pdf'
end
if !(defined? $PS2PDF_OPTS)
  $PS2PDF_OPTS = []
end
if !(defined? $EPSTOPDF)
  $EPSTOPDF = 'epstopdf'
end
if !(defined? $EPSTOPDF_OPTS)
  $EPSTOPDF_OPTS = []
end
$BIBFILES = []

def stripcomments (line)
  percentidx = 0
  esc = false
  line.each_char do |c|
    if esc
      esc = false
    elsif c == '\\'
      esc = true
    elsif c == '%'
      break
    end
    percentidx += 1
  end
  line[0,percentidx]
end

if !(defined? $LATEX_OUT_FMT)
  $LATEX_OUT_FMT = 'pdf'
  dvi_classes = ['powerdot',
                 'prosper']
  f = open($MAIN_FILE)
  f.each_line do |ln|
    match_data = stripcomments(ln).match(/\\documentclass(?:\[[^\]]*\])?\{([^}]*)\}/)
    if match_data
      doc_class = match_data[1]
      if dvi_classes.include?doc_class
        $LATEX_OUT_FMT = 'dvi'
      end
      break
    end
  end
  f.close
end
$BUILD_OUTPUT = "#{$BUILD_DIR}/#{$MAIN_JOB}.#{$LATEX_OUT_FMT}"

if $LATEX_OUT_FMT == 'dvi'
  file "#{$BUILD_DIR}/#{$MAIN_JOB}.ps" => [$BUILD_OUTPUT] do |t|
    command = [$DVIPS] + $DVIPS_OPTS + ['-o', t.name, t.prerequisites[0]]
    output = ""
    msg "Converting DVI file to Postscript"
    output = `#{shelljoin command} 2>&1`
    if $? != 0
      puts output
      fail "RAKE: Could not create PS file from DVI #{name}."
    end
  end
  file "#{$BUILD_DIR}/#{$MAIN_JOB}.pdf" => ["#{$BUILD_DIR}/#{$MAIN_JOB}.ps"] do |t|
    command = [$PS2PDF] + $PS2PDF_OPTS + [t.prerequisites[0], t.name]
    output = ""
    msg "Converting Postscript file to PDF"
    output = `#{shelljoin command}`
    if $? != 0
      puts output
      fail "RAKE: Could not create PDF file from PS #{name}."
    end
  end
elsif $LATEX_OUT_FMT != 'pdf'
  fail "Unknown LaTeX output format \"#{$LATEX_OUTPUT_FORMAT}\""
end

def msg (m)
  puts "RAKE: " + m
  STDOUT.flush
end

def warn (m)
  puts ">>> WARNING: " + m
  STDOUT.flush
end


####################################################################
# Stolen from
# http://svn.ruby-lang.org/repos/ruby/trunk/lib/shellwords.rb
# for compatibility with Ruby 1.8
def shellescape(str)
  str = str.to_s

  # An empty argument will be skipped, so return empty quotes.
  return "''" if str.empty?

  str = str.dup

  # Treat multibyte characters as is.  It is caller's responsibility
  # to encode the string in the right encoding for the shell
  # environment.
  str.gsub!(/([^A-Za-z0-9_\-.,:\/@\n])/, "\\\\\\1")

  # A LF cannot be escaped with a backslash because a backslash + LF
  # combo is regarded as line continuation and simply ignored.
  str.gsub!(/\n/, "'\n'")

  return str
end

def shelljoin(array)
  array.map { |arg| shellescape(arg) }.join(' ')
end
####################################################################


$LATEX_CMD = [$LATEX, '-interaction=nonstopmode', '-halt-on-error']
$LATEX_CMD += ['-fmt', 'latex', '-output-format', $LATEX_OUT_FMT]
if defined? $LATEX_OPTS
  $LATEX_CMD += $LATEX_OPTS
end
$BIBTEX_CMD = [$BIBTEX, '-terse']
if defined? $BIBTEX_OPTS
  $BIBTEX_CMD += $BIBTEX_OPTS
end

# latex draft mode does not create the pdf (or look at images)
def run_latex_draft (dir, name, file)
  command = $LATEX_CMD + ['-draftmode', '-jobname', name, file]
  output = ""
  Dir.chdir(dir) do
    output = `#{shelljoin command}`
  end
  if $? != 0
    puts output
    fail "RAKE: LaTeX error in job #{name}."
  end
  # When in DVI mode, the DVI file will be created even with -draftmode
  rm_f $BUILD_OUTPUT
end

def run_latex (dir, name, file, depth=0)
  command = $LATEX_CMD + ['-jobname', name, file]
  output = ""
  Dir.chdir(dir) do
    output = `#{shelljoin command}`
  end
  if $? != 0
    puts output
    fail "RAKE: LaTeX error in job #{name}."
  else
    if output["Rerun to get cross-references right."]
      if depth > 4
        fail "Failed to resolve all cross-references after 4 attempts"
      else
        msg "Rebuilding #{file} to get cross-references right"
        run_latex dir, name, file, (depth+1)
      end
    end
  end
end

for f in $INCLUDE_FILES
  if File.extname(f) == '.eps' and $LATEX_OUT_FMT == 'pdf'
    fbase = File.basename f, '.eps'
    fbuild = "#{$BUILD_DIR}/#{fbase}.pdf"
    $BUILD_FILES << fbuild
    file fbuild => [$BUILD_DIR,f] do |t|
      command = [$EPSTOPDF] + $EPSTOPDF_OPTS + ['--outfile='+t.name, t.prerequisites[1]]
      output = ""
      output = `#{shelljoin command}`
      if $? != 0
        puts "#{shelljoin command}"
        puts output
        fail "RAKE: Could not create PDF file from EPS #{name}."
      end
    end
  else
    fbase = File.basename f
    fbuild = "#{$BUILD_DIR}/#{fbase}"
    $BUILD_FILES << fbuild
    file fbuild => [$BUILD_DIR,f] do |t|
      cp t.prerequisites[1], t.name
    end
  end
end

def has_cites (auxfile)
  f = open(auxfile)
  found_cites = false
  f.each_line do |ln|
    if ln.start_with?"\\citation"
      found_cites = true
      break
    end
  end
  f.close
  found_cites
end

def find_bibfiles
  f = open($MAIN_FILE)
  $BIBFILES = []
  f.each_line do |ln|
    bibs = stripcomments(ln).scan(/\\bibliography\{([^}]*)\}/)
    for b in bibs
      b = b[0].strip
      if File.exists?("#{b}.bib")
        file "#{$BUILD_DIR}/#{b}.bib" => [$BUILD_DIR,"#{b}.bib"] do |t|
          cp t.prerequisites[1], t.name
        end
        $BIBFILES << "#{$BUILD_DIR}/#{b}.bib"
      elsif File.exists?("#{b}.bbl")
        file "#{$BUILD_DIR}/#{b}.bbl" => [$BUILD_DIR,"#{b}.bbl"] do |t|
          cp t.prerequisites[1], t.name
        end
      else
        warn "Could not find bibliography file #{b}.bib or #{b}.bbl, referenced from #{$MAIN_FILE}"
      end
    end
  end
  f.close

  if $BIBFILES.length > 0
    file "#{$BUILD_DIR}/#{$MAIN_JOB}.bbl" => $BIBFILES+["#{$BUILD_DIR}/#{$MAIN_JOB}.aux"] do |t|
      aux = "#{$BUILD_DIR}/#{$MAIN_JOB}.aux"
      old_aux = "#{$BUILD_DIR}/#{$MAIN_JOB}.last_bib_run.aux"
      if has_cites(aux)
        force = true
        if File.exists?(t.name)
          force = t.prerequisites.detect do |p|
            p.end_with?(".bib") and File.stat(p).mtime >= File.stat(t.name).mtime
          end
        end
        if force or !File.exists?old_aux or !identical?(aux,old_aux)
          msg 'Running BibTeX'
          command = $BIBTEX_CMD + [$MAIN_JOB]
          Dir.chdir($BUILD_DIR) do
            system(*command)
          end
          unless $? == 0
            fail "RAKE: BibTeX error in job #{$MAIN_JOB}."
          end
        end
      elsif !File.exists?old_aux or !identical?(aux,old_aux)
        msg 'No citations; skipping BibTeX'
        if File.exists?(t.name)
          rm t.name
        end
      end
      cp aux, old_aux
    end
    file $BUILD_OUTPUT => "#{$BUILD_DIR}/#{$MAIN_JOB}.bbl"
  end
end
find_bibfiles

def check_log(logfile)
  f = open(logfile)
  has_todos = false
  bad_cites = []
  bad_refs = []
  f.each_line do |ln|
    if ln["unresolved-TODO"]
      has_todos = true
    end
    bc = ln[/LaTeX Warning: Citation `([^']*)' on page/,1]
    if bc
      bad_cites << bc
    end
    br = ln[/LaTeX Warning: Reference `([^']*)' on page/,1]
    if br
      bad_refs << br
    end
  end
  f.close

  has_problems = false
  if has_todos
    warn 'you have TODOs left'
    has_problems = true
  end
  if bad_cites.length > 0
    warn "the following citations were unresolved: #{bad_cites.join(', ')}"
    has_problems = true
  end
  if bad_refs.length > 0
    warn "the following references were unresolved: #{bad_refs.join(', ')}"
    has_problems = true
  end
  return !has_problems
end

file $BUILD_OUTPUT => $BUILD_FILES do
  msg "Building #{$MAIN_FILE}"
  run_latex $BUILD_DIR, $MAIN_JOB, $MAIN_FILE
end

directory $BUILD_DIR
directory $DIST_NAME
directory $DIST_NAME

file "#{$BUILD_DIR}/#{$MAIN_JOB}.aux" => ($BUILD_FILES+[$MAIN_FILE]) do
  msg "Building #{$MAIN_FILE} to find refs"
  run_latex_draft $BUILD_DIR, $MAIN_JOB, $MAIN_FILE
end

file "#{$MAIN_JOB}.pdf" => ["#{$BUILD_DIR}/#{$MAIN_JOB}.pdf"] do
  cp "#{$BUILD_DIR}/#{$MAIN_JOB}.pdf", "#{$MAIN_JOB}.pdf"
end
file "#{$DIST_NAME}.pdf" => ["#{$BUILD_DIR}/#{$MAIN_JOB}.pdf"] do
  cp "#{$BUILD_DIR}/#{$MAIN_JOB}.pdf", "#{$DIST_NAME}.pdf"
end

desc "Create a draft PDF file (#{$MAIN_JOB}.pdf) [default]"
task :draft => [:check,"#{$MAIN_JOB}.pdf"]
task :default => [:draft]

desc "Create the final PDF file (#{$DIST_NAME}.pdf)"
task :final => [:check_final,"#{$DIST_NAME}.pdf"]

desc "Check for problems with the LaTeX document (eg: unresolved references)"
task :check => $BUILD_OUTPUT do
  check_log("#{$BUILD_DIR}/#{$MAIN_JOB}.log")
end
task :check_final => $BUILD_OUTPUT do
  is_ok = check_log("#{$BUILD_DIR}/#{$MAIN_JOB}.log")
  if !is_ok
    fail "There are still problems with the LaTeX document (see above)"
  end
end

desc "Create a tar archive containing all the source files"
task :tar => [$DIST_NAME,"#{$BUILD_DIR}/#{$MAIN_JOB}.bbl"] do
  msg "Creating (#{$DIST_NAME}.tar.gz)"
  rm_f "#{$DIST_NAME}.tar.gz"
  files = $INCLUDE_FILES+$BIBFILES+["#{$BUILD_DIR}/#{$MAIN_JOB}.bbl"]
  cp files, $DIST_NAME
  system('tar', 'czf', "#{$DIST_NAME}.tar.gz", $DIST_NAME)
  rm_rf $DIST_NAME
end

desc "Create a tar archive suitable for uploading to the arXiv"
# We don't include the bibfiles for arXiv
task :arxiv => [$DIST_NAME,"#{$BUILD_DIR}/#{$MAIN_JOB}.bbl"] do
  msg "Creating (#{$DIST_NAME}-arxiv.tar.gz)"
  rm_f "#{$DIST_NAME}-arxiv.tar.gz"
  files = $INCLUDE_FILES+["#{$BUILD_DIR}/#{$MAIN_JOB}.bbl"]
  cp files, $DIST_NAME
  system('tar', 'czf', "#{$DIST_NAME}-arxiv.tar.gz", $DIST_NAME)
  rm_rf $DIST_NAME
end

desc "Create the PDF file and open it in a PDF viewer"
task :view => ["#{$BUILD_DIR}/#{$MAIN_JOB}.pdf"] do
  msg "Opening application to view PDF"
  apps = ['xdg-open', # linux
          'open',     # mac
          'start']    # windows
  success = apps.detect do
    |app| system(app, "#{$BUILD_DIR}/#{$MAIN_JOB}.pdf")
  end
  if !success
    fail "Could not figure out how to open the PDF file"
  end
end

desc "Remove all build files and archives"
task :clean do
  msg "Deleting build directory and archive"
  rm_rf [$BUILD_DIR, "#{$DIST_NAME}.tar.gz", "#{$DIST_NAME}-arxiv.tar.gz"]
end

