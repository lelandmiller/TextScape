#!/usr/bin/env ruby

# Uses term-ansicolor gem for colorization
# http://stackoverflow.com/questions/1489183/colorized-ruby-output

runcommand = "cabal run"

class String
  def bold
    "\033[1m#{self}\033[22m"
  end

  def reverse_color  
    "\033[7m#{self}\033[27m" 
  end

  # colorization
  def colorize(color_code)
    "\e[#{color_code}m#{self}\e[0m"
  end

  def red
    colorize(31)
  end

  def green
    colorize(32)
  end

  def yellow
    colorize(33)
  end

  def pink
    colorize(35)
  end

end

intProc = IO.popen(runcommand, 'w+')

require "readline"
while buf = Readline.readline(">> ", true)
  if buf == ":r" then
    intProc = IO.popen(runcommand, 'w+')
  else
    intProc.puts buf
  
    while ((line = intProc.gets) != "\0\n") do
      print line.green
    end
    
    if buf == "close" then
      break
    end
  end
end
