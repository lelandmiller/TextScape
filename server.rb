#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

# https://gist.github.com/Integralist/2862917

require "webrick"

runcommand = "cabal run"

class MyNormalClass
  def self.add (a, b)
    a.to_i + b.to_i
  end
  
  def self.subtract (a, b)
    a.to_i - b.to_i
  end
end

class MyServlet < WEBrick::HTTPServlet::AbstractServlet

  def do_GET (request, response)
    #if request.query["a"] && request.query["b"]
    a = request.query["a"]
    b = request.query["b"]
    command = request.query["command"]
    
    response.status = 200
    response.content_type = "text/plain"
    result = nil
    

    case request.path
    when "/run"
      result = $interpreter.exec(command)
      puts result
    when "/subtract"
      result = MyNormalClass.subtract(a, b)
    when "/"
      response.content_type = "text/html"
      result = File.read("index.html")
    else
      result = "No such method"
    end
    
    response.body = result.to_str + "\n"
    #else
    #response.status = 200
    #response.body = "You did not provide the correct parameters"
    #end
  end
end


class ShellInterface
  def initialize()
    @proc = IO.popen(runcommand, 'w+')
  end

  def exec(c)
    @proc.puts c
    
    ret = ""

    while ((line = @proc.gets) != "\0\n") do
      ret += line
    end

    ret
  end
  
  private
  @proc

end

$interpreter = ShellInterface.new

server = WEBrick::HTTPServer.new(:Port => 1234)

server.mount "/", MyServlet

trap("INT") {
  server.shutdown
}

server.start
