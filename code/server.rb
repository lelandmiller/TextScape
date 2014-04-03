#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

# https://gist.github.com/Integralist/2862917

require "webrick"

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
    #a = request.query["a"]
    #b = request.query["b"]
    command = request.query["command"]
    
    response.status = 200
    response.content_type = "text/plain"
    result = nil
    

    case request.path
    when "/run"
      result = $interpreter.exec(command)
      puts result
    #when "/subtract"
    #  result = MyNormalClass.subtract(a, b)
    #when "/"
    #  response.content_type = "text/html"
    #  result = File.read("index.html")
    else
      super
      #result = "No such method"
    end
    
    response.body = result.to_str + "\n"
    #else
    #response.status = 200
    #response.body = "You did not provide the correct parameters"
    #end
  end
end



class NonCachingFileHandler < WEBrick::HTTPServlet::FileHandler
  def prevent_caching(res)
    res['ETag']          = nil
    res['Last-Modified'] = Time.now + 100**4
    res['Cache-Control'] = 'no-store, no-cache, must-revalidate, post-check=0, pre-check=0'
    res['Pragma']        = 'no-cache'
    res['Expires']       = Time.now - 100**4
  end
  
  def do_POST(request, response)
    command = request.query["command"]

    response.status = 200
    response.content_type = "text/plain"
    
    case request.path
    when "/run"
      response.body = $interpreter.exec(command) + "\n"

    end
    prevent_caching(response)
  end

  def do_GET(request, response)

    command = request.query["command"]
    
    response.status = 200
    response.content_type = "text/plain"
    result = nil
    

    case request.path
    when "/run"
      response.body = $interpreter.exec(command) + "\n"
      puts result
    else
      super
    end

    #super
    prevent_caching(response)
  end

end




class ShellInterface
  def initialize()
    @proc = IO.popen("cabal run", 'w+')

  end

  def exec(c)
    @proc.puts c

    # log
    puts "TS IN: " + c

    ret = ""

    while ((line = @proc.gets) != "\0\n") do
      ret += line
    end
    
    #log
    puts "TS OUT: " + ret
    ret
  end
  
  private
  @proc

end

$interpreter = ShellInterface.new

# clear the buffer of any extraneous build messages by running an unecessary command
$interpreter.exec('(listSym)')

server = WEBrick::HTTPServer.new(:Port => 1234)

server.mount "/", NonCachingFileHandler , './html'
#server.mount "/", MyServlet , './html'

trap("INT") {
  server.shutdown
}

server.start
