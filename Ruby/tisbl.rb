#!/usr/bin/env ruby
# encoding: UTF-8

$LOAD_PATH.unshift File.dirname(__FILE__)

$trace = false
$verbs = {}

require 'stdlib'

class Context
  def initialize(code = [])
    @stacks = {}
    @stacks[:p] = []
    @stacks[:s] = []
    @stacks[:c] = code
  end

  def fork(code, i, o)
    copy = Context.new(code)
    copy[:i] = @stacks[i]
    copy[:o] = @stacks[o]
    copy[:e] = @stacks[:c]
    copy
  end

  def [](n)
    @stacks[n]
  end

  def []=(n, v)
    @stacks[n] = v
  end

  def execute
    inames = { '' => :p, ':' => :s, '.' => :i, ',' => :c, ';' => :e }
    onames = { '' => :p, ':' => :s, '.' => :o, ',' => :c, ';' => :e }

    until @stacks[:c].empty?
      token = @stacks[:c].pop

      if $trace
        puts "T #{escape(token)}"
        puts "P [ #{@stacks[:p].map{ |s| escape(s) }.join ' '} ]"
        puts "S [ #{@stacks[:s].map{ |s| escape(s) }.join ' '} ]"
        puts "C [ #{@stacks[:c].map{ |s| escape(s) }.join ' '} ]"
        puts
      end

      case token
      when /^\\([,.:;]?)([^,.:;]+)([,.:;]?)$/
        error "Invalid input stack: #{$1}"  unless inames.has_key?($1)
        error "Invalid output stack: #{$3}" unless onames.has_key?($3)
        error "Unknown verb: #{$2}"         unless $verbs.has_key?($2)
        $verbs[$2].(self, inames[$1], onames[$3])
      when /^([,.:;]?)#(\d+)$/
        error "Invalid output stack: #{$3}" unless onames.has_key?($1)
        @stacks[onames[$1]].push $2.to_i
      when /^([,.:;]?)#(\d+.\d+)$/
        error "Invalid output stack: #{$3}" unless onames.has_key?($1)
        @stacks[onames[$1]].push $2.to_f
      when /^([,.:;]?)'(.*)$/
        error "Invalid output stack: #{$3}" unless onames.has_key?($1)
        @stacks[onames[$1]].push $2
      else
        error "Syntax error: #{token}"
      end
    end
  end

  def multipop(i)
    result = []
    count = @stacks[i].pop
    error "Not a number: #{escape(count)}" unless number?(count)
    count.to_i.times do
      result.push @stacks[i].pop
    end
    result
  end
end

def bool(v)
  v ? 1 : 0
end

def number?(v)
  v.is_a?(Fixnum) or v.is_a?(Float)
end

def error(message)
  puts message
  exit 1
end

def escape(t)
  case
  when t.is_a?(Fixnum)
    "i:#{t}"
  when t.is_a?(Float)
    "f:#{t}"
  else
    "s:#{t.dump}"
  end
end

if ARGV.empty?
  root = Context.new
  loop do
    print '> '
    root[:c].concat gets.gsub(/%.*$/, '').split.reverse
    root.execute
    puts
  end
else
  begin
    File.open(ARGV[0]) do |file|
      Context.new(file.read.gsub(/%.*$/, '').split.reverse).execute
    end
  rescue => e
    error e.message
  end
end

