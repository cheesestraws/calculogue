#!/usr/bin/env ruby
# encoding: UTF-8

$LOAD_PATH.unshift File.dirname(__FILE__)

$trace = false
$verbs = {}

require 'stdlib'

class Context
  def initialize
    @stacks = {}
    [:p, :s, :c].each do |n|
      @stacks[n] = []
    end
  end
  def fork(code, i, o)
    copy = Context.new
    copy[:i] = @stacks[i]
    copy[:o] = @stacks[o]
    copy[:e] = @stacks[:c]
    copy[:c] = code
    copy
  end
  def [](n)
    @stacks[n]
  end
  def []=(n, v)
    @stacks[n] = v
  end
end

def bool(v)
  v ? 1 : 0
end

def number?(v)
  v.is_a?(Fixnum) or v.is_a?(Float)
end

def multipop(c, i)
  result = []
  c[i].pop.times do
    result.push c[i].pop
  end
  result
end

def execute(context)
  inames = { '' => :p, ':' => :s, '.' => :i, ',' => :c, ';' => :e }
  onames = { '' => :p, ':' => :s, '.' => :o, ',' => :c, ';' => :e }

  until context[:c].empty?
    if $trace
      puts "P [ #{context[:p].join(' ')} ]"
      puts "S [ #{context[:s].join(' ')} ]"
      puts "C [ #{context[:c].join(' ')} ]"
    end

    case context[:c].pop
    when /^\\([,.:;]?)([^,.:;]+)([,.:;]?)$/
      $verbs[$2].(context, inames[$1], onames[$3])
    when /^([,.:;]?)#(\d+)$/
      context[onames[$1]].push $2.to_i
    when /^([,.:;]?)#(\d+.\d+)$/
      context[onames[$1]].push $2.to_f
    when /^([,.:;]?)'(.*)$/
      context[onames[$1]].push $2
    else
      raise
    end
  end
end

root = Context.new

if ARGV.empty?
  loop do
    print '> '
    root[:c].concat gets.gsub(/%.*$/, '').split.reverse
    execute root
    puts
  end
else
  File.open(ARGV[0]) do |file|
    root[:c] = file.read.gsub(/%.*$/, '').split.reverse
    execute root
  end
end

