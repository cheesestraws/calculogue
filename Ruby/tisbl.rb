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
      if $trace
        puts "P [ #{@stacks[:p].join(' ')} ]"
        puts "S [ #{@stacks[:s].join(' ')} ]"
        puts "C [ #{@stacks[:c].join(' ')} ]"
      end

      case @stacks[:c].pop
      when /^\\([,.:;]?)([^,.:;]+)([,.:;]?)$/
        $verbs[$2].(self, inames[$1], onames[$3])
      when /^([,.:;]?)#(\d+)$/
        @stacks[onames[$1]].push $2.to_i
      when /^([,.:;]?)#(\d+.\d+)$/
        @stacks[onames[$1]].push $2.to_f
      when /^([,.:;]?)'(.*)$/
        @stacks[onames[$1]].push $2
      else
        raise
      end
    end
  end

  def multipop(i)
    result = []
    @stacks[i].pop.times do
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

if ARGV.empty?
  root = Context.new
  loop do
    print '> '
    root[:c].concat gets.gsub(/%.*$/, '').split.reverse
    root.execute
    puts
  end
else
  File.open(ARGV[0]) do |file|
    Context.new(file.read.gsub(/%.*$/, '').split.reverse).execute
  end
end

