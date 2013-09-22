#!/usr/bin/env ruby

class Context
  def initialize
    @stacks = {}
    [:p, :s, :c].each do |n|
      @stacks[n] = []
    end
    [:i, :o, :e].each do |n|
      @stacks[n] = nil
    end
  end
  def clone
    copy = Context.new
    @stacks.each do |k,v|
      copy[k] = v.clone
    end
    copy
  end
  def [](n)
    @stacks[n]
  end
  def []=(n, v)
    @stacks[n] = v
  end
end

$trace = false
$verbs = {}

$verbs['_'] = ->(c, i, o) do
  c[o].push c[i].pop + ' '
end

$verbs['n'] = ->(c, i, o) do
  c[o].push c[i].pop.to_s + "\n"
end

$verbs['+'] = ->(c, i, o) do
  a = c[i].pop
  b = c[i].pop
  if a.is_a? String or b.is_a? String
    c[o].push b.to_s + a.to_s
  else
    c[o].push b + a
  end
end

$verbs['-'] = ->(c, i, o) do
  a = c[i].pop
  b = c[i].pop
  c[o].push b - a
end

$verbs['*'] = ->(c, i, o) do
  a = c[i].pop
  b = c[i].pop
  c[o].push b * a
end

$verbs['not'] = ->(c, i, o) do
  c[o].push bool(c[i].pop == 0.0)
end

$verbs['in'] = ->(c, i, o) do
  c[o].push gets
end

$verbs['out'] = ->(c, i, o) do
  print c[i].pop
end

$verbs['dup'] = ->(c, i, o) do
  c[o].push c[i][-1]
end

$verbs['swap'] = ->(c, i, o) do
  a = c[i].pop
  b = c[i].pop
  c[o].push a
  c[o].push b
end

$verbs['rm'] = ->(c, i, o) do
  c[i].pop
end

$verbs['mv'] = ->(c, i, o) do
  c[o].push c[i].pop
end

$verbs['eq?'] = ->(c, i, o) do
  c[o].push bool(c[i].pop == c[i].pop)
end

$verbs['string?'] = ->(c, i, o) do
  c[o].push bool(c[i].pop.is_a?(String))
end

$verbs['number?'] = ->(c, i, o) do
  c[o].push bool(!c[i].pop.is_a?(String))
end

$verbs['integer?'] = ->(c, i, o) do
  c[o].push bool(c[i].pop.is_a?(Fixnum))
end

$verbs['float?'] = ->(c, i, o) do
  c[o].push bool(c[i].pop.is_a?(Float))
end

$verbs['die'] = ->(c, i, o) do
  raise
end

$verbs['multipop'] = ->(c, i, o) do
  multipop(c, i)
end

$verbs['present?'] = ->(c, i, o) do
  c[o].push bool(File.exists?(c[i].pop))
end

$verbs['load'] = ->(c, i, o) do
  rasie
end

$verbs['verb'] = ->(c, i, o) do
  name = c[i].pop
  block = pop_block(c, i, o)
  $verbs[name] = ->(c, i, o) do
    copy = block.clone
    copy[:i] = c[i]
    copy[:o] = c[o]
    copy[:e] = c[:c]
    execute copy
  end
end

$verbs['exec'] = ->(c, i, o) do
  execute pop_block(c, i, o)
end

$verbs['if'] = ->(c, i, o) do
  block = pop_block(c, i, o)
  execute block unless c[i].pop == 0.0
end

$verbs['while'] = ->(c, i, o) do
  block = pop_block(c, i, o)
  until c[i].pop == 0.0
    copy = block.clone
    copy[:i] = c[i]
    copy[:o] = c[o]
    copy[:e] = c[:c]
    execute copy
  end
end

$verbs['trace=0'] = ->(c, i, o) do
  $trace = false
end

$verbs['trace=1'] = ->(c, i, o) do
  $trace = true
end

def bool(v)
  v ? 1 : 0
end

def multipop(c, i)
  result = []
  c[i].pop.times do
    result.push c[i].pop
  end
  result
end

def pop_block(c, i, o)
  block = Context.new
  block[:i] = c[i]
  block[:o] = c[o]
  block[:e] = c[:c]
  block[:c] = multipop(c, i)
  block
end

def execute(context)
  inames = { '' => :p, ':' => :s, '.' => :i, ',' => :c, ';' => :e }
  onames = { '' => :p, ':' => :s, '.' => :o, ',' => :c, ';' => :e }

  until context[:c].empty? do
    if $trace
      puts "P [ #{context[:p].join(' ')} ]"
      puts "S [ #{context[:s].join(' ')} ]"
      puts "C [ #{context[:c].join(' ')} ]"
    end

    case context[:c].pop
    when /^\\([,.:;]?)([^,.:;]+)([,.:;]?)$/
      $verbs[$2].call context, inames[$1], onames[$3]
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

def source(text)
  text = text.gsub(/%.*$/, '')
  context = Context.new
  context[:c] = text.split.reverse
  execute context
end

ARGV.each do |arg|
  File.open(arg) do |file|
    source file.read
  end
end

