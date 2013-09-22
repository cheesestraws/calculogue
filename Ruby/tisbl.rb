#!/usr/bin/env ruby

class Context
  def initialize
    @stacks = {}
    [:p, :s, :c, :i, :o].each do |n|
      @stacks[n] = []
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
  c[o].push b + a
end

$verbs['*'] = ->(c, i, o) do
  a = c[i].pop
  b = c[i].pop
  c[o].push b * a
end

$verbs['out'] = ->(c, i, o) do
  print c[i].pop
end

$verbs['dup'] = ->(c, i, o) do
  c[o].push c[i][-1]
end

$verbs['rm'] = ->(c, i, o) do
  c[i].pop
end

$verbs['mv'] = ->(c, i, o) do
  c[o].push c[i].pop
end

$verbs['eq?'] = ->(c, i, o) do
  if c[i].pop == c[i].pop
    c[o].push 1.0
  else
    c[o].push 0.0
  end
end

$verbs['multipop'] = ->(c, i, o) do
  c[i].pop.to_i.times do
    c[i].pop
  end
end

$verbs['verb'] = ->(c, i, o) do
  name = c[i].pop
  block = pop_block(c, i, o)
  $verbs[name] = ->(c, i, o) do
    copy = block.clone
    copy[:i] = c[i]
    copy[:o] = c[o]
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

def pop_block(c, i, o)
  block = Context.new
  block[:i] = c[i]
  block[:o] = c[o]
  c[i].pop.to_i.times do
    block[:c].push c[i].pop
  end
  block
end

def execute(context)
  inames = { '' => :p, ':' => :s, '.' => :i, ',' => :c }
  onames = { '' => :p, ':' => :s, '.' => :o, ',' => :c }

  until context[:c].empty? do
    token = context[:c].pop
    case token
    when /^\\([,.:]?)([+\-*\w\?]+)([,.:]?)$/
      $verbs[$2].call context, inames[$1], onames[$3]
    when /^([,.:]?)#(\d+)$/
      context[onames[$1]].push $2.to_f
    when /^([,.:]?)#(\d+.\d+)$/
      context[onames[$1]].push $2.to_f
    when /^([,.:]?)'(.*)$/
      context[onames[$1]].push $2
    else
      raise "#{token} is blort"
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

