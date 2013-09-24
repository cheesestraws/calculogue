# encoding: UTF-8

$verbs['_'] = ->(c, i, o) do
  c[o].push c[i].pop.to_s + ' '
end

$verbs['n'] = ->(c, i, o) do
  c[o].push c[i].pop.to_s + "\n"
end

$verbs['+'] = ->(c, i, o) do
  a = c[i].pop
  b = c[i].pop
  if number?(a) and number?(b)
    c[o].push b + a
  else
    c[o].push b.to_s + a.to_s
  end
end

$verbs['-'] = ->(c, i, o) do
  a = c[i].pop
  b = c[i].pop
  if number?(a) and number?(b)
    c[o].push b - a
  elsif number?(a) and not number?(b)
    c[o].push b[0, b.length - a.round]
  elsif not number?(a) and number?(b)
    c[o].push a[0, a.length - b.round]
  else
    c[o].push b.delete(a)
  end
end

$verbs['*'] = ->(c, i, o) do
  a = c[i].pop
  b = c[i].pop
  if number?(a) and number?(b)
    c[o].push b * a
  elsif number?(a) and not number?(b)
    c[o].push b * a + b[0, (b.length * (a - a.floor)).round]
  elsif not number?(a) and number?(b)
    c[o].push a * b + a[0, (a.length * (b - b.floor)).round]
  else
    c[o].push b.gsub(a[0], a)
  end
end

$verbs['div'] = ->(c, i, o) do
  a = c[i].pop
  b = c[i].pop
  if number?(a) and number?(b)
    c[o].push b.to_f / a.to_f
  elsif number?(a) and not number?(b)
    c[o].push b[0, (b.length / a.to_f).round]
  elsif not number?(a) and number?(b)
    c[o].push a[0, (a.length / b.to_f).round]
  else
    c[o].push b.gsub(a, a[0])
  end
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
  c[o].push bool(number?(c[i].pop))
end

$verbs['integer?'] = ->(c, i, o) do
  c[o].push bool(c[i].pop.is_a?(Fixnum))
end

$verbs['float?'] = ->(c, i, o) do
  c[o].push bool(c[i].pop.is_a?(Float))
end

$verbs['die'] = ->(c, i, o) do
  exit
end

$verbs['multipop'] = ->(c, i, o) do
  c.multipop(i)
end

$verbs['present?'] = ->(c, i, o) do
  c[o].push bool(File.exists?(c[i].pop))
end

$verbs['load'] = ->(c, i, o) do
  require c[i].pop
end

$verbs['verb'] = ->(c, i, o) do
  name = c[i].pop
  code = c.multipop(i)
  $verbs[name] = ->(c, i, o) do
    c.fork(code.clone, i, o).execute
  end
end

$verbs['exec'] = ->(c, i, o) do
  c.fork(c.multipop(i), i, o).execute
end

$verbs['if'] = ->(c, i, o) do
  code = c.multipop(i)
  c.fork(code, i, o).execute unless c[i].pop == 0.0
end

$verbs['while'] = ->(c, i, o) do
  code = c.multipop(i)
  until c[i].pop == 0.0
    c.fork(code.clone, i, o).execute
  end
end

$verbs['trace=0'] = ->(c, i, o) do
  $trace = false
end

$verbs['trace=1'] = ->(c, i, o) do
  $trace = true
end

