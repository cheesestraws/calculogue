#!/usr/bin/env ruby
# encoding: UTF-8

$LOAD_PATH.unshift File.dirname(__FILE__)

$trace = false
$verbs = {}

require 'stdlib'

class Context
    def initialize(code = [], parent = nil)
        @parent = parent
        @stacks = {}
        @stacks[:p] = []
        @stacks[:s] = []
        @stacks[:c] = code
        @depth = 0
        @depth = parent.depth + 1 if @parent
    end

    attr_reader :depth, :parent

    def fork(code, i, o)
        copy = Context.new(code, self)
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

    def trace(token)
        puts "(Depth #{@depth}) #{escape(token)}"
        puts "  Primary     [ #{@stacks[:p].map{ |s| escape(s) }.join(' ')} ]"
        puts "  Secondary : [ #{@stacks[:s].map{ |s| escape(s) }.join(' ')} ]"
        puts "  Execution , [ #{@stacks[:c].map{ |s| escape(s) }.join(' ')} ]"
        if @parent
            puts "  Input     . [ #{@stacks[:i].map{ |s| escape(s) }.join(' ')} ]"
            puts "  Output    . [ #{@stacks[:o].map{ |s| escape(s) }.join(' ')} ]"
            puts "  Parent    ; [ #{@stacks[:e].map{ |s| escape(s) }.join(' ')} ]"
        end
    end

    def execute
        inames = { '' => :p, ':' => :s, '.' => :i, ',' => :c, ';' => :e }
        onames = { '' => :p, ':' => :s, '.' => :o, ',' => :c, ';' => :e }

        until @stacks[:c].empty?
            token = @stacks[:c].pop
            error "Cannot execute number" unless token.is_a?(String)

            trace(token) if $trace

            case token
            when /^\\([,.:;]?)([^,.:;]+)([,.:;]?)$/
                istack = inames[$1]
                ostack = onames[$3]
                verb = $verbs[$2]
                error "Invalid input stack: #{$1}"  unless istack
                error "Invalid output stack: #{$3}" unless ostack
                error "Unknown verb: #{$2}"         unless verb
                verb.(self, istack, ostack)
            when /^([,.:;]?)#(\d+)$/
                ostack = onames[$1]
                error "Invalid output stack: #{$1}" unless ostack
                @stacks[ostack].push $2.to_i
            when /^([,.:;]?)#(\d+.\d+)$/
                ostack = onames[$1]
                error "Invalid output stack: #{$1}" unless ostack
                @stacks[ostack].push $2.to_f
            when /^([,.:;]?)'(.*)$/
                ostack = onames[$1]
                error "Invalid output stack: #{$1}" unless ostack
                @stacks[ostack].push $2
            else
                error "Syntax error: #{token}"
            end
        end
    end

    def multipop(i)
        count = @stacks[i].pop
        error "Not a number: #{escape(count)}" unless number?(count)
        @stacks[i].pop(count).reverse
    end
end

def bool(v)
    v ? 1 : 0
end

def number?(v)
    v.is_a?(Fixnum) or v.is_a?(Float)
end

def error(message)
    $stderr.puts message
    exit 1
end

def escape(t)
    if t.is_a?(String)
        t.dump
    else
        t.to_s
    end
end

def parse(text)
    text.gsub(/%.*$/, '').split.reverse
end

if ARGV.empty?
    root = Context.new
    loop do
        print '> '
        root[:c].concat parse(gets)
        root.execute
        puts
    end
else
    until ARGV.empty? or ARGV[0][0,2] != '--' do
        arg = ARGV.shift
        $trace = true if arg == '--trace'
        break         if arg == '--'
    end
    begin
        File.open(ARGV[0]) do |file|
            Context.new(parse(file.read)).execute
        end
    rescue => e
        error e.message
    end
end

