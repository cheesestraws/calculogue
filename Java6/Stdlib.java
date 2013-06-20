import java.util.Map;
import java.util.Deque;
import java.util.ArrayDeque;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import exceptions.*;

public class Stdlib
{

    public static boolean isZeroElement(StackItem item) {
        if (((item instanceof IntegerItem) || (item instanceof FloatItem)) &&
            (item.getIntValue() == 0)) {
                return true;
            } else {
                return false;
            }
    }
    
    public static void install(Map<String,Verb> vt) {
        // \exec
        vt.put("exec", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException {
                // the number of items to move onto the new context
                long count = input.pop().getIntValue();
                
                // start a new execution context and get a reference to it
                s.start(input, output);
                Ctx cur = s.getCurrentCtx();
                
                // move elements onto new execution stack
                for (long i = 0; i < count; i++) {
                    StackItem item = input.pop();
                    cur.getExec().push(item);
                }
                
                cur.go();
                
                s.stop();
            }
        });
        
        // \verb
        vt.put("verb", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                // get name
                String name = input.pop().getStringValue();
                
                // get count
                long count = input.pop().getIntValue();
                
                // ceate a new stack to wallop the tokens in
                Deque<StackItem> exec = new ArrayDeque<StackItem>();
                
                // copy things into this stack
                for (long i = 0; i < count; i++) {
                    StackItem item = input.pop();
                    exec.push(item);
                }
                
                Map<String, Verb> vt = s.getVerbTable();
                TISBLVerb v = new TISBLVerb(exec);
                vt.put(name, v);
            }
        });
        
        // \out
        vt.put("out", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                String item = input.pop().getStringValue();
                System.out.print(item);
            }
        });
        
        vt.put("if", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException {
                // the number of items to move onto the new context
                long count = input.pop().getIntValue();
                
                // start a new execution context and get a reference to it
                s.start(input, output);
                Ctx cur = s.getCurrentCtx();
                
                // move elements onto new execution stack
                for (long i = 0; i < count; i++) {
                    StackItem item = input.pop();
                    cur.getExec().push(item);
                }
                
                StackItem condition = input.pop();
                if (isZeroElement(condition)) {
                            // discard the context
                } else {
                    cur.go();
                }
                
                s.stop();
            }
        });
        
        // \while
        vt.put("while", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                // get count of tokens
                long count = input.pop().getIntValue();
                
                // ceate a new stack to wallop the tokens in
                Deque<StackItem> exec = new ArrayDeque<StackItem>();
                
                // copy things into this stack
                for (long i = 0; i < count; i++) {
                    StackItem item = input.pop();
                    exec.push(item);
                }
                
                boolean endloop = false;
                // loop!
                do {
                    // pop a condition
                    StackItem condition = input.pop();
                    if (isZeroElement(condition)) {
                            // a 0 means the end of the loop
                            endloop = true;
                    } else {
                        // start a new context
                        s.start(input, output);
                        Ctx cur = s.getCurrentCtx();
                        
                        // copy tokens onto the stack
                        for (StackItem item : exec) {
                            cur.getExec().addLast(item);
                        }
                        
                        cur.go();
                        
                        s.stop();
                    }
                } while (!endloop);
            }
        });
        
        // \not
        vt.put("not", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                StackItem condition = input.pop();
                if (isZeroElement(condition)) {
                    output.push(new IntegerItem(1));
                } else {
                    output.push(new IntegerItem(0));
                }
            }
        });
        
        // \swap
        vt.put("swap", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                StackItem a = input.pop();
                StackItem b = input.pop();
                output.push(a);
                output.push(b);
            }
        });
        
        // \dup
        vt.put("dup", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                StackItem a = input.peek();
                output.push(a);
            }
        });
        
        // \rm
        vt.put("rm", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                StackItem dummy = input.pop();
            }
        });
        
        // \mv
        vt.put("mv", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                StackItem item = input.pop();
                output.push(item);
            }
        });
        
        // \multipop
        vt.put("multipop", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                long count = input.pop().getIntValue();
                
                for (long i = 0; i < count; i++) {
                    StackItem item = input.pop();
                    output.push(item);
                }
            }
        });
        
        // \+
        vt.put("+", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                StackItem sec = input.pop();
                StackItem fst = input.pop();
                
                if ((fst instanceof StringItem) || (sec instanceof StringItem)) {
                    // if either are strings, concatenate them
                    output.push(new StringItem(fst.getStringValue() + sec.getStringValue()));
                } else if ((fst instanceof FloatItem) || (sec instanceof FloatItem)) {
                    // if either are floats, then turn both into floats
                    output.push(new FloatItem(fst.getFloatValue() + sec.getFloatValue()));
                } else {
                    // both must be integers
                    output.push(new IntegerItem(fst.getIntValue() + sec.getIntValue()));
                }
            }
        });
        
        // \-
        vt.put("-", new Verb() {
            public String sub_si(String s, long i) {
                int targetLength = s.length() - (int) i;
                return s.substring(0, targetLength);
            }
            
            public String sub_ss(String haystack, String needle) {
                StringBuffer buf = new StringBuffer();
                // iterate through haystack
                for (int i = 0; i < haystack.length(); i++) {
                    // the character in the haystack
                    char c = haystack.charAt(i);
                    if (needle.indexOf(c) == -1) {
                        // it isn't in the needle
                        buf.append(c);
                    }
                }
                
                return buf.toString();
            }
            
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                StackItem sec = input.pop();
                StackItem fst = input.pop();
                
                if ((fst instanceof IntegerItem) && (sec instanceof IntegerItem)) {
                    output.push(new IntegerItem(fst.getIntValue() - sec.getIntValue()));
                } else if (((fst instanceof IntegerItem) || (fst instanceof FloatItem))
                    && (sec instanceof StringItem)) {
                        String str = sub_si(sec.getStringValue(), fst.getIntValue());
                        output.push(new StringItem(str));
                } else if ((fst instanceof StringItem) && 
                    ((sec instanceof IntegerItem) || (sec instanceof FloatItem))) {
                        String str = sub_si(fst.getStringValue(), sec.getIntValue());
                        output.push(new StringItem(str));
                } else if ((fst instanceof StringItem) && (sec instanceof StringItem)) {
                    String str = sub_ss(fst.getStringValue(), sec.getStringValue());
                    output.push(new StringItem(str));
                } else {
                    // one or other must be a float here
                    output.push(new FloatItem(fst.getFloatValue() - sec.getFloatValue()));
                }
            }
        });
        
        // \*
        vt.put("*", new Verb() {
            private String mul_sf(String str, double d) {
                double count = d;
                StringBuffer buf = new StringBuffer();
                
                while (count >= 1) {
                    count--;
                    buf.append(str);
                }
                
                if (count > (1/str.length())) {
                    int chars = (int) Math.round(count * str.length());
                    buf.append(str.substring(0, chars));
                }
                
                return buf.toString();
            }
            
            private String mul_ss(String str, String expand) {
                StringBuffer buf = new StringBuffer();
                
                for (int i = 0; i < str.length(); i++) {
                    char c = str.charAt(i);
                    if (c == expand.charAt(0)) {
                        buf.append(expand);
                    } else {
                        buf.append(c);
                    }
                }
                
                return buf.toString();
            }
            
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                StackItem sec = input.pop();
                StackItem fst = input.pop();
                
                if ((fst instanceof IntegerItem) && (sec instanceof IntegerItem)) {
                    output.push(new IntegerItem(fst.getIntValue() * sec.getIntValue()));
                } else if (((fst instanceof IntegerItem) || (fst instanceof FloatItem))
                    && (sec instanceof StringItem)) {
                        String str = mul_sf(sec.getStringValue(), fst.getFloatValue());
                        output.push(new StringItem(str));
                } else if ((fst instanceof StringItem) && 
                    ((sec instanceof IntegerItem) || (sec instanceof FloatItem))) {
                        String str = mul_sf(fst.getStringValue(), sec.getFloatValue());
                        output.push(new StringItem(str));
                } else if ((fst instanceof StringItem) && (sec instanceof StringItem)) {
                    String str = mul_ss(fst.getStringValue(), sec.getStringValue());
                    output.push(new StringItem(str));
                } else {
                    // one or other must be a float here
                    output.push(new FloatItem(fst.getFloatValue() * sec.getFloatValue()));
                }
            }
        });
        
        // \div
        vt.put("div", new Verb() {
            private String div_sf(String str, double d) {
                int chars = (int) Math.round(str.length() / d);
                return str.substring(0, chars);
            }
            
            private String div_ss(String str, String divby) {
                StringBuffer buf = new StringBuffer(str);
                int idx;
                while ((idx = buf.toString().indexOf(divby)) >= 0) {
                    buf.delete(idx + 1, idx + divby.length());
                }
                return buf.toString();
            }
            
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                StackItem sec = input.pop();
                StackItem fst = input.pop();
                
                if ((fst instanceof IntegerItem) && (sec instanceof IntegerItem)) {
                    output.push(new IntegerItem(fst.getIntValue() / sec.getIntValue()));
                } else if (((fst instanceof IntegerItem) || (fst instanceof FloatItem))
                    && (sec instanceof StringItem)) {
                        String str = div_sf(sec.getStringValue(), fst.getFloatValue());
                        output.push(new StringItem(str));
                } else if ((fst instanceof StringItem) && 
                    ((sec instanceof IntegerItem) || (sec instanceof FloatItem))) {
                        String str = div_sf(fst.getStringValue(), sec.getFloatValue());
                        output.push(new StringItem(str));
                } else if ((fst instanceof StringItem) && (sec instanceof StringItem)) {
                    String str = div_ss(fst.getStringValue(), sec.getStringValue());
                    output.push(new StringItem(str));
                } else {
                    // one or other must be a float here
                    output.push(new FloatItem(fst.getFloatValue() / sec.getFloatValue()));
                }
            }
        });
        
        // \n
        vt.put("n", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                String val = input.pop().getStringValue();
                output.push(new StringItem(val + "\n"));
            }
        });
        
        // \_
        vt.put("_", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                String val = input.pop().getStringValue();
                output.push(new StringItem(val + " "));
            }
        });
        
        // \string?
        vt.put("string?", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                StackItem item = input.pop();
                if (item instanceof StringItem) {
                    output.push(new IntegerItem(1));
                } else {
                    output.push(new IntegerItem(0));
                }
            }
        });
        
        // \number?
        vt.put("number?", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                StackItem item = input.pop();
                if ((item instanceof IntegerItem) || (item instanceof FloatItem)) {
                    output.push(new IntegerItem(1));
                } else {
                    output.push(new IntegerItem(0));
                }
            }
        });
        
        // \integer?
        vt.put("integer?", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                StackItem item = input.pop();
                if (item instanceof IntegerItem) {
                    output.push(new IntegerItem(1));
                } else {
                    output.push(new IntegerItem(0));
                }
            }
        });
        
        // \float?
        vt.put("float?", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                StackItem item = input.pop();
                if (item instanceof FloatItem) {
                    output.push(new IntegerItem(1));
                } else {
                    output.push(new IntegerItem(0));
                }
            }
        });
        
        // \die
        vt.put("die", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                throw new ProgramDiedException("Program \\die-d.");
            }
        });
        
        // \in
        vt.put("in", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                try {
                    // Well, this is a novelty.
                    BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
                    String line = br.readLine();
                    output.push(new StringItem(line));
                } catch (IOException e) {
                    // If we can't read from stdin, just push a ""
                    output.push(new StringItem(""));
                }
            }
        });
        
        vt.put("eq?", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                StackItem a = input.pop();
                StackItem b = input.pop();
                
                if ((a instanceof StringItem) && (b instanceof StringItem) &&
                    (a.getStringValue().equals(b.getStringValue()))) {
                        output.push(new IntegerItem(1));
                } else if ((a instanceof StringItem) && !(b instanceof StringItem)) {
                    output.push(new IntegerItem(0));
                } else if (!(a instanceof StringItem) && (b instanceof StringItem)) {
                    output.push(new IntegerItem(0));
                } else if ((a instanceof FloatItem) && (b instanceof FloatItem)) {
                    if (a.getFloatValue() == b.getFloatValue()) {
                        output.push(new IntegerItem(1));
                    } else {
                        output.push(new IntegerItem(0));
                    }
                } else {
                    if (a.getIntValue() == b.getIntValue()) {
                        output.push(new IntegerItem(1));
                    } else {
                        output.push(new IntegerItem(0));
                    }
                }
            }
        });
        
        vt.put("trace=0", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                s.traceOff();
            }
        });
        
        vt.put("trace=1", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                s.traceOn();
            }
        });
        
        vt.put("present?", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                input.pop();
                output.push(new IntegerItem(0));
            }
        });
        
        vt.put("load", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                throw new BadParameterException("Loadable modules not supported in this interpreter.");
            }
        });
        
    }
    
    
    

}

/*
vt.put("exec", new Verb() {
            public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException{
                
            }
        });
 */