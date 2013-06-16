import java.util.Deque;
import java.util.ArrayDeque;
import java.util.Map;
import java.util.HashMap;
import exceptions.*;

public class InterpState implements TokenSink
{
    // the interpreter has a stack of contexts
    Deque<Ctx> ctxes;
    Map<String, Verb> verbTable;
    boolean tracebool;
    
    public InterpState() {
        ctxes = new ArrayDeque<Ctx>();
        verbTable = new HashMap<String, Verb>();
        tracebool = false;
    }
    
    public Map<String,Verb> getVerbTable() {
        return verbTable;
    }
    
    public void start(Deque<StackItem> input, Deque<StackItem> output) {
        Ctx parent = null;
        if (ctxes.size() > 0) {
            parent = ctxes.peek();
        }
        Ctx myCtx = new Ctx(parent, input, output, this);
        ctxes.push(myCtx);
    }
    
    public void stop() throws TISBLException {
        if (ctxes.size() > 0) {
            ctxes.pop();
        } else {
            throw new TISBLException("Tried to stop a context which never started. This is an interpreter bug.");
        }
    }
    
    public Ctx getCurrentCtx() {
        return ctxes.peek();
    }
    
    public int getCtxCount() {
        return ctxes.size();
    }
    
    public void takeToken(String s) {
        getCurrentCtx().getExec().addLast(new StringItem(s));
    }
    
    // trace functions
    public void traceOn() {
        tracebool = true;
    }
    
    public void traceOff() {
        tracebool = false;
    }
    
    public void trace(String s) {
        if (tracebool) {
            System.out.println("[trace] " + s);
        }
    }
    
    public void traceStack(String name, Deque<StackItem> s) {
        StringBuffer buf = new StringBuffer();
        if (tracebool) {
            for (StackItem i : s) {
                buf.insert(0, i.getStringValue() + ' ');
                
                String displayType = "";
                if (i instanceof StringItem) {
                    displayType = "str";
                } else if (i instanceof IntegerItem) {
                    displayType = "int";
                } else if (i instanceof FloatItem) {
                    displayType = "flt";
                }
                buf.insert(0, displayType + ".");
            }
        }
        
        trace(name + ": " + buf.toString());
    }
}
