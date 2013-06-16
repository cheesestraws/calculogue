import java.util.Deque;
import java.util.ArrayDeque;
import exceptions.*;

public class Ctx
{
    /* Java actually has a native stack class that does most of what we want.
       Or, rather, an interface Deque and a load of implementations.
       Let's keep things nice and generic for now and use the interface. */
    
    // These are the stacks we are responsible for creating.
    private Deque<StackItem> pri;
    private Deque<StackItem> sec;
    private Deque<StackItem> exec;
    
    // These are the stacks we're not
    private Deque<StackItem> input;
    private Deque<StackItem> output;
    private Deque<StackItem> upexec;
    
    private InterpState owner;
    
    // accessors
    public Deque<StackItem> getPri() { return pri; }
    public Deque<StackItem> getSec() { return sec; }
    public Deque<StackItem> getExec() { return exec; }
    public Deque<StackItem> getInput() { return input; }
    public Deque<StackItem> getOutput() { return output; }
    public Deque<StackItem> getUpexec() { return upexec; }
    
    public Ctx(Ctx parent, Deque<StackItem> input, Deque<StackItem> output, InterpState owner) {
        if (parent != null) {
            this.upexec = parent.exec;
        }
        this.input = input;
        this.output = output;
        
        this.pri = new ArrayDeque<StackItem>();
        this.sec = new ArrayDeque<StackItem>();
        this.exec = new ArrayDeque<StackItem>();
        
        this.owner = owner;
    }
    
    public void go() throws TISBLException {
        StackItem s;
        while (exec.size() > 0) {
            s = exec.pop();
            // can only execute string items on the exec stack
            if (s instanceof StringItem) {
                // trace, if I can
                if (owner != null) { 
                    int count = owner.getCtxCount();
                    if (count == 1) {
                        owner.trace("In topmost context:");
                    } else {
                        owner.trace("In context " + (count - 1) + " above root:");
                    }
                    owner.trace("Token is: " + s.getStringValue()); 
                }
                
                Token t = new Token(s.getStringValue());
                this.runtok(t);
                owner.trace("after which:");
                owner.traceStack(" PRI", pri);
                owner.traceStack(" SEC", sec);
                owner.traceStack("EXEC", exec);
                owner.trace("");
            } else {
                throw new BadExecException("Could not exec non-string: " + s.getStringValue());
            }
        }
    }
    
    private Deque<StackItem> resolveStackName(char name, boolean inputStack) {
        // since return returns immediately, no break;s here
        switch (name) {
            case '.': if (inputStack) { return this.input; } else { return this.output; }
            case ',': return this.exec;
            case ':': return this.sec;
            case ';': return this.upexec;
            default:  return this.pri;
        }
    }
    
    private void runtok(Token t) throws TISBLException {
        StackItem i;
        // what kind of token is it?
        switch (t.getType()) {
            case UNKNOWN:
                throw new BadTokenException("Bad token: " + t.getOriginalString());
            case WORD:
                i = new StringItem(t.getContent());
                this.resolveStackName(t.getOutputStack(), false).push(i);
                break;
            case NUMBER:
                // is it a float or an integer?
                if (t.getContent().indexOf('.') == -1) {
                    // there's no . in it, it's an integer
                    long l = Long.parseLong(t.getContent());
                    i = new IntegerItem(l);
                    this.resolveStackName(t.getOutputStack(), false).push(i);
                } else {
                    // there is!  It's a float.
                    double d = Double.parseDouble(t.getContent());
                    i = new FloatItem(d);
                    this.resolveStackName(t.getOutputStack(), false).push(i);
                }
                break;
            case VERB:
                if (this.owner != null) {
                    // get the verb object
                    Verb v = owner.getVerbTable().get(t.getContent());
                    
                    // get the input and output stacks
                    Deque<StackItem> input = this.resolveStackName(t.getInputStack(), true);
                    Deque<StackItem> output = this.resolveStackName(t.getOutputStack(), false);
                    
                    if (v == null) {
                        throw new NoSuchVerbException("No such verb: " + t.getContent());
                    }
                    
                    // run it!
                    v.run(owner, input, output);
                }
        }
    }

}
