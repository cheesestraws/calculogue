import java.util.Deque;
import exceptions.*;

public class TISBLVerb extends Verb
{
    Deque<StackItem> execTemplate;
    
    public TISBLVerb(Deque<StackItem> exec) {
        execTemplate = exec;
    }

    public void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException {
        // start a new context
        s.start(input, output);
        
        // get a reference to it
        Ctx cur = s.getCurrentCtx();
        
        // copy the instructions into the new context
        for (StackItem i : execTemplate) {
            cur.getExec().addLast(i);
        }
        
        // run the context
        cur.go();
        
        // kill the context
        s.stop();
    }
    
}
