import java.util.Deque;
import exceptions.*;

public abstract class Verb
{
    public abstract void run(InterpState s, Deque<StackItem> input, Deque<StackItem> output) throws TISBLException;
}
