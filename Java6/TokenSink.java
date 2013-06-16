/* This is an interface for things that can be on the end of a 
 * tokeniser; it's mostly implemented by InterpState.
 */

public interface TokenSink
{
    public void takeToken(String s);
}
