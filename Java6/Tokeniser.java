import java.io.Reader;
import java.io.BufferedReader;
import java.io.IOException;

/* This class implements the state machine in s, 2.6 of the spec */
public class Tokeniser
{
    private enum State { S, C, T };
    // S - whitespace - C - comment - T - program text
    
    private State state;
    private TokenSink owner;
    private StringBuffer sb;
    
    public Tokeniser(TokenSink owner) {
        this.owner = owner;
        state = State.S; // initial state is whitespace
    }
    
    public void take(char c) {
        switch (state) {
            case S:
                /* We're in whitespace state.  If we read a % then start a comment; otherwise, 
                 * if it's not a space, then start building a token.
                 */
                if (c == '%') {
                    state = State.C;
                } else if (!Character.isWhitespace(c)) {
                    sb = new StringBuffer();
                    sb.append(c);
                    state = State.T;
                }
                break;
            case C:
                /* If we're reading a comment and the char is a newline then
                 * go into whitespace state, otherwise do nothing.
                 */
                if (c == '\n' || c == '\r') {
                    state = state.S;
                }
                break;
            case T:
                /* If we're reading a token, keep reading until we hit whitespace */
                if (Character.isWhitespace(c)) {
                    // push the complete token
                    owner.takeToken(sb.toString());
                    state = state.S;
                } else {
                    // it's not whitespace, so keep building the token
                    sb.append(c);                    
        
                }
        }
    }
    
    public void takeString(String str) {
        int i;
        for (i = 0; i < str.length(); i++) {
            this.take(str.charAt(i));
        }
        this.take(' ');
    }
    
    public void takeReader(Reader r) throws IOException {
        BufferedReader rdr = new BufferedReader(r);
        int chr;
        // iterate until EOF
        while ((chr = rdr.read()) != -1) {
            this.take((char) chr);
        }
    }
    

}
