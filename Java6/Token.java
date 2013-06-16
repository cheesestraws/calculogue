/* This class keeps the result of parsing a token on an exec stack.
 * It stores the type of the token, its input/output stack names and
 * its content.
 */
public class Token
{
    // An enum type 
    public static enum Type {
        WORD, NUMBER, VERB, UNKNOWN
    }
    
    private String originalString;
    private Type type;
    private char inputStack;
    private char outputStack;
    private String content;
    
    // accessors
    public String getOriginalString() { return originalString; }
    public Type getType() { return type; }
    public char getInputStack() { return inputStack; }
    public char getOutputStack() { return outputStack; }
    public String getContent() { return content; }
    
    // The constructor creates a token from a string.  See section 2.4 of spec v. 2.0/2.1.
    public Token(String str) {
        this. originalString = str;
        // is the first character a '\'?
        if (str.startsWith("\\")) {
            this.type = Type.VERB;
            this.parseVerb(str.substring(1)); // chop off first char
        } else {
            this.parseNoun(str);
        }
    }
    
    private void parseVerb(String str) {
        // check if we have a verb name
        if (str.startsWith(".") || str.startsWith(",") | str.startsWith(":") | str.startsWith(";")) {
            this.inputStack = str.charAt(0);
            this.parseVerbName(str.substring(1));
        } else {
            this.inputStack = '\0'; // null is the default stack, it's as good a value as any!
            this.parseVerbName(str);
        }
    }
    
    private void parseVerbName(String str) {
        String content = str;
        // see if the content has a stack name at the end.
        if (content.endsWith(".") || content.endsWith(",") | content.endsWith(":") 
            | content.endsWith(";")) {
                this.outputStack = str.charAt(str.length() - 1);
                content = str.substring(0, str.length() - 1);
            }
            
        this.content = content;
    }
    
    private void parseNoun(String str) {
        if (str.startsWith(".") || str.startsWith(",") | str.startsWith(":") | str.startsWith(";")) {
            this.outputStack = str.charAt(0);
            this.parseNounType(str.substring(1));
        } else {
            this.outputStack = '\0';
            this.parseNounType(str);
        }
    }
    
    private void parseNounType(String str) {
        if (str.startsWith("#")) {
            this.type = Type.NUMBER;
            this.content = str.substring(1);
        } else if (str.startsWith("'")) {
            this.type = Type.WORD;
            this.content = str.substring(1);            
        } else {
            this.type = Type.UNKNOWN;
            this.content = str;
        }
    }
}
