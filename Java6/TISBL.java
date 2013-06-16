import exceptions.*;
import java.io.FileReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.NoSuchElementException;

public class TISBL
{
    public static void runfile(String filename) {
        try {
            // init the interpreter
            InterpState state = new InterpState();
            Stdlib.install(state.getVerbTable());
            Tokeniser toks = new Tokeniser(state);
            
            // create a context
            state.start(null, null);
            
            // run the file
            toks.takeReader(new FileReader(filename));
            state.getCurrentCtx().go();
            
            state.stop();
        } catch (TISBLException e) {
            System.out.println("[ERROR] " + e.getMessage());
        } catch (FileNotFoundException e) {
            System.out.println("[ERROR] " + e.getMessage());
        } catch (IOException e) {
            System.out.println("[ERROR] " + e.getMessage());
        } catch (NumberFormatException e) {
            System.out.println("[ERROR] " + e.getMessage());
        } catch (NoSuchElementException e) {
            System.out.println("[ERROR] Stack underflow");
        }
    }
    
    public static void printHelp() {
        System.out.println("Please give a filename.");
    }
    
    public static void main(String[] args) {
        if (args.length != 1) {
            printHelp();
        } else {
            runfile(args[0]);
        }
    }

}
