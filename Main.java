
import org.antlr.v4.runtime.ANTLRFileStream;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.CommonTokenStream;

public class Main {

	public static void main(String[] args) throws IOException {
		// TODO Auto-generated method stub
		ANTLRFileStream input = new ANTLRFileStream(args[0]);
        ANTLRInputStream ip = new ANTLRInputStream(input.toString());
        helloLexer lex = new helloLexer(ip); 
        CommonTokenStream tokenstream = new CommonTokenStream(lex);
        hello2Parser parser = new hello2Parser(tokenstream);
        parser.start();
        //Token token;
        /*while ((token = lex.nextToken()).getType()!= -1) 
        {
        	System.out.println("line " + token.getLine()+ ":" + token.getStartIndex() + 
           		 " token <" + token.getType() + ", "+token.getText() + ">");
        }*/
	}
}