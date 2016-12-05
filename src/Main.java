import java.util.ArrayList;

public class Main
{
public static void main(String arg[]) throws Exception
{
	LexicalAnalyser scanner=new  LexicalAnalyser(null);
	ArrayList<Symbol> tokens= scanner.RunScanner(arg);
	System.out.println("tokens "+tokens.size());
	Parser parser =new Parser(tokens);
	parser.parse();
}


}