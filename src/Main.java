import java.util.ArrayList;

public class Main
{
public static void main(String arg[]) throws Exception
{
	ArrayList<Symbol> tokens= (new  LexicalAnalyser(null)).RunScanner(arg);
	Parser parser =new Parser(tokens);
	parser.parse();
}


}