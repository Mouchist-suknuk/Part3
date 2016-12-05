import java.util.ArrayList;

public class Main
{
public static void main(String arg[]) throws Exception
{
	LexicalAnalyser scanner=new  LexicalAnalyser(null);
	ArrayList<Symbol> tokens= scanner.RunScanner(arg);
	Parser parser =new Parser(tokens);
	parser.parse();
}


}