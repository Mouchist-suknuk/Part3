import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

public class Parser {


	protected Symbol token;
	private LexicalAnalyser scanner;
	protected ArrayList<Symbol> tokenList;
	private int index = -1;
	private boolean inBuffer = false;
	private Map<Integer, String> Rules;
	private ArrayList<Integer> derivations;
	public Parser(ArrayList<Symbol> tokenList)
	{
		this.tokenList=tokenList;
		this.derivations=new ArrayList<Integer>();
		this.Rules=new HashMap<Integer, String>();
		initActionTable();
	}
	
	protected void read() throws Exception{
		if(this.inBuffer ) this.inBuffer = false;
		else 
		this.token = tokenList.get(++index);
	}
	
	protected void unread() throws Exception{
		this.inBuffer = true;
	}
	
	private void handle_PROGRAM() throws Exception{
		this.read();
		this.match(LexicalUnit.PROGRAM);
		this.read();
		this.match(LexicalUnit.VARNAME);
		this.read();
		this.match(LexicalUnit.ENDLINE);
		this.handle_VARS();
		this.handle_CODE();
		this.read();
		this.match(LexicalUnit.END);
		//$
		this.match(null);
	}
	private void handle_VARS() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case INTEGER:
			this.match(LexicalUnit.INTEGER);
			this.handle_VARLIST();
			this.read();
			this.match(LexicalUnit.ENDLINE);
			break;
		case END: 
		case VARNAME:
		case IF:
		case DO:
		case PRINT:
		case READ:
			this.unread();
			break;
		default:
			throw new Exception("");
			
		}
	}
	private void handle_VARLIST() throws Exception{
		this.read();
		this.match(LexicalUnit.VARNAME);
		this.handle_VARLISTTAIL();
	}
	private void handle_VARLISTTAIL() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case COMMA:
			this.match(LexicalUnit.COMMA);
			this.handle_VARLIST();
			break;
		case ENDLINE:
			this.unread();
			break;
		default:
			throw new Exception("");
		}
	}
	private void handle_CODE() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case VARNAME:
		case IF:
		case DO:
		case PRINT:
		case READ:
			this.handle_INSTRUCTION();
			this.read();
			this.match(LexicalUnit.ENDLINE);
			this.handle_CODE();
			break;
		case END:
		case ENDIF:
		case ELSE:
		case ENDDO:
			this.unread();
			break;
		default:
			throw new Exception("");
		}
	}
	private void handle_INSTRUCTION() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case VARNAME:
			this.handle_ASSIGN();
			break;
		case IF: 
			this.handle_IF();
			break;
		case DO:
			this.handle_DO();
			break;
		case PRINT:
			this.handle_PRINT();
			break;
		case READ:
			this.handle_READ();
			break;
		default:
			break;
		}
	}
	private void handle_ASSIGN() throws Exception{ 
	}
	private void handle_EXPRARITH() throws Exception{ 
	}
	private void handle_EXPRARITHTAIL() throws Exception{ 
	}
	private void handle_EXPRARITHA() throws Exception{ 
	}
	private void handle_EXPRARITHATAIL() throws Exception{ 
	}
	private void handle_OPADDMINUS() throws Exception{ 
	}
	private void handle_OPMULTIDIVIDE() throws Exception{ 
	}
	private void handle_EXPRARITHB() throws Exception{ 
	}
	private void handle_IF() throws Exception{ 
	}
	private void handle_IFTAIL() throws Exception{ 
	}
	private void handle_COND() throws Exception{ 
	}
	private void handle_CONDA() throws Exception{ 
	}
	private void handle_CONDHEAD() throws Exception{ 
	}
	private void handle_CONDATAIL() throws Exception{ 
	}
	private void handle_CONDB() throws Exception{ 
	}
	private void handle_SIMPLECOND() throws Exception{ 
	}
	private void handle_COMP() throws Exception{ 
	}
	private void handle_DO() throws Exception{ 
	}
	private void handle_PRINT() throws Exception{ 
	}
	private void handle_READ() throws Exception{ 
	}
	private void handle_EXPLIST() throws Exception{ 
	}
	private void handle_EXPLISTTAIL() throws Exception{ 
	}

	protected void match(LexicalUnit unit) throws Exception{
		if(!this.token.getType().equals(unit)) this.handle_bad_token(unit);
	}
	
	protected void handle_bad_token(LexicalUnit[] units) throws Exception{
		//throw new GrammaticalException(units, this.token.unit, this.token.getValue(), (Integer) this.token.get(Symbol.LINE), (Integer) this.token.get(Symbol.COLUMN));
	}

	protected void handle_bad_token(LexicalUnit unit) throws Exception{
		//throw new GrammaticalException(unit, this.token.unit, this.token.getValue(), (Integer) this.token.get(Symbol.LINE), (Integer) this.token.get(Symbol.COLUMN));
	}

	public void parse() throws Exception {
		this.handle_PROGRAM();
		
	}
	
	private void initActionTable()
	{
		Rules.put(1, "<Program> --> PROGRAM [ProgName] [EndLine] <Vars> <Code> END $");
		Rules.put(2, "<Vars> --> INTEGER <VarList> [EndLine]");
		Rules.put(3, "<Vars> --> EPSILON");
		Rules.put(4, "<VarList> --> [VarName]<VarListTail>");
		Rules.put(5, "<VarListTail> --> , <VarList>");
		Rules.put(6, "<VarListTail> --> EPSILON");
		Rules.put(7, "<Code> --> <Instruction> [EndLine] <Code>");
		Rules.put(8, "<Code> --> EPSILON");
		Rules.put(9, "<Instruction> --> <Assign>");
		Rules.put(10, "<Instruction> --> <If>");
		Rules.put(11, "<Instruction> --> <Do>");
		Rules.put(12, "<Instruction> --> <Print>");
		Rules.put(13, "<Instruction> --> <Read>");
		Rules.put(14, "<Assign> --> [VarName] = <ExprArith>");
		Rules.put(15, "<ExprArith> --> <ExprArithA> <ExprArithTail>");
		Rules.put(16, "<ExprArithTail> --> <OpAddMinus> <ExprArithA> <ExprArithTail>");
		Rules.put(17, "<ExprArithTail> --> EPSILON				  ");
		Rules.put(18, "<ExprArithA> --> <ExprArithB> <ExprArithATail>");
		Rules.put(19, "<ExprArithATail> --> <OpMultiDivide> <ExprArithB> <ExprArithATail>");
		Rules.put(20, "<ExprArithATail> --> EPSSILON");
		Rules.put(21, "<OpAddMinus> --> +");
		Rules.put(22, "<OpAddMinus> --> -");
		Rules.put(23, "<OpMultiDivide> --> *");
		Rules.put(24, "<OpMultiDivide> --> /");
		Rules.put(25, "<ExprArithB> --> [VarName]");
		Rules.put(26, "<ExprArithB> --> [Number]");
		Rules.put(27, "<ExprArithB> --> ( <ExprArith> )");
		Rules.put(28, "<ExprArithB> --> - <ExprArith>");
		Rules.put(29, "<If> --> IF (<Cond>) THEN [EndLine] <Code> <IfTail>");
		Rules.put(30, "<IfTail> --> ELSE [EndLine] <Code> ENDIF");
		Rules.put(31, "<IfTail> --> ENDIF");
		Rules.put(32, "<Cond> --> <CondA> <CondB>");
		Rules.put(33, "<CondA> --> <CondHead> <SimpleCond> <CondATail>");
		Rules.put(34, "<CondHead> --> .NOT.");
		Rules.put(35, "<CondHead> --> EPSILON");
		Rules.put(36, "<CondATail> --> .AND. <CondHead> <SimpleCond> <CondATail>");
		Rules.put(37, "<CondATail> --> EPSILON");
		Rules.put(38, "<CondB> --> .OR. <CondA> <CondB>");
		Rules.put(39, "<CondB> --> EPSILON");
		Rules.put(40, "<SimpleCond> --> <ExprArith> <Comp> <ExprArith>");
		Rules.put(41, "<Comp> --> .EQ.");
		Rules.put(42, "<Comp> --> .GE.");
		Rules.put(43, "<Comp> --> .GT.");
		Rules.put(44, "<Comp> --> .LE.");
		Rules.put(45, "<Comp> --> .LT.");
		Rules.put(46, "<Comp> --> .NE.");
		Rules.put(47, "<Do> --> DO [VarName] = [Number], [Number] [EndLine] <Code> ENDDO");
		Rules.put(48, "<Print> --> PRINT*, <ExpList>");
		Rules.put(49, "<Read> --> READ*, <VarList>");
		Rules.put(50, "<ExpList> --> <ExprArith><ExpListTail>");
		Rules.put(51, "<ExpListTail> --> , <ExpList>");
		Rules.put(52, "<ExpListTail> --> EPSILON");
	}
}
