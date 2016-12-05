import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

public class Parser {


	protected Symbol token;
	protected ArrayList<Symbol> tokenList;
	private int index = 0;
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
		this.token = tokenList.get(index++);
	}
	
	protected void unread() throws Exception{
		this.inBuffer = true;
	}
	
	private void handle_PROGRAM() throws Exception{
		this.derivations.add(1);
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
			this.derivations.add(2);
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
			this.derivations.add(3);
			this.unread();
			break;
		default:
			handleError(this.derivations.size());
			
		}
	}
	private void handle_VARLIST() throws Exception{
		this.derivations.add(4);
		this.read();
		this.match(LexicalUnit.VARNAME);
		this.handle_VARLISTTAIL();
	}
	private void handle_VARLISTTAIL() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case COMMA:
			this.derivations.add(5);
			this.match(LexicalUnit.COMMA);
			this.handle_VARLIST();
			break;
		case ENDLINE:
			this.derivations.add(6);
			this.unread();
			break;
		default:
			handleError(this.derivations.size());
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
			this.derivations.add(7);
			this.handle_INSTRUCTION();
			this.read();
			this.match(LexicalUnit.ENDLINE);
			this.handle_CODE();
			break;
		case END:
		case ENDIF:
		case ELSE:
		case ENDDO:
			this.derivations.add(8);
			this.unread();
			break;
		default:
			handleError(this.derivations.size());
		}
	}
	private void handle_INSTRUCTION() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case VARNAME:
			this.derivations.add(9);
			this.handle_ASSIGN();
			break;
		case IF: 
			this.derivations.add(10);
			this.handle_IF();
			break;
		case DO:
			this.derivations.add(11);
			this.handle_DO();
			break;
		case PRINT:
			this.derivations.add(12);
			this.handle_PRINT();
			break;
		case READ:
			this.derivations.add(13);
			this.handle_READ();
			break;
		default:
			handleError(this.derivations.size());
		}
	}
	
	private void handle_ASSIGN() throws Exception{
		this.derivations.add(14);
		this.read();
		this.match(LexicalUnit.VARNAME);
		this.read();
		this.match(LexicalUnit.EQUAL);
		this.handle_EXPRARITH();
	}
	
	private void handle_EXPRARITH() throws Exception{
		this.derivations.add(15);
		this.handle_EXPRARITHA();
		this.handle_EXPRARITHTAIL();
	}
	
	private void handle_EXPRARITHTAIL() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case PLUS:
		case MINUS:
			this.derivations.add(16);
			this.handle_OPADDMINUS();
			this.handle_EXPRARITHA();
			this.handle_EXPLISTTAIL();
			break;
		case ENDLINE:
		case RIGHT_PARENTHESIS:
		case AND:
		case OR:
		case EQUAL_COMPARE:
		case GREATER:
		case SMALLER:
		case SMALLER_EQUAL:
		case GREATER_EQUAL:
		case DIFFERENT:
			this.derivations.add(17);
			this.unread();
			break;
		default:
			handleError(this.derivations.size());
		}
	}
	
	private void handle_EXPRARITHA() throws Exception{
		this.derivations.add(18);
		this.handle_EXPRARITHB();
		this.handle_EXPRARITHATAIL();
	}
	
	private void handle_EXPRARITHATAIL() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case TIMES:
		case DIVIDE:
			this.derivations.add(19);
			this.handle_OPMULTIDIVIDE();
			this.handle_EXPRARITHB();
			this.handle_EXPRARITHATAIL();
			break;
		case ENDLINE:
		case PLUS:
		case MINUS:
		case RIGHT_PARENTHESIS:
		case AND:
		case OR:
		case EQUAL_COMPARE:
		case GREATER:
		case SMALLER:
		case SMALLER_EQUAL:
		case GREATER_EQUAL:
		case DIFFERENT:
		case COMMA:
			this.derivations.add(20);
			this.unread();
			break;
		default:
			handleError(this.derivations.size());
		}
	}
	
	private void handle_OPADDMINUS() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case PLUS:
			this.derivations.add(21);
			this.match(LexicalUnit.PLUS);
			break;
		case MINUS:
			this.derivations.add(22);
			this.match(LexicalUnit.MINUS);
			break;

		default:
			handleError(this.derivations.size());
		}
	}
	
	private void handle_OPMULTIDIVIDE() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case TIMES:
			this.derivations.add(23);
			this.match(LexicalUnit.TIMES);
			break;
		case DIVIDE:
			this.derivations.add(24);
			this.match(LexicalUnit.DIVIDE);
			break;

		default:
			handleError(this.derivations.size());
		}
	}
	
	private void handle_EXPRARITHB() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case VARNAME:
			this.derivations.add(25);
			this.match(LexicalUnit.VARNAME);
			break;
		case NUMBER:
			this.derivations.add(26);
			this.match(LexicalUnit.NUMBER);
			break;
		case LEFT_PARENTHESIS:
			this.derivations.add(27);
			this.match(LexicalUnit.LEFT_PARENTHESIS);
			this.handle_EXPRARITH();
			this.read();
			this.match(LexicalUnit.RIGHT_PARENTHESIS);
			break;
		case MINUS:
			this.derivations.add(28);
			this.match(LexicalUnit.MINUS);
			this.handle_EXPRARITH();
			break;
		default:
			handleError(this.derivations.size());
		}
	}
	
	private void handle_IF() throws Exception{
		this.derivations.add(29);
		this.read();
		this.match(LexicalUnit.IF);
		this.read();
		this.match(LexicalUnit.LEFT_PARENTHESIS);
		this.handle_COND();
		this.read();
		this.match(LexicalUnit.RIGHT_PARENTHESIS);
		this.read();
		this.match(LexicalUnit.THEN);
		this.read();
		this.match(LexicalUnit.ENDLINE);
		this.handle_CODE();
		this.handle_IFTAIL();
	}
	
	private void handle_IFTAIL() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case ELSE:
			this.derivations.add(30);
			this.match(LexicalUnit.ELSE);
			this.read();
			this.match(LexicalUnit.ENDLINE);
			this.handle_CODE();
			this.read();
			this.match(LexicalUnit.ENDIF);
			break;
		case ENDIF:
			this.derivations.add(31);
			this.match(LexicalUnit.ENDIF);
			break;
		default:
			handleError(this.derivations.size());
		}
	}
	private void handle_COND() throws Exception{
		this.derivations.add(32);
		this.handle_CONDA();
		this.handle_CONDB();
	}
	private void handle_CONDA() throws Exception{
		this.derivations.add(33);
		this.handle_CONDHEAD();
		this.handle_SIMPLECOND();
		this.handle_CONDATAIL();
	}
	
	private void handle_CONDHEAD() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case NOT:
			this.derivations.add(34);
			this.match(LexicalUnit.NOT);
			break;
		case VARNAME:
		case NUMBER:
		case LEFT_PARENTHESIS:
		case MINUS:
			this.derivations.add(35);
			this.unread();
			break;
		default:
			handleError(this.derivations.size());
		}
	}
	
	private void handle_CONDATAIL() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case AND:
			this.derivations.add(36);
			this.match(LexicalUnit.AND);
			this.handle_CONDHEAD();
			this.handle_SIMPLECOND();
			this.handle_CONDATAIL();
			break;
		case OR:
		case RIGHT_PARENTHESIS:
			this.derivations.add(37);
			this.unread();
			break;
		default:
			handleError(this.derivations.size());
		}
	}
	
	private void handle_CONDB() throws Exception{
		this.read();
		switch (this.token.getType()) {
		case OR:
			this.derivations.add(38);
			this.match(LexicalUnit.OR);
			this.handle_CONDA();
			this.handle_CONDB();
			break;
		case RIGHT_PARENTHESIS:
			this.derivations.add(39);
			this.unread();
			break;
		default:
			handleError(this.derivations.size());
		}
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
		if(!this.token.getType().equals(unit)) this.handleError(this.derivations.size());
	}
	
	public void parse() throws Exception {
		this.handle_PROGRAM();
		
		//Output
		for (Integer i: this.derivations)
		{
			System.out.println("["+i+"]"+ Rules.get(i));
		}
		
	}
	
	private void initActionTable()
	{
		Rules.put(1, "<Program> --> PROGRAM [ProgName] [EndLine] <Vars> <Code> END");
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
	

	private void handleError(int ruleNumber) throws Exception
	{
		throw new Exception("A syntax error occurs at rule ["+ruleNumber+"]");
	}
}
