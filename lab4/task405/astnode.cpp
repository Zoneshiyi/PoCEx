#include "astnode.h"
#include<map>
#include<string>
#include<list>

extern int spaces;
extern std::unique_ptr<LLVMContext> theContext;
extern std::unique_ptr<Module> theModule;
extern std::unique_ptr<IRBuilder<>> builder;
extern std::map<std::string, AllocaInst *> namedValues;
extern std::unique_ptr<legacy::FunctionPassManager> theFPM;
extern int grammererror;
extern std::map<std::string, AllocaInst *> curNamedValues;

extern BasicBlock *continueBasicBlock;

struct Var {
  std::string vName;
  AllocaInst *alloc;
};
std::list<Var*>vars;

Var* search(std::string varName) {
    std::list<Var*>::iterator it = vars.begin();
    for(;it != vars.end();it++) 
    {
      if((*it)->vName == varName) 
      {
        return *it;
      }
    }
    return NULL;
}

Var* searchInComp(std::string varName) {
    std::list<Var*>::iterator it = vars.begin();
    for(;it != vars.end();it++) 
    {
      if((*it)->vName == varName) 
      {
        return *it;
      }
      if((*it)->vName == "") 
      {
        return NULL;
      }
    }
    return NULL;
}

void printspaces() {
  for (int i = 0; i < spaces; ++i)
    std::cout << " ";
}
void printGrammerInfo(std::string nodeName, int line) {
  printspaces();
  std::cout << nodeName << " (" << line << ")" << std::endl;
}

void printSemanticError(int type, int line, std::string info = "") {
  grammererror = 1;
  std::cout << "Error type " << type << " at Line " << line << "."
            << std::endl;
}

int parseNIdentifier(NIdentifier &nIdentifier) {
  printspaces();
  std::cout << "ID: " << nIdentifier.name << std::endl;
  return 0;
}

Value *LogErrorV(const char *Str) {
  // std::cout << Str << std::endl;
  return nullptr;
}

void InitializeModuleAndPassManager() {
  // Open a new module.
  theContext = std::make_unique<LLVMContext>();
  theModule = std::make_unique<Module>("test", *theContext);

  // theModule->setDataLayout(dL);

  // Create a new builder for the module.
  builder = std::make_unique<IRBuilder<>>(*theContext);

  // Create a new pass manager attached to it.
  theFPM = std::make_unique<legacy::FunctionPassManager>(theModule.get());

  // Promote allocas to registers.
  //theFPM->add(createPromoteMemoryToRegisterPass());
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  //theFPM->add(createInstructionCombiningPass());
  // Reassociate expressions.
  //theFPM->add(createReassociatePass());
  // Eliminate Common SubExpressions.
  //theFPM->add(createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  //theFPM->add(createCFGSimplificationPass());

  theFPM->doInitialization();
}

Function *getFunction(std::string Name) {
  // First, see if the function has already been added to the current module.
  if (auto *F = theModule->getFunction(Name))
    return F;
  return nullptr;
}

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block
/// of the function.  This is used for mutable variables etc.
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                          StringRef VarName, Type *varType) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                   TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(varType, nullptr, VarName);
}

int NInteger::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  printspaces();
  std::cout << "INT"
            << ": " << value << std::endl;
  spaces -= 2;
  return 0;
}
int NFloat::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  printspaces();
  std::cout << "FLOAT"
            << ": " << value << std::endl;
  spaces -= 2;
  return 0;
}
int NChar::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  printspaces();
  std::cout << "CHAR"
            << ": " << value << std::endl;
  spaces -= 2;
  return 0;
}
int NIdentifier::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "ID"
            << ": " << name << std::endl;
  spaces -= 2;
  return 0;
}
int NDotOperator::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  exp.parse();
  printspaces();
  std::cout << "DOT" << std::endl;
  parseNIdentifier(id);
  // id.parse();
  spaces -= 2;
  return 0;
}
int NListOperator::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  lhs.parse();
  printspaces();
  std::cout << "LB" << std::endl;
  rhs.parse();
  printspaces();
  std::cout << "RB" << std::endl;
  spaces -= 2;
  return 0;
}
int NArgs::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  exp.parse();
  if (nArgs) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nArgs->parse();
  }
  spaces -= 2;
  return 0;
}
int NMethodCall::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  parseNIdentifier(id);
  // id.parse();
  printspaces();
  std::cout << "LP" << std::endl;
  if (nargs) {
    nargs->parse();
  }
  printspaces();
  std::cout << "RP" << std::endl;
  spaces -= 2;
  return 0;
}
int NParenOperator::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "LP" << std::endl;
  printspaces();
  exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  spaces -= 2;
  return 0;
}
int NSingleOperator::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << name << std::endl;
  hs.parse();
  spaces -= 2;
  return 0;
}
int NBinaryOperator::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  lhs.parse();
  printspaces();
  if (name.substr(0, 5) == "RELOP")
    std::cout << "RELOP" << std::endl;
  else
    std::cout << name << std::endl;
  rhs.parse();
  spaces -= 2;
  return 0;
}
int NAssignment::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  lhs.parse();
  printspaces();
  std::cout << name << std::endl;
  rhs.parse();
  spaces -= 2;
  return 0;
}
int NSpecifier::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "TYPE: " << type << std::endl;
  spaces -= 2;
  return 0;
}
int NVarDec::parse() {
  printGrammerInfo(getNodeName(), line);

  if (v.size()) {
    spaces += 2;
    for (int i = 0; i < v.size(); ++i) {
      printGrammerInfo(getNodeName(), line);

      spaces += 2;
    }
    parseNIdentifier(Id);
    // Id.parse();
    spaces -= 2;
    for (int i = 0; i < v.size(); ++i) {
      printspaces();
      std::cout << "LB" << std::endl;
      printspaces();
      std::cout << "INT: " << v[i] << std::endl;
      printspaces();
      std::cout << "RB" << std::endl;
      spaces -= 2;
    }
  } else {
    spaces += 2;
    parseNIdentifier(Id);
    // Id.parse();
    spaces -= 2;
  }
  return 0;
}
int NParamDec::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nSpecifier.parse();
  varDec.parse();
  spaces -= 2;
  return 0;
}
int NVarList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nParamDec.parse();
  if (nVarList) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nVarList->parse();
  }
  spaces -= 2;
  return 0;
}
int NFunDec::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  parseNIdentifier(Id);
  // Id.parse();
  printspaces();
  std::cout << "LP" << std::endl;
  if (arguments)
    arguments->parse();
  printspaces();
  std::cout << "RP" << std::endl;
  spaces -= 2;
  return 0;
}
int NDec::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  vardec.parse();
  if (exp) {
    printspaces();
    std::cout << "ASSIGNOP" << std::endl;
    exp->parse();
  }
  spaces -= 2;
  return 0;
}
int NDecList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  dec.parse();
  if (nDecList) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nDecList->parse();
  }
  spaces -= 2;
  return 0;
}
int NDef::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nSpecifier.parse();
  if (nDecList)
    nDecList->parse();
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NDefList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nDef.parse();
  if (nDefList) {
    nDefList->parse();
  }
  spaces -= 2;
  return 0;
}
int NStructSpecifier::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printGrammerInfo("StructSpecifier", line);

  spaces += 2;
  printspaces();
  std::cout << "STRUCT" << std::endl;
  if (deflist) {
    if (tag) {
      printGrammerInfo("OptTag", line);
      spaces += 2;
      parseNIdentifier(*tag);
      spaces -= 2;
      printspaces();
      std::cout << "LC" << std::endl;
      deflist->parse();
      printspaces();
      std::cout << "RC" << std::endl;
    } else {
      deflist->parse();
    }
  } else if (tag) {
    printGrammerInfo("Tag", line);

    spaces += 2;
    parseNIdentifier(*tag);
    spaces -= 2;
  }
  spaces -= 2;
  spaces -= 2;
  return 0;
}
int NStmtList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nStmt.parse();
  if (nStmtList)
    nStmtList->parse();
  spaces -= 2;
  return 0;
}

int NCompSt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "LC" << std::endl;
  if (ndeflist)
    ndeflist->parse();
  if (nstmtlist)
    nstmtlist->parse();
  printspaces();
  std::cout << "RC" << std::endl;
  spaces -= 2;
  return 0;
}
int NExpStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  this->exp.parse();
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NCompStStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  compst.parse();
  spaces -= 2;
  return 0;
}
int NRetutnStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "RETURN" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NIfStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "IF" << std::endl;
  printspaces();
  std::cout << "LP" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  this->stmt.parse();
  spaces -= 2;
  return 0;
}
int NIfElseStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "IF" << std::endl;
  printspaces();
  std::cout << "LP" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  this->stmt.parse();
  printspaces();
  std::cout << "ELSE" << std::endl;
  this->stmt_else.parse();
  spaces -= 2;
  return 0;
}
int NWhileStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "WHILE" << std::endl;
  printspaces();
  std::cout << "LP" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  this->stmt.parse();
  spaces -= 2;
  return 0;
}
int NBreakStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "BREAK" << std::endl;
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NExtDecList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nVarDec.parse();
  if (nExtDecList) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nExtDecList->parse();
  }
  spaces -= 2;
  return 0;
}
int NExtDef::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  specifier.parse();
  if (fundec) {
    fundec->parse();
    if (compst) {
      compst->parse();
    }
  } else {
    if (nextdeclist) {
      nextdeclist->parse();
    }
    printspaces();
    std::cout << "SEMI" << std::endl;
  }

  spaces -= 2;
  return 0;
}
int NExtDefList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nExtDef.parse();
  if (nExtDefList)
    nExtDefList->parse();
  spaces -= 2;
  return 0;
}
int NProgram::parse() {
  printGrammerInfo("Program", line);
  spaces += 2;
  if (nextdeflist)
    nextdeflist->parse();
  spaces -= 2;
  return 0;
}

// codegen()
Value *Node::codegen() {
  assert(false); // Never use this function.
  // This is a list.
  return ConstantInt::get(*theContext, APInt(32, 0, true));
}
Value *NExpression::codegen() {
  return ConstantInt::get(*theContext, APInt(32, 0, true));
}
Value *NInteger::codegen() {
  return ConstantInt::get(*theContext, APInt(32, value, true));
}    
Value *NFloat::codegen() {
  // begin

  return ConstantFP::get(*theContext,APFloat(value));
  // end
}
Value *NChar::codegen() {
  // begin

  return ConstantInt::get(*theContext,APInt(8,value,false));
  // end
}
Value *NIdentifier::codegen() {
  // begin
  Var *v = search(name);
  if(v)
  {
    return builder->CreateLoad(v->alloc->getAllocatedType(),v->alloc); 
  }
  else
  {
  printSemanticError(1,line);
  exit(0);
  }
  // end
}
Value *NArgs::codegen() { return exp.codegen(); }
Value *NMethodCall::codegen() {
  // begin
  Function *func = theModule->getFunction(id.name);
  //函数在调用时未经定义
  if(!func) 
  {
      printSemanticError(2,line);
      exit(0);
  }
  std::vector<Value *> argsV;
  NArgs *arg = nargs;
  for(auto &Farg : func->args()) 
  {
      //参数数目不足
      if(!arg) 
      {
        printSemanticError(8,line);
        exit(0);
      }
      Type *t = Farg.getType();
      Value *v = arg->codegen();
      //参数类型不匹配
      if (t != v->getType()) 
      {
        printSemanticError(8,line);
        exit(0);
      }
      //压入参数
      argsV.push_back(v);
      //继续向后取参检测
      arg = arg->nArgs;
   }
   if(arg) 
   {
      printSemanticError(8,line);
      exit(0);
   } 
  return builder->CreateCall(func,argsV);
  // end
}
Value *NParenOperator::codegen() { return exp.codegen(); }
Value *NSingleOperator::codegen() {
  // begin
  Value * v = hs.codegen();
  if(op==282)
    return builder->CreateNot(v);
  else if(op==275)
    return builder->CreateNeg(v);
  else
    return nullptr;
  // end
}
Value *NBinaryOperator::codegen() {
  // begin
  Value *l,*r;
  l = lhs.codegen();
  r = rhs.codegen();
  std::string relop;
  if(op == 260)
    relop = name.substr(5,2);
  switch (op)
  {
  case 260:     
    if(relop == "<=") 
    {
        return builder->CreateICmpULE(l,r);
    } 
    else if(relop == "==") 
    {
        return builder->CreateICmpEQ(l,r);
    } 
    else if(relop == "!=") 
    {
        return builder->CreateICmpNE(l,r);
    } 
    else if(relop == ">") 
    { 
        return builder->CreateICmpSGT(l,r);
    } 
    else if(relop == "<") 
    {
        return builder->CreateICmpSLT(l,r);
    } 
    else if(relop == ">=") 
    {
        return builder->CreateICmpSGE(l,r);
    }
  case 280:  
      return builder->CreateAnd(l,r);
  case 281:  
      return builder->CreateOr(l,r);
  case 274:
      return builder->CreateAdd(l,r);
  case 275:
      return builder->CreateSub(l,r);
  case 276:
      return builder->CreateMul(l,r);
  case 277:
      return builder->CreateSDiv(l,r);
  default:
    break;
  }
  return nullptr;
  // end
}
Value *NAssignment::codegen() {
  // Assignment requires the LHS to be an identifier.
  // begin
  Var *v;
  Value *rv;
  rv = rhs.codegen(); 
  if (lhs.name == "") 
  {
    //赋值号左边出现一个只有右值的表达式
    printSemanticError(6,line); 
    exit(0);
  } 
  else 
  {
    v = search(lhs.name);
    if(!v) 
    {
      printSemanticError(1,line);  
      exit(0);
    }
    if(v->alloc->getAllocatedType() != rv->getType()) 
    {
      //赋值号两边的表达式类型不匹配
      printSemanticError(5,line);
      exit(0);
    }
  }
  return builder->CreateStore(rv,v->alloc);
  // end
}
Value *NSpecifier::codegen() {
  // begin
   if(type=="int")
   return ConstantInt::get(*theContext,APInt(32,0,true));
   if(type=="float")
   return ConstantFP::get(*theContext,APFloat(0.0));
   if(type=="char")
   return ConstantInt::get(*theContext,APInt(8,0,false));
  assert(false);
   
  return ConstantInt::get(*theContext,APInt(32,0,true));
  // end
}
Type *NSpecifier::getType() {
  if (type == "int")
    return Type::getInt32Ty(*theContext);
  if (type == "float")
    return Type::getFloatTy(*theContext);
  if (type == "char")
    return Type::getInt8Ty(*theContext);
  assert(false);
  return Type::getInt32Ty(*theContext);
}
Value *NVarDec::codegen() {
  // begin

  return nullptr;
  // end
}
Value *NParamDec::codegen() {
  // begin

  return nullptr;
  // end
}

std::pair<std::string, Type *> NParamDec::getType() {
  assert(varDec.v.size() == 0);
  std::pair<std::string, Type *> tmp(varDec.Id.name, nSpecifier.getType());
  return tmp;
}
Value *NVarList::codegen() {
  assert(false); // Never use this function.
  // This is a list.
  return ConstantInt::get(*theContext, APInt(32, 0, true));
}
Function *NFunDec::funcodegen(Type *retType) {
  // check if it exists the same name of fun
  if (theModule->getFunction(Id.name)) {
    printSemanticError(4, line, "Redefined " + Id.name);
    return nullptr;
  }

  std::vector<Type *> argsTypes;
  std::vector<std::string> argNames;
  for (NVarList *item = arguments; item; item = item->nVarList) {
    auto tmp = item->nParamDec.getType();
    argNames.push_back(tmp.first);
    argsTypes.push_back(tmp.second);
  }

  FunctionType *ft = FunctionType::get(retType, argsTypes, false);
  Function *f =
      Function::Create(ft, Function::ExternalLinkage, Id.name, theModule.get());
  unsigned idx = 0;
  for (auto &arg : f->args()) {
    arg.setName(argNames[idx++]);
  }
  return f;
}
Value *NDef::codegen() {
  // begin
  Type *s = nSpecifier.getType();
  if(nDecList != nullptr) 
  {
    NDecList *d = nDecList;
    for(;d != nullptr;d = d->nDecList) 
    {
      if(searchInComp(d->dec.vardec.Id.name) != nullptr) 
      {
          printSemanticError(3,line);
          exit(0);
      }
      AllocaInst *alloca_tmp = builder->CreateAlloca(s,nullptr,d->dec.vardec.Id.name );

      if(d->dec.exp != nullptr) 
      {
          Value *v = d->dec.exp->codegen();
          builder->CreateStore(v,alloca_tmp);
      } 
      Var *var = new Var;
      var->vName = d->dec.vardec.Id.name;
      var->alloc = alloca_tmp;
      vars.push_front(var);
    } 
  } 
  return nullptr;
  // end
}
Value *NDefList::codegen() {
  // begin
  nDef.codegen();
  if(nDefList != nullptr) 
     nDefList->codegen();
  return nullptr;
  // end
}
Value *NStmtList::codegen() {
  auto *retVal = nStmt.codegen();
  if (nStmtList)
    retVal = nStmtList->codegen();
  return retVal;
}
Value *NCompSt::codegen() {
  // 自行处理变量作用域的问题
  Var *var = new Var;
  if(var == nullptr) {
      printf("malloc error\n");
      exit(0);
  }
  var->alloc = nullptr;
  vars.push_front(var);
  Value *retVal = nullptr;
  if (ndeflist)
    retVal = ndeflist->codegen();
  if (nstmtlist)
    retVal = nstmtlist->codegen();
    for(std::list<Var*>::iterator it = vars.begin();it != vars.end();) {
    Var *var_tmp = *it;
    std::string tname = var_tmp->vName;
    it++;
    vars.pop_front();
    if(tname == "") {
      break;
    }
    // free(var_tmp);
    delete var_tmp;
  }
  return retVal;
}
//返回nullptr，而不是exp.codegen()
Value *NExpStmt::codegen() { exp.codegen(); return nullptr;}
Value *NCompStStmt::codegen() {
  // begin

  return compst.codegen();
  // end
}
Value *NRetutnStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  BasicBlock *bb = BasicBlock::Create(*theContext, "ret", theFun);
  builder->CreateBr(bb);
  builder->SetInsertPoint(bb);
  auto *retVal = exp.codegen();
  // check the return type and fundec type
  // begin
  Type *t = theFun->getReturnType();
  if(t != retVal->getType()) 
  {
    printSemanticError(7,line);
    exit(0);
  }
  // end
    builder->CreateRet(retVal);
  return retVal;
}
Value *NIfStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  // begin
  BasicBlock *thenb = BasicBlock::Create(*theContext,"ifthen",theFun); 
  BasicBlock *ifnotb = BasicBlock::Create(*theContext,"ifnot"); 
  builder->CreateCondBr(exp.codegen(),thenb,ifnotb);
  builder->SetInsertPoint(thenb);
  if(stmt.codegen() == nullptr) 
  {
    builder->CreateBr(ifnotb);
  } 
  theFun->getBasicBlockList().push_back(ifnotb);
  builder->SetInsertPoint(ifnotb);
  
  return nullptr;
  // end
}
Value *NIfElseStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  // begin
  BasicBlock *thenb = BasicBlock::Create(*theContext, "ifthen", theFun); 
  BasicBlock *ifnotb = BasicBlock::Create(*theContext,"ifnot"); 
  BasicBlock *after = BasicBlock::Create(*theContext,"ifafter"); 
  builder->CreateCondBr(exp.codegen(),thenb,ifnotb);
  builder->SetInsertPoint(thenb);
  if(stmt.codegen() == nullptr) 
  {
    builder->CreateBr(after);
  }
  theFun->getBasicBlockList().push_back(ifnotb);
  builder->SetInsertPoint(ifnotb);
  if(stmt_else.codegen() == nullptr) 
  {
    builder->CreateBr(after);
  }
  theFun->getBasicBlockList().push_back(after);
  builder->SetInsertPoint(after);
  return nullptr;
  // end
}
Value *NWhileStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  BasicBlock *condb = BasicBlock::Create(*theContext, "cond", theFun);
  // begin
  builder->CreateBr(condb);
  BasicBlock *rundb = BasicBlock::Create(*theContext, "zmy");
  BasicBlock *after = BasicBlock::Create(*theContext, "zmy2");
  builder->SetInsertPoint(condb);
  builder->CreateCondBr(exp.codegen(),rundb,after);
  theFun->getBasicBlockList().push_back(rundb);
  builder->SetInsertPoint(rundb);  
  if(stmt.codegen() == nullptr) 
  {
     builder->CreateBr(condb);
  }
  theFun->getBasicBlockList().push_back(after);
  builder->SetInsertPoint(after);  
  return nullptr;
  // end
}
Value *NBreakStmt::codegen() {
  // begin

  return nullptr;
  // end
}
Value *NExtDefVarDec::codegen() {
  // begin

  return nullptr;
  // end
}
Value *NExtDefFunDec::codegen() {
  Type *retType = specifier.getType();

  Function *f = fundec->funcodegen(retType);
  if (!f) {
    return nullptr;
  }
  assert(compst != nullptr); // Assert compst is not null.
  BasicBlock *bb = BasicBlock::Create(*theContext, "entry", f);
  builder->SetInsertPoint(bb);
  namedValues.clear();
    curNamedValues.clear();
  for(std::list<Var*>::iterator it = vars.begin();it != vars.end();) {
    Var *var_tmp = *it;
    std::string tname = var_tmp->vName;
    it++;
    vars.pop_front();

    delete var_tmp;
    if(tname == "") {
      break;
    }
  }
  for (auto &arg : f->args()) {
    // Create an alloca for this variable.
    AllocaInst *alloca =
        CreateEntryBlockAlloca(f, arg.getName(), arg.getType());

    if (curNamedValues[std::string(arg.getName())]) {
      printSemanticError(3, line, "Redefined " + arg.getName().str());
      return LogErrorV("Unknown function referenced");
    }
    // Store the initial value into the alloca.
    builder->CreateStore(&arg, alloca);
    // Add arguments to variable symbol table.
    namedValues[std::string(arg.getName())] = alloca;
    curNamedValues[std::string(arg.getName())] = alloca;
    
    Var *var = new Var;
    var->vName = std::string(arg.getName());
    var->alloc = alloca;
    vars.push_front(var);
  }
  if (Value *retVal = compst->codegen()) {
    // Finish off the function.

    // Validate the generated code, checking for consistency.
    verifyFunction(*f);

    // Run the optimizer on the function.
    // theFPM->run(*f);
    return f;
  }
  // Error reading body, remove function.
  f->eraseFromParent();

  return nullptr;
}
Value *NExtDefList::codegen() {
  auto *lastCode = nExtDef.codegen();
  // lastCode->print(errs());
  // assert(nExtDefList == nullptr);
  if (nExtDefList)
    lastCode = nExtDefList->codegen();
  return lastCode;
}
Value *NProgram::codegen() {

  //默认输出函数putchar
  std::vector<Type *> putArgs;
  putArgs.push_back(Type::getInt32Ty(*theContext));

  FunctionType *putType =
      FunctionType::get(builder->getInt32Ty(), putArgs, false);
  Function *putFunc = Function::Create(putType, Function::ExternalLinkage,
                                       "putchar", theModule.get());

  //默认输入函数getchar
  std::vector<Type *> getArgs;
  // getArgs.push_back(Type::getInt32Ty(*theContext));

  FunctionType *getType =
      FunctionType::get(builder->getInt32Ty(), getArgs, false);
  Function *getFunc = Function::Create(getType, Function::ExternalLinkage,
                                       "getchar", theModule.get());

  Value *lastCode = nextdeflist->codegen();
  if (grammererror)
    return nullptr;
  return lastCode;
}

