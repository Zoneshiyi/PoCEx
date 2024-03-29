#include <iostream>

#include <string>
#include <vector>

#include <algorithm>
#include <assert.h>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"

#include <fstream>

using namespace llvm;

std::unique_ptr<LLVMContext> theContext;
std::unique_ptr<Module> theModule;
std::unique_ptr<IRBuilder<>> builder;
std::map<std::string, AllocaInst *> namedValues;
std::unique_ptr<legacy::FunctionPassManager> theFPM;

std::map<std::string, AllocaInst *> curNamedValues;

BasicBlock *continueBasicBlock = nullptr;

void InitializeModuleAndPassManager()
{
  // Open a new module.
  theContext = std::make_unique<LLVMContext>();
  theModule = std::make_unique<Module>("test", *theContext);

  // theModule->setDataLayout(dL);

  // Create a new builder for the module.
  builder = std::make_unique<IRBuilder<>>(*theContext);

  // Create a new pass manager attached to it.
  theFPM = std::make_unique<legacy::FunctionPassManager>(theModule.get());

  // Promote allocas to registers.
  // theFPM->add(createPromoteMemoryToRegisterPass());
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  // theFPM->add(createInstructionCombiningPass());
  // Reassociate expressions.
  // theFPM->add(createReassociatePass());
  // Eliminate Common SubExpressions.
  // theFPM->add(createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  // theFPM->add(createCFGSimplificationPass());

  theFPM->doInitialization();
}

int main(int argc, char *argv[])
{
  // Init
  InitializeModuleAndPassManager();

  //默认输出函数putchar
  std::vector<Type *> putArgs;
  putArgs.push_back(Type::getInt32Ty(*theContext));

  FunctionType *putType =
      FunctionType::get(builder->getInt32Ty(), putArgs, false);
  Function *putFunc = Function::Create(putType, Function::ExternalLinkage,
                                       "putchar", theModule.get());
  //默认输入函数getchar
  std::vector<Type *> getArgs;

  FunctionType *getType =
      FunctionType::get(builder->getInt32Ty(), getArgs, false);
  Function *getFunc = Function::Create(getType, Function::ExternalLinkage,
                                       "getchar", theModule.get());
  //根据输入的单字符，判断，如果是'a'，则输出'Y'，否则输出'N'。
  //设置返回类型
  // begin

  Value *load_a1 = builder->CreateCall(calleeFg);

  Value *compare_a_97 = builder->CreateICmpEQ(load_a1, const_1, "comp");
  // //判断结果真假
  Value *condVal = builder->CreateICmpNE(
      compare_a_97, Constant::getNullValue(compare_a_97->getType()), "cond");

  //创建条件为真和假应跳转的两个基本块
  BasicBlock *thenb = BasicBlock::Create(*theContext, "then", f);
  BasicBlock *ifcontb = BasicBlock::Create(*theContext, "ifcont");
  BasicBlock *outif = BasicBlock::Create(*theContext, "outif");

  //根据condVal值跳转 真为thenb 否则为ifcontb
  builder->CreateCondBr(condVal, thenb, ifcontb);
  //进入thenb基本块
  builder->SetInsertPoint(thenb);
  Value *const_2 = ConstantInt::get(*theContext, APInt(32, 'Y', true));
  builder->CreateCall(calleeFp, const_2, "callputchar");
  builder->CreateBr(outif);

  //将创建的ifcontb 基本块 插入
  f->getBasicBlockList().push_back(ifcontb);
  //进入 infcontb
  builder->SetInsertPoint(ifcontb);
  Value *const_3 = ConstantInt::get(*theContext, APInt(32, 'N', true));
  builder->CreateCall(calleeFp, const_3, "callputchar");
  builder->CreateBr(outif);

  //将创建的outif 基本块 插入
  f->getBasicBlockList().push_back(outif);
  builder->SetInsertPoint(outif);

  // end
  //设置返回值
  builder->CreateRet(const_0);
  verifyFunction(*f);
  // Run the optimizer on the function.
  // theFPM->run(*f);
  //输出
  theModule->print(outs(), nullptr);

  return 0;
}