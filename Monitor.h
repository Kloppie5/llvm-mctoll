
// Monitor.h is used purely for the development of the mctoll tool.
// Usage in production code is not allowed to have side effects
// and usually indicates the code in question is not entirely correct.

#ifndef LLVM_TOOLS_LLVM_MCTOLL_MONITOR_H_
#define LLVM_TOOLS_LLVM_MCTOLL_MONITOR_H_

#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;

class Monitor {
    private:
        Monitor(raw_ostream &OS) : OS(OS) {}

        const MCInstrInfo *MCII;
        const MCRegisterInfo *MCRI;

        raw_ostream &OS;
        std::vector<const char*> EventStack;

    public:
        static Monitor& getInstance() {
            static Monitor instance(WithColor(dbgs(), HighlightColor::Remark));
            return instance;
        }
        Monitor ( Monitor const& ) = delete;
        void operator= ( Monitor const& ) = delete;

        static void registerMCInstrInfo ( const MCInstrInfo *MCII ) { getInstance().MCII = MCII; }
        static void registerMCRegisterInfo ( const MCRegisterInfo *MCRI ) { getInstance().MCRI = MCRI; }

        static void printMCInst ( const MCInst* Inst, bool linebreak = true, raw_ostream &OS = getInstance().OS) {
            OS << getInstance().MCII->getName(Inst->getOpcode());
            OS << " (" << Inst->getOpcode() << ") {";
            for (unsigned i = 0, e = Inst->getNumOperands(); i != e; ++i) {
                OS << " ";
                const MCOperand& OP = Inst->getOperand(i);
                if (OP.isReg()) {
                    if (OP.getReg() < getInstance().MCRI->getNumRegs())
                        OS << "Reg:" << getInstance().MCRI->getName(OP.getReg());
                    else
                        OS << "InvalidReg:" << OP.getReg();
                }
                else if (OP.isImm())
                    OS << "Imm:" << OP.getImm();
                else if (OP.isInst())
                    OS << "Inst:" << *OP.getInst();
                else {
                    OS << "{";
                    OP.print(OS, getInstance().MCRI);
                    OS << "}";
                }
            }
            OS << " }";
            if (linebreak) OS << "\n";
        }
        static void printMachineInstr ( const MachineInstr* MI, bool linebreak = true, raw_ostream &OS = getInstance().OS) {
            const TargetSubtargetInfo& STI = MI->getMF()->getSubtarget();
            const TargetRegisterInfo* TRI = STI.getRegisterInfo();
            const TargetInstrInfo* TII = STI.getInstrInfo();
            const MachineRegisterInfo* MRI = &MI->getMF()->getRegInfo();

            OS << TII->getName(MI->getOpcode());
            OS << " (" << MI->getOpcode() << ") {";

            const MCInstrDesc &MCID = MI->getDesc();
            for (unsigned i = 0, e = MCID.getNumOperands(); i != e; ++i) {
                // MCID.getNumOperands() is used instead of MI->getNumOperands()
                // to force the supplied MachineInstr to be valid and skip
                // any additional operands that may be added to the end of
                // the MachineInstr.
                OS << " ";
                const MachineOperand& MO = MI->getOperand(i);
                switch(MO.getType()) {
                    case MachineOperand::MO_Register: {
                        Register Reg = MO.getReg();
                        if (Register::isStackSlot(Reg))
                            OS << "SReg:SS#" << Register::stackSlot2Index(Reg);
                        else if (Register::isVirtualRegister(Reg)) {
                            StringRef Name = MRI->getVRegName(Reg);
                            if (Name != "")
                                OS << "VReg:%" << Name;
                            else
                                OS << "VReg:%" << Register::virtReg2Index(Reg);
                        } else if (Reg < TRI->getNumRegs()) {
                            OS << "Reg:$" << TRI->getName(Reg);
                        } else
                            OS << "InvalidReg:" << Reg;
                    } break;
                    case MachineOperand::MO_Immediate:
                        OS << "Imm:" << MO.getImm();
                        break;
                    case MachineOperand::MO_MachineBasicBlock:
                        OS << "MBB:" << MO.getMBB()->getName();
                        break;
                    case MachineOperand::MO_FrameIndex:
                        OS << "FI:" << MO.getIndex();
                        break;
                    case MachineOperand::MO_ConstantPoolIndex:
                        OS << "CPI:" << MO.getIndex();
                        break;
                    case MachineOperand::MO_JumpTableIndex:
                        OS << "JTI:" << MO.getIndex();
                        break;
                    case MachineOperand::MO_GlobalAddress:
                        OS << "GA:" << MO.getGlobal()->getName();
                        break;
                    case MachineOperand::MO_ExternalSymbol:
                        OS << "ES:" << MO.getSymbolName();
                        break;
                    case MachineOperand::MO_BlockAddress:
                        OS << "BA:" << MO.getBlockAddress();
                        break;
                    case MachineOperand::MO_Metadata:
                        OS << "MD:" << MO.getMetadata();
                        break;
                    case MachineOperand::MO_RegisterMask:
                        OS << "Mask:";
                        for (const unsigned *Mask = MO.getRegMask(); *Mask; ++Mask)
                            OS << " " << TRI->getName(*Mask);
                        break;
                    case MachineOperand::MO_MCSymbol:
                        OS << "Sym:" << MO.getMCSymbol();
                        break;
                    default:
                        OS << "{";
                        MO.print(OS, TRI);
                        OS << "}";
                        break;
                }
            }
            OS << " }";
            if (linebreak) OS << "\n";
        }
        static void printInstruction ( const Instruction* Instr, bool linebreak = true, raw_ostream &OS = getInstance().OS ) {
            if (Instr == nullptr) {
                OS << "nullptr";
                if (linebreak) OS << "\n";
                return;
            }
            OS << Instr->getOpcodeName();
            OS << " (" << Instr->getOpcode() << ") {";
            for (unsigned i = 0, e = Instr->getNumOperands(); i != e; ++i) {
                OS << " ";
                const Value* V = Instr->getOperand(i);
                if (const ConstantInt* CI = dyn_cast<ConstantInt>(V)) {
                    OS << "Imm:" << CI->getSExtValue();
                }
                else if (const ConstantFP* CF = dyn_cast<ConstantFP>(V)) {
                    OS << "Imm:" << CF->getValueAPF().convertToDouble();
                }
                else if (const ConstantExpr* CE = dyn_cast<ConstantExpr>(V)) {
                    OS << "Imm:" << CE->getOpcodeName();
                }
                else if (const GlobalValue* GV = dyn_cast<GlobalValue>(V)) {
                    OS << "GV:" << GV->getName();
                }
                else if (const Instruction* I = dyn_cast<Instruction>(V)) {
                    OS << "Inst:" << I->getOpcodeName();
                }
                else {
                    OS << "{";
                    V->printAsOperand(OS, false);
                    OS << "}";
                }
            }
            OS << " }";
            if (linebreak) OS << "\n";
        }

        static void event_start ( const char* name, raw_ostream &OS = getInstance().OS ) {
            getInstance().EventStack.push_back(name);
            OS << name << " {\n";
        }
        static void event_stateswitch ( raw_ostream &OS = getInstance().OS ) {
            OS << "} => {\n";
        }
        static void event_end ( const char* name, raw_ostream &OS = getInstance().OS ) {
            if (getInstance().EventStack.back() != name) {
                OS << "EventStack mismatch: " << getInstance().EventStack.back() << " != " << name << "\n";
                return;
            }
            OS << "}\n";
            getInstance().EventStack.pop_back();
        }
        static raw_ostream& event_raw ( raw_ostream &OS = getInstance().OS ) {
            OS << "  ";
            return OS;
        }
        static void event_Bytes ( const ArrayRef<uint8_t> Bytes, raw_ostream &OS = getInstance().OS ) {
            OS << "  ";
            dumpBytes(Bytes, OS);
        }
        static void event_MCInst ( const MCInst *Inst, raw_ostream &OS = getInstance().OS ) {
            OS << "  ";
            printMCInst(Inst, true, OS);
        }
        static void event_MachineInstr ( const MachineInstr* MI, raw_ostream &OS = getInstance().OS ) {
            OS << "  ";
            printMachineInstr(MI, true, OS);
        }
        static void event_Instruction ( const Instruction* Instr, raw_ostream &OS = getInstance().OS ) {
            OS << "  ";
            printInstruction(Instr, true, OS);
        }

        static void event_ParsedMCInst ( const ArrayRef<uint8_t> Bytes, const MCInst* Inst, raw_ostream &OS = getInstance().OS ) {
            event_start("Parsed", OS);
            event_Bytes(Bytes, OS);
            event_stateswitch(OS);
            event_MCInst(Inst, OS);
            event_end("Parsed", OS);
        }
        static void event_RaisedMachineInstr ( const MCInst* Inst, const MachineInstr* MI, raw_ostream &OS = getInstance().OS ) {
            event_start("Raised", OS);
            event_MCInst(Inst, OS);
            event_stateswitch(OS);
            event_MachineInstr(MI, OS);
            event_end("Raised", OS);
        }
        static void event_MachineInstrsToMachineInstrs ( const char* prefix, std::vector<MachineInstr *> OldMIs, std::vector<MachineInstr *> NewMIs, raw_ostream &OS = getInstance().OS ) {
            event_start(prefix, OS);
            for (auto MI : OldMIs)
                event_MachineInstr(MI, OS);
            event_stateswitch(OS);
            for (auto MI : NewMIs)
                event_MachineInstr(MI, OS);
            event_end(prefix, OS);
        }
        static void event_RaisedInstruction ( const MachineInstr* MI, std::vector<Instruction* > Instrs, raw_ostream &OS = getInstance().OS ) {
            event_start("Raised", OS);
            event_MachineInstr(MI, OS);
            event_stateswitch(OS);
            for (auto I : Instrs)
                event_Instruction(I, OS);
            event_end("Raised", OS);
        }

        static void TODO ( const char* Message, bool linebreak = true, raw_ostream& OS = WithColor(dbgs(), HighlightColor::Warning) ) {
            OS << "TODO: " << Message;
            if (linebreak) OS << "\n";
        }
        static void NOTE ( const char* Message, bool linebreak = true, raw_ostream& OS = WithColor(dbgs(), HighlightColor::Warning) ) {
            OS << "NOTE: " << Message;
            if (linebreak) OS << "\n";
        }
        static void ERROR ( const char* Message, bool linebreak = true, raw_ostream& OS = WithColor(errs(), HighlightColor::Error) ) {
            OS << "ERROR: " << Message;
            if (linebreak) OS << "\n";
        }
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_MONITOR_H_
