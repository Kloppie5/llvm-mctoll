
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
    public:
        static Monitor& getInstance() {
            static Monitor instance;
            return instance;
        }

        static void registerMCInstrInfo ( const MCInstrInfo *MCII ) { getInstance().MCII = MCII; }
        static void registerMCRegisterInfo ( const MCRegisterInfo *MCRI ) { getInstance().MCRI = MCRI; }

        static void printMCInst ( const MCInst* Inst, bool linebreak = true, raw_ostream& OS = WithColor(dbgs(), HighlightColor::Remark) ) {
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
                else if (OP.isImm()) OS << "Imm:" << OP.getImm();
                else if (OP.isInst()) OS << "Inst:" << *OP.getInst();
                else { OS << "{"; OP.print(OS, getInstance().MCRI); OS << "}"; }
            }
            OS << " }";
            if (linebreak) OS << "\n";
        }
        static void printMachineInstr ( const MachineInstr* MI, bool linebreak = true, raw_ostream& OS = WithColor(dbgs(), HighlightColor::Remark) ) {
            const TargetSubtargetInfo& STI = MI->getMF()->getSubtarget();
            const TargetRegisterInfo* TRI = STI.getRegisterInfo();
            const TargetInstrInfo* TII = STI.getInstrInfo();

            OS << TII->getName(MI->getOpcode());
            OS << " (" << MI->getOpcode() << ") {";
            for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
                OS << " ";
                const MachineOperand& MO = MI->getOperand(i);
                switch(MO.getType()) {
                    case MachineOperand::MO_Register:
                        if (MO.getReg() < TRI->getNumRegs())
                            OS << "Reg:" << TRI->getName(MO.getReg());
                        else
                            OS << "InvalidReg:" << MO.getReg();
                        break;
                    case MachineOperand::MO_Immediate:
                        OS << "Imm:" << MO.getImm();
                        break;
                    case MachineOperand::MO_MachineBasicBlock:
                        OS << "MBB:" << MO.getMBB()->getName();
                        break;
                    case MachineOperand::MO_FrameIndex:
                        OS << "FI:" << MO.getIndex();
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
        static void printInstruction ( const Instruction* Instr, bool linebreak = true, raw_ostream& OS = WithColor(dbgs(), HighlightColor::Remark) ) {
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
        static void printSDNode ( const SDNode* N, bool linebreak = true, raw_ostream& OS = WithColor(dbgs(), HighlightColor::Remark) ) {
            if (N->isMachineOpcode()) {
                OS << "(" << getInstance().MCII->getName(N->getMachineOpcode()) << ")\n";
            } else {
                OS << "SDNode:";
                N->dump();
            }
            if (linebreak) OS << "\n";
        }

        static void event_ParsedMCInst ( const ArrayRef<uint8_t> Bytes, const MCInst* Inst, bool linebreak = true, raw_ostream& OS = WithColor(dbgs(), HighlightColor::Remark) ) {
            OS << "Parsed [ ";
                dumpBytes(Bytes, OS);
            OS << " ] to [ ";
                Monitor::printMCInst(Inst, false, OS);
            OS << " ]";
            if (linebreak) OS << "\n";
        }
        static void event_RaisedMachineInstr ( const MCInst* Inst, const MachineInstr* MI, bool linebreak = true, raw_ostream& OS = WithColor(dbgs(), HighlightColor::Remark) ) {
            OS << "Raised [ ";
                Monitor::printMCInst(Inst, false);
            OS << " ] to [ ";
                Monitor::printMachineInstr(MI, false);
            OS << " ]";
            if (linebreak) OS << "\n";
        }
        static void event_RaisedInstruction ( const MachineInstr* MI, const Instruction* Instr, bool linebreak = true, raw_ostream& OS = WithColor(dbgs(), HighlightColor::Remark) ) {
            OS << "Raised [ ";
                Monitor::printMachineInstr(MI, false);
            OS << " ] to [ ";
                Monitor::printInstruction(Instr, false);
            OS << " ]";
            if (linebreak) OS << "\n";
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

    private:
        Monitor() {}

        const MCInstrInfo *MCII;
        const MCRegisterInfo *MCRI;
    public:
        Monitor ( Monitor const& ) = delete;
        void operator= ( Monitor const& ) = delete;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_MONITOR_H_
