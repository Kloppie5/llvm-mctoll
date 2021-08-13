//===- IREmitter.cpp - Binary raiser utility llvm-mctoll ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of IREmitter class or use by
// llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "IREmitter.h"
#include "ARMModuleRaiser.h"
#include "SelectionCommon.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"

using namespace llvm;

// TODO: Move helper function
string getARMISDName(ARMISD::NodeType Opcode) {
  switch (Opcode) {
  default:
  case ARMISD::FIRST_NUMBER: // 350
    return "UNKNOWN";
  case ARMISD::Wrapper: // 351
    return "ARMISD::Wrapper";
  case ARMISD::WrapperPIC: // 352
    return "ARMISD::WrapperPIC";
  case ARMISD::WrapperJT: // 353
    return "ARMISD::WrapperJT";
  case ARMISD::COPY_STRUCT_BYVAL: // 354
    return "ARMISD::COPY_STRUCT_BYVAL";
  case ARMISD::CALL: // 355
    return "ARMISD::CALL";
  case ARMISD::CALL_PRED: // 356
    return "ARMISD::CALL_PRED";
  case ARMISD::CALL_NOLINK: // 357
    return "ARMISD::CALL_NOLINK";
  case ARMISD::tSECALL: // 358
    return "ARMISD::tSECALL";
  case ARMISD::BRCOND: // 359
    return "ARMISD::BRCOND";
  case ARMISD::BR_JT: // 360
    return "ARMISD::BR_JT";
  case ARMISD::BR2_JT: // 361
    return "ARMISD::BR2_JT";
  case ARMISD::RET_FLAG: // 362
    return "ARMISD::RET_FLAG";
  case ARMISD::SERET_FLAG: // 363
    return "ARMISD::SERET_FLAG";
  case ARMISD::INTRET_FLAG: // 364
    return "ARMISD::INTRET_FLAG";
  case ARMISD::PIC_ADD: // 365
    return "ARMISD::PIC_ADD";
  case ARMISD::ASRL: // 366
    return "ARMISD::ASRL";
  case ARMISD::LSRL: // 367
    return "ARMISD::LSRL";
  case ARMISD::LSLL: // 368
    return "ARMISD::LSLL";
  case ARMISD::CMP: // 369
    return "ARMISD::CMP";
  case ARMISD::CMN: // 370
    return "ARMISD::CMN";
  case ARMISD::CMPZ: // 371
    return "ARMISD::CMPZ";
  case ARMISD::CMPFP: // 372
    return "ARMISD::CMPFP";
  case ARMISD::CMPFPE: // 373
    return "ARMISD::CMPFPE";
  case ARMISD::CMPFPw0: // 374
    return "ARMISD::CMPFPw0";
  case ARMISD::CMPFPEw0: // 375
    return "ARMISD::CMPFPEw0";
  case ARMISD::FMSTAT: // 376
    return "ARMISD::FMSTAT";
  case ARMISD::CMOV: // 377
    return "ARMISD::CMOV";
  case ARMISD::SUBS: // 378
    return "ARMISD::SUBS";
  case ARMISD::SSAT: // 379
    return "ARMISD::SSAT";
  case ARMISD::USAT: // 380
    return "ARMISD::USAT";
  case ARMISD::BCC_i64: // 381
    return "ARMISD::BCC_i64";
  case ARMISD::SRL_FLAG: // 382
    return "ARMISD::SRL_FLAG";
  case ARMISD::SRA_FLAG: // 383
    return "ARMISD::SRA_FLAG";
  case ARMISD::RRX: // 384
    return "ARMISD::RRX";
  case ARMISD::ADDC: // 385
    return "ARMISD::ADDC";
  case ARMISD::ADDE: // 386
    return "ARMISD::ADDE";
  case ARMISD::SUBC: // 387
    return "ARMISD::SUBC";
  case ARMISD::SUBE: // 388
    return "ARMISD::SUBE";
  case ARMISD::LSLS: // 389
    return "ARMISD::LSLS";
  case ARMISD::VMOVRRD: // 390
    return "ARMISD::VMOVRRD";
  case ARMISD::VMOVDRR: // 391
    return "ARMISD::VMOVDRR";
  case ARMISD::VMOVSR: // 392
    return "ARMISD::VMOVSR";
  case ARMISD::EH_SJLJ_SETJMP: // 393
    return "ARMISD::EH_SJLJ_SETJMP";
  case ARMISD::EH_SJLJ_LONGJMP: // 394
    return "ARMISD::EH_SJLJ_LONGJMP";
  case ARMISD::EH_SJLJ_SETUP_DISPATCH: // 395
    return "ARMISD::EH_SJLJ_SETUP_DISPATCH";
  case ARMISD::TC_RETURN: // 396
    return "ARMISD::TC_RETURN";
  case ARMISD::THREAD_POINTER: // 397
    return "ARMISD::THREAD_POINTER";
  case ARMISD::DYN_ALLOC: // 398
    return "ARMISD::DYN_ALLOC";
  case ARMISD::MEMBARRIER_MCR: // 399
    return "ARMISD::MEMBARRIER_MCR";
  case ARMISD::PRELOAD: // 400
    return "ARMISD::PRELOAD";
  case ARMISD::WIN__CHKSTK: // 401
    return "ARMISD::WIN__CHKSTK";
  case ARMISD::WIN__DBZCHK: // 402
    return "ARMISD::WIN__DBZCHK";
  case ARMISD::WLS: // 403
    return "ARMISD::WLS";
  case ARMISD::WLSSETUP: // 404
    return "ARMISD::WLSSETUP";
  case ARMISD::LOOP_DEC: // 405
    return "ARMISD::LOOP_DEC";
  case ARMISD::LE: // 406
    return "ARMISD::LE";
  case ARMISD::PREDICATE_CAST: // 407
    return "ARMISD::PREDICATE_CAST";
  case ARMISD::VECTOR_REG_CAST: // 408
    return "ARMISD::VECTOR_REG_CAST";
  case ARMISD::MVETRUNC: // 409
    return "ARMISD::MVETRUNC";
  case ARMISD::VCMP: // 410
    return "ARMISD::VCMP";
  case ARMISD::VCMPZ: // 411
    return "ARMISD::VCMPZ";
  case ARMISD::VTST: // 412
    return "ARMISD::VTST";
  case ARMISD::VSHLs: // 413
    return "ARMISD::VSHLs";
  case ARMISD::VSHLu: // 414
    return "ARMISD::VSHLu";
  case ARMISD::VSHLIMM: // 415
    return "ARMISD::VSHLIMM";
  case ARMISD::VSHRsIMM: // 416
    return "ARMISD::VSHRsIMM";
  case ARMISD::VSHRuIMM: // 417
    return "ARMISD::VSHRuIMM";
  case ARMISD::VRSHRsIMM: // 418
    return "ARMISD::VRSHRsIMM";
  case ARMISD::VRSHRuIMM: // 419
    return "ARMISD::VRSHRuIMM";
  case ARMISD::VRSHRNIMM: // 420
    return "ARMISD::VRSHRNIMM";
  case ARMISD::VQSHLsIMM: // 421
    return "ARMISD::VQSHLsIMM";
  case ARMISD::VQSHLuIMM: // 422
    return "ARMISD::VQSHLuIMM";
  case ARMISD::VQSHLsuIMM: // 423
    return "ARMISD::VQSHLsuIMM";
  case ARMISD::VQSHRNsIMM: // 424
    return "ARMISD::VQSHRNsIMM";
  case ARMISD::VQSHRNuIMM: // 425
    return "ARMISD::VQSHRNuIMM";
  case ARMISD::VQSHRNsuIMM: // 426
    return "ARMISD::VQSHRNsuIMM";
  case ARMISD::VQRSHRNsIMM: // 427
    return "ARMISD::VQRSHRNsIMM";
  case ARMISD::VQRSHRNuIMM: // 428
    return "ARMISD::VQRSHRNuIMM";
  case ARMISD::VQRSHRNsuIMM: // 429
    return "ARMISD::VQRSHRNsuIMM";
  case ARMISD::VSLIIMM: // 430
    return "ARMISD::VSLIIMM";
  case ARMISD::VSRIIMM: // 431
    return "ARMISD::VSRIIMM";
  case ARMISD::VGETLANEu: // 432
    return "ARMISD::VGETLANEu";
  case ARMISD::VGETLANEs: // 433
    return "ARMISD::VGETLANEs";
  case ARMISD::VMOVIMM: // 434
    return "ARMISD::VMOVIMM";
  case ARMISD::VMVNIMM: // 435
    return "ARMISD::VMVNIMM";
  case ARMISD::VMOVFPIMM: // 436
    return "ARMISD::VMOVFPIMM";
  case ARMISD::VMOVrh: // 437
    return "ARMISD::VMOVrh";
  case ARMISD::VMOVhr: // 438
    return "ARMISD::VMOVhr";
  case ARMISD::VDUP: // 439
    return "ARMISD::VDUP";
  case ARMISD::VDUPLANE: // 440
    return "ARMISD::VDUPLANE";
  case ARMISD::VEXT: // 441
    return "ARMISD::VEXT";
  case ARMISD::VREV64: // 442
    return "ARMISD::VREV64";
  case ARMISD::VREV32: // 443
    return "ARMISD::VREV32";
  case ARMISD::VREV16: // 444
    return "ARMISD::VREV16";
  case ARMISD::VZIP: // 445
    return "ARMISD::VZIP";
  case ARMISD::VUZP: // 446
    return "ARMISD::VUZP";
  case ARMISD::VTRN: // 447
    return "ARMISD::VTRN";
  case ARMISD::VTBL1: // 448
    return "ARMISD::VTBL1";
  case ARMISD::VTBL2: // 449
    return "ARMISD::VTBL2";
  case ARMISD::VMOVN: // 450
    return "ARMISD::VMOVN";
  case ARMISD::VQMOVNs: // 451
    return "ARMISD::VQMOVNs";
  case ARMISD::VQMOVNu: // 452
    return "ARMISD::VQMOVNu";
  case ARMISD::VCVTN: // 453
    return "ARMISD::VCVTN";
  case ARMISD::VCVTL: // 454
    return "ARMISD::VCVTL";
  case ARMISD::VIDUP: // 455
    return "ARMISD::VIDUP";
  case ARMISD::VMULLs: // 456
    return "ARMISD::VMULLs";
  case ARMISD::VMULLu: // 457
    return "ARMISD::VMULLu";
  case ARMISD::VQDMULH: // 458
    return "ARMISD::VQDMULH";
  case ARMISD::VADDVs: // 459
    return "ARMISD::VADDVs";
  case ARMISD::VADDVu: // 460
    return "ARMISD::VADDVu";
  case ARMISD::VADDVps: // 461
    return "ARMISD::VADDVps";
  case ARMISD::VADDVpu: // 462
    return "ARMISD::VADDVpu";
  case ARMISD::VADDLVs: // 463
    return "ARMISD::VADDLVs";
  case ARMISD::VADDLVu: // 464
    return "ARMISD::VADDLVu";
  case ARMISD::VADDLVAs: // 465
    return "ARMISD::VADDLVAs";
  case ARMISD::VADDLVAu: // 466
    return "ARMISD::VADDLVAu";
  case ARMISD::VADDLVps: // 467
    return "ARMISD::VADDLVps";
  case ARMISD::VADDLVpu: // 468
    return "ARMISD::VADDLVpu";
  case ARMISD::VADDLVAps: // 469
    return "ARMISD::VADDLVAps";
  case ARMISD::VADDLVApu: // 470
    return "ARMISD::VADDLVApu";
  case ARMISD::VMLAVs: // 471
    return "ARMISD::VMLAVs";
  case ARMISD::VMLAVu: // 472
    return "ARMISD::VMLAVu";
  case ARMISD::VMLAVps: // 473
    return "ARMISD::VMLAVps";
  case ARMISD::VMLAVpu: // 474
    return "ARMISD::VMLAVpu";
  case ARMISD::VMLALVs: // 475
    return "ARMISD::VMLALVs";
  case ARMISD::VMLALVu: // 476
    return "ARMISD::VMLALVu";
  case ARMISD::VMLALVps: // 477
    return "ARMISD::VMLALVps";
  case ARMISD::VMLALVpu: // 478
    return "ARMISD::VMLALVpu";
  case ARMISD::VMLALVAs: // 479
    return "ARMISD::VMLALVAs";
  case ARMISD::VMLALVAu: // 480
    return "ARMISD::VMLALVAu";
  case ARMISD::VMLALVAps: // 481
    return "ARMISD::VMLALVAps";
  case ARMISD::VMLALVApu: // 482
    return "ARMISD::VMLALVApu";
  case ARMISD::VMINVu: // 483
    return "ARMISD::VMINVu";
  case ARMISD::VMINVs: // 484
    return "ARMISD::VMINVs";
  case ARMISD::VMAXVu: // 485
    return "ARMISD::VMAXVu";
  case ARMISD::VMAXVs: // 486
    return "ARMISD::VMAXVs";
  case ARMISD::SMULWB: // 487
    return "ARMISD::SMULWB";
  case ARMISD::SMULWT: // 488
    return "ARMISD::SMULWT";
  case ARMISD::UMLAL: // 489
    return "ARMISD::UMLAL";
  case ARMISD::SMLAL: // 490
    return "ARMISD::SMLAL";
  case ARMISD::UMAAL: // 491
    return "ARMISD::UMAAL";
  case ARMISD::SMLALBB: // 492
    return "ARMISD::SMLALBB";
  case ARMISD::SMLALBT: // 493
    return "ARMISD::SMLALBT";
  case ARMISD::SMLALTB: // 494
    return "ARMISD::SMLALTB";
  case ARMISD::SMLALTT: // 495
    return "ARMISD::SMLALTT";
  case ARMISD::SMLALD: // 496
    return "ARMISD::SMLALD";
  case ARMISD::SMLALDX: // 497
    return "ARMISD::SMLALDX";
  case ARMISD::SMLSLD: // 498
    return "ARMISD::SMLSLD";
  case ARMISD::SMLSLDX: // 499
    return "ARMISD::SMLSLDX";
  case ARMISD::SMMLAR: // 500
    return "ARMISD::SMMLAR";
  case ARMISD::SMMLSR: // 501
    return "ARMISD::SMMLSR";
  case ARMISD::QADD8b: // 502
    return "ARMISD::QADD8b";
  case ARMISD::QSUB8b: // 503
    return "ARMISD::QSUB8b";
  case ARMISD::QADD16b: // 504
    return "ARMISD::QADD16b";
  case ARMISD::QSUB16b: // 505
    return "ARMISD::QSUB16b";
  case ARMISD::BUILD_VECTOR: // 506
    return "ARMISD::BUILD_VECTOR";
  case ARMISD::BFI: // 507
    return "ARMISD::BFI";
  case ARMISD::VORRIMM: // 508
    return "ARMISD::VORRIMM";
  case ARMISD::VBICIMM: // 509
    return "ARMISD::VBICIMM";
  case ARMISD::VBSP: // 510
    return "ARMISD::VBSP";
  case ARMISD::MEMCPY: // 511
    return "ARMISD::MEMCPY";
  case ARMISD::MEMCPYLOOP: // 512
    return "ARMISD::MEMCPYLOOP";
  case ARMISD::MEMSETLOOP: // 513
    return "ARMISD::MEMSETLOOP";
  case ARMISD::CSINV: // 514
    return "ARMISD::CSINV";
  case ARMISD::CSNEG: // 515
    return "ARMISD::CSNEG";
  case ARMISD::CSINC: // 516
    return "ARMISD::CSINC";

  // NOTE: EXT_ARMISD warnings are intentionally left in
  // for now until the entire enum is removed.
  case EXT_ARMISD::BX_RET: // 551
    return "EXT_ARMISD::BX_RET";
  case EXT_ARMISD::BRD: // 552
    return "EXT_ARMISD::BRD";
  case EXT_ARMISD::LOAD: // 553
    return "EXT_ARMISD::LOAD";
  case EXT_ARMISD::STORE: // 554
    return "EXT_ARMISD::STORE";
  case EXT_ARMISD::MSR: // 555
    return "EXT_ARMISD::MSR";
  case EXT_ARMISD::MRS: // 556
    return "EXT_ARMISD::MRS";
  case EXT_ARMISD::RSB: // 557
    return "EXT_ARMISD::RSB";
  case EXT_ARMISD::RSC: // 558
    return "EXT_ARMISD::RSC";
  case EXT_ARMISD::SBC: // 559
    return "EXT_ARMISD::SBC";
  case EXT_ARMISD::TEQ: // 560
    return "EXT_ARMISD::TEQ";
  case EXT_ARMISD::TST: // 561
    return "EXT_ARMISD::TST";
  case EXT_ARMISD::BIC: // 562
    return "EXT_ARMISD::BIC";
  case EXT_ARMISD::MLA: // 563
    return "EXT_ARMISD::MLA";
  case EXT_ARMISD::UXTB: // 564
    return "EXT_ARMISD::UXTB";

  case ARMISD::VLD1DUP: // 850
    return "ARMISD::VLD1DUP";
  case ARMISD::VLD2DUP: // 851
    return "ARMISD::VLD2DUP";
  case ARMISD::VLD3DUP: // 852
    return "ARMISD::VLD3DUP";
  case ARMISD::VLD4DUP: // 853
    return "ARMISD::VLD4DUP";
  case ARMISD::VLD1_UPD: // 854
    return "ARMISD::VLD1_UPD";
  case ARMISD::VLD2_UPD: // 855
    return "ARMISD::VLD2_UPD";
  case ARMISD::VLD3_UPD: // 856
    return "ARMISD::VLD3_UPD";
  case ARMISD::VLD4_UPD: // 857
    return "ARMISD::VLD4_UPD";
  case ARMISD::VLD2LN_UPD: // 858
    return "ARMISD::VLD2LN_UPD";
  case ARMISD::VLD3LN_UPD: // 859
    return "ARMISD::VLD3LN_UPD";
  case ARMISD::VLD4LN_UPD: // 860
    return "ARMISD::VLD4LN_UPD";
  case ARMISD::VLD1DUP_UPD: // 861
    return "ARMISD::VLD1DUP_UPD";
  case ARMISD::VLD2DUP_UPD: // 862
    return "ARMISD::VLD2DUP_UPD";
  case ARMISD::VLD3DUP_UPD: // 863
    return "ARMISD::VLD3DUP_UPD";
  case ARMISD::VLD4DUP_UPD: // 864
    return "ARMISD::VLD4DUP_UPD";
  case ARMISD::VLD1x2_UPD: // 865
    return "ARMISD::VLD1x2_UPD";
  case ARMISD::VLD1x3_UPD: // 866
    return "ARMISD::VLD1x3_UPD";
  case ARMISD::VLD1x4_UPD: // 867
    return "ARMISD::VLD1x4_UPD";
  case ARMISD::VST1_UPD: // 868
    return "ARMISD::VST1_UPD";
  case ARMISD::VST2_UPD: // 869
    return "ARMISD::VST2_UPD";
  case ARMISD::VST3_UPD: // 870
    return "ARMISD::VST3_UPD";
  case ARMISD::VST4_UPD: // 871
    return "ARMISD::VST4_UPD";
  case ARMISD::VST2LN_UPD: // 872
    return "ARMISD::VST2LN_UPD";
  case ARMISD::VST3LN_UPD: // 873
    return "ARMISD::VST3LN_UPD";
  case ARMISD::VST4LN_UPD: // 874
    return "ARMISD::VST4LN_UPD";
  case ARMISD::VST1x2_UPD: // 875
    return "ARMISD::VST1x2_UPD";
  case ARMISD::VST1x3_UPD: // 876
    return "ARMISD::VST1x3_UPD";
  case ARMISD::VST1x4_UPD: // 877
    return "ARMISD::VST1x4_UPD";
  case ARMISD::LDRD: // 878
    return "ARMISD::LDRD";
  case ARMISD::STRD: // 879
    return "ARMISD::STRD";
  }
}

IREmitter::IREmitter(BasicBlock *bb, DAGRaisingInfo *DAGInfo,
                     FunctionRaisingInfo *funcInfo)
    : FT(bb->getParent()), BB(bb), CurBB(bb), DAGInfo(DAGInfo),
      DAG(&DAGInfo->getCurDAG()), CTX(DAG->getContext()), FuncInfo(funcInfo),
      DLT(funcInfo->DLT), MR(funcInfo->MR), IRB(bb) {}

Value *IREmitter::getIRValue(SDValue val) {
  SDNode *N = val.getNode();

  if (ConstantSDNode::classof(N))
    return const_cast<ConstantInt *>(
        (static_cast<ConstantSDNode *>(N))->getConstantIntValue());

  return DAGInfo->NPMap[N]->Val;
}

static const std::vector<StringRef> CPSR({"N_Flag", "Z_Flag", "C_Flag",
                                          "V_Flag"});

// Match condition state, make corresponding processing.
void IREmitter::emitCondCode(unsigned CondValue, BasicBlock *BB,
                             BasicBlock *IfBB, BasicBlock *ElseBB) {
  switch (CondValue) {
  default:
    break;
  case ARMCC::EQ: { // EQ  Z set
    Value *Z_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[1]);
    Value *InstEQ = IRB.CreateICmpEQ(Z_Flag, IRB.getTrue());
    IRB.CreateCondBr(InstEQ, IfBB, ElseBB);
  } break;
  case ARMCC::NE: { // NE Z clear
    Value *Z_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[1]);
    Value *InstEQ = IRB.CreateICmpEQ(Z_Flag, IRB.getFalse());
    IRB.CreateCondBr(InstEQ, IfBB, ElseBB);
  } break;
  case ARMCC::HS: { // CS  C set
    Value *C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
    Value *InstEQ = IRB.CreateICmpEQ(C_Flag, IRB.getTrue());
    IRB.CreateCondBr(InstEQ, IfBB, ElseBB);
  } break;
  case ARMCC::LO: { // CC  C clear
    Value *C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
    Value *InstEQ = IRB.CreateICmpEQ(C_Flag, IRB.getFalse());
    IRB.CreateCondBr(InstEQ, IfBB, ElseBB);
  } break;
  case ARMCC::MI: { // MI  N set
    Value *N_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[0]);
    Value *InstEQ = IRB.CreateICmpEQ(N_Flag, IRB.getTrue());
    IRB.CreateCondBr(InstEQ, IfBB, ElseBB);
  } break;
  case ARMCC::PL: { // PL  N clear
    Value *N_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[0]);
    Value *InstEQ = IRB.CreateICmpEQ(N_Flag, IRB.getFalse());
    IRB.CreateCondBr(InstEQ, IfBB, ElseBB);
  } break;
  case ARMCC::VS: { // VS  V set
    Value *V_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[3]);
    Value *InstEQ = IRB.CreateICmpEQ(V_Flag, IRB.getTrue());
    IRB.CreateCondBr(InstEQ, IfBB, ElseBB);
  } break;
  case ARMCC::VC: { // VC  V clear
    Value *V_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[3]);
    Value *InstEQ = IRB.CreateICmpEQ(V_Flag, IRB.getFalse());
    IRB.CreateCondBr(InstEQ, IfBB, ElseBB);
  } break;
  case ARMCC::HI: { // HI  C set & Z clear
    Value *C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
    Value *Z_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[1]);
    Value *InstCEQ = IRB.CreateICmpEQ(C_Flag, IRB.getTrue());
    Value *InstZEQ = IRB.CreateICmpEQ(Z_Flag, IRB.getFalse());
    Value *CondPass = IRB.CreateICmpEQ(InstCEQ, InstZEQ);
    IRB.CreateCondBr(CondPass, IfBB, ElseBB);
  } break;
  case ARMCC::LS: { // LS  C clear or Z set
    Value *C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
    Value *Z_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[1]);
    Value *InstCEQ = IRB.CreateICmpEQ(C_Flag, IRB.getFalse());
    Value *InstZEQ = IRB.CreateICmpEQ(Z_Flag, IRB.getTrue());
    Value *CondPass = IRB.CreateXor(InstCEQ, InstZEQ);
    IRB.CreateCondBr(CondPass, IfBB, ElseBB);
  } break;
  case ARMCC::GE: { // GE  N = V
    Value *N_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[0]);
    Value *V_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[3]);
    Value *InstEQ = IRB.CreateICmpEQ(N_Flag, V_Flag);
    IRB.CreateCondBr(InstEQ, IfBB, ElseBB);
  } break;
  case ARMCC::LT: { // LT  N != V
    Value *N_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[0]);
    Value *V_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[3]);
    Value *InstNE = IRB.CreateICmpNE(N_Flag, V_Flag);
    IRB.CreateCondBr(InstNE, IfBB, ElseBB);
  } break;
  case ARMCC::GT: { // GT  Z clear & N = V
    Value *N_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[0]);
    Value *Z_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[1]);
    Value *V_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[3]);
    Value *InstZEQ = IRB.CreateICmpEQ(Z_Flag, IRB.getFalse());
    Value *InstNZEQ = IRB.CreateICmpEQ(N_Flag, V_Flag);
    Value *CondPass = IRB.CreateICmpEQ(InstZEQ, InstNZEQ);
    IRB.CreateCondBr(CondPass, IfBB, ElseBB);
  } break;
  case ARMCC::LE: { // LE  Z set or N != V
    Value *N_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[0]);
    Value *Z_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[1]);
    Value *V_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[3]);
    Value *InstZEQ = IRB.CreateICmpEQ(Z_Flag, IRB.getTrue());
    Value *InstNZNE = IRB.CreateICmpNE(N_Flag, V_Flag);
    Value *CondPass = IRB.CreateXor(InstZEQ, InstNZNE);
    IRB.CreateCondBr(CondPass, IfBB, ElseBB);
  } break;
  case ARMCC::AL: { // AL
    assert(false && "Emit conditional code [ARMCC::AL]. Should not get here!");
  } break;
  }
}

/// Create PHINode for value use selection when running.
PHINode *IREmitter::createAndEmitPHINode(SDNode *Node, BasicBlock *BB,
                                         BasicBlock *IfBB, BasicBlock *ElseBB,
                                         Instruction *IfInst) {
  PHINode *phi = PHINode::Create(getDefaultType(), 2, "", ElseBB);

  if (FuncInfo->ArgValMap.count(FuncInfo->NodeRegMap[Node]) > 0) {
    phi->addIncoming(FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]], BB);
  } else {
    ConstantInt *zero = ConstantInt::get(getDefaultType(), 0, true);
    Instruction *ti = BB->getTerminator();
    Value *p = BinaryOperator::CreateAdd(zero, zero, "", ti);
    phi->addIncoming(p, BB);
  }

  phi->addIncoming(IfInst, IfBB);
  return phi;
}

/// Update the N Z C V flags of global variable.
/// Implement AddWithCarry of encoding of instruction.
/// AddWithCarry(Operand0, Operand1, Flag);
void IREmitter::emitCPSR(Value *Operand0, Value *Operand1, BasicBlock *BB,
                         unsigned Flag) {
  Module &M = *MR->getModule();
  Type *Ty = IRB.getInt1Ty();
  Type *OperandTy = getDefaultType();
  Function *F_Signed =
      Intrinsic::getDeclaration(&M, Intrinsic::sadd_with_overflow, OperandTy);
  Function *F_Unsigned =
      Intrinsic::getDeclaration(&M, Intrinsic::uadd_with_overflow, OperandTy);
  Value *Args[] = {Operand0, Operand1};
  Value *Unsigned_Sum;
  Value *Signed_Sum;
  if (Flag) {
    Value *OperandFlag = IRB.CreateAdd(Operand0, IRB.getInt32(1));
    Value *Args_Flag[] = {Operand1, OperandFlag};
    Unsigned_Sum = IRB.CreateCall(F_Unsigned, Args_Flag);
    Signed_Sum = IRB.CreateCall(F_Signed, Args_Flag);
  } else {
    Unsigned_Sum = IRB.CreateCall(F_Unsigned, Args);
    Signed_Sum = IRB.CreateCall(F_Signed, Args);
  }

  Value *Sum = ExtractValueInst::Create(Unsigned_Sum, 0, "", BB);
  Value *Result = Sum;
  // Update the corresponding flags.
  // Update N flag.
  Value *N_Flag = IRB.CreateLShr(Result, IRB.getInt32(31));
  Value *NTrunc = IRB.CreateTrunc(N_Flag, Ty);
  IRB.CreateStore(NTrunc, FuncInfo->AllocaMap[0]);

  // Update Z flag.
  Value *Z_Flag = IRB.CreateICmpEQ(Result, IRB.getInt32(0));
  Value *ZTrunc = IRB.CreateTrunc(Z_Flag, Ty);
  IRB.CreateStore(ZTrunc, FuncInfo->AllocaMap[1]);

  // Update C flag.
  Value *C_Flag = ExtractValueInst::Create(Unsigned_Sum, 1, "", BB);
  IRB.CreateStore(C_Flag, FuncInfo->AllocaMap[2]);

  // Update V flag.
  Value *V_Flag = ExtractValueInst::Create(Signed_Sum, 1, "", BB);
  IRB.CreateStore(V_Flag, FuncInfo->AllocaMap[3]);
}

void IREmitter::emitSpecialCPSR(Value *Result, BasicBlock *BB, unsigned Flag) {
  Type *Ty = IRB.getInt1Ty();
  // Update N flag.
  Value *N_Flag = IRB.CreateLShr(Result, IRB.getInt32(31));
  N_Flag = IRB.CreateTrunc(N_Flag, Ty);
  IRB.CreateStore(N_Flag, FuncInfo->AllocaMap[0]);
  // Update Z flag.
  Value *Z_Flag = IRB.CreateICmpEQ(Result, IRB.getInt32(0));

  IRB.CreateStore(Z_Flag, FuncInfo->AllocaMap[1]);
}

Type *IREmitter::getIntTypeByPtr(Type *pty) {
  assert(pty && pty->isPointerTy() && "The input type is not a pointer!");
  Type *ty = nullptr;

  if (pty == Type::getInt64PtrTy(*CTX))
    ty = Type::getInt64Ty(*CTX);
  else if (pty == Type::getInt32PtrTy(*CTX))
    ty = Type::getInt32Ty(*CTX);
  else if (pty == Type::getInt16PtrTy(*CTX))
    ty = Type::getInt16Ty(*CTX);
  else if (pty == Type::getInt8PtrTy(*CTX))
    ty = Type::getInt8Ty(*CTX);
  else if (pty == Type::getInt1PtrTy(*CTX))
    ty = Type::getInt1Ty(*CTX);
  else
    ty = getDefaultType();

  return ty;
}

void IREmitter::emitBinaryCPSR(Value *Inst, BasicBlock *BB, unsigned Opcode,
                               SDNode *Node) {
  Value *S0 = getIRValue(Node->getOperand(0));
  Value *S1 = getIRValue(Node->getOperand(1));

  switch (Opcode) {
  case Instruction::Add: {
    emitCPSR(S0, S1, BB, 0);
  } break;
  case Instruction::Sub: {
    Value *InstNot = nullptr;
    if (ConstantSDNode::classof(Node->getOperand(1).getNode())) {
      Value *InstTp = IRB.CreateSub(S0, S0);
      Value *InstAdd = IRB.CreateAdd(InstTp, S1);
      InstNot = IRB.CreateNot(InstAdd);
    } else {
      InstNot = IRB.CreateNot(S1, "");
    }
    emitCPSR(S0, InstNot, BB, 1);
  } break;
  case Instruction::And: {
    emitSpecialCPSR(Inst, BB, 0);
  } break;
  case Instruction::Mul: {
    emitSpecialCPSR(Inst, BB, 0);
  } break;
  case Instruction::Or: {
    emitSpecialCPSR(Inst, BB, 0);
    /* How to deal with C Flag? */
  } break;
  case Instruction::Xor: {
    emitSpecialCPSR(Inst, BB, 0);
    /* How to deal with C Flag? */
  } break;
  case Instruction::Shl: {
    emitSpecialCPSR(Inst, BB, 0);

    // Update C flag.
    // extended_x = x : Zeros(shift), c flag = extend_x[N];
    // c flag = (s0 lsl (s1 -1))[31]
    Type *Ty = IRB.getInt1Ty();
    Value *Val = cast<Value>(ConstantInt::get(getDefaultType(), 1, true));
    Value *C_Flag = IRB.CreateSub(S1, Val);
    C_Flag = IRB.CreateShl(S0, C_Flag);
    C_Flag = IRB.CreateLShr(C_Flag, IRB.getInt32(31));
    Value *CTrunc = IRB.CreateTrunc(C_Flag, Ty);

    IRB.CreateStore(CTrunc, FuncInfo->AllocaMap[2]);
  } break;
  case Instruction::LShr: {
    emitSpecialCPSR(Inst, BB, 0);

    // Update C flag.
    // c flag = (s0 lsr (s1 -1))[0]
    Type *Ty = IRB.getInt1Ty();
    Value *Val = cast<Value>(ConstantInt::get(getDefaultType(), 1, true));
    Value *C_Flag = IRB.CreateSub(S1, Val);
    C_Flag = IRB.CreateLShr(S0, C_Flag);
    C_Flag = IRB.CreateAnd(C_Flag, Val);
    Value *CTrunc = IRB.CreateTrunc(C_Flag, Ty);

    IRB.CreateStore(CTrunc, FuncInfo->AllocaMap[2]);
  } break;
  case Instruction::AShr: {
    emitSpecialCPSR(Inst, BB, 0);

    // Update C flag.
    // c flag = (s0 asr (s1 -1))[0]
    Type *Ty = IRB.getInt1Ty();
    Value *Val = ConstantInt::get(getDefaultType(), 1, true);
    Value *C_Flag = IRB.CreateSub(S1, Val);
    C_Flag = IRB.CreateAShr(S0, C_Flag);
    C_Flag = IRB.CreateAnd(C_Flag, Val);
    Value *CTrunc = IRB.CreateTrunc(C_Flag, Ty);
    IRB.CreateStore(CTrunc, FuncInfo->AllocaMap[2]);
  } break;
  }
}

// Extract the offset of MachineInstr MI from the Metadata operand.
static uint64_t getMCInstIndex(const MachineInstr &MI) {
  unsigned NumExpOps = MI.getNumExplicitOperands();
  const MachineOperand &MO = MI.getOperand(NumExpOps);
  assert(MO.isMetadata() &&
         "Unexpected non-metadata operand in branch instruction!");
  const MDNode *MDN = MO.getMetadata();
  // Unwrap metadata of the instruction to get the MCInstIndex of
  // the MCInst corresponding to this MachineInstr.
  ConstantAsMetadata *CAM = dyn_cast<ConstantAsMetadata>(MDN->getOperand(0));
  assert(CAM != nullptr && "Unexpected metadata type!");
  Constant *CV = CAM->getValue();
  ConstantInt *CI = dyn_cast<ConstantInt>(CV);
  assert(CI != nullptr && "Unexpected metadata constant type!");
  APInt ArbPrecInt = CI->getValue();
  return ArbPrecInt.getSExtValue();
}

void IREmitter::emitMachineOpcodeNode(SDNode *Node) {
  if (Node->isTargetOpcode()) {
    ARMISD::NodeType ARMISDOpcode = (ARMISD::NodeType)Node->getOpcode();
    WithColor(errs(), HighlightColor::Error)
        << "emitMachineOpcodeNode should not be called for TargetOpcode "
        << ARMISDOpcode << " (" << format("%04x", ARMISDOpcode) << "); "
        << getARMISDName(ARMISDOpcode) << "\n";
    return;
  }
  if (!Node->isMachineOpcode()) {
    const TargetInstrInfo *TII = FuncInfo->MF->getSubtarget().getInstrInfo();
    unsigned ARMISDOpcode = Node->getOpcode();
    WithColor(errs(), HighlightColor::Error)
        << "emitMachineOpcodeNode should not be called for target-independent "
           "opcode "
        << ARMISDOpcode << " (" << format("%04x", ARMISDOpcode) << "); "
        << (ARMISDOpcode < TII->getNumOpcodes() ? TII->getName(ARMISDOpcode)
                                                : "UNKNOWN")
        << "\n";
    return;
  }

  unsigned ARMOpcode = Node->getMachineOpcode();

  switch (ARMOpcode) {
  default: {
    const TargetInstrInfo *TII = FuncInfo->MF->getSubtarget().getInstrInfo();
    WithColor(errs(), HighlightColor::Error)
        << "emitMachineOpcodeNode called for MachineOpcode " << ARMOpcode
        << " (" << format("%04x", ARMOpcode) << "); "
        << (ARMOpcode < TII->getNumOpcodes() ? TII->getName(ARMOpcode)
                                             : "UNKNOWN")
        << "\n";
    assert(false && "Unsupported opcode");
  }
  case ARM::BX_RET: // 718
  case ARM::DMB:    // 773
  case ARM::DSB:    // 774
  case ARM::ISB:    // 793
  case ARM::LDAEX:  // 796 v8 LL
  case ARM::STLEX:  // 1887 v8 SC
  case ARM::LDREX:  // 836 v6/v7 LL
  case ARM::STREX:  // 837 v6/v7 SC
  case ARM::SVC:    // 1932
    break;
  }
  return;
}

void IREmitter::emitTargetDependentNode(SDNode *Node) {
  if (Node->isMachineOpcode()) {
    const TargetInstrInfo *TII = FuncInfo->MF->getSubtarget().getInstrInfo();
    unsigned ARMISDOpcode = Node->getMachineOpcode();
    WithColor(errs(), HighlightColor::Error)
        << "emitTargetDependentNode should not be called for MachineOpcode "
        << ARMISDOpcode << " (" << format("%04x", ARMISDOpcode) << "); "
        << (ARMISDOpcode < TII->getNumOpcodes() ? TII->getName(ARMISDOpcode)
                                                : "UNKNOWN")
        << "\n";
    return;
  }
  if (!Node->isTargetOpcode()) {
    const TargetInstrInfo *TII = FuncInfo->MF->getSubtarget().getInstrInfo();
    ARMISD::NodeType ARMISDOpcode = (ARMISD::NodeType)Node->getOpcode();
    WithColor(errs(), HighlightColor::Error)
        << "emitTargetDependentNode should not be called for "
           "target-independent Opcode "
        << ARMISDOpcode << " (" << format("%04x", ARMISDOpcode) << "); "
        << (ARMISDOpcode < TII->getNumOpcodes() ? TII->getName(ARMISDOpcode)
                                                : "UNKNOWN")
        << "\n";
    return;
  }

  Module &M = *MR->getModule();
  BasicBlock *BB = getBlock();

  ARMISD::NodeType ARMISDOpcode = (ARMISD::NodeType)Node->getOpcode();
  switch (ARMISDOpcode) {
  default: {
    WithColor(errs(), HighlightColor::Error)
        << "emitTargetDependentNode called for TargetOpcode " << ARMISDOpcode
        << " (" << format("%04x", ARMISDOpcode) << "); "
        << getARMISDName(ARMISDOpcode) << "\n";
    assert(false && "Unsupported opcode");
  }

  case ARMISD::CMN: { // 370
    assert(false && "ARMISD::CMN is untested");
    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    Value *Inst;
    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
      BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
      emitCondCode(CondValue, BB, IfBB, ElseBB);
      Value *Inst = BinaryOperator::CreateAdd(S0, S1);
      IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
      PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                          dyn_cast<Instruction>(Inst));
      DAGInfo->NPMap[Node]->Val = Phi;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
      emitCPSR(S0, S1, IfBB, 0);
      IRB.CreateBr(ElseBB);
      IRB.SetInsertPoint(ElseBB);
    } else {
      Inst = IRB.CreateAdd(S0, S1);
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
      emitCPSR(S0, S1, BB, 0);
    }
  } break;
  case ARMISD::CMOV: { // 377
    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      if (!(DAGInfo->NPMap[Node]->UpdateCPSR)) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateAdd(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        IRB.SetInsertPoint(IfBB);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else if (DAGInfo->NPMap[Node]->Special) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateAdd(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        emitBinaryCPSR(Inst, IfBB, Instruction::Add, Node);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else {
        Value *Inst = IRB.CreateAdd(S0, S1);
        DAGInfo->NPMap[Node]->Val = Inst;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
        emitBinaryCPSR(Inst, BB, Instruction::Add, Node);
      }
    } else {
      Value *Inst = BinaryOperator::CreateAdd(S0, S1);
      BasicBlock *CBB = IRB.GetInsertBlock();
      CBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case ARMISD::RRX: { // 384
    Value *S0 = getIRValue(Node->getOperand(0));
    Type *Ty = getDefaultType();
    Value *Val1 = ConstantInt::get(Ty, 1, true);
    Value *Val2 = ConstantInt::get(Ty, 31, true);
    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;

      if (DAGInfo->NPMap[Node]->UpdateCPSR) {
        Value *InstLShr = IRB.CreateLShr(S0, Val1);
        Value *C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
        C_Flag = IRB.CreateZExt(C_Flag, Ty);
        Value *Bit31 = IRB.CreateShl(C_Flag, Val2);
        Value *Inst = IRB.CreateAdd(InstLShr, Bit31);
        DAGInfo->NPMap[Node]->Val = Inst;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;

        /**************************************/
        emitSpecialCPSR(Inst, BB, 0);
        // Update C flag.
        // c flag = s0[0]
        C_Flag = IRB.CreateAnd(S0, Val1);
        IRB.CreateStore(C_Flag, FuncInfo->AllocaMap[2]);
      } else {
        // Create new BB for EQ instructin exectute.
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        // Create new BB to update the DAG BB.
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());

        // Emit the condition code.
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        IRB.SetInsertPoint(IfBB);
        Value *InstLShr = IRB.CreateLShr(S0, Val1);
        Value *C_Flag = nullptr;

        C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
        C_Flag = IRB.CreateZExt(C_Flag, Ty);
        Value *Bit31 = IRB.CreateShl(C_Flag, Val2);
        Value *Inst = IRB.CreateAdd(InstLShr, Bit31);
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      }
    } else {
      Value *InstLShr = IRB.CreateLShr(S0, Val1);
      Value *C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
      C_Flag = IRB.CreateZExt(C_Flag, Ty);
      Value *Bit31 = IRB.CreateShl(C_Flag, Val2);
      Value *Inst = IRB.CreateAdd(InstLShr, Bit31);
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case EXT_ARMISD::BX_RET: { // 551
    assert(false && "EXT_ARMISD::BX_RET is untested");

    Value *Ret = getIRValue(Node->getOperand(0));
    if (!ConstantSDNode::classof(Node->getOperand(0).getNode()))
      IRB.CreateRet(Ret);
    else
      IRB.CreateRetVoid();
  } break;
  case EXT_ARMISD::BRD: { // 552
    // Get the function call Index.
    uint64_t Index = Node->getConstantOperandVal(0);
    // Get function from ModuleRaiser.
    Function *CallFunc = MR->getRaisedFunctionAt(Index);
    unsigned IFFuncArgNum = 0; // The argument number which gets from analyzing
                               // variadic function prototype.
    bool IsSyscall = false;
    if (CallFunc == nullptr) {
      // According MI to get BL instruction address.
      // uint64_t callAddr = DAGInfo->NPMap[Node]->InstAddr;
      uint64_t CallAddr = MR->getTextSectionAddress() +
                          getMCInstIndex(*(DAGInfo->NPMap[Node]->MI));
      Function *IndefiniteFunc = MR->getCallFunc(CallAddr);
      CallFunc = MR->getSyscallFunc(Index);
      if (CallFunc != nullptr && IndefiniteFunc != nullptr) {
        IFFuncArgNum = MR->getFunctionArgNum(CallAddr);
        IsSyscall = true;
      }
    }
    assert(CallFunc && "Failed to get called function!");
    // Get argument number from callee.
    unsigned ArgNum = CallFunc->arg_size();
    if (IFFuncArgNum > ArgNum)
      ArgNum = IFFuncArgNum;
    Argument *CalledFuncArgs = CallFunc->arg_begin();
    std::vector<Value *> CallInstFuncArgs;
    CallInst *Inst = nullptr;
    if (ArgNum > 0) {
      Value *ArgVal = nullptr;
      const MachineFrameInfo &MFI = FuncInfo->MF->getFrameInfo();
      unsigned StackArg = 0; // Initialize argument size on stack to 0.
      if (ArgNum > 4) {
        StackArg = ArgNum - 4;

        unsigned StackNum = MFI.getNumObjects() - 2;
        if (StackNum > StackArg)
          StackArg = StackNum;
      }
      for (unsigned i = 0; i < ArgNum; i++) {
        if (i < 4)
          ArgVal = FuncInfo->ArgValMap[ARM::R0 + i];
        else {
          const Value *StackAlloc =
              MFI.getObjectAllocation(StackArg - i - 4 + 1);
          ArgVal = CallCreateAlignedLoad(
              const_cast<Value *>(StackAlloc),
              MaybeAlign(Log2(DLT->getPointerPrefAlignment())));
        }
        if (IsSyscall && i < CallFunc->arg_size() &&
            ArgVal->getType() != CalledFuncArgs[i].getType()) {
          CastInst *CInst = CastInst::Create(
              CastInst::getCastOpcode(ArgVal, false,
                                      CalledFuncArgs[i].getType(), false),
              ArgVal, CalledFuncArgs[i].getType());
          IRB.GetInsertBlock()->getInstList().push_back(CInst);
          ArgVal = CInst;
        }
        CallInstFuncArgs.push_back(ArgVal);
      }
      Inst = IRB.CreateCall(CallFunc, ArrayRef<Value *>(CallInstFuncArgs));
    } else
      Inst = IRB.CreateCall(CallFunc);

    DAGInfo->NPMap[Node]->Val = Inst;
  } break;
  case EXT_ARMISD::LOAD: { // 553
    Value *S = getIRValue(Node->getOperand(0));
    Value *Ptr = nullptr;
    if (S->getType()->isPointerTy())
      Ptr = S;
    else
      Ptr = IRB.CreateIntToPtr(
          S, Node->getValueType(0).getTypeForEVT(*CTX)->getPointerTo());

    Value *Inst = nullptr;
    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      // Create new BB for EQ instructin exectute.
      BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
      // Create new BB to update the DAG BB.
      BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());

      // Emit the condition code.
      emitCondCode(CondValue, BB, IfBB, ElseBB);
      IRB.SetInsertPoint(IfBB);
      if (GlobalVariable::classof(Ptr))
        Inst = IRB.CreatePtrToInt(Ptr, getDefaultType());
      else
        Inst = CallCreateAlignedLoad(
            Ptr, MaybeAlign(Log2(DLT->getPointerPrefAlignment())));

      PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                          dyn_cast<Instruction>(Inst));
      DAGInfo->NPMap[Node]->Val = Phi;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;

      IRB.CreateBr(ElseBB);
      IRB.SetInsertPoint(ElseBB);
    } else {
      if (GlobalVariable::classof(Ptr)) {
        // Inst = IRB.CreatePtrToInt(Ptr, getDefaultType());
        Inst = new PtrToIntInst(Ptr, getDefaultType(), "", BB);
      } else {
        Inst = CallCreateAlignedLoad(
            Ptr, MaybeAlign(Log2(DLT->getPointerPrefAlignment())));

        // TODO:
        // Temporary method for this.
        if (Inst->getType() == Type::getInt64Ty(*CTX))
          Inst = IRB.CreateTrunc(Inst, getDefaultType());
        else if (Inst->getType() != getDefaultType())
          Inst = IRB.CreateSExt(Inst, getDefaultType());
      }

      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case EXT_ARMISD::STORE: { // 554
    Node->dumpr();
    Value *Val = getIRValue(Node->getOperand(0));
    Value *S = getIRValue(Node->getOperand(1));
    Value *Ptr = nullptr;
    Type *Nty = Node->getValueType(0).getTypeForEVT(*CTX);

    if (Val->getType() != Nty) {
      Val = IRB.CreateTrunc(Val, Nty);
    }

    if (S->getType()->isPointerTy()) {
      if (S->getType() != Nty->getPointerTo()) {
        Ptr = IRB.CreateBitCast(S, Nty->getPointerTo());
      } else {
        Ptr = S;
      }
    } else {
      Ptr = IRB.CreateIntToPtr(S, Nty->getPointerTo());
    }

    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      // Create new BB for EQ instructin exectute.
      BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
      // Create new BB to update the DAG BB.
      BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());

      // Emit the condition code.
      emitCondCode(CondValue, BB, IfBB, ElseBB);
      IRB.SetInsertPoint(IfBB);

      IRB.CreateAlignedStore(Val, Ptr,
                             MaybeAlign(Log2(DLT->getPointerPrefAlignment())));

      IRB.CreateBr(ElseBB);
      IRB.SetInsertPoint(ElseBB);
    } else {
      IRB.CreateAlignedStore(Val, Ptr,
                             MaybeAlign(Log2(DLT->getPointerPrefAlignment())));
    }
  } break;
  case EXT_ARMISD::MSR: { // 555
    assert(false && "EXT_ARMISD::MSR is untested");

    Value *Cond = getIRValue(Node->getOperand(0));
    // 1 1 1 1
    // N set 1 0 0 0   8
    // Z set 0 1 0 0   4
    // C set 0 0 1 0   2
    // Z set 0 0 0 1   1
    IRB.CreateStore(Cond, dyn_cast<Value>(M.getGlobalVariable("Reserved")));
    // Pattern msr CPSR_f, Rn
    if (1) {
      Value *Shift_Num = IRB.getInt32(28);
      Value *Shift = IRB.CreateLShr(Cond, Shift_Num);
      // Update N Flag.
      Value *N_Cmp = IRB.getInt32(8);
      Value *N_Flag = IRB.CreateICmpEQ(Shift, N_Cmp);
      IRB.CreateStore(N_Flag, FuncInfo->AllocaMap[0]);
      // Update Z Flag.
      Value *Z_Cmp = IRB.getInt32(4);
      Value *Z_Flag = IRB.CreateICmpEQ(Shift, Z_Cmp);
      IRB.CreateStore(Z_Flag, FuncInfo->AllocaMap[1]);
      // Update C Flag.
      Value *C_Cmp = IRB.getInt32(2);
      Value *C_Flag = IRB.CreateICmpEQ(Shift, C_Cmp);
      IRB.CreateStore(C_Flag, FuncInfo->AllocaMap[2]);
      // Update V Flag.
      Value *V_Cmp = IRB.getInt32(1);
      Value *V_Flag = IRB.CreateICmpEQ(Shift, V_Cmp);
      IRB.CreateStore(V_Flag, FuncInfo->AllocaMap[3]);
    } else {
      // Pattern msr CSR_f, #const.
    }
  } break;
  case EXT_ARMISD::MRS: { // 556
    assert(false && "EXT_ARMISD::MRS is untested");

    Value *Rn = getIRValue(Node->getOperand(0));
    // Reserved || N_Flag << 31 || Z_Flag << 30 || C_Flag << 29 || V_Flag << 28
    PointerType *PtrTy = PointerType::getInt32PtrTy(*CTX);
    Type *Ty = Type::getInt32Ty(*CTX);

    Value *BitNShift = IRB.getInt32(31);
    Value *BitZShift = IRB.getInt32(30);
    Value *BitCShift = IRB.getInt32(29);
    Value *BitVShift = IRB.getInt32(28);

    Value *N_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[0]);
    Value *Z_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[1]);
    Value *C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
    Value *V_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[3]);

    N_Flag = IRB.CreateZExt(N_Flag, Ty);
    Z_Flag = IRB.CreateZExt(Z_Flag, Ty);
    C_Flag = IRB.CreateZExt(C_Flag, Ty);
    V_Flag = IRB.CreateZExt(V_Flag, Ty);

    Value *N_Shift = IRB.CreateShl(N_Flag, BitNShift);
    Value *Z_Shift = IRB.CreateShl(Z_Flag, BitZShift);
    Value *C_Shift = IRB.CreateShl(C_Flag, BitCShift);
    Value *V_Shift = IRB.CreateShl(V_Flag, BitVShift);
    Value *NZ_Val = IRB.CreateAdd(N_Shift, Z_Shift);
    Value *CV_Val = IRB.CreateAdd(C_Shift, V_Shift);
    Value *NZCV_Val = IRB.CreateAdd(NZ_Val, CV_Val);
    Value *Reserved =
        CallCreateAlignedLoad(dyn_cast<Value>(M.getGlobalVariable("Reserved")));

    Value *CPSR_Val = IRB.CreateAdd(NZCV_Val, Reserved);
    Value *Rn_Ptr = IRB.CreateIntToPtr(Rn, PtrTy);
    Value *RnStore = IRB.CreateStore(CPSR_Val, Rn_Ptr);

    DAGInfo->NPMap[Node]->Val = RnStore;
    FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = RnStore;
  } break;
  case EXT_ARMISD::RSB: { // 557
    assert(false && "EXT_ARMISD::RSB is untested");

    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));

    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      if (DAGInfo->NPMap[Node]->UpdateCPSR) {
        // Create add emit.
        Value *Inst = IRB.CreateSub(S0, S1);
        DAGInfo->NPMap[Node]->Val = Inst;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;

        Value *InstNot = IRB.CreateNot(S1);
        emitCPSR(InstNot, S0, BB, 1);
      } else {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateSub(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        IRB.SetInsertPoint(IfBB);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      }
    } else {
      Value *Inst = IRB.CreateSub(S0, S1);
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case EXT_ARMISD::RSC: { // 558
    assert(false && "EXT_ARMISD::RSC is untested");

    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));

    Value *C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
    Value *CZext = IRB.CreateZExt(C_Flag, getDefaultType());

    Value *Inst = IRB.CreateAdd(S0, CZext);
    Inst = IRB.CreateSub(S1, Inst);
    DAGInfo->NPMap[Node]->Val = Inst;
    FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
  } break;
  case EXT_ARMISD::SBC: { // 559
    assert(false && "EXT_ARMISD::SBC is untested");

    Value *S1 = getIRValue(Node->getOperand(0));
    Value *S2 = getIRValue(Node->getOperand(1));
    Type *Ty = getDefaultType();

    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;

      if (DAGInfo->NPMap[Node]->UpdateCPSR) {
        Value *InstSub = IRB.CreateSub(S1, S2);
        Value *C_Flag = nullptr;
        C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
        Value *CZext = IRB.CreateZExt(C_Flag, Ty);
        Value *InstSBC = IRB.CreateAdd(InstSub, CZext);
        DAGInfo->NPMap[Node]->Val = InstSBC;
        Value *InstNot = IRB.CreateNot(S2);
        if (1)
          emitCPSR(S1, InstNot, BB, 0);
        else
          emitCPSR(S1, InstNot, BB, 1);
      } else {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());

        emitCondCode(CondValue, BB, IfBB, ElseBB);

        IRB.SetInsertPoint(IfBB);
        Value *InstSub = IRB.CreateSub(S1, S2);
        Value *C_Flag = nullptr;
        C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
        Value *CZext = IRB.CreateZExt(C_Flag, Ty);
        Value *Inst = IRB.CreateAdd(InstSub, CZext);
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;

        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      }
    } else {
      Value *InstSub = IRB.CreateSub(S1, S2);
      Value *C_Flag = nullptr;
      C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
      Value *CZext = IRB.CreateZExt(C_Flag, Ty);
      Value *InstSBC = IRB.CreateAdd(InstSub, CZext);
      DAGInfo->NPMap[Node]->Val = InstSBC;
    }
  } break;
  case EXT_ARMISD::TEQ: { // 560
    assert(false && "EXT_ARMISD::TEQ is untested");

    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));

    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      // Create new BB for EQ instructin exectute.
      BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
      // Create new BB to update the DAG BB.
      BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());

      // TODO:
      // This instruction not change def, consider phi later.

      emitCondCode(CondValue, BB, IfBB, ElseBB);
      IRB.SetInsertPoint(IfBB);
      Value *Inst = IRB.CreateXor(S0, S1);
      emitSpecialCPSR(Inst, IfBB, 0);
      IRB.CreateBr(ElseBB);
      IRB.SetInsertPoint(ElseBB);
    } else {
      Value *Inst = IRB.CreateXor(S0, S1);
      emitSpecialCPSR(Inst, BB, 0);
    }
  } break;
  case EXT_ARMISD::TST: { // 561
    assert(false && "EXT_ARMISD::TST is untested");

    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));

    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      // Create new BB for EQ instructin exectute.
      BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
      // Create new BB to update the DAG BB.
      BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());

      // TODO:
      // Not change def. Consider how to use PHI.
      // PHINode *Phi = createAndEmitPHINode(Node, BB, ElseBB);

      emitCondCode(CondValue, BB, IfBB, ElseBB);
      IRB.SetInsertPoint(IfBB);
      Value *Inst = IRB.CreateAnd(S0, S1);
      emitSpecialCPSR(Inst, IfBB, 0);
      IRB.CreateBr(ElseBB);
      IRB.SetInsertPoint(ElseBB);
    } else {
      Value *Inst = IRB.CreateAnd(S0, S1);
      emitSpecialCPSR(Inst, BB, 0);
    }
  } break;
  case EXT_ARMISD::BIC: { // 562
    assert(false && "EXT_ARMISD::BIC is untested");

    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    Type *tp = getDefaultType();
    Value *val = ConstantInt::get(tp, -1, true);

    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;

      if (DAGInfo->NPMap[Node]->UpdateCPSR) {
        Value *InstXor = IRB.CreateXor(val, S1);
        Value *Inst = IRB.CreateAnd(S0, InstXor);

        DAGInfo->NPMap[Node]->Val = Inst;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;

        emitSpecialCPSR(Inst, BB, 0);
        // Update C flag.
        // C flag not change.

        // Update V flag.
        // unchanged.
      } else {
        // Create new BB for EQ instructin exectute.
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        // Create new BB to update the DAG BB.
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        // Emit the condition code.
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        IRB.SetInsertPoint(IfBB);
        Value *InstXor = IRB.CreateXor(val, S1);
        Value *Inst = IRB.CreateAnd(S0, InstXor);
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;

        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      }
    } else {
      Value *InstXor, *Inst;
      InstXor = IRB.CreateXor(val, S1);
      Inst = IRB.CreateAnd(S0, InstXor);
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case EXT_ARMISD::MLA: { // 563
    assert(false && "EXT_ARMISD::MLA is untested");

    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    Value *S2 = getIRValue(Node->getOperand(2));

    Value *InstMul = IRB.CreateMul(S0, S1);
    Value *Inst = IRB.CreateAdd(InstMul, S2);

    DAGInfo->NPMap[Node]->Val = Inst;
    FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
  } break;
  case EXT_ARMISD::UXTB: { // 564
    assert(false && "EXT_ARMISD::UXTB is untested");

    Value *S1 = getIRValue(Node->getOperand(1));
    Value *Rotation = getIRValue(Node->getOperand(2));
    Value *ror_val = ConstantInt::get(getDefaultType(), 8, true);
    Value *add_val = ConstantInt::get(getDefaultType(), 0, true);
    Value *and_val = ConstantInt::get(getDefaultType(), 0xff, true);
    Value *Inst_mul = IRB.CreateMul(Rotation, ror_val);
    Value *Inst_lshr = IRB.CreateLShr(S1, Inst_mul);
    Value *Inst_add = IRB.CreateAdd(Inst_lshr, add_val);
    Value *Inst_and = IRB.CreateAnd(Inst_add, and_val);
    DAGInfo->NPMap[Node]->Val = Inst_and;
  } break;
  }
}

void IREmitter::emitTargetIndependentNode(SDNode *Node) {
  if (Node->isMachineOpcode()) {
    const TargetInstrInfo *TII = FuncInfo->MF->getSubtarget().getInstrInfo();
    unsigned ARMISDOpcode = Node->getMachineOpcode();
    WithColor(errs(), HighlightColor::Error)
        << "emitTargetIndependentNode should not be called for MachineOpcode "
        << ARMISDOpcode << " (" << format("%04x", ARMISDOpcode) << "); "
        << (ARMISDOpcode < TII->getNumOpcodes() ? TII->getName(ARMISDOpcode)
                                                : "UNKNOWN")
        << "\n";
    return;
  }
  if (Node->isTargetOpcode()) {
    ARMISD::NodeType ARMISDOpcode = (ARMISD::NodeType)Node->getOpcode();
    WithColor(errs(), HighlightColor::Error)
        << "emitTargetIndependentNode should not be called for TargetOpcode "
        << ARMISDOpcode << " (" << format("%04x", ARMISDOpcode) << "); "
        << getARMISDName(ARMISDOpcode) << "\n";
    return;
  }

  BasicBlock *BB = getBlock();
  BasicBlock *CurBB = getCurBlock();

  ISD::NodeType ISDOpcode = (ISD::NodeType)Node->getOpcode();
  switch (ISDOpcode) {
  default: {
    WithColor(errs(), HighlightColor::Error)
        << "emitTargetIndependentNode called for opcode " << ISDOpcode << " ("
        << format("%04x", ISDOpcode) << "); " << Node->getOperationName()
        << "\n";
    assert(false && "Unsupported opcode");
  }

  case ISD::EntryToken:     // 1
  case ISD::Register:       // 9
  case ISD::Constant:       // 11
  case ISD::FrameIndex:     // 15
  case ISD::ExternalSymbol: // 18
    break;
  case ISD::ADD: { // 55
    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      if (!(DAGInfo->NPMap[Node]->UpdateCPSR)) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateAdd(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        IRB.SetInsertPoint(IfBB);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else if (DAGInfo->NPMap[Node]->Special) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateAdd(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        emitBinaryCPSR(Inst, IfBB, Instruction::Add, Node);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else {
        Value *Inst = IRB.CreateAdd(S0, S1);
        DAGInfo->NPMap[Node]->Val = Inst;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
        emitBinaryCPSR(Inst, BB, Instruction::Add, Node);
      }
    } else {
      Value *Inst = BinaryOperator::CreateAdd(S0, S1);
      BasicBlock *CBB = IRB.GetInsertBlock();
      CBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case ISD::SUB: { // 56
    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      if (!(DAGInfo->NPMap[Node]->UpdateCPSR)) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateSub(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        IRB.SetInsertPoint(IfBB);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else if (DAGInfo->NPMap[Node]->Special) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateSub(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        emitBinaryCPSR(Inst, IfBB, Instruction::Sub, Node);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else {
        Value *Inst = IRB.CreateSub(S0, S1);
        DAGInfo->NPMap[Node]->Val = Inst;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
        emitBinaryCPSR(Inst, BB, Instruction::Sub, Node);
      }
    } else {
      Value *Inst = BinaryOperator::CreateSub(S0, S1);
      BasicBlock *CBB = IRB.GetInsertBlock();
      CBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case ISD::MUL: { // 57
    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      if (!(DAGInfo->NPMap[Node]->UpdateCPSR)) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateMul(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        IRB.SetInsertPoint(IfBB);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else if (DAGInfo->NPMap[Node]->Special) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateMul(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        emitBinaryCPSR(Inst, IfBB, Instruction::Mul, Node);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else {
        Value *Inst = IRB.CreateMul(S0, S1);
        DAGInfo->NPMap[Node]->Val = Inst;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
        emitBinaryCPSR(Inst, BB, Instruction::Mul, Node);
      }
    } else {
      Value *Inst = BinaryOperator::CreateMul(S0, S1);
      BasicBlock *CBB = IRB.GetInsertBlock();
      CBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case ISD::ADDC: { // 67
    assert(false && "ISD::ADDC is untested");

    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    Type *OperandTy = getDefaultType();

    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;

      if (DAGInfo->NPMap[Node]->UpdateCPSR) {
        // Create add emit.
        Value *C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
        Value *Result = IRB.CreateAdd(S0, S1);
        Value *CZext = IRB.CreateZExt(C_Flag, OperandTy);
        Value *InstADC = IRB.CreateAdd(Result, CZext);
        DAGInfo->NPMap[Node]->Val = InstADC;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] =
            dyn_cast<Instruction>(InstADC);

        // Update CPSR.
        // TODO:
        // Should consider how to do this.
        if (1)
          emitCPSR(S0, S1, BB, 1);
        else
          emitCPSR(S0, S1, BB, 0);
      } else {
        // Create new BB for EQ instructin exectute.
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        // Create new BB to update the DAG BB.
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());

        // Emit the condition code.
        emitCondCode(CondValue, BB, IfBB, ElseBB);

        Value *C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
        IRB.SetInsertPoint(IfBB);
        Value *InstAdd = IRB.CreateAdd(S0, S1);
        Value *CZext = IRB.CreateZExtOrTrunc(C_Flag, OperandTy);
        Value *Inst = IRB.CreateAdd(InstAdd, CZext);
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;

        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      }
    } else {
      Value *C_Flag = CallCreateAlignedLoad(FuncInfo->AllocaMap[2]);
      Value *Inst = IRB.CreateAdd(S0, S1);
      Value *CTrunc = IRB.CreateZExtOrTrunc(C_Flag, getDefaultType());
      Value *InstADC = IRB.CreateAdd(Inst, CTrunc);

      DAGInfo->NPMap[Node]->Val = InstADC;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = InstADC;
    }
  } break;
  case ISD::AND: { // 165
    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      if (!(DAGInfo->NPMap[Node]->UpdateCPSR)) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateAnd(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        IRB.SetInsertPoint(IfBB);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else if (DAGInfo->NPMap[Node]->Special) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateAnd(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        emitBinaryCPSR(Inst, IfBB, Instruction::And, Node);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else {
        Value *Inst = IRB.CreateAnd(S0, S1);
        DAGInfo->NPMap[Node]->Val = Inst;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
        emitBinaryCPSR(Inst, BB, Instruction::And, Node);
      }
    } else {
      Value *Inst = BinaryOperator::CreateAnd(S0, S1);
      BasicBlock *CBB = IRB.GetInsertBlock();
      CBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case ISD::OR: { // 166
    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      if (!(DAGInfo->NPMap[Node]->UpdateCPSR)) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateOr(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        IRB.SetInsertPoint(IfBB);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else if (DAGInfo->NPMap[Node]->Special) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateOr(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        emitBinaryCPSR(Inst, IfBB, Instruction::Or, Node);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else {
        Value *Inst = IRB.CreateOr(S0, S1);
        DAGInfo->NPMap[Node]->Val = Inst;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
        emitBinaryCPSR(Inst, BB, Instruction::Or, Node);
      }
    } else {
      Value *Inst = BinaryOperator::CreateOr(S0, S1);
      BasicBlock *CBB = IRB.GetInsertBlock();
      CBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case ISD::XOR: { // 167
    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      if (!(DAGInfo->NPMap[Node]->UpdateCPSR)) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateXor(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        IRB.SetInsertPoint(IfBB);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else if (DAGInfo->NPMap[Node]->Special) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateXor(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        emitBinaryCPSR(Inst, IfBB, Instruction::Xor, Node);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else {
        Value *Inst = IRB.CreateXor(S0, S1);
        DAGInfo->NPMap[Node]->Val = Inst;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
        emitBinaryCPSR(Inst, BB, Instruction::Xor, Node);
      }
    } else {
      Value *Inst = BinaryOperator::CreateXor(S0, S1);
      BasicBlock *CBB = IRB.GetInsertBlock();
      CBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case ISD::SHL: { // 169
    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      if (!(DAGInfo->NPMap[Node]->UpdateCPSR)) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateShl(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        IRB.SetInsertPoint(IfBB);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else if (DAGInfo->NPMap[Node]->Special) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateShl(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        emitBinaryCPSR(Inst, IfBB, Instruction::Shl, Node);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else {
        Value *Inst = IRB.CreateShl(S0, S1);
        DAGInfo->NPMap[Node]->Val = Inst;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
        emitBinaryCPSR(Inst, BB, Instruction::Shl, Node);
      }
    } else {
      Value *Inst = BinaryOperator::CreateShl(S0, S1);
      BasicBlock *CBB = IRB.GetInsertBlock();
      CBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case ISD::SRA: { // 170
    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      if (!(DAGInfo->NPMap[Node]->UpdateCPSR)) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateAShr(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        IRB.SetInsertPoint(IfBB);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else if (DAGInfo->NPMap[Node]->Special) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateAShr(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        emitBinaryCPSR(Inst, IfBB, Instruction::AShr, Node);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else {
        Value *Inst = IRB.CreateAShr(S0, S1);
        DAGInfo->NPMap[Node]->Val = Inst;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
        emitBinaryCPSR(Inst, BB, Instruction::AShr, Node);
      }
    } else {
      Value *Inst = BinaryOperator::CreateAShr(S0, S1);
      BasicBlock *CBB = IRB.GetInsertBlock();
      CBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case ISD::SRL: { // 171
    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;
      if (!(DAGInfo->NPMap[Node]->UpdateCPSR)) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateLShr(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        IRB.SetInsertPoint(IfBB);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else if (DAGInfo->NPMap[Node]->Special) {
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        Value *Inst = BinaryOperator::CreateLShr(S0, S1);
        IfBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        emitBinaryCPSR(Inst, IfBB, Instruction::LShr, Node);
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      } else {
        Value *Inst = IRB.CreateLShr(S0, S1);
        DAGInfo->NPMap[Node]->Val = Inst;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
        emitBinaryCPSR(Inst, BB, Instruction::LShr, Node);
      }
    } else {
      Value *Inst = BinaryOperator::CreateLShr(S0, S1);
      BasicBlock *CBB = IRB.GetInsertBlock();
      CBB->getInstList().push_back(dyn_cast<Instruction>(Inst));
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case ISD::ROTR: { // 173
    Value *S0 = getIRValue(Node->getOperand(0));
    Value *S1 = getIRValue(Node->getOperand(1));
    Type *Ty = getDefaultType();
    Value *Val = ConstantInt::get(Ty, 32, true);

    if (DAGInfo->NPMap[Node]->HasCPSR) {
      unsigned CondValue = DAGInfo->NPMap[Node]->Cond;

      if (DAGInfo->NPMap[Node]->UpdateCPSR) {
        Value *InstSub = IRB.CreateSub(Val, S1);
        Value *InstLShr = IRB.CreateLShr(S0, S1);
        Value *InstShl = IRB.CreateShl(S0, InstSub);
        Value *Inst = IRB.CreateOr(InstLShr, InstShl);
        DAGInfo->NPMap[Node]->Val = Inst;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;

        emitSpecialCPSR(Inst, BB, 0);
      } else {
        // Create new BB for EQ instructin exectute.
        BasicBlock *IfBB = BasicBlock::Create(*CTX, "", BB->getParent());
        // Create new BB to update the DAG BB.
        BasicBlock *ElseBB = BasicBlock::Create(*CTX, "", BB->getParent());

        // Emit the condition code.
        emitCondCode(CondValue, BB, IfBB, ElseBB);
        IRB.SetInsertPoint(IfBB);
        Value *InstSub = IRB.CreateSub(Val, S1);
        Value *InstLShr = IRB.CreateLShr(S0, S1);
        Value *InstShl = IRB.CreateShl(S0, InstSub);
        Value *Inst = IRB.CreateOr(InstLShr, InstShl);
        PHINode *Phi = createAndEmitPHINode(Node, BB, IfBB, ElseBB,
                                            dyn_cast<Instruction>(Inst));
        DAGInfo->NPMap[Node]->Val = Phi;
        FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Phi;
        IRB.CreateBr(ElseBB);
        IRB.SetInsertPoint(ElseBB);
      }
    } else {
      Value *InstSub = IRB.CreateSub(Val, S1);
      Value *InstLShr = IRB.CreateLShr(S0, S1);
      Value *InstShl = IRB.CreateShl(S0, InstSub);
      Value *Inst = IRB.CreateOr(InstLShr, InstShl);
      DAGInfo->NPMap[Node]->Val = Inst;
      FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
    }
  } break;
  case ISD::CTLZ: { // 178
    assert(false && "ISD::CTLZ is untested");
    Value *S0 = getIRValue(Node->getOperand(0));
    Function *CTLZ = Intrinsic::getDeclaration(BB->getParent()->getParent(),
                                               Intrinsic::ctlz, S0->getType());
    Type *i1_type = llvm::IntegerType::getInt1Ty(*CTX);
    Value *is_zero_undef = ConstantInt::get(i1_type, true, true);

    std::vector<Value *> Vec;
    Vec.push_back(S0);
    Vec.push_back(is_zero_undef);
    ArrayRef<Value *> Args(Vec);

    Value *Inst = IRB.CreateCall(CTLZ, Args);
    DAGInfo->NPMap[Node]->Val = Inst;
    FuncInfo->ArgValMap[FuncInfo->NodeRegMap[Node]] = Inst;
  } break;
  case ISD::SETCC: { // 187
    Value *LHS = getIRValue(Node->getOperand(0));
    Value *RHS = getIRValue(Node->getOperand(1));

    Value *InstNot = nullptr;
    if (ConstantSDNode::classof(Node->getOperand(1).getNode())) {
      Value *InstTp = IRB.CreateSub(LHS, LHS);
      Value *InstAdd = IRB.CreateAdd(InstTp, RHS);
      InstNot = IRB.CreateNot(InstAdd);
    } else {
      InstNot = IRB.CreateNot(RHS);
    }
    emitCPSR(LHS, InstNot, BB, 1);
  } break;
  case ISD::BR: { // 250
    // br label %xxx
    MachineBasicBlock *LMBB = FuncInfo->MBBMap[CurBB];
    MachineBasicBlock::succ_iterator SuI = LMBB->succ_begin();
    if (SuI != LMBB->succ_end()) {
      BasicBlock *BrDest = FuncInfo->getOrCreateBasicBlock(*SuI);
      IRB.CreateBr(BrDest);
      break;
    }

    // Get the function call Index.
    uint64_t Index = Node->getConstantOperandVal(0);
    // Get function from ModuleRaiser.
    Function *CallFunc = MR->getRaisedFunctionAt(Index);
    unsigned IFFuncArgNum = 0; // The argument number which gets from analyzing
                               // variadic function prototype.
    bool IsSyscall = false;
    if (CallFunc == nullptr) {
      // According MI to get BL instruction address.
      // uint64_t callAddr = DAGInfo->NPMap[Node]->InstAddr;
      uint64_t CallAddr = MR->getTextSectionAddress() +
                          getMCInstIndex(*(DAGInfo->NPMap[Node]->MI));
      Function *IndefiniteFunc = MR->getCallFunc(CallAddr);
      CallFunc = MR->getSyscallFunc(Index);
      if (CallFunc != nullptr && IndefiniteFunc != nullptr) {
        IFFuncArgNum = MR->getFunctionArgNum(CallAddr);
        IsSyscall = true;
      }
    }
    assert(CallFunc && "Failed to get called function!");
    // Get argument number from callee.
    unsigned ArgNum = CallFunc->arg_size();
    if (IFFuncArgNum > ArgNum)
      ArgNum = IFFuncArgNum;
    Argument *CalledFuncArgs = CallFunc->arg_begin();
    std::vector<Value *> CallInstFuncArgs;
    CallInst *Inst = nullptr;
    if (ArgNum > 0) {
      Value *ArgVal = nullptr;
      const MachineFrameInfo &MFI = FuncInfo->MF->getFrameInfo();
      unsigned StackArg = 0; // Initialize argument size on stack to 0.
      if (ArgNum > 4) {
        StackArg = ArgNum - 4;

        unsigned StackNum = MFI.getNumObjects() - 2;
        if (StackNum > StackArg)
          StackArg = StackNum;
      }
      for (unsigned i = 0; i < ArgNum; i++) {
        if (i < 4)
          ArgVal = FuncInfo->ArgValMap[ARM::R0 + i];
        else {
          const Value *StackAlloc =
              MFI.getObjectAllocation(StackArg - i - 4 + 1);
          ArgVal = CallCreateAlignedLoad(
              const_cast<Value *>(StackAlloc),
              MaybeAlign(Log2(DLT->getPointerPrefAlignment())));
        }
        if (IsSyscall && i < CallFunc->arg_size() &&
            ArgVal->getType() != CalledFuncArgs[i].getType()) {
          CastInst *CInst = CastInst::Create(
              CastInst::getCastOpcode(ArgVal, false,
                                      CalledFuncArgs[i].getType(), false),
              ArgVal, CalledFuncArgs[i].getType());
          IRB.GetInsertBlock()->getInstList().push_back(CInst);
          ArgVal = CInst;
        }
        CallInstFuncArgs.push_back(ArgVal);
      }
      Inst = IRB.CreateCall(CallFunc, ArrayRef<Value *>(CallInstFuncArgs));
    } else
      Inst = IRB.CreateCall(CallFunc);

    DAGInfo->NPMap[Node]->Val = Inst;
  } break;
  case ISD::BRIND: { // 251
    Value *Func = getIRValue(Node->getOperand(0));
    unsigned NumDests = Node->getNumOperands();
    IRB.CreateIndirectBr(Func, NumDests);
  } break;
  case ISD::BR_JT: { // 252
    if (jtList.size() > 0) {
      MachineBasicBlock *mbb = FuncInfo->MBBMap[CurBB];
      MachineFunction *MF = mbb->getParent();

      std::vector<JumpTableBlock> JTCases;
      const MachineJumpTableInfo *MJT = MF->getJumpTableInfo();
      unsigned jtIndex = Node->getConstantOperandVal(0);
      std::vector<MachineJumpTableEntry> JumpTables = MJT->getJumpTables();
      for (unsigned j = 0, f = JumpTables[jtIndex].MBBs.size(); j != f; ++j) {
        llvm::Type *i32_type = llvm::IntegerType::getInt32Ty(*CTX);
        llvm::ConstantInt *i32_val =
            cast<ConstantInt>(llvm::ConstantInt::get(i32_type, j, true));
        MachineBasicBlock *Succ = JumpTables[jtIndex].MBBs[j];
        ConstantInt *CaseVal = i32_val;
        JTCases.push_back(std::make_pair(CaseVal, Succ));
      }
      // main->getEntryBlock().setName("entry");

      unsigned int numCases = JTCases.size();
      BasicBlock *def_bb =
          FuncInfo->getOrCreateBasicBlock(jtList[jtIndex].df_MBB);

      BasicBlock *cd_bb =
          FuncInfo->getOrCreateBasicBlock(jtList[jtIndex].conditionMBB);

      Instruction *cdi = nullptr;
      for (BasicBlock::iterator DI = cd_bb->begin(); DI != cd_bb->end(); DI++) {
        Instruction *ins = dyn_cast<Instruction>(DI);
        if (isa<LoadInst>(DI) && !cdi) {
          cdi = ins;
        }

        if (cdi && (ins->getOpcode() == Instruction::Sub)) {
          if (isa<ConstantInt>(ins->getOperand(1))) {
            ConstantInt *opr = dyn_cast<ConstantInt>(ins->getOperand(1));
            if (opr->uge(0)) {
              cdi = ins;
            }
          }
        }
      }

      SwitchInst *Inst = IRB.CreateSwitch(cdi, def_bb, numCases);
      for (unsigned i = 0, e = numCases; i != e; ++i) {
        BasicBlock *case_bb =
            FuncInfo->getOrCreateBasicBlock(JTCases[i].second);
        Inst->addCase(JTCases[i].first, case_bb);
      }
    }
  } break;
  case ISD::BRCOND: { // 253
    unsigned Cond = cast<ConstantSDNode>(Node->getOperand(1))->getZExtValue();
    // br i1 %cmp, label %if.then, label %if.else
    MachineBasicBlock *MBB = FuncInfo->MBBMap[CurBB];
    MachineBasicBlock::succ_iterator SuI = MBB->succ_begin();
    BasicBlock *Iftrue = FuncInfo->getOrCreateBasicBlock(*SuI);
    MachineBasicBlock *NextMBB = &*std::next(MBB->getIterator());
    BasicBlock *NextBB = FuncInfo->getOrCreateBasicBlock(NextMBB);

    emitCondCode(Cond, BB, Iftrue, NextBB);
  } break;
  case ISD::MDNODE_SDNODE: // 272
    break;
  }
}
