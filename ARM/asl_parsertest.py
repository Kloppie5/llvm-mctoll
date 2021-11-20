import asl/ASLToULDLVisitor

with open('./asl_files/decode/ADC_i_A1.asl', 'r') as f:
  input_string = f.read()

ASLToULDLVisitor.ASLToULDLVisitor().generateULDL()
