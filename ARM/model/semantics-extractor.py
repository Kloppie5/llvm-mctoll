from lxml import etree as ET
import re
import os

import generator as Gen
import match_tree as MTree

# get files
def get_files(path, debug = False):
  semantics_files = []
  for root, dirs, files in os.walk(path):
    for file in files:
      if file.endswith(".xml"):
        semantics_files.append(os.path.join(root, file))
  if debug:
    print("Found %d semantics files" % len(semantics_files))
  return semantics_files

def parse_semantics(semantics_file, debug = False):
  xml = ET.parse(semantics_file)
  instructions = []
  if xml.find('.//aliasto') is not None:
    if debug:
      print("Skipping %s" % semantics_file)
    return instructions
  if debug:
    print("Processing %s" % semantics_file)
  for iclass in xml.findall('.//classes/iclass'):
    instruction = {}
    isa = iclass.attrib['isa']
    instruction['isa'] = isa

    pattern = []
    constraints = []
    for box in iclass.find('regdiagram').findall('box'):
      hibit = box.get('hibit')
      width = box.get('width')
      if width is None:
        width = 1
      name = box.get('name')
      if name is None:
        name = "%s/%s" % (hibit, width)
      settings = box.get('settings')
      constraint = box.get('constraint')

      element = {}
      element['name'] = name
      element['hibit'] = hibit
      element['width'] = width
      if settings is not None and constraint is None:
        contents = ''.join(c.text for c in box.findall('c'))
        element['contents'] = contents
      if constraint is not None:
        constraints.append("%s %s" % (name, constraint))
      pattern.append(element)
    instruction['pattern'] = pattern
    instruction['constraints'] = constraints

    mnemonics = set()
    for encoding in iclass.findall('encoding'):
      mnemonic = encoding.xpath('./docvars/docvar[@key="mnemonic"]/@value')[0]
      if mnemonic is not None:
        mnemonics.add(mnemonic)
    instruction['mnemonics'] = mnemonics

    pseudocode_decode = cleanup_ASL(ET.tostring(iclass.find('ps_section/ps/pstext')).decode())
    pseudocode_execute = cleanup_ASL(ET.tostring(xml.find('ps_section/ps/pstext')).decode())

    pseudocode = pseudocode_decode + '\n' + pseudocode_execute
    clean_pseudocode = []
    for line in pseudocode.split('\n'):
      line = line.rstrip('\r')
      line = line.split('//')[0]
      line = line.rstrip(' ')
      if line == "":
        continue
      clean_pseudocode.append(line)
    instruction['pseudocode'] = '\n'.join(clean_pseudocode)

    instructions.append(instruction)
  return instructions

tag_re = re.compile(r'<\/?a[^>]*>')
def cleanup_ASL(asl):
  asl = asl.replace('<pstext mayhavelinks="1" section="Decode" rep_section="decode">', '')
  asl = asl.replace('<pstext mayhavelinks="1" section="Execute" rep_section="execute">', '')
  asl = asl.replace('</pstext>', '')

  asl = asl.replace('&lt;', '<')
  asl = asl.replace('&gt;', '>')
  asl = asl.replace('&amp;', '&')

  asl = tag_re.sub('', asl)

  return asl

def process_semantics(instructions, debug = False):
  if debug:
    print(f"Processing {len(instructions)} instructions")
  tree = MTree.build_tree(instructions, debug)
  MTree.print_tree(tree)
  if debug:
    print(f"  {len(tree)} main patterns")

  tree = MTree.to_byte_tree(tree, debug)
  if debug:
    print(f"  {len(tree)} byte patterns")

  return tree

semantics_files = get_files("./operational_semantics/")

ISAs = {}
for semantics_file in semantics_files:
  instructions = parse_semantics(semantics_file)
  for instruction in instructions:
    if instruction['isa'] not in ISAs:
      ISAs[instruction['isa']] = []
    ISAs[instruction["isa"]].append(instruction)

for isa in ISAs:
  instructions = ISAs[isa]
  tree = process_semantics(instructions)

  disasm = Gen.generate_disasm(tree, f"{isa}")
  disasm = Gen.generate_raiser(instructions, f"{isa}")
