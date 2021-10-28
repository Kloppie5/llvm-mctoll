from lxml import etree as ET
import re
import os

# get files
def get_files(path):
  semantics_files = []
  for root, dirs, files in os.walk(path):
    for file in files:
      if file.endswith(".xml"):
        semantics_files.append(os.path.join(root, file))
  print("Found %d semantics files" % len(semantics_files))
  return semantics_files

def parse_semantics(semantics_file):
  xml = ET.parse(semantics_file)
  instructions = []
  if xml.find('.//aliasto') is not None:
    print("Skipping %s" % semantics_file)
    return instructions
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
      else:
        element['contents'] = '-' * int(width)
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

    instruction['psuedocode'] = (pseudocode_decode, pseudocode_execute)

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

def build_trie(instructions):
  trie = {}
  for instruction in instructions:
    l = trie
    for element in instruction['pattern']:
      cont = element['contents']
      if cont not in l:
        l[cont] = {}
      l = l[cont]
    l['instruction'] = instruction
  return trie

def simplify_trie(trie):
  for key in list(trie.keys()):
    if key == 'instruction':
      continue
    trie[key] = simplify_trie(trie[key])

    if len(trie[key]) == 1:
      subkey = list(trie[key].keys())[0]
      if subkey == 'instruction':
        continue

      trie[key + subkey] = trie[key][subkey]
      del trie[key]
  return trie

def print_trie(trie, indent=0):
  for key in trie:
    if key == 'instruction':
      print("%s%s" % (" " * indent, trie['instruction']['mnemonics']))
    else:
      print("%s%s" % (" " * indent, key.replace('(', '').replace(')', '')))
      print_trie(trie[key], indent + len(key.replace('(', '').replace(')', '')))

semantics_files = get_files("./operational_semantics/")

ISAs = {}
ISAs["A32"] = []
ISAs["T32"] = []
for semantics_file in semantics_files:
  instructions = parse_semantics(semantics_file)
  for instruction in instructions:
    ISAs[instruction["isa"]].append(instruction)

for isa in ISAs:
  print("%s:" % isa)
  trie = build_trie(ISAs[isa])
  print("  %d instructions" % len(ISAs[isa]))
  print_trie(trie)
  trie = simplify_trie(trie)
  print_trie(trie)
